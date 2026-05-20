:- module(dialogue_state, [
    initial_state/1,
    update_state/4,
    state_temperature/2,
    near_catastrophe/1,
    render_state/2
]).

% ═══════════════════════════════════════════════════════════════
% ZCM-inspired dialogue state
% ═══════════════════════════════════════════════════════════════
%
% Tio's metaphor (half-baked, taken in spirit):
%
%   The dialogue is a Zeeman Catastrophe Machine.
%   * Two elastic bands pull on the wheel.
%   * Pulling TOWARD the external fixed point = assessing (necessity,
%     grounding, "what are you thinking?").
%   * Pulling AWAY from the external fixed point = advancing (new
%     possibilities, "now try X").
%   * Friction on the wheel = how entrenched the participant is
%     ("temperature" of the conversation).
%   * Near the cusp = something is about to happen. Could be good (aha
%     insight, strategy generalises) or bad (frustration snap, commitment
%     to a misconception hardens).
%
% Concretely, we carry a 4-tuple per turn:
%
%   state(AssessingPull, AdvancingPull, Temperature, History)
%
% - AssessingPull, AdvancingPull  in [0.0, 1.0], one continues while the
%   other relaxes — they aren't a strict sum-to-one because a confused
%   turn can pull both ("you need to ground AND advance").
% - Temperature in [0.0, 1.0], increases when:
%     * commitments keep firing turn after turn (misconception sticky)
%     * the bot's chosen move tag doesn't align with what the student
%       actually offered (parser missed the read)
% - History is the list of recent (move_tag, commitment_count) pairs
%   the state was built from — bounded to the last 6 turns for
%   computability. Older turns get rolled into the mean pulls.
%
% "near_catastrophe" fires when |AssessingPull - AdvancingPull| is
% small (the two tensions compete) AND Temperature is high. Borrowing
% cusp-catastrophe intuition: dual pull + high internal friction is
% where a sudden state change is likely.

% initial_state(-State)
%   Start of conversation: neutral, cold, empty history.
initial_state(state(0.2, 0.2, 0.0, [])).

% update_state(+PrevState, +MoveTag, +CommitmentCount, -NewState)
%
% PrevState: state/4
% MoveTag  : one of 'FMST','LST','AQST','PS','NONE'
% CommitmentCount: non-negative integer — how many incompatibility
%                   triggers fired on the final answer of this turn
update_state(state(A0, V0, T0, Hist0), MoveTag, CommitCount, state(A, V, T, Hist)) :-
    move_pull(MoveTag, AssessInc, AdvanceInc),
    % Commitments are friction — they raise temperature and, when
    % sticky, dampen whichever pull was attempted.
    commit_effect(CommitCount, HeatInc, Damp),
    A1 is A0 * 0.8 + AssessInc * (1.0 - Damp),
    V1 is V0 * 0.8 + AdvanceInc * (1.0 - Damp),
    clamp(A1, 0.0, 1.0, A),
    clamp(V1, 0.0, 1.0, V),
    % Temperature decays slowly; spikes with commitments; also spikes
    % when consecutive turns fire commitments (see history_stickiness)
    history_stickiness(Hist0, MoveTag, Sticky),
    T1 is T0 * 0.85 + HeatInc + 0.15 * Sticky,
    clamp(T1, 0.0, 1.0, T),
    trim_history([(MoveTag, CommitCount) | Hist0], 6, Hist).

% move_pull(+MoveTag, -AssessingIncrement, -AdvancingIncrement)
%   How much each move type adds to each pull. PS (perturbing) is
%   strong-assessing with a hint of advancing because it disturbs to
%   reveal.
move_pull('FMST', 0.5, 0.05).
move_pull('PS',   0.6, 0.15).
move_pull('LST',  0.1, 0.5).
move_pull('AQST', 0.08, 0.6).
move_pull('NONE', 0.0, 0.0).

% commit_effect(+Count, -HeatIncrement, -PullDamping)
commit_effect(0, 0.0,  0.0).
commit_effect(1, 0.15, 0.1).
commit_effect(2, 0.25, 0.2).
commit_effect(N, 0.35, 0.3) :- N >= 3.

% history_stickiness — a misconception firing repeatedly on the same
% move type is the real friction signal. Two consecutive turns with
% commitments firing on the same move tag counts as sticky.
history_stickiness([(M, C1), (M, C2) | _], _, 1.0) :- C1 > 0, C2 > 0, !.
history_stickiness([(_, C1), (_, C2) | _], _, 0.5) :- C1 > 0, C2 > 0, !.
history_stickiness(_, _, 0.0).

trim_history(L, N, L) :- length(L, K), K =< N, !.
trim_history(L, N, Kept) :- length(Kept, N), append(Kept, _, L).

clamp(X, Lo, _, Lo) :- X < Lo, !.
clamp(X, _, Hi, Hi) :- X > Hi, !.
clamp(X, _, _, X).

% state_temperature(+State, -T)
state_temperature(state(_, _, T, _), T).

% near_catastrophe(+State)
%   Assessing and advancing competing near the cusp while temperature
%   is high. Fires when dual pull + friction is present.
near_catastrophe(state(A, V, T, _)) :-
    Sum is A + V,
    Diff is abs(A - V),
    % Cusp signature is "pull is present on both axes AND friction is
    % doing work." Intuition: a conversation with heavy assessing and
    % heavy advancing at once, with sticky commitments underneath, is
    % on the edge of either breaking open or snapping back.
    Sum > 1.0,
    T > 0.55,
    Diff < 0.5.

% render_state(+State, -AsciiString)
%
%  A 20-col horizontal bar: assessing (left) and advancing (right)
%  pulls, temperature as a third row. '!' markers flag catastrophe
%  vicinity.
render_state(state(A, V, T, _), Text) :-
    bar(A, BarA),
    bar(V, BarV),
    bar(T, BarT),
    ( near_catastrophe(state(A, V, T, _))
    -> Warn = "near cusp — something about to give"
    ;  Warn = ""
    ),
    format(
        string(Text),
        "  assessing  |~w| ~2f~n  advancing  |~w| ~2f~n  friction   |~w| ~2f~n  ~w",
        [BarA, A, BarV, V, BarT, T, Warn]
    ).

bar(X, Bar) :-
    N is round(X * 20),
    length(Fill, N),
    maplist(=(0'#), Fill),
    Rem is 20 - N,
    length(Empty, Rem),
    maplist(=(0'·), Empty),
    append(Fill, Empty, Chars),
    string_codes(Bar, Chars).
