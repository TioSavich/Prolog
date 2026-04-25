:- module(visualization, [
    strategy_jumps/3,
    history_to_dicts/3
]).

:- use_module(library(lists)).

%!  strategy_jumps(+Strategy:atom, +History:list, -Jumps:list) is det.
%
%   Extract number-line jumps from a strategy's execution history.
%   Each jump is a dict: _{from: Int, to: Int, label: String}.
%   Jumps are the consecutive pairs (prev_sum, this_sum) where sum changed.
%   For strategies whose step shape is not yet handled, Jumps = [].
strategy_jumps(Strategy, History, Jumps) :-
    step_sum_trace(Strategy, History, Sums),
    !,
    sums_to_jumps(Sums, Jumps).
strategy_jumps(_, _, []).

%!  step_sum_trace(+Strategy, +History, -Sums) is semidet.
%
%   Per-strategy extraction of the running-sum trajectory. Returns a list
%   of integers in execution order.
step_sum_trace('Chunking', History, Sums) :-
    findall(Sum,
            member(step(state(_, Sum, _, _, _, _, _), _, _), History),
            Sums).
step_sum_trace('COBO', History, Sums) :-
    findall(Sum,
            member(step(_, Sum, _, _, _), History),
            Sums).
step_sum_trace('RMB', History, Sums) :-
    % RMB's history is step(state(Name, A, B, K, AT, BT, TB, B_init), _, Interp).
    % The "sum" users care about is the developing A_temp (AT) which slides
    % up toward the target base; the decomposition phase slides B_temp (BT)
    % down; then the final recombination is A+B. For the number-line view,
    % show AT as the running value.
    findall(AT,
            member(step(state(_, _, _, _, AT, _, _, _), _, _), History),
            Sums).
step_sum_trace('Rounding', History, Sums) :-
    % Addition Rounding. state: (Name, K, AR, TS, R, T, O, TB, BC, OC, A_in, B_in).
    % TS is the running sum through the COBO sub-phase.
    findall(TS,
            member(step(state(_, _, _, TS, _, _, _, _, _, _, _, _), _, _), History),
            Sums).

%% --- Subtraction strategies ------------------------------------------------

step_sum_trace('Sliding', History, Sums) :-
    % state: (Name, K, MSlid, SSlid, TB, S_running, M, S). Show S_running
    % climbing toward TB during the K-calc phase. The final q_adjust step
    % resets S_running to 0 — drop the trailing zero.
    findall(SR,
            member(step(state(_, _, _, _, _, SR, _, _), _, _), History),
            Raw),
    drop_trailing_zero(Raw, Sums).

step_sum_trace('COBO (Missing Addend)', History, Sums) :-
    % state: (Name, S_running, Distance, M_target). S_running climbs from S
    % up to M.
    findall(SR,
            member(step(state(_, SR, _, _), _, _), History),
            Sums).

step_sum_trace('CBBO (Take Away)', History, Sums) :-
    % state: (Name, CurrentVal, BC, OC). CurrentVal falls from M.
    findall(CV,
            member(step(state(_, CV, _, _), _, _), History),
            Sums).

step_sum_trace('Decomposition', History, Sums) :-
    % Legacy step shape: step(StateName, MT, MO, Interpretation). Running
    % value is MT*10 + MO. During decomposition the value is constant.
    findall(V,
            ( member(step(_, MT, MO, _), History),
              V is MT * 10 + MO
            ),
            Sums).

step_sum_trace('Sub Rounding', History, Sums) :-
    % state: (Name, K_M, K_S, RunningResult, _, _, M, S, RoundedM, RoundedS).
    % RunningResult is 0 until the intermediate subtraction; after that it
    % walks toward the final difference. Prepend M so the number line starts
    % at M and slides down to the answer. Drop the leading 0s.
    findall(V,
            member(step(state(_, _, _, V, _, _, _, _, _, _), _, _), History),
            Raw),
    drop_leading_zeros(Raw, Sums).

step_sum_trace('Chunking A', History, Sums) :-
    % state: (Name, CurrentValue, S_Remaining, Subtracted). CurrentValue
    % drops from M by chunks.
    findall(CV,
            member(step(state(_, CV, _, _), _, _), History),
            Sums).

step_sum_trace('Chunking B', History, Sums) :-
    % state: (Name, Current, Distance, DistanceCounter, Chunk, Iter, M).
    % Current climbs from S toward M. Arg 2 = Current.
    findall(C,
            member(step(state(_, C, _, _, _, _, _), _, _), History),
            Sums).

step_sum_trace('Chunking C', History, Sums) :-
    % state: (Name, CurrentValue, Distance, K_inner, TB, IS, S). CurrentValue
    % drops from M by chunks back toward S.
    findall(CV,
            member(step(state(_, CV, _, _, _, _, _), _, _), History),
            Sums).

%% --- Multiplication strategies ---------------------------------------------

step_sum_trace('C2C', History, Sums) :-
    % state: (Name, GroupsDone, ItemInGroup, Total, NumGroups, GroupSize).
    % Total is arg 4. Plot Total (the embodied count climbs by 1 per item).
    findall(T,
            member(step(state(_, _, _, T, _, _), _, _), History),
            Sums).

step_sum_trace('Commutative Reasoning', History, Sums) :-
    % state: (Name, A, B, Total, Counter). Total is arg 4. Counter ticks down,
    % Total grows by B per iteration.
    findall(T,
            member(step(state(_, _, _, T, _), _, _), History),
            Sums).

step_sum_trace('DR', History, Sums) :-
    % state: (Name, S1, S2, P1, P2, Final, _, N, S). The cleanest running
    % value is P1 + P2 + Final. P1 climbs first, then P2, then Final = sum.
    findall(V,
            ( member(step(state(_, _, _, P1, P2, Final, _, _, _), _, _), History),
              V is P1 + P2 + Final
            ),
            Sums).

%% --- Division strategies ---------------------------------------------------

step_sum_trace('Dealing by Ones', History, Sums) :-
    % state: (Name, ItemsRemaining, GroupsList, NextGroup). Plot the largest
    % group size (i.e., max of GroupsList) as it grows.
    findall(M,
            ( member(step(state(_, _, GL, _), _, _), History),
              GL = [_|_], max_list(GL, M)
            ; member(step(state(_, _, GL, _), _, _), History),
              GL == [], M = 0
            ),
            Sums).

step_sum_trace('UCR', History, Sums) :-
    % state: (Name, T_distributed, RoundCount, E, G). T (arg 2) accumulates
    % by G each round, climbing toward E.
    findall(T,
            member(step(state(_, T, _, _, _), _, _), History),
            Sums).

drop_trailing_zero(L, R) :-
    ( append(Front, [Last], L),
      number(Last),
      Last =:= 0
    -> drop_trailing_zero(Front, R)
    ;  R = L
    ).

drop_leading_zeros([], []).
drop_leading_zeros([0 | T], R) :- !, drop_leading_zeros(T, R).
drop_leading_zeros(L, L).

%!  sums_to_jumps(+Sums:list, -Jumps:list) is det.
%
%   Collapse consecutive-equal entries, emit a jump for each delta.
sums_to_jumps([], []).
sums_to_jumps([_], []).
sums_to_jumps([A, B | Rest], Jumps) :-
    A =:= B, !,
    sums_to_jumps([B | Rest], Jumps).
sums_to_jumps([A, B | Rest], [J | RestJumps]) :-
    Delta is B - A,
    ( Delta >= 0
    -> format(string(Label), "+~w", [Delta])
    ;  format(string(Label), "~w", [Delta])
    ),
    J = _{from: A, to: B, label: Label},
    sums_to_jumps([B | Rest], RestJumps).

%!  history_to_dicts(+Strategy, +History, -Dicts) is det.
%
%   Produce a JSON-serializable list of step dicts:
%     _{state: StateAtom, interpretation: InterpAtom}
history_to_dicts(_Strategy, History, Dicts) :-
    findall(_{state: StateAtom, interpretation: InterpAtom},
            ( member(Step, History),
              step_state_and_interp(Step, State, Interp),
              term_to_atom(State, StateAtom),
              interp_to_atom(Interp, InterpAtom)
            ),
            Dicts).

step_state_and_interp(step(S, _, I), S, I) :- !.
step_state_and_interp(step(S, _, _, _, I), S, I) :- !.
step_state_and_interp(step(S, _, _, _, _, I), S, I) :- !.
step_state_and_interp(Step, Step, '').

interp_to_atom(X, X) :- atom(X), !.
interp_to_atom(X, A) :- string(X), !, atom_string(A, X).
interp_to_atom(X, A) :- term_to_atom(X, A).
