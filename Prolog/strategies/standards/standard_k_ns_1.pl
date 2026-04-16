/** <module> Standard K.NS.1 — Count to 100 by ones and tens
 *
 * Indiana: K.NS.1 — "Count to at least 100 by ones and tens.
 *          Count by one from any given number." (E)
 * CCSS:    K.CC.A.1 — "Count to 100 by ones and by tens."
 *          K.CC.A.2 — "Count forward beginning from a given number
 *                      within the known sequence."
 *
 * VPV MAPPING:
 *   V  (target vocabulary): number words one through one hundred;
 *      decade words ten, twenty, ..., one hundred
 *   P  (practices): forward successor iteration; decade-skip iteration;
 *      counting-on from arbitrary start
 *   V' (metavocabulary): "count", "count by", "count on from",
 *      "the next number is", "skip count"
 *
 * LEARNING COMPONENTS (from LearningCommons Knowledge Graph v1.7.0):
 *   - Count to 10 by ones    (67200ac5-9ecb-5f5d-975c-a343ea14082b)
 *   - Count to 100 by ones   (555df2b6-7a87-5114-8ba5-398a82a7f6e3)
 *   - Count to 100 by tens   (4ae29496-ad30-54cb-8b3d-0c5472c7d2f9)
 *
 * RELATED: K.CC.A.2 (count forward from given number),
 *          K.CC.B.4.a (one-to-one correspondence)
 * BUILDS TOWARD: 1.NBT.A.1 (count to 120 from any number < 120)
 *
 * ILLUSTRATIVE MATH GROUNDING (paywalled — summary from public materials):
 *   Grade K, Unit 1 "Math in Our World" — students count collections
 *   of objects using one-to-one correspondence. Activities: counting
 *   collections of connecting cubes, organizing objects to track
 *   counting, comparing groups by matching. Teacher moves: "How many
 *   did you count?", "Can you show me how you counted?", prompting
 *   students to organize before counting. Representations: connecting
 *   cubes, counters, fingers, dot images.
 *
 * BRANDOM CONNECTION: Counting is the primordial algorithmic elaboration.
 *   The practice (P) of successor iteration transforms the vocabulary (V)
 *   from empty (no number words) to number words through 100. The
 *   metavocabulary (V') — "say the next number", "skip count" — is what
 *   the teacher uses to describe the practice. The weaker vocabulary
 *   (pre-counting) is elaborated into the stronger (number words) via
 *   the counting practice. This is Brandom's PP-sufficiency: the
 *   practice is sufficient to deploy the vocabulary.
 *
 * LIMITATIONS:
 *   - Carry detection (tens boundary events) is not implemented.
 *     Design/05 specifies that crossing 9->10, 19->20, etc. should
 *     produce carry events in the trace. This module records only
 *     successor steps. Carry detection requires the reflection
 *     mechanism operating on stored traces — it is a discovery,
 *     not a given.
 *   - count_by_tens requires From and To to be exact multiples of
 *     ten apart. If the target is not reachable by decade steps,
 *     the predicate fails. This is honest: the automaton does not
 *     model approximate or flexible skip counting.
 *   - Number-word assignment is not handled here. The automaton
 *     operates on recollection structures (tally sequences). Naming
 *     is the teacher's job (see design/04, standard K.NS.2).
 *   - integer_to_recollection/2 is used as a transition utility for
 *     building the decade increment. Full grounding would build "ten"
 *     by iterating successor from zero.
 */

:- module(standard_k_ns_1, [
    count_by_ones/3,       % +From, +To, -Trace
    count_by_tens/3,       % +From, +To, -Trace
    count_on_from/4,       % +Start, +Steps, -End, -Trace
    stored_trace/4,        % ?From, ?To, ?Direction, ?Trace
    reset_traces/0
]).

:- use_module(formalization(grounded_arithmetic), [
    successor/2,
    predecessor/2,
    zero/1,
    equal_to/2,
    smaller_than/2,
    add_grounded/3,
    integer_to_recollection/2,
    recollection_to_integer/2,
    incur_cost/1
]).

:- dynamic stored_trace/4.
%% stored_trace(+From, +To, +Direction, +Trace)
%% Persists counting traces for later reflection.
%% Direction: forward | forward_by_tens | count_on

%% reset_traces is det.
%% Clear all stored traces. For testing.
reset_traces :-
    retractall(stored_trace(_, _, _, _)).

% ============================================================
% Learning Component 1 & 2: Count by ones
% ============================================================

%!  count_by_ones(+From, +To, -Trace) is semidet.
%
%   Count forward from From to To by ones, producing a trace of
%   each state visited. From and To are recollection structures.
%   Fails if From > To (counting is forward only at this stage).
%
%   The trace is a list of state/2 terms:
%     state(Recollection, TransitionType)
%   where TransitionType is 'start' or 'successor'.
%
%   Models the primordial counting practice: iterate successor,
%   record every step. Cost is O(To - From) unit_count operations.

count_by_ones(From, To, Trace) :-
    incur_cost(inference),
    count_ones_(From, To, [state(From, start)], RevTrace),
    reverse(RevTrace, Trace),
    assertz(stored_trace(From, To, forward, Trace)).

count_ones_(Current, Target, Acc, Acc) :-
    equal_to(Current, Target), !.
count_ones_(Current, Target, Acc, Trace) :-
    smaller_than(Current, Target),
    successor(Current, Next),
    count_ones_(Next, Target, [state(Next, successor)|Acc], Trace).


% ============================================================
% Learning Component 3: Count by tens
% ============================================================

%!  count_by_tens(+From, +To, -Trace) is semidet.
%
%   Skip-count from From to To by tens. Each step adds ten to
%   the current recollection using add_grounded (which is ten
%   individual tally appends — the cost is real).
%
%   Fails if To is not reachable from From by exact decade steps.
%   This is a genuine constraint: the automaton models decade
%   skip counting, not approximate counting.
%
%   Trace entries use transition type 'decade_step'.

count_by_tens(From, To, Trace) :-
    incur_cost(inference),
    integer_to_recollection(10, Ten),
    count_tens_(From, To, Ten, [state(From, start)], RevTrace),
    reverse(RevTrace, Trace),
    assertz(stored_trace(From, To, forward_by_tens, Trace)).

count_tens_(Current, Target, _Ten, Acc, Acc) :-
    equal_to(Current, Target), !.
count_tens_(Current, Target, Ten, Acc, Trace) :-
    smaller_than(Current, Target),
    add_grounded(Current, Ten, Next),
    count_tens_(Next, Target, Ten, [state(Next, decade_step)|Acc], Trace).


% ============================================================
% Indiana K.NS.1 extension: Count on from any given number
% (Maps to CCSS K.CC.A.2)
% ============================================================

%!  count_on_from(+Start, +Steps, -End, -Trace) is det.
%
%   Count forward from Start by Steps successor operations.
%   Start and Steps are recollection structures.
%   End is the recollection reached after counting.
%
%   This models the ability to begin counting from any number,
%   not just from one. "Start at 7, count on 3 more" → 10.

count_on_from(Start, Steps, End, Trace) :-
    incur_cost(inference),
    count_on_(Start, Steps, [state(Start, start)], RevTrace),
    reverse(RevTrace, Trace),
    last(Trace, state(End, _)),
    assertz(stored_trace(Start, End, count_on, Trace)).

count_on_(_Current, Remaining, Acc, Acc) :-
    zero(Zero),
    equal_to(Remaining, Zero), !.
count_on_(Current, Remaining, Acc, Trace) :-
    successor(Current, Next),
    predecessor(Remaining, NewRemaining),
    count_on_(Next, NewRemaining, [state(Next, successor)|Acc], Trace).
