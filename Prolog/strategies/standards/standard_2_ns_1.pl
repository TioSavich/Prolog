/** <module> Standard 2.NS.1 — Count by 1s, 2s, 5s, 10s, 100s to 1000
 *
 * Indiana: 2.NS.1 — "Count by ones, twos, fives, tens, and hundreds
 *          up to at least 1,000 from any given number." (E)
 * CCSS:    2.NBT.A.2 — "Count within 1000; skip-count by 5s, 10s,
 *                       and 100s."
 *
 * Extends 1.NS.1 with counting by 2s and 100s.
 * All skip-counting predicates reuse the same add-and-iterate pattern.
 */

:- module(standard_2_ns_1, [
    count_by_twos/3,       % +From, +To, -Trace
    count_by_hundreds/3    % +From, +To, -Trace
]).

:- use_module(formalization(grounded_arithmetic), [
    equal_to/2,
    smaller_than/2,
    add_grounded/3,
    integer_to_recollection/2,
    incur_cost/1
]).

%!  count_by_twos(+From, +To, -Trace) is semidet.
count_by_twos(From, To, Trace) :-
    incur_cost(inference),
    integer_to_recollection(2, Two),
    skip_count_(From, To, Two, [state(From, start)], RevTrace),
    reverse(RevTrace, Trace).

%!  count_by_hundreds(+From, +To, -Trace) is semidet.
count_by_hundreds(From, To, Trace) :-
    incur_cost(inference),
    integer_to_recollection(100, Hundred),
    skip_count_(From, To, Hundred, [state(From, start)], RevTrace),
    reverse(RevTrace, Trace).

%% Generic skip-counting engine
skip_count_(Current, Target, _Step, Acc, Acc) :-
    equal_to(Current, Target), !.
skip_count_(Current, Target, Step, Acc, Trace) :-
    smaller_than(Current, Target),
    add_grounded(Current, Step, Next),
    skip_count_(Next, Target, Step, [state(Next, skip_step)|Acc], Trace).
