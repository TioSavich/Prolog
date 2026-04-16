/** <module> Standard 2.NS.5 — Compare three-digit numbers
 *
 * Indiana: 2.NS.5 — "Use place value understanding to compare two
 *          three-digit numbers based on meanings of the hundreds,
 *          tens, and ones digits, using >, =, and < symbols." (E)
 * CCSS:    2.NBT.A.4
 *
 * Extends K.NS.5-6 comparison to three-digit numbers using
 * place-value decomposition rather than counting.
 */

:- module(standard_2_ns_5, [
    compare_by_place_value/3  % +A, +B, -Result
]).

:- use_module(formalization(grounded_arithmetic), [
    equal_to/2,
    smaller_than/2,
    greater_than/2,
    recollection_to_integer/2,
    incur_cost/1
]).

:- use_module(standard_2_ns_2_4, [
    decompose_three_digit/4
]).

%!  compare_by_place_value(+A, +B, -Result) is det.
%
%   Compare two numbers by decomposing into hundreds, tens, ones
%   and comparing place by place (most significant first).
%   Result is one of: greater_than, less_than, equal_to.

compare_by_place_value(A, B, Result) :-
    incur_cost(inference),
    decompose_three_digit(A, HA, TA, OA),
    decompose_three_digit(B, HB, TB, OB),
    compare_places_(HA, HB, TA, TB, OA, OB, Result).

compare_places_(HA, HB, _TA, _TB, _OA, _OB, greater_than) :-
    greater_than(HA, HB), !.
compare_places_(HA, HB, _TA, _TB, _OA, _OB, less_than) :-
    smaller_than(HA, HB), !.
compare_places_(HA, HB, TA, TB, _OA, _OB, greater_than) :-
    equal_to(HA, HB), greater_than(TA, TB), !.
compare_places_(HA, HB, TA, TB, _OA, _OB, less_than) :-
    equal_to(HA, HB), smaller_than(TA, TB), !.
compare_places_(HA, HB, TA, TB, OA, OB, greater_than) :-
    equal_to(HA, HB), equal_to(TA, TB), greater_than(OA, OB), !.
compare_places_(HA, HB, TA, TB, OA, OB, less_than) :-
    equal_to(HA, HB), equal_to(TA, TB), smaller_than(OA, OB), !.
compare_places_(HA, HB, TA, TB, OA, OB, equal_to) :-
    equal_to(HA, HB), equal_to(TA, TB), equal_to(OA, OB).
