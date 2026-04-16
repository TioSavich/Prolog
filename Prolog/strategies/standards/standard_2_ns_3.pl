/** <module> Standard 2.NS.3 — Odd and even
 *
 * Indiana: 2.NS.3 — "Determine whether a group of objects (up to 20)
 *          has an odd or even number of members."
 * CCSS:    2.OA.C.3
 *
 * BRANDOM CONNECTION: Odd/even is the first classification that
 *   partitions ALL numbers into two exhaustive, exclusive categories.
 *   It introduces a new incompatibility: a number is odd XOR even,
 *   never both. This is a structural property discovered through
 *   the pairing practice (try to pair objects; leftover = odd).
 */

:- module(standard_2_ns_3, [
    is_even/1,         % +Number
    is_odd/1,          % +Number
    classify_parity/2  % +Number, -Result
]).

:- use_module(formalization(grounded_arithmetic), [
    zero/1,
    equal_to/2,
    predecessor/2,
    integer_to_recollection/2,
    recollection_to_integer/2,
    incur_cost/1
]).

%!  is_even(+Number) is semidet.
%   True if Number (recollection) has an even number of tallies.
%   Determined by removing tallies two at a time.
is_even(Number) :-
    incur_cost(inference),
    parity_(Number, even).

%!  is_odd(+Number) is semidet.
is_odd(Number) :-
    incur_cost(inference),
    parity_(Number, odd).

%!  classify_parity(+Number, -Result) is det.
classify_parity(Number, Result) :-
    (   parity_(Number, even)
    ->  Result = even
    ;   Result = odd
    ).

%% Remove two at a time; if zero remains, even; if one remains, odd.
parity_(N, even) :-
    zero(Z), equal_to(N, Z), !.
parity_(N, odd) :-
    predecessor(N, N1),
    zero(Z), equal_to(N1, Z), !.
parity_(N, Result) :-
    predecessor(N, N1),
    predecessor(N1, N2),
    parity_(N2, Result).
