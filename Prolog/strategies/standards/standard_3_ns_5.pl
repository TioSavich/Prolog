/** <module> Standard 3.NS.5 — Compare fractions
 *
 * Indiana: 3.NS.5 — "Compare two fractions with the same numerator
 *          or the same denominator by reasoning about their size
 *          based on the same whole." (E)
 * CCSS:    3.NF.A.3d
 *
 * Two comparison strategies:
 *   1. Same denominator: larger numerator = larger fraction
 *      (more pieces of the same size)
 *   2. Same numerator: larger denominator = SMALLER fraction
 *      (same number of pieces, but each piece is smaller)
 *
 * BRANDOM CONNECTION: Same-numerator comparison introduces a
 *   non-obvious inference: MORE parts means SMALLER pieces.
 *   This is counter-intuitive for children accustomed to
 *   whole-number reasoning where "more" always means "bigger."
 *   The incompatibility between whole-number and fraction
 *   reasoning at this exact point is one of the documented
 *   fraction crisis symptoms.
 */

:- module(standard_3_ns_5, [
    compare_fractions/3  % +FracA, +FracB, -Result
]).

:- use_module(formalization(grounded_arithmetic), [
    equal_to/2,
    smaller_than/2,
    greater_than/2,
    incur_cost/1
]).

%!  compare_fractions(+FracA, +FracB, -Result) is semidet.
%
%   Compare two fractions. Requires same numerator OR same
%   denominator (per Grade 3 standard). Fails otherwise.
%
%   Same denominator: compare numerators directly.
%   Same numerator: INVERT denominator comparison
%   (larger denominator = smaller fraction).

compare_fractions(fraction(NA, DA), fraction(NB, DB), Result) :-
    incur_cost(inference),
    (   equal_to(DA, DB)
    ->  % Same denominator: compare numerators
        compare_same_den_(NA, NB, Result)
    ;   equal_to(NA, NB)
    ->  % Same numerator: invert denominator comparison
        compare_same_num_(DA, DB, Result)
    ;   % Cannot compare (different numerator AND denominator)
        fail
    ).

compare_same_den_(NA, NB, greater_than) :- greater_than(NA, NB), !.
compare_same_den_(NA, NB, less_than) :- smaller_than(NA, NB), !.
compare_same_den_(NA, NB, equal_to) :- equal_to(NA, NB).

%% INVERTED: larger denominator means smaller fraction
compare_same_num_(DA, DB, less_than) :- greater_than(DA, DB), !.
compare_same_num_(DA, DB, greater_than) :- smaller_than(DA, DB), !.
compare_same_num_(DA, DB, equal_to) :- equal_to(DA, DB).
