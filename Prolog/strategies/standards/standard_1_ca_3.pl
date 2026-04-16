/** <module> Standard 1.CA.3 — Addition within 100 using place value
 *
 * Indiana: 1.CA.3 — "Using number sense and place value strategies,
 *          add within 100, including adding a two-digit number and a
 *          one-digit number, and adding a two-digit number and a
 *          multiple of 10." (E)
 * CCSS:    1.NBT.C.4 — "Add within 100, including adding a two-digit
 *                       number and a one-digit number..."
 *
 * VPV MAPPING:
 *   V  (target vocabulary): multi-digit addition results, "regroup",
 *      "carry", place-value addition language
 *   P  (practices): add ones to ones then combine with tens;
 *      add a multiple of ten by incrementing the tens digit;
 *      regroup when ones exceed 9 (carry)
 *   V' (metavocabulary): "add the ones first", "add the tens",
 *      "do I need to regroup?", "10 ones make a new ten"
 *
 * CONNECTION TO EXISTING AUTOMATA:
 *   This standard is where the existing addition strategy automata
 *   begin to apply:
 *   - sar_add_counting_on.pl  — O(B) counting
 *   - sar_add_cobo.pl         — COBO (count on bases + ones)
 *   - sar_add_rmb.pl          — Round to Make Base
 *   - sar_add_chunking.pl     — Chunking by place value
 *
 *   The progression from 1.CA.1 to 1.CA.3 IS the crisis that
 *   drives strategy elaboration: counting-on 38+55 costs 55
 *   successor operations. Place-value strategies reduce this.
 *
 * BRANDOM CONNECTION: Place-value addition is a paradigm case of
 *   algorithmic elaboration. The practice of decomposing addends
 *   into tens and ones, adding each place separately, and
 *   regrouping TRANSFORMS the addition vocabulary from "count on"
 *   to "add by place." The stronger vocabulary (place-value
 *   addition) makes visible the structure that counting hides.
 */

:- module(standard_1_ca_3, [
    add_by_place_value/3,    % +A, +B, -Sum
    add_two_digit_one_digit/3, % +TwoDigit, +OneDigit, -Sum
    add_two_digit_mult_ten/3   % +TwoDigit, +MultTen, -Sum
]).

:- use_module(formalization(grounded_arithmetic), [
    zero/1,
    equal_to/2,
    smaller_than/2,
    add_grounded/3,
    subtract_grounded/3,
    integer_to_recollection/2,
    recollection_to_integer/2,
    incur_cost/1
]).

:- use_module(standard_1_ns_2, [
    decompose_two_digit/3,
    compose_two_digit/3
]).

% ============================================================
% General place-value addition
% ============================================================

%!  add_by_place_value(+A, +B, -Sum) is det.
%
%   Add two numbers using place-value decomposition:
%   1. Decompose both into tens and ones
%   2. Add ones to ones
%   3. Add tens to tens
%   4. Handle regrouping if ones sum ≥ 10
%   5. Compose the result
%
%   This is the general strategy that subsumes the two
%   special cases below.

add_by_place_value(A, B, Sum) :-
    incur_cost(inference),
    decompose_two_digit(A, TensA, OnesA),
    decompose_two_digit(B, TensB, OnesB),
    % Add ones
    add_grounded(OnesA, OnesB, OnesSum),
    % Add tens
    add_grounded(TensA, TensB, TensSum),
    % Check for regrouping (ones ≥ 10)
    integer_to_recollection(10, TenRec),
    (   smaller_than(OnesSum, TenRec)
    ->  % No regrouping needed
        compose_two_digit(TensSum, OnesSum, Sum)
    ;   % Regroup: subtract 10 from ones, add 1 to tens
        subtract_grounded(OnesSum, TenRec, NewOnes),
        integer_to_recollection(1, One),
        add_grounded(TensSum, One, NewTens),
        compose_two_digit(NewTens, NewOnes, Sum)
    ).


% ============================================================
% Special case: two-digit + one-digit
% ============================================================

%!  add_two_digit_one_digit(+TwoDigit, +OneDigit, -Sum) is det.
%
%   Add a two-digit number and a one-digit number.
%   Only the ones place is affected (with possible regrouping).

add_two_digit_one_digit(TwoDigit, OneDigit, Sum) :-
    add_by_place_value(TwoDigit, OneDigit, Sum).


% ============================================================
% Special case: two-digit + multiple of ten
% ============================================================

%!  add_two_digit_mult_ten(+TwoDigit, +MultTen, -Sum) is det.
%
%   Add a two-digit number and a multiple of 10.
%   Only the tens place is affected (no regrouping possible
%   within two-digit range).

add_two_digit_mult_ten(TwoDigit, MultTen, Sum) :-
    add_by_place_value(TwoDigit, MultTen, Sum).
