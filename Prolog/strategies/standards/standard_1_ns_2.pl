/** <module> Standard 1.NS.2 — Two-digit place value
 *
 * Indiana: 1.NS.2 — "Model place value concepts of two-digit numbers,
 *          multiples of 10, and equivalent forms of whole numbers using
 *          objects and drawings." (E)
 * CCSS:    1.NBT.B.2 — "Understand that the two digits of a two-digit
 *                       number represent amounts of tens and ones."
 *
 * VPV MAPPING:
 *   V  (target vocabulary): "tens place", "ones place", "__ tens and
 *      __ ones", multiples of ten (10, 20, ..., 90)
 *   P  (practices): decomposing any two-digit number into tens and ones;
 *      composing from tens and ones; recognizing multiples of ten as
 *      special cases (N tens and 0 ones); understanding 10 as ten ones
 *      (generalized from K.NS.7)
 *   V' (metavocabulary): "how many tens?", "how many ones?",
 *      "what number has ___ tens and ___ ones?"
 *
 * BUILDS UPON: K.NS.7 (teen place value, one ten-group only)
 * BUILDS TOWARD: 2.NBT (three-digit place value, hundreds)
 *
 * BRANDOM CONNECTION: Full two-digit place value is the first
 *   vocabulary that is genuinely STRONGER than counting. "47" as
 *   "four tens and seven ones" contains structural information
 *   that "forty-seven" (a mere name) does not. The practice of
 *   decomposition elaborates the counting vocabulary into the
 *   place-value vocabulary. This is the algorithmic elaboration
 *   that makes addition strategies possible: you can only add
 *   "by tens and ones" if you can see a number AS tens and ones.
 *
 * CONNECTION TO EXISTING CODE:
 *   grounded_utils.pl has decompose_base10/3 which does structural
 *   decomposition. This module wraps it with curriculum-appropriate
 *   interface and connects to the naming layer.
 */

:- module(standard_1_ns_2, [
    decompose_two_digit/3, % +Number, -Tens, -Ones
    compose_two_digit/3,   % +Tens, +Ones, -Number
    is_multiple_of_ten/1,  % +Number
    describe_two_digit/2   % +Number, -Description
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

% ============================================================
% Decompose any two-digit number into tens and ones
% ============================================================

%!  decompose_two_digit(+Number, -Tens, -Ones) is det.
%
%   Decompose a number (0-99) into tens count and ones count.
%   Both Tens and Ones are recollection structures.
%
%   Example: decompose_two_digit(47) → Tens=4, Ones=7
%
%   This generalizes K.NS.7's decompose_teen (which only
%   handled 0-1 tens) to 0-9 tens.

decompose_two_digit(Number, Tens, Ones) :-
    incur_cost(inference),
    integer_to_recollection(10, Ten),
    count_tens_(Number, Ten, Tens, Ones).

%% Count how many tens fit, return remainder as ones
count_tens_(Number, Ten, TensCount, Ones) :-
    zero(Zero),
    count_tens_acc_(Number, Ten, Zero, TensCount, Ones).

count_tens_acc_(Number, Ten, Acc, Acc, Number) :-
    smaller_than(Number, Ten), !.
count_tens_acc_(Number, _Ten, Acc, Acc, Number) :-
    equal_to(Number, Zero),
    zero(Zero), !.
count_tens_acc_(Number, Ten, Acc, TensCount, Ones) :-
    subtract_grounded(Number, Ten, Remainder),
    successor_rec(Acc, NextAcc),
    count_tens_acc_(Remainder, Ten, NextAcc, TensCount, Ones).

%% Increment a recollection by one (without importing successor
%% to avoid confusion with grounded_arithmetic:successor which
%% expects recollection format)
successor_rec(recollection(H), recollection([tally|H])).


% ============================================================
% Compose a number from tens and ones
% ============================================================

%!  compose_two_digit(+Tens, +Ones, -Number) is det.
%
%   Compose a number from tens count and ones count.
%   Number = Tens * 10 + Ones (done via grounded operations).

compose_two_digit(Tens, Ones, Number) :-
    incur_cost(inference),
    integer_to_recollection(10, Ten),
    multiply_tens_(Tens, Ten, TensValue),
    add_grounded(TensValue, Ones, Number).

%% Multiply tens count by ten via repeated addition
multiply_tens_(Tens, _Ten, Zero) :-
    zero(Zero),
    equal_to(Tens, Zero), !.
multiply_tens_(Tens, Ten, Result) :-
    predecessor_rec(Tens, PrevTens),
    multiply_tens_(PrevTens, Ten, Partial),
    add_grounded(Partial, Ten, Result).

predecessor_rec(recollection([_|H]), recollection(H)).


% ============================================================
% Multiples of ten
% ============================================================

%!  is_multiple_of_ten(+Number) is semidet.
%
%   True if Number is a multiple of 10 (ones digit is zero).

is_multiple_of_ten(Number) :-
    incur_cost(inference),
    decompose_two_digit(Number, _Tens, Ones),
    zero(Zero),
    equal_to(Ones, Zero).


% ============================================================
% Description
% ============================================================

%!  describe_two_digit(+Number, -Description) is det.
%
%   Produce a structured place-value description.
%   Returns place_value(TensInt, OnesInt).

describe_two_digit(Number, place_value(TensInt, OnesInt)) :-
    decompose_two_digit(Number, Tens, Ones),
    recollection_to_integer(Tens, TensInt),
    recollection_to_integer(Ones, OnesInt).
