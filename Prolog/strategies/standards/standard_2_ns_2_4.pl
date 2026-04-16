/** <module> Standards 2.NS.2 + 2.NS.4 — Three-digit place value
 *
 * Indiana: 2.NS.2 — "Read and write whole numbers up to 1,000. Use
 *          words, models, standard form, and expanded form." (E)
 *          2.NS.4 — "Define and model a 'hundred' as a group of ten
 *          tens. Model place value concepts of three-digit numbers." (E)
 * CCSS:    2.NBT.A.1 — "Understand that the three digits of a three-
 *                       digit number represent amounts of hundreds,
 *                       tens, and ones."
 *
 * Extends 1.NS.2 from two digits to three digits.
 *
 * BRANDOM CONNECTION: "Hundred as a group of ten tens" is recursive
 *   place-value composition — the same grouping practice applied at
 *   a higher level. The vocabulary "3 hundreds, 4 tens, 7 ones" is
 *   stronger than "three hundred forty-seven" because it exposes the
 *   internal structure needed for multi-digit computation.
 */

:- module(standard_2_ns_2_4, [
    decompose_three_digit/4, % +Number, -Hundreds, -Tens, -Ones
    compose_three_digit/4,   % +Hundreds, +Tens, +Ones, -Number
    expanded_form/2,         % +Number, -ExpandedTerms
    describe_three_digit/2   % +Number, -Description
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

%!  decompose_three_digit(+Number, -Hundreds, -Tens, -Ones) is det.
%
%   Decompose a number (0-999) into hundreds, tens, and ones.
%   All outputs are recollection structures.

decompose_three_digit(Number, Hundreds, Tens, Ones) :-
    incur_cost(inference),
    integer_to_recollection(100, HundredUnit),
    count_units_(Number, HundredUnit, Hundreds, Remainder),
    decompose_two_digit(Remainder, Tens, Ones).

%% Count how many complete units fit
count_units_(Number, Unit, Count, Remainder) :-
    zero(Zero),
    count_units_acc_(Number, Unit, Zero, Count, Remainder).

count_units_acc_(Number, Unit, Acc, Acc, Number) :-
    smaller_than(Number, Unit), !.
count_units_acc_(Number, _Unit, Acc, Acc, Number) :-
    zero(Zero), equal_to(Number, Zero), !.
count_units_acc_(Number, Unit, Acc, Count, Remainder) :-
    subtract_grounded(Number, Unit, Rest),
    successor_rec(Acc, NextAcc),
    count_units_acc_(Rest, Unit, NextAcc, Count, Remainder).

successor_rec(recollection(H), recollection([tally|H])).
predecessor_rec(recollection([_|H]), recollection(H)).

%!  compose_three_digit(+Hundreds, +Tens, +Ones, -Number) is det.
compose_three_digit(Hundreds, Tens, Ones, Number) :-
    incur_cost(inference),
    integer_to_recollection(100, HundredUnit),
    multiply_units_(Hundreds, HundredUnit, HundredsValue),
    compose_two_digit(Tens, Ones, TensOnesValue),
    add_grounded(HundredsValue, TensOnesValue, Number).

multiply_units_(Count, _Unit, Zero) :-
    zero(Zero), equal_to(Count, Zero), !.
multiply_units_(Count, Unit, Result) :-
    predecessor_rec(Count, PrevCount),
    multiply_units_(PrevCount, Unit, Partial),
    add_grounded(Partial, Unit, Result).

%!  expanded_form(+Number, -ExpandedTerms) is det.
%
%   Return the expanded form as a list of terms.
%   Example: expanded_form(347) → [hundreds(3), tens(4), ones(7)]

expanded_form(Number, ExpandedTerms) :-
    decompose_three_digit(Number, H, T, O),
    recollection_to_integer(H, HI),
    recollection_to_integer(T, TI),
    recollection_to_integer(O, OI),
    ExpandedTerms = [hundreds(HI), tens(TI), ones(OI)].

%!  describe_three_digit(+Number, -Description) is det.
describe_three_digit(Number, place_value(HI, TI, OI)) :-
    decompose_three_digit(Number, H, T, O),
    recollection_to_integer(H, HI),
    recollection_to_integer(T, TI),
    recollection_to_integer(O, OI).
