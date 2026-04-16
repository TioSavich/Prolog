/** <module> Tests for Grade 3 Number Sense (fractions)
 */

:- use_module(standards(standard_3_ns_2)).
:- use_module(standards(standard_3_ns_5)).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2
]).

run_tests :-
    writeln('=== Grade 3 Fractions Tests ==='),
    writeln(''),

    % 3.NS.2: Unit and non-unit fractions
    test_unit_fraction,
    test_iterate_unit_fraction,
    test_partition_whole,
    test_fraction_from_partition,
    test_invalid_denominator,
    test_whole_as_fraction,
    % 3.NS.5: Compare fractions
    test_compare_same_den,
    test_compare_same_num,
    test_compare_same_num_inverse,
    test_compare_equal,
    test_compare_incomparable,

    writeln(''),
    writeln('=== All Grade 3 NS Tests Passed ===').

%% Unit fraction: 1/4
test_unit_fraction :-
    integer_to_recollection(4, R4),
    make_unit_fraction(R4, Frac),
    fraction_to_pair(Frac, 1, 4),
    format('  PASS: make_unit_fraction(4) → 1/4~n').

%% Iterate: 3 × (1/4) = 3/4
test_iterate_unit_fraction :-
    integer_to_recollection(4, R4),
    integer_to_recollection(3, R3),
    make_unit_fraction(R4, UF),
    iterate_unit_fraction(UF, R3, Frac),
    fraction_to_pair(Frac, 3, 4),
    format('  PASS: 3 × (1/4) = 3/4~n').

%% Partition a whole into 6 parts
test_partition_whole :-
    integer_to_recollection(6, R6),
    partition_whole(R6, Parts),
    length(Parts, 6),
    format('  PASS: partition(6) → 6 unit_parts~n').

%% Fraction from partition: 3 of 8 parts = 3/8
test_fraction_from_partition :-
    integer_to_recollection(8, R8),
    integer_to_recollection(3, R3),
    fraction_from_partition(R8, R3, Frac),
    fraction_to_pair(Frac, 3, 8),
    format('  PASS: 3 of 8 parts = 3/8~n').

%% Invalid denominator (5 not in Grade 3 set)
test_invalid_denominator :-
    integer_to_recollection(5, R5),
    (   make_unit_fraction(R5, _)
    ->  writeln('  FAIL: denominator 5 should be invalid'), fail
    ;   true
    ),
    format('  PASS: denominator 5 rejected (not in {2,3,4,6,8})~n').

%% Whole as fraction: 4/4 = 1 whole
test_whole_as_fraction :-
    integer_to_recollection(4, R4),
    make_unit_fraction(R4, UF),
    iterate_unit_fraction(UF, R4, Frac),
    fraction_to_pair(Frac, 4, 4),
    format('  PASS: 4 × (1/4) = 4/4 (one whole)~n').

%% Compare same denominator: 3/8 vs 5/8
test_compare_same_den :-
    integer_to_recollection(3, R3),
    integer_to_recollection(5, R5),
    integer_to_recollection(8, R8),
    compare_fractions(fraction(R3, R8), fraction(R5, R8), less_than),
    format('  PASS: 3/8 < 5/8 (same denominator)~n').

%% Compare same numerator: 2/3 vs 2/6
%% 2/3 > 2/6 because thirds are bigger than sixths
test_compare_same_num :-
    integer_to_recollection(2, R2),
    integer_to_recollection(3, R3),
    integer_to_recollection(6, R6),
    compare_fractions(fraction(R2, R3), fraction(R2, R6), greater_than),
    format('  PASS: 2/3 > 2/6 (same numerator, inverted)~n').

%% Same numerator: 1/8 < 1/4
test_compare_same_num_inverse :-
    integer_to_recollection(1, R1),
    integer_to_recollection(4, R4),
    integer_to_recollection(8, R8),
    compare_fractions(fraction(R1, R8), fraction(R1, R4), less_than),
    format('  PASS: 1/8 < 1/4 (more parts = smaller pieces)~n').

%% Equal fractions
test_compare_equal :-
    integer_to_recollection(3, R3),
    integer_to_recollection(4, R4),
    compare_fractions(fraction(R3, R4), fraction(R3, R4), equal_to),
    format('  PASS: 3/4 = 3/4~n').

%% Incomparable: different numerator AND denominator
test_compare_incomparable :-
    integer_to_recollection(2, R2),
    integer_to_recollection(3, R3),
    integer_to_recollection(4, R4),
    (   compare_fractions(fraction(R2, R3), fraction(R3, R4), _)
    ->  writeln('  FAIL: 2/3 vs 3/4 should be incomparable'), fail
    ;   true
    ),
    format('  PASS: 2/3 vs 3/4 incomparable at Grade 3~n').
