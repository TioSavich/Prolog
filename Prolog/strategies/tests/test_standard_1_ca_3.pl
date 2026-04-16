/** <module> Tests for Standard 1.CA.3 — Addition within 100
 */

:- use_module(standards(standard_1_ca_3)).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2
]).

run_tests :-
    writeln('=== 1.CA.3 Place Value Addition Tests ==='),
    writeln(''),

    test_add_no_regroup,
    test_add_with_regroup,
    test_add_two_digit_one_digit,
    test_add_two_digit_one_digit_regroup,
    test_add_two_digit_mult_ten,
    test_add_38_plus_55,
    test_add_edge_99_plus_1,
    test_add_zero,

    writeln(''),
    writeln('=== All 1.CA.3 Tests Passed ===').

%% No regrouping: 23 + 14 = 37
test_add_no_regroup :-
    integer_to_recollection(23, R23),
    integer_to_recollection(14, R14),
    add_by_place_value(R23, R14, Sum),
    recollection_to_integer(Sum, 37),
    format('  PASS: 23 + 14 = 37 (no regroup)~n').

%% With regrouping: 28 + 15 = 43
test_add_with_regroup :-
    integer_to_recollection(28, R28),
    integer_to_recollection(15, R15),
    add_by_place_value(R28, R15, Sum),
    recollection_to_integer(Sum, 43),
    format('  PASS: 28 + 15 = 43 (regroup)~n').

%% Two-digit + one-digit: 34 + 5 = 39
test_add_two_digit_one_digit :-
    integer_to_recollection(34, R34),
    integer_to_recollection(5, R5),
    add_two_digit_one_digit(R34, R5, Sum),
    recollection_to_integer(Sum, 39),
    format('  PASS: 34 + 5 = 39~n').

%% Two-digit + one-digit with regroup: 37 + 8 = 45
test_add_two_digit_one_digit_regroup :-
    integer_to_recollection(37, R37),
    integer_to_recollection(8, R8),
    add_two_digit_one_digit(R37, R8, Sum),
    recollection_to_integer(Sum, 45),
    format('  PASS: 37 + 8 = 45 (regroup)~n').

%% Two-digit + multiple of 10: 46 + 30 = 76
test_add_two_digit_mult_ten :-
    integer_to_recollection(46, R46),
    integer_to_recollection(30, R30),
    add_two_digit_mult_ten(R46, R30, Sum),
    recollection_to_integer(Sum, 76),
    format('  PASS: 46 + 30 = 76~n').

%% The canonical crisis problem: 38 + 55 = 93
%% Counting-on costs 55 steps. Place value: decompose, add, regroup.
test_add_38_plus_55 :-
    integer_to_recollection(38, R38),
    integer_to_recollection(55, R55),
    add_by_place_value(R38, R55, Sum),
    recollection_to_integer(Sum, 93),
    format('  PASS: 38 + 55 = 93 (crisis problem)~n').

%% Edge case: 99 + 1 = 100
test_add_edge_99_plus_1 :-
    integer_to_recollection(99, R99),
    integer_to_recollection(1, R1),
    add_by_place_value(R99, R1, Sum),
    recollection_to_integer(Sum, 100),
    format('  PASS: 99 + 1 = 100 (century boundary)~n').

%% Adding zero
test_add_zero :-
    integer_to_recollection(42, R42),
    integer_to_recollection(0, R0),
    add_by_place_value(R42, R0, Sum),
    recollection_to_integer(Sum, 42),
    format('  PASS: 42 + 0 = 42~n').
