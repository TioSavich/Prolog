/** <module> Tests for Standard 2.CA.2 — Add/subtract within 1000
 */

:- use_module(standards(standard_2_ca_2)).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2
]).

run_tests :-
    writeln('=== 2.CA.2 Three-Digit Operations Tests ==='),
    writeln(''),

    % Addition
    test_add_no_regroup,
    test_add_ones_regroup,
    test_add_tens_regroup,
    test_add_cascade_regroup,
    test_add_999_plus_1,
    % Subtraction
    test_sub_no_borrow,
    test_sub_borrow_ones,
    test_sub_borrow_tens,
    test_sub_cascade_borrow,
    % COBO
    test_cobo_addition,
    % Decomposition subtraction
    test_decompose_subtraction,
    % Strategies agree
    test_strategies_agree,

    writeln(''),
    writeln('=== All 2.CA.2 Tests Passed ===').

%% Addition: no regrouping (234 + 345 = 579)
test_add_no_regroup :-
    integer_to_recollection(234, RA),
    integer_to_recollection(345, RB),
    add_three_digit(RA, RB, Sum),
    recollection_to_integer(Sum, 579),
    format('  PASS: 234 + 345 = 579 (no regroup)~n').

%% Addition: ones regroup (158 + 67 = 225)
test_add_ones_regroup :-
    integer_to_recollection(158, RA),
    integer_to_recollection(67, RB),
    add_three_digit(RA, RB, Sum),
    recollection_to_integer(Sum, 225),
    format('  PASS: 158 + 67 = 225 (ones regroup)~n').

%% Addition: tens regroup (281 + 130 = 411)
test_add_tens_regroup :-
    integer_to_recollection(281, RA),
    integer_to_recollection(130, RB),
    add_three_digit(RA, RB, Sum),
    recollection_to_integer(Sum, 411),
    format('  PASS: 281 + 130 = 411 (tens regroup)~n').

%% Addition: cascade regroup (486 + 357 = 843)
test_add_cascade_regroup :-
    integer_to_recollection(486, RA),
    integer_to_recollection(357, RB),
    add_three_digit(RA, RB, Sum),
    recollection_to_integer(Sum, 843),
    format('  PASS: 486 + 357 = 843 (cascade regroup)~n').

%% Edge: 999 + 1 = 1000
test_add_999_plus_1 :-
    integer_to_recollection(999, RA),
    integer_to_recollection(1, RB),
    add_three_digit(RA, RB, Sum),
    recollection_to_integer(Sum, 1000),
    format('  PASS: 999 + 1 = 1000~n').

%% Subtraction: no borrow (579 - 234 = 345)
test_sub_no_borrow :-
    integer_to_recollection(579, RA),
    integer_to_recollection(234, RB),
    sub_three_digit(RA, RB, Diff),
    recollection_to_integer(Diff, 345),
    format('  PASS: 579 - 234 = 345 (no borrow)~n').

%% Subtraction: borrow from tens (523 - 167 = 356)
test_sub_borrow_ones :-
    integer_to_recollection(523, RA),
    integer_to_recollection(167, RB),
    sub_three_digit(RA, RB, Diff),
    recollection_to_integer(Diff, 356),
    format('  PASS: 523 - 167 = 356 (borrow ones)~n').

%% Subtraction: borrow from hundreds (412 - 170 = 242)
test_sub_borrow_tens :-
    integer_to_recollection(412, RA),
    integer_to_recollection(170, RB),
    sub_three_digit(RA, RB, Diff),
    recollection_to_integer(Diff, 242),
    format('  PASS: 412 - 170 = 242 (borrow tens)~n').

%% Subtraction: cascade borrow (500 - 123 = 377)
test_sub_cascade_borrow :-
    integer_to_recollection(500, RA),
    integer_to_recollection(123, RB),
    sub_three_digit(RA, RB, Diff),
    recollection_to_integer(Diff, 377),
    format('  PASS: 500 - 123 = 377 (cascade borrow)~n').

%% COBO-style addition (matches sar_add_cobo pattern)
test_cobo_addition :-
    integer_to_recollection(234, RA),
    integer_to_recollection(345, RB),
    add_cobo_style(RA, RB, Sum),
    recollection_to_integer(Sum, 579),
    format('  PASS: COBO 234 + 345 = 579~n').

%% Decomposition subtraction (matches sar_sub_decomposition pattern)
test_decompose_subtraction :-
    integer_to_recollection(579, RA),
    integer_to_recollection(234, RB),
    sub_decompose_style(RA, RB, Diff),
    recollection_to_integer(Diff, 345),
    format('  PASS: decompose 579 - 234 = 345~n').

%% All addition strategies agree
test_strategies_agree :-
    integer_to_recollection(347, RA),
    integer_to_recollection(286, RB),
    add_three_digit(RA, RB, S1),
    add_cobo_style(RA, RB, S2),
    recollection_to_integer(S1, N1),
    recollection_to_integer(S2, N2),
    N1 =:= N2,
    format('  PASS: place-value and COBO agree on 347+286=~w~n', [N1]).
