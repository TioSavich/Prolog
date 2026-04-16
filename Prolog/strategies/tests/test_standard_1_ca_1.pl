/** <module> Tests for Standard 1.CA.1 — Addition/subtraction strategies
 */

:- use_module(standards(standard_1_ca_1)).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2
]).

run_tests :-
    writeln('=== 1.CA.1 Strategy Tests ==='),
    writeln(''),

    test_counting_on_basic,
    test_counting_on_8_plus_6,
    test_making_ten_8_plus_6,
    test_making_ten_9_plus_7,
    test_decompose_to_ten_13_minus_4,
    test_decompose_to_ten_15_minus_8,
    test_fact_family,
    test_doubles,
    test_near_doubles,
    test_strategies_agree,

    writeln(''),
    writeln('=== All 1.CA.1 Tests Passed ===').

%% Counting on: 5 + 3 = 8
test_counting_on_basic :-
    integer_to_recollection(5, R5),
    integer_to_recollection(3, R3),
    add_counting_on(R5, R3, Sum),
    recollection_to_integer(Sum, 8),
    format('  PASS: counting_on(5, 3) → 8~n').

%% Counting on: 8 + 6 = 14
test_counting_on_8_plus_6 :-
    integer_to_recollection(8, R8),
    integer_to_recollection(6, R6),
    add_counting_on(R8, R6, Sum),
    recollection_to_integer(Sum, 14),
    format('  PASS: counting_on(8, 6) → 14~n').

%% Making ten: 8 + 6 = 8 + 2 + 4 = 10 + 4 = 14
test_making_ten_8_plus_6 :-
    integer_to_recollection(8, R8),
    integer_to_recollection(6, R6),
    add_making_ten(R8, R6, Sum),
    recollection_to_integer(Sum, 14),
    format('  PASS: making_ten(8, 6) → 14~n').

%% Making ten: 9 + 7 = 9 + 1 + 6 = 10 + 6 = 16
test_making_ten_9_plus_7 :-
    integer_to_recollection(9, R9),
    integer_to_recollection(7, R7),
    add_making_ten(R9, R7, Sum),
    recollection_to_integer(Sum, 16),
    format('  PASS: making_ten(9, 7) → 16~n').

%% Decompose to ten: 13 - 4 = 13 - 3 - 1 = 10 - 1 = 9
test_decompose_to_ten_13_minus_4 :-
    integer_to_recollection(13, R13),
    integer_to_recollection(4, R4),
    sub_decompose_to_ten(R13, R4, Diff),
    recollection_to_integer(Diff, 9),
    format('  PASS: decompose_to_ten(13, 4) → 9~n').

%% Decompose to ten: 15 - 8 = 15 - 5 - 3 = 10 - 3 = 7
test_decompose_to_ten_15_minus_8 :-
    integer_to_recollection(15, R15),
    integer_to_recollection(8, R8),
    sub_decompose_to_ten(R15, R8, Diff),
    recollection_to_integer(Diff, 7),
    format('  PASS: decompose_to_ten(15, 8) → 7~n').

%% Fact family: 8 + 4 = 12 generates all four facts
test_fact_family :-
    integer_to_recollection(8, R8),
    integer_to_recollection(4, R4),
    fact_family(R8, R4, Sum, Facts),
    recollection_to_integer(Sum, 12),
    length(Facts, 4),
    format('  PASS: fact_family(8, 4) → sum=12, 4 facts~n').

%% Doubles: 7 + 7 = 14
test_doubles :-
    integer_to_recollection(7, R7),
    add_doubles_near(R7, R7, Sum),
    recollection_to_integer(Sum, 14),
    format('  PASS: doubles(7, 7) → 14~n').

%% Near doubles: 6 + 7 = 6 + 6 + 1 = 13
test_near_doubles :-
    integer_to_recollection(6, R6),
    integer_to_recollection(7, R7),
    add_doubles_near(R6, R7, Sum),
    recollection_to_integer(Sum, 13),
    format('  PASS: near_doubles(6, 7) → 13~n').

%% All strategies agree on 8 + 6
test_strategies_agree :-
    integer_to_recollection(8, R8),
    integer_to_recollection(6, R6),
    add_counting_on(R8, R6, S1),
    add_making_ten(R8, R6, S2),
    add_doubles_near(R8, R6, S3),
    recollection_to_integer(S1, N1),
    recollection_to_integer(S2, N2),
    recollection_to_integer(S3, N3),
    N1 =:= N2, N2 =:= N3,
    format('  PASS: all strategies agree on 8+6=~w~n', [N1]).
