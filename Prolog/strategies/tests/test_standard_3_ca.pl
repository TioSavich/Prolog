/** <module> Tests for Grade 3 Computation standards
 */

:- use_module(standards(standard_3_ca_3_4)).
:- use_module(standards(standard_3_ca_5)).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2
]).

run_tests :-
    writeln('=== Grade 3 Computation Tests ==='),
    writeln(''),

    % 3.CA.3: Multiplication models
    test_mult_equal_groups,
    test_mult_array,
    test_mult_repeated_add,
    test_mult_by_zero,
    test_mult_by_one,
    % 3.CA.4: Division models
    test_div_partition,
    test_div_repeated_sub,
    test_mult_div_family,
    % 3.CA.5: Strategies within 100
    test_skip_count,
    test_derived_fact,
    test_distributive,
    test_div_inverse,
    test_strategies_agree,

    writeln(''),
    writeln('=== All Grade 3 CA Tests Passed ===').

%% Multiplication models
test_mult_equal_groups :-
    integer_to_recollection(4, R4),
    integer_to_recollection(6, R6),
    multiply_equal_groups(R4, R6, Product),
    recollection_to_integer(Product, 24),
    format('  PASS: 4 groups of 6 = 24~n').

test_mult_array :-
    integer_to_recollection(3, R3),
    integer_to_recollection(7, R7),
    multiply_array(R3, R7, Product),
    recollection_to_integer(Product, 21),
    format('  PASS: 3×7 array = 21~n').

test_mult_repeated_add :-
    integer_to_recollection(5, R5),
    integer_to_recollection(4, R4),
    multiply_repeated_add(R5, R4, Product),
    recollection_to_integer(Product, 20),
    format('  PASS: 5+5+5+5 = 20~n').

test_mult_by_zero :-
    integer_to_recollection(7, R7),
    integer_to_recollection(0, R0),
    multiply_equal_groups(R7, R0, Product),
    recollection_to_integer(Product, 0),
    format('  PASS: 7 × 0 = 0~n').

test_mult_by_one :-
    integer_to_recollection(9, R9),
    integer_to_recollection(1, R1),
    multiply_equal_groups(R1, R9, Product),
    recollection_to_integer(Product, 9),
    format('  PASS: 1 × 9 = 9~n').

%% Division models
test_div_partition :-
    integer_to_recollection(20, R20),
    integer_to_recollection(4, R4),
    divide_partition(R20, R4, GroupSize),
    recollection_to_integer(GroupSize, 5),
    format('  PASS: 20 ÷ 4 = 5 (partition)~n').

test_div_repeated_sub :-
    integer_to_recollection(35, R35),
    integer_to_recollection(7, R7),
    divide_repeated_sub(R35, R7, Quotient),
    recollection_to_integer(Quotient, 5),
    format('  PASS: 35 ÷ 7 = 5 (repeated sub)~n').

test_mult_div_family :-
    integer_to_recollection(6, R6),
    integer_to_recollection(8, R8),
    mult_div_family(R6, R8, Product, Facts),
    recollection_to_integer(Product, 48),
    length(Facts, 4),
    format('  PASS: family(6, 8) → 48, 4 facts~n').

%% 3.CA.5: Strategies
test_skip_count :-
    integer_to_recollection(7, R7),
    integer_to_recollection(6, R6),
    mult_skip_count(R7, R6, Product),
    recollection_to_integer(Product, 42),
    format('  PASS: skip_count 7×6 = 42~n').

test_derived_fact :-
    integer_to_recollection(7, R7),
    integer_to_recollection(8, R8),
    mult_derived_fact(R7, R8, Product, _Derivation),
    recollection_to_integer(Product, 56),
    format('  PASS: derived 7×8 = 7×7+7 = 56~n').

test_distributive :-
    integer_to_recollection(6, R6),
    integer_to_recollection(7, R7),
    mult_distributive(R6, R7, Product, _Steps),
    recollection_to_integer(Product, 42),
    format('  PASS: distributive 6×7 = 6×5+6×2 = 42~n').

test_div_inverse :-
    integer_to_recollection(56, R56),
    integer_to_recollection(8, R8),
    div_by_inverse(R56, R8, Quotient),
    recollection_to_integer(Quotient, 7),
    format('  PASS: 56 ÷ 8 = 7 (inverse)~n').

%% All strategies agree on 7×8
test_strategies_agree :-
    integer_to_recollection(7, R7),
    integer_to_recollection(8, R8),
    mult_skip_count(R7, R8, S1),
    mult_derived_fact(R7, R8, S2, _),
    mult_distributive(R7, R8, S3, _),
    multiply_equal_groups(R8, R7, S4),
    recollection_to_integer(S1, N1),
    recollection_to_integer(S2, N2),
    recollection_to_integer(S3, N3),
    recollection_to_integer(S4, N4),
    N1 =:= N2, N2 =:= N3, N3 =:= N4,
    format('  PASS: all strategies agree on 7×8=~w~n', [N1]).
