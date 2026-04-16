/** <module> Tests for Standard 1.NS.2 — Two-digit place value
 */

:- use_module(standards(standard_1_ns_2)).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2
]).

run_tests :-
    writeln('=== 1.NS.2 Place Value Tests ==='),
    writeln(''),

    test_decompose_47,
    test_decompose_10,
    test_decompose_90,
    test_decompose_5,
    test_decompose_0,
    test_compose_47,
    test_compose_roundtrip,
    test_multiple_of_ten,
    test_not_multiple_of_ten,
    test_describe,

    writeln(''),
    writeln('=== All 1.NS.2 Tests Passed ===').

test_decompose_47 :-
    integer_to_recollection(47, R47),
    decompose_two_digit(R47, Tens, Ones),
    recollection_to_integer(Tens, 4),
    recollection_to_integer(Ones, 7),
    format('  PASS: decompose(47) → 4 tens, 7 ones~n').

test_decompose_10 :-
    integer_to_recollection(10, R10),
    decompose_two_digit(R10, Tens, Ones),
    recollection_to_integer(Tens, 1),
    recollection_to_integer(Ones, 0),
    format('  PASS: decompose(10) → 1 ten, 0 ones~n').

test_decompose_90 :-
    integer_to_recollection(90, R90),
    decompose_two_digit(R90, Tens, Ones),
    recollection_to_integer(Tens, 9),
    recollection_to_integer(Ones, 0),
    format('  PASS: decompose(90) → 9 tens, 0 ones~n').

test_decompose_5 :-
    integer_to_recollection(5, R5),
    decompose_two_digit(R5, Tens, Ones),
    recollection_to_integer(Tens, 0),
    recollection_to_integer(Ones, 5),
    format('  PASS: decompose(5) → 0 tens, 5 ones~n').

test_decompose_0 :-
    integer_to_recollection(0, R0),
    decompose_two_digit(R0, Tens, Ones),
    recollection_to_integer(Tens, 0),
    recollection_to_integer(Ones, 0),
    format('  PASS: decompose(0) → 0 tens, 0 ones~n').

test_compose_47 :-
    integer_to_recollection(4, R4),
    integer_to_recollection(7, R7),
    compose_two_digit(R4, R7, Result),
    recollection_to_integer(Result, 47),
    format('  PASS: compose(4, 7) → 47~n').

test_compose_roundtrip :-
    integer_to_recollection(83, R83),
    decompose_two_digit(R83, Tens, Ones),
    compose_two_digit(Tens, Ones, Recomposed),
    recollection_to_integer(Recomposed, 83),
    format('  PASS: decompose(83) → compose → 83~n').

test_multiple_of_ten :-
    integer_to_recollection(30, R30),
    (   is_multiple_of_ten(R30)
    ->  true
    ;   writeln('  FAIL: 30 should be multiple of ten'), fail
    ),
    format('  PASS: is_multiple_of_ten(30) → true~n').

test_not_multiple_of_ten :-
    integer_to_recollection(31, R31),
    (   is_multiple_of_ten(R31)
    ->  writeln('  FAIL: 31 should not be multiple of ten'), fail
    ;   true
    ),
    format('  PASS: is_multiple_of_ten(31) → false~n').

test_describe :-
    integer_to_recollection(56, R56),
    describe_two_digit(R56, Desc),
    Desc = place_value(5, 6),
    format('  PASS: describe(56) → place_value(5, 6)~n').
