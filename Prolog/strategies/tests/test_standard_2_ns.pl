/** <module> Tests for Grade 2 Number Sense standards
 */

:- use_module(standards(standard_2_ns_1)).
:- use_module(standards(standard_2_ns_2_4)).
:- use_module(standards(standard_2_ns_3)).
:- use_module(standards(standard_2_ns_5)).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2
]).

run_tests :-
    writeln('=== Grade 2 Number Sense Tests ==='),
    writeln(''),

    % 2.NS.1
    test_count_by_twos,
    test_count_by_hundreds,
    % 2.NS.2/4
    test_decompose_347,
    test_decompose_500,
    test_decompose_7,
    test_compose_roundtrip,
    test_expanded_form,
    % 2.NS.3
    test_even,
    test_odd,
    test_zero_even,
    % 2.NS.5
    test_compare_hundreds,
    test_compare_tens,
    test_compare_ones,
    test_compare_equal,

    writeln(''),
    writeln('=== All Grade 2 NS Tests Passed ===').

%% 2.NS.1: Count by twos
test_count_by_twos :-
    integer_to_recollection(0, Zero),
    integer_to_recollection(20, Twenty),
    count_by_twos(Zero, Twenty, Trace),
    length(Trace, 11),  % 0,2,4,...,20
    format('  PASS: count_by_twos(0, 20) — ~w states~n', [11]).

test_count_by_hundreds :-
    integer_to_recollection(0, Zero),
    integer_to_recollection(1000, Thousand),
    count_by_hundreds(Zero, Thousand, Trace),
    length(Trace, 11),  % 0,100,...,1000
    format('  PASS: count_by_hundreds(0, 1000) — ~w states~n', [11]).

%% 2.NS.2/4: Three-digit place value
test_decompose_347 :-
    integer_to_recollection(347, R),
    decompose_three_digit(R, H, T, O),
    recollection_to_integer(H, 3),
    recollection_to_integer(T, 4),
    recollection_to_integer(O, 7),
    format('  PASS: decompose(347) → 3h, 4t, 7o~n').

test_decompose_500 :-
    integer_to_recollection(500, R),
    decompose_three_digit(R, H, T, O),
    recollection_to_integer(H, 5),
    recollection_to_integer(T, 0),
    recollection_to_integer(O, 0),
    format('  PASS: decompose(500) → 5h, 0t, 0o~n').

test_decompose_7 :-
    integer_to_recollection(7, R),
    decompose_three_digit(R, H, T, O),
    recollection_to_integer(H, 0),
    recollection_to_integer(T, 0),
    recollection_to_integer(O, 7),
    format('  PASS: decompose(7) → 0h, 0t, 7o~n').

test_compose_roundtrip :-
    integer_to_recollection(819, R),
    decompose_three_digit(R, H, T, O),
    compose_three_digit(H, T, O, Recomposed),
    recollection_to_integer(Recomposed, 819),
    format('  PASS: decompose(819) → compose → 819~n').

test_expanded_form :-
    integer_to_recollection(256, R),
    expanded_form(R, Terms),
    Terms = [hundreds(2), tens(5), ones(6)],
    format('  PASS: expanded_form(256) → ~w~n', [Terms]).

%% 2.NS.3: Odd/even
test_even :-
    integer_to_recollection(14, R),
    (is_even(R) -> true ; (writeln('FAIL'), fail)),
    format('  PASS: is_even(14)~n').

test_odd :-
    integer_to_recollection(7, R),
    (is_odd(R) -> true ; (writeln('FAIL'), fail)),
    format('  PASS: is_odd(7)~n').

test_zero_even :-
    integer_to_recollection(0, R),
    (is_even(R) -> true ; (writeln('FAIL'), fail)),
    format('  PASS: is_even(0)~n').

%% 2.NS.5: Three-digit comparison
test_compare_hundreds :-
    integer_to_recollection(500, R5),
    integer_to_recollection(300, R3),
    compare_by_place_value(R5, R3, greater_than),
    format('  PASS: 500 > 300~n').

test_compare_tens :-
    integer_to_recollection(350, R350),
    integer_to_recollection(370, R370),
    compare_by_place_value(R350, R370, less_than),
    format('  PASS: 350 < 370~n').

test_compare_ones :-
    integer_to_recollection(341, R341),
    integer_to_recollection(347, R347),
    compare_by_place_value(R341, R347, less_than),
    format('  PASS: 341 < 347~n').

test_compare_equal :-
    integer_to_recollection(456, R456a),
    integer_to_recollection(456, R456b),
    compare_by_place_value(R456a, R456b, equal_to),
    format('  PASS: 456 = 456~n').
