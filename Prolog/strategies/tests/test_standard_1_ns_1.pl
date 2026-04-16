/** <module> Tests for Standard 1.NS.1 — Count to 120
 */

:- use_module(standards(standard_1_ns_1)).
:- use_module(standards(standard_k_ns_1), [count_by_ones/3, count_by_tens/3, count_on_from/4, reset_traces/0]).
:- use_module(standards(standard_k_ns_2), [reset_numerals/0, teach_numerals_to/1, write_numeral/2]).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2
]).

run_tests :-
    writeln('=== 1.NS.1 Counting to 120 Tests ==='),
    writeln(''),
    reset_numerals,
    reset_traces,

    test_count_by_fives_0_to_25,
    test_count_by_fives_0_to_100,
    test_count_backward_by_ones,
    test_count_backward_by_ones_to_zero,
    test_count_backward_by_tens,
    test_count_by_ones_to_120,
    test_count_by_tens_to_120,
    test_count_on_from_beyond_100,
    test_teach_numerals_to_120,
    test_numeral_naming_spot_check,

    writeln(''),
    writeln('=== All 1.NS.1 Tests Passed ===').

%% Count by fives: 0 to 25
test_count_by_fives_0_to_25 :-
    integer_to_recollection(0, Zero),
    integer_to_recollection(25, TwentyFive),
    count_by_fives(Zero, TwentyFive, Trace),
    length(Trace, 6),  % 0, 5, 10, 15, 20, 25
    format('  PASS: count_by_fives(0, 25) — ~w states~n', [6]).

%% Count by fives: 0 to 100
test_count_by_fives_0_to_100 :-
    integer_to_recollection(0, Zero),
    integer_to_recollection(100, Hundred),
    count_by_fives(Zero, Hundred, Trace),
    length(Trace, 21),  % 0, 5, 10, ..., 100
    format('  PASS: count_by_fives(0, 100) — ~w states~n', [21]).

%% Count backward by ones: 10 to 5
test_count_backward_by_ones :-
    integer_to_recollection(10, Ten),
    integer_to_recollection(5, Five),
    count_backward_by_ones(Ten, Five, Trace),
    length(Trace, 6),  % 10, 9, 8, 7, 6, 5
    format('  PASS: count_backward_by_ones(10, 5) — ~w states~n', [6]).

%% Count backward to zero
test_count_backward_by_ones_to_zero :-
    integer_to_recollection(5, Five),
    integer_to_recollection(0, Zero),
    count_backward_by_ones(Five, Zero, Trace),
    length(Trace, 6),  % 5, 4, 3, 2, 1, 0
    last(Trace, state(Last, predecessor)),
    recollection_to_integer(Last, 0),
    format('  PASS: count_backward_by_ones(5, 0) — reaches zero~n').

%% Count backward by tens: 100 to 20
test_count_backward_by_tens :-
    integer_to_recollection(100, Hundred),
    integer_to_recollection(20, Twenty),
    count_backward_by_tens(Hundred, Twenty, Trace),
    length(Trace, 9),  % 100, 90, 80, ..., 20
    format('  PASS: count_backward_by_tens(100, 20) — ~w states~n', [9]).

%% K.NS.1 predicates work for extended range: count to 120
test_count_by_ones_to_120 :-
    integer_to_recollection(0, Zero),
    integer_to_recollection(120, OneHundredTwenty),
    count_by_ones(Zero, OneHundredTwenty, Trace),
    length(Trace, 121),  % 0 through 120
    format('  PASS: count_by_ones(0, 120) — ~w states~n', [121]).

%% Count by tens to 120
test_count_by_tens_to_120 :-
    integer_to_recollection(0, Zero),
    integer_to_recollection(120, OneHundredTwenty),
    count_by_tens(Zero, OneHundredTwenty, Trace),
    length(Trace, 13),  % 0, 10, 20, ..., 120
    format('  PASS: count_by_tens(0, 120) — ~w states~n', [13]).

%% Count on from beyond 100
test_count_on_from_beyond_100 :-
    integer_to_recollection(105, R105),
    integer_to_recollection(15, Fifteen),
    count_on_from(R105, Fifteen, End, Trace),
    recollection_to_integer(End, 120),
    length(Trace, 16),  % 105, 106, ..., 120
    format('  PASS: count_on_from(105, 15) → ~w~n', [120]).

%% Teach all numerals to 120
test_teach_numerals_to_120 :-
    reset_numerals,
    teach_numerals_to(20),
    teach_numerals_to_120,
    % Spot check: can we name 120?
    integer_to_recollection(120, R120),
    write_numeral(R120, Name120),
    Name120 == 'one hundred twenty',
    format('  PASS: teach_numerals_to_120 — 120 = ~w~n', [Name120]).

%% Spot check naming for various numbers
test_numeral_naming_spot_check :-
    integer_to_recollection(47, R47),
    write_numeral(R47, Name47),
    Name47 == 'forty-seven',
    integer_to_recollection(100, R100),
    write_numeral(R100, Name100),
    Name100 == 'one hundred',
    integer_to_recollection(113, R113),
    write_numeral(R113, Name113),
    Name113 == 'one hundred thirteen',
    format('  PASS: naming spot check — 47=~w, 100=~w, 113=~w~n',
           [Name47, Name100, Name113]).
