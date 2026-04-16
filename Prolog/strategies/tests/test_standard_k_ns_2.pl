/** <module> Tests for Standard K.NS.2 — Writing numerals and number words
 *
 * Tests the four learning components:
 *   1. Write numbers from 0 to 9
 *   2. Write numbers from 0 to 20
 *   3. Use 0 for no objects
 *   4. Represent objects with a written numeral
 */

:- use_module(standards(standard_k_ns_2)).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2,
    zero/1
]).

run_tests :-
    writeln('=== K.NS.2 Number Writing Tests ==='),
    writeln(''),
    reset_numerals,

    test_learn_and_write_single,
    test_incremental_learning,
    test_write_fails_untaught,
    test_read_numeral,
    test_zero_represents_nothing,
    test_represent_count,
    test_represent_count_fails_untaught,
    test_teach_to_ten,
    test_teach_to_twenty,
    test_idempotent_learning,

    writeln(''),
    writeln('=== All K.NS.2 Tests Passed ===').

%% Teaching a single name and writing it back
test_learn_and_write_single :-
    reset_numerals,
    integer_to_recollection(3, Three),
    learn_numeral(Three, three),
    write_numeral(Three, Name),
    Name == three,
    format('  PASS: learn three, write three → ~w~n', [Name]).

%% Names are learned incrementally — only taught names are available
test_incremental_learning :-
    reset_numerals,
    integer_to_recollection(1, One),
    integer_to_recollection(2, Two),
    learn_numeral(One, one),
    % one is known
    write_numeral(One, _),
    % two is not yet known
    (   write_numeral(Two, _)
    ->  writeln('  FAIL: two should not be known yet'), fail
    ;   true
    ),
    % now teach two
    learn_numeral(Two, two),
    write_numeral(Two, Name2),
    Name2 == two,
    format('  PASS: incremental learning — one before two~n').

%% Writing fails for untaught numerals
test_write_fails_untaught :-
    reset_numerals,
    integer_to_recollection(15, Fifteen),
    (   write_numeral(Fifteen, _)
    ->  writeln('  FAIL: fifteen should be untaught'), fail
    ;   true
    ),
    format('  PASS: write_numeral fails for untaught numeral~n').

%% Reading a numeral (name → recollection)
test_read_numeral :-
    reset_numerals,
    integer_to_recollection(5, Five),
    learn_numeral(Five, five),
    read_numeral(five, Rec),
    recollection_to_integer(Rec, 5),
    format('  PASS: read_numeral(five) → 5~n').

%% Learning Component 3: zero represents no objects
test_zero_represents_nothing :-
    reset_numerals,
    integer_to_recollection(0, Zero),
    learn_numeral(Zero, zero),
    write_numeral(Zero, Name),
    Name == zero,
    % zero is recollection([]) — the empty tally sequence
    Zero = recollection([]),
    format('  PASS: zero = recollection([]), named "zero"~n').

%% Learning Component 4: represent objects with numeral
test_represent_count :-
    reset_numerals,
    teach_numerals_to(10),
    Objects = [apple, banana, cherry, date, elderberry],
    represent_count(Objects, Count, Name),
    recollection_to_integer(Count, 5),
    Name == five,
    format('  PASS: represent_count([5 objects]) → ~w~n', [Name]).

%% represent_count fails when count exceeds taught range
test_represent_count_fails_untaught :-
    reset_numerals,
    teach_numerals_to(3),    % only know 0-3
    Objects = [a, b, c, d, e],  % 5 objects
    (   represent_count(Objects, _, _)
    ->  writeln('  FAIL: should fail — 5 is untaught'), fail
    ;   true
    ),
    format('  PASS: represent_count fails for count beyond taught range~n').

%% Teach all names 0-10 (Learning Component 1)
test_teach_to_ten :-
    reset_numerals,
    teach_numerals_to(10),
    % verify all 11 names are known
    findall(Name, numeral_known(_, Name), Names),
    length(Names, 11),
    % spot check
    integer_to_recollection(7, Seven),
    write_numeral(Seven, seven),
    format('  PASS: teach_numerals_to(10) — ~w names learned~n', [11]).

%% Teach all names 0-20 (Learning Component 2)
test_teach_to_twenty :-
    reset_numerals,
    teach_numerals_to(20),
    findall(Name, numeral_known(_, Name), Names),
    length(Names, 21),
    % spot check teens and twenty
    integer_to_recollection(13, Thirteen),
    write_numeral(Thirteen, thirteen),
    integer_to_recollection(20, Twenty),
    write_numeral(Twenty, twenty),
    format('  PASS: teach_numerals_to(20) — ~w names learned~n', [21]).

%% Learning is idempotent
test_idempotent_learning :-
    reset_numerals,
    integer_to_recollection(1, One),
    learn_numeral(One, one),
    learn_numeral(One, one),  % teach again
    findall(N, numeral_known(_, N), Names),
    length(Names, 1),  % should still be just one entry
    format('  PASS: idempotent — teaching twice does not duplicate~n').
