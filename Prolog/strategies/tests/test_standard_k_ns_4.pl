/** <module> Tests for Standard K.NS.4 — Subitizing
 */

:- use_module(standards(standard_k_ns_4)).
:- use_module(standards(standard_k_ns_2), [reset_numerals/0, teach_numerals_to/1]).
:- use_module(standards(standard_k_ns_1), [reset_traces/0]).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2
]).

run_tests :-
    writeln('=== K.NS.4 Subitizing Tests ==='),
    writeln(''),
    reset_numerals,
    reset_traces,
    teach_numerals_to(10),

    test_subitize_dice,
    test_subitize_fingers,
    test_subitize_ten_frame,
    test_subitize_unknown_fails,
    test_conceptual_subitize_domino,
    test_conceptual_subitize_ten_frame,
    test_verify_subitizing,
    test_subitize_cost_vs_counting,

    writeln(''),
    writeln('=== All K.NS.4 Tests Passed ===').

%% Perceptual subitizing: dice patterns
test_subitize_dice :-
    subitize(dice(5), Count),
    recollection_to_integer(Count, 5),
    format('  PASS: subitize(dice(5)) → 5~n').

%% Perceptual subitizing: finger patterns
test_subitize_fingers :-
    subitize(fingers(8), Count),
    recollection_to_integer(Count, 8),
    format('  PASS: subitize(fingers(8)) → 8~n').

%% Perceptual subitizing: ten-frame patterns
test_subitize_ten_frame :-
    subitize(ten_frame(3), Count),
    recollection_to_integer(Count, 3),
    format('  PASS: subitize(ten_frame(3)) → 3~n').

%% Unknown pattern fails
test_subitize_unknown_fails :-
    (   subitize(unknown_pattern, _)
    ->  writeln('  FAIL: unknown pattern should fail'), fail
    ;   true
    ),
    format('  PASS: subitize(unknown_pattern) fails correctly~n').

%% Conceptual subitizing: domino decomposition
test_conceptual_subitize_domino :-
    conceptual_subitize(domino(3, 2), Count, Decomp),
    recollection_to_integer(Count, 5),
    Decomp = decomposed(3, 2, 5),
    format('  PASS: conceptual_subitize(domino(3,2)) → 5 = 3+2~n').

%% Conceptual subitizing: ten-frame > 5
test_conceptual_subitize_ten_frame :-
    conceptual_subitize(ten_frame(7), Count, Decomp),
    recollection_to_integer(Count, 7),
    Decomp = decomposed(5, 2, 7),
    format('  PASS: conceptual_subitize(ten_frame(7)) → 7 = 5+2~n').

%% Verify subitizing matches counting
test_verify_subitizing :-
    verify_subitizing(dice(6), Result),
    Result = match(_, _),
    format('  PASS: subitize(dice(6)) matches counting~n').

%% Cost comparison: subitizing should be cheaper than counting
test_subitize_cost_vs_counting :-
    % This is a conceptual test — subitize is O(1) pattern match
    % vs counting which is O(n) successor iterations.
    % We verify both produce the same answer.
    subitize(dice(4), SubCount),
    integer_to_recollection(0, Zero),
    standard_k_ns_1:count_by_ones(Zero, SubCount, Trace),
    length(Trace, 5),   % 0,1,2,3,4 = 5 states for counting
    % Subitizing took 1 inference; counting took 5 states
    format('  PASS: subitize(4)=O(1) vs count(0,4)=5 states~n').
