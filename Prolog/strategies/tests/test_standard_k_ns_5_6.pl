/** <module> Tests for Standards K.NS.5-6 — Comparison
 */

:- use_module(standards(standard_k_ns_5_6)).
:- use_module(standards(standard_k_ns_2), [reset_numerals/0, teach_numerals_to/1]).
:- use_module(standards(standard_k_ns_1), [reset_traces/0]).

run_tests :-
    writeln('=== K.NS.5-6 Comparison Tests ==='),
    writeln(''),
    reset_numerals,
    reset_traces,
    teach_numerals_to(20),

    test_compare_groups_greater,
    test_compare_groups_less,
    test_compare_groups_equal,
    test_compare_empty,
    test_matching_strategy,
    test_counting_strategy,
    test_strategies_agree,
    test_compare_numerals,
    test_compare_numerals_equal,

    writeln(''),
    writeln('=== All K.NS.5-6 Tests Passed ===').

test_compare_groups_greater :-
    compare_groups([a,b,c,d,e], [x,y,z], Result),
    Result == greater_than,
    format('  PASS: [5] > [3] → greater_than~n').

test_compare_groups_less :-
    compare_groups([a,b], [x,y,z,w], Result),
    Result == less_than,
    format('  PASS: [2] < [4] → less_than~n').

test_compare_groups_equal :-
    compare_groups([a,b,c], [x,y,z], Result),
    Result == equal_to,
    format('  PASS: [3] = [3] → equal_to~n').

test_compare_empty :-
    compare_groups([], [], Result),
    Result == equal_to,
    format('  PASS: [] = [] → equal_to~n').

test_matching_strategy :-
    compare_by_matching([a,b,c,d], [x,y], Result),
    Result == greater_than,
    format('  PASS: matching [4] vs [2] → greater_than~n').

test_counting_strategy :-
    compare_by_counting([a], [x,y,z,w,v], Result),
    Result == less_than,
    format('  PASS: counting [1] vs [5] → less_than~n').

%% Both strategies should agree
test_strategies_agree :-
    A = [a,b,c,d,e,f,g],
    B = [x,y,z],
    compare_by_matching(A, B, R1),
    compare_by_counting(A, B, R2),
    R1 == R2,
    format('  PASS: matching and counting agree (~w)~n', [R1]).

%% K.NS.6: Compare written numerals
test_compare_numerals :-
    compare_numerals(seven, three, Result),
    Result == greater_than,
    format('  PASS: seven > three → greater_than~n').

test_compare_numerals_equal :-
    compare_numerals(five, five, Result),
    Result == equal_to,
    format('  PASS: five = five → equal_to~n').
