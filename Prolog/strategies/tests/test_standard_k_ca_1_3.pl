/** <module> Tests for Standards K.CA.1-3 — Operations within 10
 */

:- use_module(standards(standard_k_ca_1_3)).
:- use_module(standards(standard_k_ns_2), [reset_numerals/0, teach_numerals_to/1]).
:- use_module(standards(standard_k_ns_1), [reset_traces/0]).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2
]).

run_tests :-
    writeln('=== K.CA.1-3 Operations Tests ==='),
    writeln(''),
    reset_numerals,
    reset_traces,
    teach_numerals_to(10),

    test_add_objects,
    test_add_objects_with_zero,
    test_subtract_objects,
    test_subtract_to_zero,
    test_decompose_pairs_5,
    test_decompose_pairs_10,
    test_decompose_pairs_1,
    test_complement_to_ten,
    test_complement_all,
    test_complement_fails_at_ten,

    writeln(''),
    writeln('=== All K.CA.1-3 Tests Passed ===').

%% K.CA.1: Add two groups
test_add_objects :-
    add_objects([a,b,c], [x,y], Total),
    recollection_to_integer(Total, 5),
    format('  PASS: add_objects([3], [2]) → 5~n').

test_add_objects_with_zero :-
    add_objects([a,b,c], [], Total),
    recollection_to_integer(Total, 3),
    format('  PASS: add_objects([3], []) → 3~n').

%% K.CA.1: Subtract
test_subtract_objects :-
    integer_to_recollection(3, Three),
    subtract_objects([a,b,c,d,e,f,g], Three, Remaining),
    recollection_to_integer(Remaining, 4),
    format('  PASS: subtract_objects([7], 3) → 4~n').

test_subtract_to_zero :-
    integer_to_recollection(5, Five),
    subtract_objects([a,b,c,d,e], Five, Remaining),
    recollection_to_integer(Remaining, 0),
    format('  PASS: subtract_objects([5], 5) → 0~n').

%% K.CA.2: Decompose 5 into pairs
test_decompose_pairs_5 :-
    integer_to_recollection(5, Five),
    decompose_pairs(Five, Pairs),
    length(Pairs, 3),  % (0,5), (1,4), (2,3)
    format('  PASS: decompose_pairs(5) → ~w pairs~n', [3]).

%% K.CA.2: Decompose 10
test_decompose_pairs_10 :-
    integer_to_recollection(10, Ten),
    decompose_pairs(Ten, Pairs),
    length(Pairs, 6),  % (0,10), (1,9), (2,8), (3,7), (4,6), (5,5)
    format('  PASS: decompose_pairs(10) → ~w pairs~n', [6]).

%% K.CA.2: Decompose 1
test_decompose_pairs_1 :-
    integer_to_recollection(1, One),
    decompose_pairs(One, Pairs),
    length(Pairs, 1),  % only (0,1)
    format('  PASS: decompose_pairs(1) → ~w pair~n', [1]).

%% K.CA.3: Complement to make 10
test_complement_to_ten :-
    integer_to_recollection(7, Seven),
    find_complement_to_ten(Seven, Complement),
    recollection_to_integer(Complement, 3),
    format('  PASS: 7 + ? = 10 → 3~n').

%% K.CA.3: All complements 1-9
test_complement_all :-
    forall(
        (   between(1, 9, N),
            integer_to_recollection(N, Rec)
        ),
        (   find_complement_to_ten(Rec, Comp),
            recollection_to_integer(Comp, C),
            Expected is 10 - N,
            C =:= Expected
        )
    ),
    format('  PASS: all complements 1-9 correct~n').

%% K.CA.3: Fails for 10 (already at ten)
test_complement_fails_at_ten :-
    integer_to_recollection(10, Ten),
    (   find_complement_to_ten(Ten, _)
    ->  writeln('  FAIL: complement(10) should fail'), fail
    ;   true
    ),
    format('  PASS: find_complement_to_ten(10) fails correctly~n').
