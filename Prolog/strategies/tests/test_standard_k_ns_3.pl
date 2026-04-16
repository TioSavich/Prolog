/** <module> Tests for Standard K.NS.3 — One-to-one correspondence and cardinality
 */

:- use_module(standards(standard_k_ns_3)).
:- use_module(standards(standard_k_ns_2), [reset_numerals/0, teach_numerals_to/1]).
:- use_module(standards(standard_k_ns_1), [count_by_ones/3, reset_traces/0]).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2,
    zero/1
]).

run_tests :-
    writeln('=== K.NS.3 Cardinality Tests ==='),
    writeln(''),
    reset_numerals,
    reset_traces,
    teach_numerals_to(20),

    test_count_collection,
    test_one_to_one_pairing,
    test_how_many,
    test_how_many_zero,
    test_cardinality_from_trace,
    test_count_out,
    test_count_out_roundtrip,
    test_order_independence,

    writeln(''),
    writeln('=== All K.NS.3 Tests Passed ===').

%% Count a collection of objects
test_count_collection :-
    count_collection([a, b, c, d, e], Count, Pairing),
    recollection_to_integer(Count, 5),
    length(Pairing, 5),
    format('  PASS: count_collection([a,b,c,d,e]) → 5~n').

%% Verify one-to-one pairing structure
test_one_to_one_pairing :-
    count_collection([cat, dog, bird], _Count, Pairing),
    % Each object paired with successive recollections
    Pairing = [pair(cat, R1), pair(dog, R2), pair(bird, R3)],
    recollection_to_integer(R1, 1),
    recollection_to_integer(R2, 2),
    recollection_to_integer(R3, 3),
    format('  PASS: one-to-one pairing: cat→1, dog→2, bird→3~n').

%% "How many?" with named answer
test_how_many :-
    how_many([x, y, x, y, x, y, x], Name),
    Name == seven,
    format('  PASS: how_many([7 objects]) → ~w~n', [Name]).

%% "How many?" for empty collection
test_how_many_zero :-
    how_many([], Name),
    Name == zero,
    format('  PASS: how_many([]) → ~w~n', [Name]).

%% Extract cardinality from a counting trace
test_cardinality_from_trace :-
    integer_to_recollection(0, Zero),
    integer_to_recollection(8, Eight),
    count_by_ones(Zero, Eight, Trace),
    cardinality(Trace, LastCount),
    recollection_to_integer(LastCount, 8),
    format('  PASS: cardinality of count(0,8) trace → 8~n').

%% Count out objects given a name
test_count_out :-
    count_out(four, Objects),
    length(Objects, 4),
    format('  PASS: count_out(four) → ~w objects~n', [4]).

%% Roundtrip: count objects, then count out the same number
test_count_out_roundtrip :-
    Original = [a, b, c, d, e, f],
    how_many(Original, Name),
    count_out(Name, Produced),
    length(Original, N1),
    length(Produced, N2),
    N1 == N2,
    format('  PASS: roundtrip — 6 objects → ~w → 6 objects~n', [Name]).

%% Order independence: same count regardless of arrangement
test_order_independence :-
    verify_order_independence([p, q, r, s, t], Result),
    Result = same(Count),
    recollection_to_integer(Count, 5),
    format('  PASS: order independence — [p,q,r,s,t] same count both ways~n').
