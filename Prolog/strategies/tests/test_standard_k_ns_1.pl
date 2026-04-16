/** <module> Tests for Standard K.NS.1 — Counting
 *
 * Tests the three learning components:
 *   1. Count to 10 by ones
 *   2. Count to 100 by ones
 *   3. Count to 100 by tens
 * Plus the Indiana extension: count on from any given number.
 */

:- use_module(standards(standard_k_ns_1)).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2,
    zero/1
]).

run_tests :-
    writeln('=== K.NS.1 Counting Tests ==='),
    writeln(''),
    reset_traces,

    test_count_by_ones_to_ten,
    test_count_by_ones_zero_to_zero,
    test_count_by_tens_to_hundred,
    test_count_on_from,
    test_count_on_from_zero_steps,
    test_trace_structure,
    test_stored_traces,
    test_count_by_ones_to_hundred,

    writeln(''),
    writeln('=== All K.NS.1 Tests Passed ===').

%% Learning Component 1: Count to 10 by ones
test_count_by_ones_to_ten :-
    integer_to_recollection(0, Zero),
    integer_to_recollection(10, Ten),
    count_by_ones(Zero, Ten, Trace),
    length(Trace, 11),   % 0 through 10 = 11 states
    % Verify first and last states
    Trace = [state(First, start)|_],
    last(Trace, state(Last, successor)),
    recollection_to_integer(First, 0),
    recollection_to_integer(Last, 10),
    format('  PASS: count_by_ones(0, 10) — ~w states~n', [11]).

%% Edge case: counting from zero to zero
test_count_by_ones_zero_to_zero :-
    integer_to_recollection(0, Zero),
    count_by_ones(Zero, Zero, Trace),
    length(Trace, 1),
    Trace = [state(_, start)],
    format('  PASS: count_by_ones(0, 0) — 1 state (start only)~n').

%% Learning Component 3: Count to 100 by tens
test_count_by_tens_to_hundred :-
    integer_to_recollection(0, Zero),
    integer_to_recollection(100, Hundred),
    count_by_tens(Zero, Hundred, Trace),
    length(Trace, 11),   % 0, 10, 20, ..., 100 = 11 states
    Trace = [state(First, start)|_],
    last(Trace, state(Last, decade_step)),
    recollection_to_integer(First, 0),
    recollection_to_integer(Last, 100),
    format('  PASS: count_by_tens(0, 100) — ~w states~n', [11]).

%% Indiana extension: count on from any number
test_count_on_from :-
    integer_to_recollection(7, Seven),
    integer_to_recollection(3, Three),
    count_on_from(Seven, Three, End, Trace),
    recollection_to_integer(End, 10),
    length(Trace, 4),    % 7, 8, 9, 10 = 4 states
    format('  PASS: count_on_from(7, 3) — end=~w, ~w states~n', [10, 4]).

%% Edge case: count on with zero steps
test_count_on_from_zero_steps :-
    integer_to_recollection(5, Five),
    integer_to_recollection(0, Zero),
    count_on_from(Five, Zero, End, Trace),
    recollection_to_integer(End, 5),
    length(Trace, 1),
    format('  PASS: count_on_from(5, 0) — end=5, 1 state~n').

%% Verify trace structure
test_trace_structure :-
    integer_to_recollection(0, Zero),
    integer_to_recollection(3, Three),
    count_by_ones(Zero, Three, Trace),
    % Every element should be state(Recollection, Type)
    forall(
        member(State, Trace),
        (   State = state(recollection(_), _)
        ->  true
        ;   format('  FAIL: bad trace element: ~w~n', [State]),
            fail
        )
    ),
    % Check sequence: 0, 1, 2, 3
    maplist(trace_to_int, Trace, Ints),
    Ints = [0, 1, 2, 3],
    format('  PASS: trace structure verified (recollection states, sequential)~n').

trace_to_int(state(Rec, _), Int) :-
    recollection_to_integer(Rec, Int).

%% Verify stored traces persist
test_stored_traces :-
    % Previous tests should have stored traces
    findall(Dir, stored_trace(_, _, Dir, _), Dirs),
    length(Dirs, N),
    N > 0,
    format('  PASS: ~w traces stored for reflection~n', [N]).

%% Learning Component 2: Count to 100 by ones (performance test)
test_count_by_ones_to_hundred :-
    integer_to_recollection(0, Zero),
    integer_to_recollection(100, Hundred),
    count_by_ones(Zero, Hundred, Trace),
    length(Trace, 101),  % 0 through 100 = 101 states
    last(Trace, state(Last, successor)),
    recollection_to_integer(Last, 100),
    format('  PASS: count_by_ones(0, 100) — ~w states~n', [101]).
