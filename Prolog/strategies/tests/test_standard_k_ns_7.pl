/** <module> Tests for Standard K.NS.7 — Place value
 */

:- use_module(standards(standard_k_ns_7)).
:- use_module(formalization(grounded_arithmetic), [
    integer_to_recollection/2,
    recollection_to_integer/2,
    zero/1
]).

run_tests :-
    writeln('=== K.NS.7 Place Value Tests ==='),
    writeln(''),

    test_make_ten_exact,
    test_make_ten_with_remainder,
    test_make_ten_fails_under_ten,
    test_decompose_teen_14,
    test_decompose_teen_10,
    test_decompose_teen_20,
    test_decompose_under_10,
    test_compose_teen,
    test_compose_roundtrip,
    test_describe_place_value,

    writeln(''),
    writeln('=== All K.NS.7 Tests Passed ===').

%% Make a ten from exactly 10
test_make_ten_exact :-
    integer_to_recollection(10, Ten),
    make_ten(Ten, ten_group(TenRec, RemRec)),
    recollection_to_integer(TenRec, 10),
    recollection_to_integer(RemRec, 0),
    format('  PASS: make_ten(10) → ten_group(10, 0)~n').

%% Make a ten from more than 10
test_make_ten_with_remainder :-
    integer_to_recollection(14, Fourteen),
    make_ten(Fourteen, ten_group(TenRec, RemRec)),
    recollection_to_integer(TenRec, 10),
    recollection_to_integer(RemRec, 4),
    format('  PASS: make_ten(14) → ten_group(10, 4)~n').

%% Make a ten fails with fewer than 10
test_make_ten_fails_under_ten :-
    integer_to_recollection(7, Seven),
    (   make_ten(Seven, _)
    ->  writeln('  FAIL: make_ten(7) should fail'), fail
    ;   true
    ),
    format('  PASS: make_ten(7) fails — not enough ones~n').

%% Decompose 14 → 1 ten, 4 ones
test_decompose_teen_14 :-
    integer_to_recollection(14, Fourteen),
    decompose_teen(Fourteen, Tens, Ones),
    recollection_to_integer(Tens, 1),
    recollection_to_integer(Ones, 4),
    format('  PASS: decompose_teen(14) → 1 ten, 4 ones~n').

%% Decompose 10 → 1 ten, 0 ones
test_decompose_teen_10 :-
    integer_to_recollection(10, Ten),
    decompose_teen(Ten, Tens, Ones),
    recollection_to_integer(Tens, 1),
    recollection_to_integer(Ones, 0),
    format('  PASS: decompose_teen(10) → 1 ten, 0 ones~n').

%% Decompose 20 → 1 ten, 10 ones (K.NS.7 only handles one ten-group)
test_decompose_teen_20 :-
    integer_to_recollection(20, Twenty),
    decompose_teen(Twenty, Tens, Ones),
    recollection_to_integer(Tens, 1),
    recollection_to_integer(Ones, 10),
    format('  PASS: decompose_teen(20) → 1 ten, 10 ones~n').

%% Numbers under 10: zero tens
test_decompose_under_10 :-
    integer_to_recollection(7, Seven),
    decompose_teen(Seven, Tens, Ones),
    recollection_to_integer(Tens, 0),
    recollection_to_integer(Ones, 7),
    format('  PASS: decompose_teen(7) → 0 tens, 7 ones~n').

%% Compose from tens and ones
test_compose_teen :-
    integer_to_recollection(1, One),
    integer_to_recollection(6, Six),
    compose_teen(One, Six, Result),
    recollection_to_integer(Result, 16),
    format('  PASS: compose_teen(1, 6) → 16~n').

%% Roundtrip: decompose then compose
test_compose_roundtrip :-
    integer_to_recollection(13, Thirteen),
    decompose_teen(Thirteen, Tens, Ones),
    compose_teen(Tens, Ones, Recomposed),
    recollection_to_integer(Recomposed, 13),
    format('  PASS: decompose(13) → compose → 13 roundtrip~n').

%% Place-value description
test_describe_place_value :-
    integer_to_recollection(17, Seventeen),
    describe_place_value(Seventeen, Desc),
    Desc = place_value(1, 7),
    format('  PASS: describe(17) → place_value(1, 7)~n').
