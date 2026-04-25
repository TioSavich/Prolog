:- ensure_loaded('../../paths').

:- use_module(library(time), [call_with_time_limit/2]).
:- use_module(arche_trace(incompatibility_semantics)).
:- use_module(arche_trace(automata), [highlander/2]).
:- use_module(strategies(hermeneutic_calculator), [calculate/6]).
:- use_module(math(sar_add_cobo), [run_cobo/4]).
:- use_module(math(sar_sub_cobo_missing_addend), [run_cobo_ma/4]).
:- use_module(math(sar_sub_sliding), [run_sliding/4]).

:- dynamic test_result/3.

run_test(Name, Goal) :-
    format('~n[TEST] ~w~n', [Name]),
    ( catch(Goal, Error, (format('  ERROR: ~w~n', [Error]), fail)) ->
        assertz(test_result(Name, pass, ok)),
        writeln('  PASS')
    ;
        assertz(test_result(Name, fail, goal_failed)),
        writeln('  FAIL')
    ).

print_summary :-
    format('~n~n=== REGRESSION SUMMARY ===~n', []),
    findall(_, test_result(_, pass, _), Passes),
    findall(_, test_result(_, fail, _), Fails),
    length(Passes, PassCount),
    length(Fails, FailCount),
    format('Passed: ~w~n', [PassCount]),
    format('Failed: ~w~n', [FailCount]),
    ( FailCount > 0 ->
        writeln('Failed tests:'),
        forall(test_result(Name, fail, _), format('  - ~w~n', [Name]))
    ; true
    ).

cobo_addition_is_deterministic :-
    findall(H, run_cobo(2, 1, 3, H), Histories),
    Histories = [_].

cobo_missing_addend_is_deterministic :-
    findall(H, run_cobo_ma(15, 8, 7, H), Histories),
    Histories = [_].

sliding_is_deterministic :-
    findall(H, run_sliding(15, 8, 7, H), Histories),
    Histories = [_].

run_tests :-
    retractall(test_result(_, _, _)),
    writeln('=== AXIOM / AUTOMATA REGRESSION TESTS ==='),

    run_test('Loader script can be loaded cleanly', (
        load_files(arche_trace(load), [if(not_loaded)])
    )),

    run_test('Domain placeholders do not succeed as raw goals', (
        \+ incompatibility_semantics:square(foo),
        \+ incompatibility_semantics:prime(4),
        \+ incompatibility_semantics:rdiv(a, b)
    )),

    run_test('Automata highlander remains strict', (
        highlander([single], single),
        \+ highlander([a, b], _)
    )),

    run_test('Rational multiplication is correct', (
        proves([] => [o(mult(1 rdiv 2, 1 rdiv 3, 1 rdiv 6))])
    )),

    run_test('Positive recollection is canonical', (
        findall(H, is_recollection(3, H), Histories),
        Histories = [[succ(2), succ(1), succ(0), axiom(zero)]]
    )),

    run_test('Negative recollection terminates quickly', (
        call_with_time_limit(1, once(is_recollection(-1, History))),
        History = [integer_extension(negative, 1), succ(0), axiom(zero)]
    )),

    run_test('Natural-number crisis works on raw integers', (
        catch(call_with_time_limit(1, check_norms(subtract(1, 2, _))),
              normative_crisis(subtract(1, 2, _), natural_numbers),
              true)
    )),

    run_test('Robinson pack can be switched off per query', (
        safe_proves([] => [o(plus(1, 2, 3))], [packs([robinson]), time_limit(1)]),
        \+ safe_proves([] => [o(plus(1, 2, 3))], [packs([geometry]), time_limit(1)])
    )),

    run_test('COBO addition is deterministic', (
        cobo_addition_is_deterministic
    )),

    run_test('COBO missing-addend subtraction is deterministic', (
        cobo_missing_addend_is_deterministic
    )),

    run_test('Sliding subtraction is deterministic', (
        sliding_is_deterministic
    )),

    run_test('Calculator accepts Sub Rounding alias', (
        calculate(10, -, 3, 'Sub Rounding', 7, _)
    )),

    print_summary.
