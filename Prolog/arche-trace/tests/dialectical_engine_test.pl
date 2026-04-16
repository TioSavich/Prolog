/** <module> Dialectical Engine Tests
 *
 *  Tests the dialectical_engine module: run_computation/2 (the ORR cycle
 *  entry point) and run_fsm/4 (the generic FSM executor).
 *
 *  run_computation/2 wraps proves/4 in a catch/3, routing perturbations
 *  through critique:accommodate/1. For resource_exhaustion, accommodate/1
 *  always fails (records stress, then fails), so the computation halts.
 *  These tests verify both the happy path (proof succeeds) and the
 *  perturbation path (proof exhausts resources, stress is recorded).
 *
 *  run_fsm/4 is a generic FSM stepper that calls Module:transition/3 and
 *  Module:accept_state/1. Tests use inline test modules to exercise the
 *  happy path, stuck state, and immediate accept.
 */

:- ensure_loaded('../../paths').
:- ensure_loaded(arche_trace(load)).

% Mirror operator declarations for parsing convenience
:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, comp_poss).
:- op(500, fx, neg).
:- op(1050, xfy, =>).

% =================================================================
% Test Infrastructure (follows core_test.pl pattern)
% =================================================================

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
    format('~n~n=== TEST SUMMARY ===~n', []),
    findall(_, test_result(_, pass, _), Passes),
    findall(_, test_result(_, fail, _), Fails),
    length(Passes, PassCount),
    length(Fails, FailCount),
    Total is PassCount + FailCount,
    format('Passed: ~w / ~w~n', [PassCount, Total]),
    format('Failed: ~w / ~w~n', [FailCount, Total]),
    (FailCount > 0 ->
        writeln('\nFailed tests:'),
        forall(test_result(Name, fail, _), format('  - ~w~n', [Name]))
    ; true).

% =================================================================
% Inline FSM Test Modules (asserted dynamically)
% =================================================================
% SWI-Prolog creates modules automatically when we assert into them.

setup_test_fsm_modules :-
    % --- A simple two-state automaton: q0 -> q1 (accept) ---
    assertz(test_fsm_simple:transition(state(q0, start), state(q1, done), 'Moved from q0 to q1')),
    assertz((test_fsm_simple:accept_state(state(q1, _)))),

    % --- An automaton with no valid transition from q0 (transition exists but doesn't match) ---
    assertz((test_fsm_stuck:transition(state(q_unreachable, _), state(q_unreachable, _), 'Never matched'))),
    assertz((test_fsm_stuck:accept_state(state(q_accept, _)))),

    % --- An automaton where initial state is already an accept state ---
    assertz((test_fsm_immediate:accept_state(state(q_done, _)))),
    assertz((test_fsm_immediate:final_interpretation(state(q_done, _), 'Already at goal'))).

:- setup_test_fsm_modules.

% =================================================================
% Test Suite
% =================================================================

run_all_tests :-
    retractall(test_result(_, _, _)),
    writeln('=== DIALECTICAL ENGINE TEST SUITE ==='),

    test_run_computation,
    test_perturbation_handling,
    test_fsm_engine,

    print_summary.

% =================================================================
% 1. run_computation/2 — Happy Path
% =================================================================

test_run_computation :-
    writeln('\n--- RUN_COMPUTATION: HAPPY PATH ---'),
    critique:reset_stress_map,

    run_test('Identity sequent succeeds', (
        dialectical_engine:run_computation([a] => [a], 10)
    )),

    run_test('PML rhythm: U -> Box_down(A)', (
        dialectical_engine:run_computation([s(u)] => [s(comp_nec a)], 50)
    )),

    run_test('PML rhythm: A -> Diamond_up(LG)', (
        dialectical_engine:run_computation([s(a)] => [s(exp_poss lg)], 50)
    )),

    run_test('Oobleck dynamic: S -> O transfer', (
        dialectical_engine:run_computation([s(comp_nec p)] => [o(comp_nec p)], 50)
    )),

    run_test('Explosion from contradiction', (
        dialectical_engine:run_computation([a, neg(a)] => [b], 10)
    )).

% =================================================================
% 2. run_computation/2 — Perturbation Handling
% =================================================================

test_perturbation_handling :-
    writeln('\n--- RUN_COMPUTATION: PERTURBATION HANDLING ---'),
    critique:reset_stress_map,

    run_test('Zero resources triggers perturbation and fails', (
        \+ dialectical_engine:run_computation([a] => [a], 0)
    )),

    run_test('Stress map updated after resource exhaustion', (
        critique:reset_stress_map,
        % This will fail (resource exhaustion, accommodate fails)
        ( dialectical_engine:run_computation([a] => [a], 0) -> true ; true ),
        % But stress should have been recorded
        critique:get_stress_map(Map1),
        Map1 \= []
    )),

    run_test('Insufficient resources for PML dynamics fails', (
        % PML rhythm costs at least 2 per compressive step
        \+ dialectical_engine:run_computation([s(u)] => [s(comp_nec a)], 1)
    )),

    run_test('Stress accumulates across repeated failures', (
        critique:reset_stress_map,
        % Fail twice on the same sequent
        ignore(dialectical_engine:run_computation([a] => [a], 0)),
        ignore(dialectical_engine:run_computation([a] => [a], 0)),
        critique:get_stress_map(Map2),
        % Should have at least one stress entry with count >= 2
        Map2 \= [],
        member(stress(_, Count2), Map2),
        Count2 >= 2
    )),

    run_test('Non-perturbation errors are caught gracefully', (
        % An unstructured error should be caught by the second
        % handle_perturbation clause and fail without crashing
        \+ catch(
            dialectical_engine:run_computation(not_a_valid_sequent, 10),
            _,
            true  % Any error is fine, as long as we don't crash
        )
    )).

% =================================================================
% 3. run_fsm/4 — Generic FSM Engine
% =================================================================

test_fsm_engine :-
    writeln('\n--- RUN_FSM: GENERIC FSM ENGINE ---'),

    run_test('FSM: simple two-state automaton', (
        dialectical_engine:run_fsm(test_fsm_simple, state(q0, start), [], H1),
        length(H1, 2),
        H1 = [step(q0, start, 'Moved from q0 to q1'), step(q1, _, accept)]
    )),

    run_test('FSM: stuck state (no valid transition)', (
        dialectical_engine:run_fsm(test_fsm_stuck, state(q0, init), [], H2),
        H2 = [step(q0, init, stuck)]
    )),

    run_test('FSM: immediate accept state', (
        dialectical_engine:run_fsm(test_fsm_immediate, state(q_done, ready), [], H3),
        H3 = [step(q_done, ready, 'Already at goal')]
    )),

    run_test('FSM: bare atom state (no state/2 wrapper)', (
        dialectical_engine:run_fsm(test_fsm_stuck, q_unknown_bare, [], H4),
        H4 = [step(q_unknown_bare, [], stuck)]
    )),

    run_test('FSM: history preserves execution order', (
        dialectical_engine:run_fsm(test_fsm_simple, state(q0, start), [], H5),
        nth1(1, H5, step(q0, _, _)),
        nth1(2, H5, step(q1, _, _))
    )).

% =================================================================
% Entry Point
% =================================================================

:- initialization(run_all_tests).
