/** <module> PML Core Framework Tests
 *
 *  Tests the fundamental functionality of the PML Core Framework.
 *  Tests are organized by module and theoretical concept.
 */

:- use_module('../load').

% =================================================================
% Test Infrastructure
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
    format('Passed: ~w~n', [PassCount]),
    format('Failed: ~w~n', [FailCount]),
    (FailCount > 0 ->
        writeln('\nFailed tests:'),
        forall(test_result(Name, fail, _), format('  - ~w~n', [Name]))
    ; true).

% =================================================================
% Test Suite
% =================================================================

run_all_tests :-
    retractall(test_result(_, _, _)),
    writeln('=== PML CORE FRAMEWORK TEST SUITE ==='),

    % 1. Basic Infrastructure
    test_basic_infrastructure,

    % 2. Automata
    test_automata,

    % 3. Prover Basics
    test_prover_basics,

    % 4. PML Dynamics
    test_pml_dynamics,

    % 5. Trace Mechanism
    test_trace_mechanism,

    print_summary.

% =================================================================
% 1. Basic Infrastructure Tests
% =================================================================

test_basic_infrastructure :-
    writeln('\n--- BASIC INFRASTRUCTURE ---'),

    run_test('Module loading', (
        current_module(pml_operators),
        current_module(incompatibility_semantics),
        current_module(automata)
    )),

    run_test('Operator definitions', (
        current_op(500, fx, comp_nec),
        current_op(500, fx, exp_nec),
        current_op(1050, xfy, =>)
    )),

    run_test('Utils: select/3', (
        utils:select(2, [1,2,3], [1,3])
    )),

    run_test('Utils: match_antecedents/2', (
        utils:match_antecedents([a, b], [a, b, c])
    )).

% =================================================================
% 2. Automata Tests
% =================================================================

test_automata :-
    writeln('\n--- AUTOMATA ---'),

    run_test('Highlander: single element', (
        automata:highlander([x], x)
    )),

    run_test('Highlander: rejects multiple', (
        \+ automata:highlander([x, y], _)
    )),

    run_test('Highlander: rejects empty', (
        \+ automata:highlander([], _)
    )),

    run_test('Prime utilities: is_prime/1', (
        automata:is_prime(2),
        automata:is_prime(3),
        automata:is_prime(7),
        \+ automata:is_prime(4),
        \+ automata:is_prime(9)
    )),

    run_test('Prime utilities: nth_prime/2', (
        automata:nth_prime(1, 2),
        automata:nth_prime(2, 3),
        automata:nth_prime(4, 7)
    )),

    run_test('Trace: generate_trace/1', (
        automata:generate_trace(T),
        automata:contains_trace(T)
    )),

    run_test('Trace: resistance to stabilization', (
        automata:generate_trace(T),
        \+ (T = concrete_term)
    )).

% =================================================================
% 3. Prover Basics Tests
% =================================================================

test_prover_basics :-
    writeln('\n--- PROVER BASICS ---'),

    run_test('Identity rule', (
        incompatibility_semantics:proves([a] => [a], 10, _, _)
    )),

    run_test('Explosion from incoherence', (
        incompatibility_semantics:proves([a, neg(a)] => [b], 10, _, _)
    )),

    run_test('Left negation', (
        incompatibility_semantics:proves([neg(a)] => [b], 10, _, Proof),
        Proof \= erasure(_)
    )),

    run_test('Resource tracking', (
        incompatibility_semantics:proves([a] => [a], 100, R_Out, _),
        R_Out < 100
    )),

    run_test('Resource exhaustion', (
        \+ incompatibility_semantics:proves([a] => [a], 0, _, _)
    )).

% =================================================================
% 4. PML Dynamics Tests
% =================================================================

test_pml_dynamics :-
    writeln('\n--- PML DYNAMICS ---'),

    run_test('Dialectical rhythm: U -> Box_down(A)', (
        incompatibility_semantics:proves([s(u)] => [s(comp_nec a)], 50, _, _)
    )),

    run_test('Dialectical rhythm: A -> Diamond_up(LG)', (
        incompatibility_semantics:proves([s(a)] => [s(exp_poss lg)], 50, _, _)
    )),

    run_test('Dialectical rhythm: LG -> Box_up(U\')', (
        incompatibility_semantics:proves([s(lg)] => [s(exp_nec u_prime)], 50, _, _)
    )),

    run_test('Fixation pathway: T -> Box_down(neg(U))', (
        incompatibility_semantics:proves([s(t)] => [s(comp_nec neg(u))], 50, _, _)
    )),

    run_test('Bad Infinite: Being <-> Nothing', (
        incompatibility_semantics:proves([s(t_b)] => [s(comp_nec t_n)], 50, _, _),
        incompatibility_semantics:proves([s(t_n)] => [s(comp_nec t_b)], 50, _, _)
    )),

    run_test('Oobleck Dynamic: S -> O transfer', (
        incompatibility_semantics:proves([s(comp_nec p)] => [o(comp_nec p)], 50, _, _)
    )),

    run_test('Modal context switch: compressive', (
        incompatibility_semantics:proves([s(u)] => [s(comp_nec a)], 100, R_Out, _),
        R_Out < 98  % Should cost more than 1 due to context switch
    )).

% =================================================================
% 5. Trace Mechanism Tests
% =================================================================

test_trace_mechanism :-
    writeln('\n--- TRACE MECHANISM ---'),

    run_test('Pragmatic axiom: i_feeling/1', (
        pragmatic_axioms:i_feeling(I_f),
        automata:contains_trace(I_f)
    )),

    run_test('Pragmatic axiom: identity_claim/1', (
        pragmatic_axioms:identity_claim(concrete_me),
        \+ automata:contains_trace(concrete_me)
    )),

    run_test('Elusive Subject axiom: S-O inversion', (
        pragmatic_axioms:i_feeling(I_f),
        incompatibility_semantics:proves([s(comp_nec I_f)] => [o(exp_nec I_f)], 50, _, _)
    )),

    run_test('Unsatisfiable Desire: incoherence', (
        pragmatic_axioms:i_feeling(I_f),
        pragmatic_axioms:identity_claim(me),
        incompatibility_semantics:incoherent([n(represents(me, I_f))])
    )),

    run_test('Proof erasure: trace propagation', (
        pragmatic_axioms:i_feeling(I_f),
        incompatibility_semantics:proves([s(I_f)] => [s(I_f)], 50, _, Proof),
        Proof = erasure(_)
    )).

% =================================================================
% Entry Point
% =================================================================

:- initialization(run_all_tests).
