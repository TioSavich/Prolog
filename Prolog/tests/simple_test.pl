/** <module> Simple PML Core Tests
 *
 *  Basic functionality tests for the PML Core Framework.
 */

% Load the framework
:- ['../load.pl'].

% =================================================================
% Helper Predicates
% =================================================================

test(Name) :-
    format('~n[TEST] ~w~n', [Name]).

pass(Result) :-
    format('  ~w~n', [Result]),
    writeln('  PASS').

fail_test(Error) :-
    format('  ERROR: ~w~n', [Error]),
    writeln('  FAIL').

% =================================================================
% Test 1: Basic Module Loading
% =================================================================

test_modules :-
    test('Module Loading'),
    ( current_module(pml_operators),
      current_module(incompatibility_semantics),
      current_module(automata),
      current_module(utils)
    ) -> pass('All core modules loaded')
    ; fail_test('Module loading failed').

% =================================================================
% Test 2: Automata
% =================================================================

test_highlander :-
    test('Highlander Automaton'),
    ( automata:highlander([single], single) ->
        pass('Accepts single element')
    ; fail_test('Should accept single element')
    ),
    ( \+ automata:highlander([a, b], _) ->
        pass('Rejects multiple elements')
    ; fail_test('Should reject multiple elements')
    ).

test_primes :-
    test('Prime Utilities'),
    ( automata:is_prime(7),
      \+ automata:is_prime(9),
      automata:nth_prime(1, 2),
      automata:nth_prime(4, 7)
    ) -> pass('Prime utilities working')
    ; fail_test('Prime utilities failed').

test_trace :-
    test('Arche-Trace'),
    ( automata:generate_trace(T),
      automata:contains_trace(T),
      \+ (T = concrete_term)
    ) -> pass('Trace generation and resistance working')
    ; fail_test('Trace mechanism failed').

% =================================================================
% Test 3: Prover Basics
% =================================================================

test_identity :-
    test('Identity Rule'),
    catch(
        ( incompatibility_semantics:proves([a] => [a], 10, R, _Proof),
          R < 10,  % Should consume some resources
          pass('Identity rule works with resource tracking')
        ),
        Error,
        fail_test(Error)
    ).

test_explosion :-
    test('Explosion Rule'),
    catch(
        ( incompatibility_semantics:proves([p, neg(p)] => [anything], 10, _, _),
          pass('Explosion from contradiction works')
        ),
        Error,
        fail_test(Error)
    ).

% =================================================================
% Test 4: PML Dynamics
% =================================================================

test_dialectical_rhythm :-
    test('Dialectical Rhythm: U -> A'),
    catch(
        ( incompatibility_semantics:proves([s(u)] => [s(comp_nec(a))], 50, _, _),
          pass('First negation (U -> Box_down(A)) works')
        ),
        Error,
        fail_test(Error)
    ).

test_oobleck :-
    test('Oobleck Dynamic: S -> O'),
    catch(
        ( incompatibility_semantics:proves([s(comp_nec(p))] => [o(comp_nec(p))], 50, _, _),
          pass('S-O transfer works')
        ),
        Error,
        fail_test(Error)
    ).

% =================================================================
% Test 5: Pragmatic Axioms
% =================================================================

test_i_feeling :-
    test('I-Feeling (Elusive Subject)'),
    catch(
        ( pragmatic_axioms:i_feeling(I_f),
          automata:contains_trace(I_f),
          pass('I-Feeling contains trace')
        ),
        Error,
        fail_test(Error)
    ).

test_unsatisfiable_desire :-
    test('Unsatisfiable Desire'),
    catch(
        ( pragmatic_axioms:i_feeling(I_f),
          pragmatic_axioms:identity_claim(me),
          incompatibility_semantics:incoherent([n(represents(me, I_f))]),
          pass('Cannot represent I_f with finite claim')
        ),
        Error,
        fail_test(Error)
    ).

% =================================================================
% Run All Tests
% =================================================================

run_tests :-
    writeln(''),
    writeln('=== PML CORE FRAMEWORK: SIMPLE TESTS ==='),
    writeln(''),

    % Basic Infrastructure
    test_modules,

    % Automata
    test_highlander,
    test_primes,
    test_trace,

    % Prover
    test_identity,
    test_explosion,

    % PML Dynamics
    test_dialectical_rhythm,
    test_oobleck,

    % Pragmatic Axioms
    test_i_feeling,
    test_unsatisfiable_desire,

    writeln(''),
    writeln('=== ALL TESTS COMPLETE ==='),
    writeln('').

:- initialization(run_tests, main).
