/** <module> Test Phase 5: FSM Synthesis Engine
 *
 * Tests the new FSM synthesis engine that enables genuine emergent learning.
 */

:- use_module(primordial_start).
:- use_module(execution_handler).
:- use_module(config).

:- initialization(test_phase5_synthesis).

test_phase5_synthesis :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  Phase 5: FSM Synthesis Engine Test                       ║'),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln(''),
    
    % Get current inference limit
    config:max_inferences(Limit),
    format('Inference Limit: ~w~n', [Limit]),
    writeln(''),
    
    % Test 1: Simple addition that succeeds
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Test 1: add(3,2) - Should succeed with primordial strategy'),
    writeln('═══════════════════════════════════════════════════════════'),
    (   catch(
            execution_handler:run_computation(object_level:add(s(s(s(0))), s(s(0)), Result1), Limit),
            Error1,
            (format('ERROR: ~w~n', [Error1]), fail)
        )
    ->  format('SUCCESS: Result = ~w~n', [Result1]),
        writeln('✓ Test 1 PASSED')
    ;   writeln('✗ Test 1 FAILED')
    ),
    writeln(''),
    
    % Test 2: Complex addition that triggers crisis and learning
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Test 2: add(8,5) - Should trigger crisis, consult oracle, synthesize strategy'),
    writeln('═══════════════════════════════════════════════════════════'),
    (   catch(
            execution_handler:run_computation(object_level:add(s(s(s(s(s(s(s(s(0)))))))), s(s(s(s(s(0))))), Result2), Limit),
            Error2,
            (format('ERROR: ~w~n', [Error2]), fail)
        )
    ->  format('SUCCESS: Result = ~w~n', [Result2]),
        writeln('✓ Test 2 PASSED - Learning occurred!'),
        writeln(''),
        writeln('Checking learned strategies...'),
        findall(Strategy, clause(more_machine_learner:run_learned_strategy(_,_,_,Strategy,_), _), Strategies),
        length(Strategies, StratCount),
        format('Found ~w learned strategies: ~w~n', [StratCount, Strategies])
    ;   writeln('✗ Test 2 FAILED - Crisis or synthesis failed')
    ),
    writeln(''),
    
    % Test 3: Retry same problem - should use learned strategy
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Test 3: add(8,5) again - Should use learned strategy (no crisis)'),
    writeln('═══════════════════════════════════════════════════════════'),
    (   catch(
            execution_handler:run_computation(object_level:add(s(s(s(s(s(s(s(s(0)))))))), s(s(s(s(s(0))))), Result3), Limit),
            Error3,
            (format('ERROR: ~w~n', [Error3]), fail)
        )
    ->  format('SUCCESS: Result = ~w~n', [Result3]),
        writeln('✓ Test 3 PASSED - Learned strategy is working!')
    ;   writeln('✗ Test 3 FAILED - Learned strategy not being used')
    ),
    writeln(''),
    
    % Test 4: Different numbers, same pattern
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Test 4: add(7,6) - Should use learned strategy (generalization)'),
    writeln('═══════════════════════════════════════════════════════════'),
    (   catch(
            execution_handler:run_computation(object_level:add(s(s(s(s(s(s(s(0))))))), s(s(s(s(s(s(0)))))), Result4), Limit),
            Error4,
            (format('ERROR: ~w~n', [Error4]), fail)
        )
    ->  format('SUCCESS: Result = ~w~n', [Result4]),
        writeln('✓ Test 4 PASSED - Strategy generalizes!')
    ;   writeln('✗ Test 4 FAILED - Strategy doesn\'t generalize or new crisis')
    ),
    writeln(''),
    
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  Phase 5 Testing Complete                                 ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    % Halt after tests
    halt(0).
