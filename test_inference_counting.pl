/** Test Inference Counting
 *
 * Diagnose why primordial add/3 isn't exhausting resources
 */

:- use_module(execution_handler).
:- use_module(object_level).
:- use_module(config).

test_counting :-
    writeln('Testing inference counting mechanism...'),
    writeln(''),
    
    config:max_inferences(Limit),
    format('Inference limit: ~w~n', [Limit]),
    writeln(''),
    
    % Test small problem (should succeed)
    writeln('Test 1: add(2, 1, R) - Should succeed'),
    ( execution_handler:run_computation(object_level:add(s(s(0)), s(0), _), Limit)
    -> writeln('✓ SUCCESS')
    ; writeln('✗ FAILED (unexpected)')
    ),
    writeln(''),
    
    % Test medium problem (might succeed or fail)
    writeln('Test 2: add(5, 3, R) - Borderline'),
    ( execution_handler:run_computation(object_level:add(s(s(s(s(s(0))))), s(s(s(0))), _), Limit)
    -> writeln('✓ SUCCESS')
    ; writeln('✗ RESOURCE EXHAUSTION (good - forces learning)')
    ),
    writeln(''),
    
    % Test large problem (should exhaust if counting works)
    writeln('Test 3: add(8, 5, R) - Should exhaust with limit=10'),
    ( execution_handler:run_computation(object_level:add(s(s(s(s(s(s(s(s(0)))))))), s(s(s(s(s(0))))), _), Limit)
    -> writeln('✗ SUCCESS (BUG: should have exhausted!)')
    ; writeln('✓ RESOURCE EXHAUSTION (correct - triggers learning)')
    ),
    writeln(''),
    
    % Test very large problem (definitely should exhaust)
    writeln('Test 4: add(15, 9, R) - Should definitely exhaust'),
    ( execution_handler:run_computation(object_level:add(
        s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))), 
        s(s(s(s(s(s(s(s(s(0))))))))),
        _), Limit)
    -> writeln('✗ SUCCESS (BUG: counting is broken!)')
    ; writeln('✓ RESOURCE EXHAUSTION (correct)')
    ),
    writeln(''),
    
    writeln('Diagnosis:'),
    writeln('  If all tests succeed → Inference counting is BROKEN'),
    writeln('  If tests 3-4 exhaust → System is working correctly').

:- initialization(test_counting, main).
