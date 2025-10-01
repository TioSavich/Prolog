#!/usr/bin/env swipl

/** Phase 3 Simple Test: Controlled Sequential Learning
 *
 * Tests crisis-driven learning one operation at a time with careful control.
 * Avoids infinite loops by testing each learned capability before proceeding.
 */

:- use_module(library(lists)).

% Load the system
:- ['config.pl'].
:- use_module(oracle_server).
:- use_module(execution_handler).

%!      test_phase_3_simple
%
%       Simple sequential test: Learn one operation at a time, verify it works.
%
test_phase_3_simple :-
    writeln(''),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln('PHASE 3 SIMPLE TEST: Sequential Crisis-Driven Learning'),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln(''),
    
    % Reset to primordial state
    writeln('Resetting to primordial state...'),
    retractall(object_level:subtract(_,_,_)),
    retractall(object_level:multiply(_,_,_)),
    retractall(object_level:divide(_,_,_)),
    writeln('✓ Only addition (primordial) available'),
    writeln(''),
    
    % Test 1: Learn subtraction
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln('TEST 1: Learn Subtraction (7 - 3)'),
    writeln('═══════════════════════════════════════════════════════════════════'),
    test_learn_operation(subtract, s(s(s(s(s(s(s(0))))))), s(s(s(0))), 'subtract(7,3,R)'),
    
    writeln(''),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln('TEST 2: Learn Multiplication (4 * 3)'),
    writeln('═══════════════════════════════════════════════════════════════════'),
    test_learn_operation(multiply, s(s(s(s(0)))), s(s(s(0))), 'multiply(4,3,R)'),
    
    writeln(''),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln('TEST 3: Learn Division (12 ÷ 4)'),
    writeln('═══════════════════════════════════════════════════════════════════'),
    test_learn_operation(divide, s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))), s(s(s(s(0)))), 'divide(12,4,R)'),
    
    writeln(''),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln('PHASE 3 SIMPLE TEST: ALL TESTS PASSED'),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln(''),
    writeln('Summary:'),
    writeln('  ✓ Subtraction learned via crisis'),
    writeln('  ✓ Multiplication learned via crisis'),
    writeln('  ✓ Division learned via crisis'),
    writeln('  ✓ All operations now functional'),
    writeln(''),
    writeln('System successfully bootstrapped all 4 arithmetic operations!'),
    writeln('').

%!      test_learn_operation(+Op, +A, +B, +Description)
%
%       Test learning a single operation through crisis.
%
test_learn_operation(Op, A, B, Description) :-
    format('Testing: ~w~n', [Description]),
    writeln(''),
    
    % Check if operation is already defined
    Goal =.. [Op, A, B, Result],
    ObjectGoal = object_level:Goal,
    
    (   clause(ObjectGoal, _)
    ->  writeln('  Operation already learned (skipping crisis)'),
        format('  Verifying ~w works...~n', [Description]),
        (   execution_handler:run_computation(ObjectGoal, 50)
        ->  format('  ✓ ~w succeeded: Result = ~w~n', [Description, Result])
        ;   format('  ✗ ~w failed (unexpected)~n', [Description]),
            fail
        )
    ;   writeln('  Operation not yet defined - will trigger crisis'),
        writeln(''),
        
        % Try the operation - should trigger unknown_operation crisis
        (   execution_handler:run_computation(ObjectGoal, 50)
        ->  format('  ✓ ~w succeeded after learning: Result = ~w~n', [Description, Result]),
            writeln(''),
            
            % Verify the operation was actually learned
            (   clause(ObjectGoal, _)
            ->  writeln('  ✓ Operation learned (strategy asserted)')
            ;   writeln('  ✗ Operation NOT learned (crisis handler failed)'),
                fail
            )
        ;   format('  ✗ ~w failed (crisis not handled properly)~n', [Description]),
            fail
        )
    ).

%!      test_all
%
%       Run the simple Phase 3 test.
%
test_all :-
    catch(
        test_phase_3_simple,
        Error,
        (   format('~n✗ TEST FAILED WITH ERROR: ~w~n~n', [Error]),
            fail
        )
    ).

% Make it easy to run
:- initialization((test_all -> halt(0) ; halt(1)), main).
