#!/usr/bin/env swipl

:- use_module(library(lists)).

% Load the system
:- ['config.pl'].
:- use_module(oracle_server).
:- use_module(execution_handler).

%!      test_unknown_operation_crisis
%
%       PHASE 2 TEST: Verify that attempting an undefined operation (subtract/multiply/divide)
%       from primordial state triggers unknown_operation crisis, learns first available strategy.
%
test_unknown_operation_crisis :-
    writeln(''),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln('PHASE 2 TEST: Unknown Operation Crisis Detection'),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln(''),
    
    % Reset to primordial state
    writeln('1. Resetting to primordial state (only Counting All addition)...'),
    retractall(object_level:subtract(_,_,_)),
    retractall(object_level:multiply(_,_,_)),
    retractall(object_level:divide(_,_,_)),
    
    % Verify primordial state
    writeln(''),
    writeln('2. Verifying primordial state...'),
    (   \+ clause(object_level:subtract(_,_,_), _)
    ->  writeln('   ✓ subtract/3 NOT defined (correct)')
    ;   writeln('   ✗ subtract/3 IS defined (WRONG - should be primordial)'),
        fail
    ),
    (   \+ clause(object_level:multiply(_,_,_), _)
    ->  writeln('   ✓ multiply/3 NOT defined (correct)')
    ;   writeln('   ✗ multiply/3 IS defined (WRONG - should be primordial)'),
        fail
    ),
    (   \+ clause(object_level:divide(_,_,_), _)
    ->  writeln('   ✓ divide/3 NOT defined (correct)')
    ;   writeln('   ✗ divide/3 IS defined (WRONG - should be primordial)'),
        fail
    ),
    
    writeln(''),
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln('TEST 1: Subtract - Should trigger unknown_operation crisis'),
    writeln('───────────────────────────────────────────────────────────────────'),
    
    % Test subtract - should trigger crisis and learn
    (   execution_handler:run_computation(object_level:subtract(s(s(s(s(s(0))))), s(s(s(0))), R1), 20)
    ->  writeln(''),
        format('✓ Subtract succeeded: 5 - 3 = ~w~n', [R1]),
        
        % Verify a subtract strategy was learned
        (   clause(object_level:subtract(_,_,_), _)
        ->  writeln('✓ Subtract strategy LEARNED (crisis handled correctly)')
        ;   writeln('✗ No subtract strategy learned (crisis handler failed)'),
            fail
        )
    ;   writeln('✗ Subtract failed (crisis not handled)'),
        fail
    ),
    
    writeln(''),
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln('TEST 2: Multiply - Should trigger unknown_operation crisis'),
    writeln('───────────────────────────────────────────────────────────────────'),
    
    % Test multiply - should trigger crisis and learn
    (   execution_handler:run_computation(object_level:multiply(s(s(s(0))), s(s(s(s(0)))), R2), 30)
    ->  writeln(''),
        format('✓ Multiply succeeded: 3 * 4 = ~w~n', [R2]),
        
        % Verify a multiply strategy was learned
        (   clause(object_level:multiply(_,_,_), _)
        ->  writeln('✓ Multiply strategy LEARNED (crisis handled correctly)')
        ;   writeln('✗ No multiply strategy learned (crisis handler failed)'),
            fail
        )
    ;   writeln('✗ Multiply failed (crisis not handled)'),
        fail
    ),
    
    writeln(''),
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln('TEST 3: Divide - Should trigger unknown_operation crisis'),
    writeln('───────────────────────────────────────────────────────────────────'),
    
    % Test divide - should trigger crisis and learn
    (   execution_handler:run_computation(object_level:divide(s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))), s(s(s(0))), R3), 40)
    ->  writeln(''),
        format('✓ Divide succeeded: 12 ÷ 3 = ~w~n', [R3]),
        
        % Verify a divide strategy was learned
        (   clause(object_level:divide(_,_,_), _)
        ->  writeln('✓ Divide strategy LEARNED (crisis handled correctly)')
        ;   writeln('✗ No divide strategy learned (crisis handler failed)'),
            fail
        )
    ;   writeln('✗ Divide failed (crisis not handled)'),
        fail
    ),
    
    writeln(''),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln('PHASE 2 TEST: ALL TESTS PASSED'),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln(''),
    writeln('Summary:'),
    writeln('  ✓ Unknown operation detection works'),
    writeln('  ✓ Crisis handler triggers correctly'),
    writeln('  ✓ Oracle consultation successful'),
    writeln('  ✓ Strategy synthesis works for all operations'),
    writeln('  ✓ System learns from crises'),
    writeln(''),
    writeln('Next Phase: Design full curriculum (Addition → Multiplication → Subtraction → Division)'),
    writeln('').

%!      test_all
%
%       Run all Phase 2 tests.
%
test_all :-
    catch(
        test_unknown_operation_crisis,
        Error,
        (   format('~n✗ TEST FAILED WITH ERROR: ~w~n~n', [Error]),
            fail
        )
    ).

% Make it easy to run
:- initialization((test_all -> halt(0) ; halt(1)), main).
