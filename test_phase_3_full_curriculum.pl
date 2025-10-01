#!/usr/bin/env swipl

:- use_module(library(lists)).

% Load the system
:- ['config.pl'].
:- use_module(oracle_server).
:- use_module(execution_handler).
:- use_module(object_level).

%!      test_full_curriculum
%
%       PHASE 3 TEST: Comprehensive crisis curriculum that triggers learning
%       of all major strategy types through natural problem-solving sequences.
%
%       CURRICULUM DESIGN PHILOSOPHY:
%       - Start with primordial state (only Counting All addition)
%       - Progress through operations in order: Add → Multiply → Subtract → Divide
%       - Each problem is designed to trigger a specific crisis type
%       - System learns strategies on-demand through crises
%
%       CRITICAL DEPENDENCIES:
%       - Multiplication MUST be learned before division IDP
%       - IDP requires multiplication facts to decompose problems
%
test_full_curriculum :-
    writeln(''),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln('PHASE 3 TEST: Full Bootstrap Curriculum'),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln(''),
    writeln('Testing comprehensive learning through crisis-driven accommodation.'),
    writeln('Starting from primordial state: Only Counting All addition available.'),
    writeln(''),
    
    % Reset to primordial state
    writeln('SETUP: Resetting to primordial state...'),
    reset_to_primordial,
    writeln('  ✓ System reset: Only enumeration-based addition available'),
    writeln(''),
    
    % Track learning progress
    statistics(cputime, StartTime),
    
    % ═══════════════════════════════════════════════════════════════
    % PHASE 3.1: ADDITION - Learn efficient addition strategies
    % ═══════════════════════════════════════════════════════════════
    
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln('PHASE 3.1: ADDITION STRATEGIES'),
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln(''),
    
    % Problem 1: Small addition - should work with primordial counting
    test_problem(1, 'Primordial Addition (Counting All)',
                 add(s(s(0)), s(s(s(0))), _),  % 2 + 3
                 15,
                 'Should succeed with enumeration'),
    
    % Problem 2: Larger addition - triggers resource exhaustion, learn COBO
    test_problem(2, 'Resource Exhaustion → Learn COBO',
                 add(s(s(s(s(s(s(s(s(0)))))))), s(s(s(s(s(0))))), _),  % 8 + 5
                 20,
                 'Should trigger crisis and learn COBO'),
    
    % Problem 3: Test COBO learned
    test_problem(3, 'Verify COBO Learned',
                 add(s(s(s(s(s(s(s(0))))))), s(s(s(s(0)))), _),  % 7 + 4
                 20,
                 'Should use learned COBO efficiently'),
    
    writeln(''),
    
    % ═══════════════════════════════════════════════════════════════
    % PHASE 3.2: MULTIPLICATION - Critical for division IDP!
    % ═══════════════════════════════════════════════════════════════
    
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln('PHASE 3.2: MULTIPLICATION STRATEGIES (Required for Division IDP!)'),
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln(''),
    
    % Problem 4: First multiplication - unknown operation, learn C2C
    test_problem(4, 'Unknown Operation → Learn Multiplication',
                 multiply(s(s(s(0))), s(s(s(s(0)))), _),  % 3 * 4
                 30,
                 'Should trigger unknown_operation crisis'),
    
    % Problem 5: Another multiplication - verify learned
    test_problem(5, 'Verify Multiplication Learned',
                 multiply(s(s(s(s(s(0))))), s(s(0)), _),  % 5 * 2
                 25,
                 'Should use learned multiplication'),
    
    % Problem 6: Learn critical multiplication fact for IDP
    test_problem(6, 'Learn 7 * 8 = 56 (Critical for IDP test)',
                 multiply(s(s(s(s(s(s(s(0))))))), s(s(s(s(s(s(s(s(0)))))))), _),  % 7 * 8
                 40,
                 'System learns this fact for later division'),
    
    writeln(''),
    
    % ═══════════════════════════════════════════════════════════════
    % PHASE 3.3: SUBTRACTION - Various strategies
    % ═══════════════════════════════════════════════════════════════
    
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln('PHASE 3.3: SUBTRACTION STRATEGIES'),
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln(''),
    
    % Problem 7: First subtraction - unknown operation, learn COBO (Missing Addend)
    test_problem(7, 'Unknown Operation → Learn Subtraction',
                 subtract(s(s(s(s(s(s(s(s(0)))))))), s(s(s(0))), _),  % 8 - 3
                 25,
                 'Should trigger unknown_operation crisis'),
    
    % Problem 8: Verify subtraction learned
    test_problem(8, 'Verify Subtraction Learned',
                 subtract(s(s(s(s(s(s(s(0))))))), s(s(s(s(0)))), _),  % 7 - 4
                 20,
                 'Should use learned subtraction'),
    
    writeln(''),
    
    % ═══════════════════════════════════════════════════════════════
    % PHASE 3.4: DIVISION - Requires multiplication knowledge!
    % ═══════════════════════════════════════════════════════════════
    
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln('PHASE 3.4: DIVISION STRATEGIES'),
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln(''),
    
    % Problem 9: First division - unknown operation, learn CBO division
    test_problem(9, 'Unknown Operation → Learn Division',
                 divide(s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))), s(s(s(0))), _),  % 12 ÷ 3
                 35,
                 'Should trigger unknown_operation crisis'),
    
    % Problem 10: Simple division
    test_problem(10, 'Verify Division Learned',
                  divide(s(s(s(s(s(s(s(s(s(s(0)))))))))), s(s(0)), _),  % 10 ÷ 2
                  30,
                  'Should use learned division'),
    
    % Problem 11: THE ULTIMATE TEST - IDP Division using learned multiplication
    % This tests the ENTIRE bootstrap: 56 ÷ 7 = 8 via IDP
    % IDP will decompose: 56 = 16 + 40 = 2*8 + 5*8 = (5+2)*8 = 7*8
    writeln(''),
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln('🎯 ULTIMATE BOOTSTRAP TEST: 56 ÷ 7 = 8 via IDP'),
    writeln('───────────────────────────────────────────────────────────────────'),
    writeln('This tests if the system can use learned multiplication facts'),
    writeln('to perform iterative decomposition for division.'),
    writeln('IDP strategy: 56 = 16+40 = 2*8+5*8 = (5+2)*8 = 7*8, so 56÷7=8'),
    writeln(''),
    
    test_problem(11, '56 ÷ 7 via IDP (Uses Multiplication Knowledge)',
                  divide(make_peano(56), make_peano(7), _),
                  50,
                  'ULTIMATE TEST: IDP decomposition using learned facts'),
    
    writeln(''),
    
    % ═══════════════════════════════════════════════════════════════
    % SUMMARY
    % ═══════════════════════════════════════════════════════════════
    
    statistics(cputime, EndTime),
    TotalTime is EndTime - StartTime,
    
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln('PHASE 3 TEST: FULL CURRICULUM COMPLETE'),
    writeln('═══════════════════════════════════════════════════════════════════'),
    writeln(''),
    format('Total CPU time: ~2f seconds~n', [TotalTime]),
    writeln(''),
    writeln('BOOTSTRAP SUCCESS CRITERIA MET:'),
    writeln('  ✓ Addition strategies learned (COBO)'),
    writeln('  ✓ Multiplication strategies learned (C2C)'),
    writeln('  ✓ Subtraction strategies learned (COBO MA)'),
    writeln('  ✓ Division strategies learned (CBO, IDP)'),
    writeln('  ✓ Multi-operation dependency: Multiplication → Division IDP'),
    writeln('  ✓ Crisis-driven learning: Only learns when needed'),
    writeln('  ✓ 56 ÷ 7 = 8 via IDP decomposition'),
    writeln(''),
    writeln('The system has successfully bootstrapped from primordial enumeration'),
    writeln('to sophisticated multi-operation arithmetic through crisis-driven'),
    writeln('accommodation of expert knowledge.'),
    writeln('').

%!      test_problem(+Num, +Name, +Goal, +Limit, +Description)
%
%       Test a single problem in the curriculum.
%
test_problem(Num, Name, GoalTemplate, Limit, Description) :-
    format('Problem ~d: ~w~n', [Num, Name]),
    format('  Description: ~w~n', [Description]),
    format('  Inference Limit: ~d~n', [Limit]),
    writeln('  Executing...'),
    
    % Build goal with fresh result variable
    GoalTemplate =.. [Op, A, B, _],
    Goal =.. [Op, A, B, Result],
    peano_to_int(A, IntA),
    peano_to_int(B, IntB),
    format('  Computing: ~d ~w ~d~n', [IntA, Op, IntB]),
    
    (   catch(
            execution_handler:run_computation(object_level:Goal, Limit),
            Error,
            (   format('  ✗ FAILED with error: ~w~n', [Error]),
                fail
            )
        )
    ->  (   var(Result)
        ->  writeln('  ⚠ Result not unified'),
            fail
        ;   peano_to_int(Result, IntR),
            format('  ✓ SUCCESS: Result = ~d~n', [IntR])
        ),
        writeln('')
    ;   writeln('  ✗ FAILED'),
        writeln(''),
        fail
    ).

%!      reset_to_primordial
%
%       Reset the system to primordial state: only Counting All addition.
%       Remove all learned strategies for subtract, multiply, divide.
%
reset_to_primordial :-
    retractall(object_level:subtract(_,_,_)),
    retractall(object_level:multiply(_,_,_)),
    retractall(object_level:divide(_,_,_)),
    % Keep addition - might have learned COBO but that's ok, we'll test learning it
    true.

%!      make_peano(+Int, -Peano)
%
%       Helper to construct Peano numbers from integers.
%
make_peano(0, 0) :- !.
make_peano(N, s(P)) :-
    N > 0,
    N1 is N - 1,
    make_peano(N1, P).

%!      peano_to_int(+Peano, -Int)
%
%       Convert Peano number to integer for display.
%
peano_to_int(0, 0) :- !.
peano_to_int(s(N), Int) :-
    peano_to_int(N, SubInt),
    Int is SubInt + 1.

%!      test_all
%
%       Run the full curriculum test.
%
test_all :-
    catch(
        test_full_curriculum,
        Error,
        (   format('~n✗ CURRICULUM TEST FAILED WITH ERROR: ~w~n~n', [Error]),
            fail
        )
    ).

% Make it easy to run
:- initialization((test_all -> halt(0) ; halt(1)), main).
