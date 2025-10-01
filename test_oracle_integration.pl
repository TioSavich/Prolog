%! test_oracle_integration
%
% Tests the integration of the oracle into the crisis response cycle

:- initialization(test_oracle_integration, main).

% Load the primordial system
:- consult(primordial_start).

test_oracle_integration :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  TESTING ORACLE INTEGRATION INTO CRISIS CYCLE              ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    writeln('Test: add(3,2) - should succeed with primordial "Counting All"'),
    writeln('------------------------------------------------------------'),
    (   execution_handler:run_computation(object_level:add(s(s(s(0))), s(s(0)), R1), 50)
    ->  format('✓ SUCCESS: Result = ~w~n~n', [R1])
    ;   writeln('✗ FAILED~n~n')
    ),
    
    writeln('Test: add(8,5) - should trigger crisis and consult oracle'),
    writeln('------------------------------------------------------------'),
    (   execution_handler:run_computation(object_level:add(s(s(s(s(s(s(s(s(0)))))))), s(s(s(s(s(0))))), R2), 10)
    ->  format('✓ CRISIS RESOLVED: Result = ~w~n~n', [R2])
    ;   writeln('✗ CRISIS NOT RESOLVED (Expected if synthesis not yet implemented)~n~n')
    ),
    
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('  Test Complete'),
    writeln('═══════════════════════════════════════════════════════════'),
    halt.
