/** Test Oracle Strategy Wiring
 *
 * Test that all oracle strategies are properly wired and functional.
 */

:- use_module(oracle_server).
:- use_module(hermeneutic_calculator).

test_oracle_wiring :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Testing Oracle Strategy Wiring'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln(''),
    
    % Test Addition Strategies
    writeln('ADDITION STRATEGIES:'),
    test_strategy(add(8, 5), 'COBO'),
    test_strategy(add(12, 8), 'Chunking'),
    test_strategy(add(9, 7), 'RMB'),
    test_strategy(add(19, 6), 'Rounding'),
    writeln(''),
    
    % Test Subtraction Strategies
    writeln('SUBTRACTION STRATEGIES:'),
    test_strategy(subtract(13, 5), 'COBO (Missing Addend)'),
    test_strategy(subtract(13, 5), 'CBBO (Take Away)'),
    test_strategy(subtract(20, 8), 'Decomposition'),
    test_strategy(subtract(31, 19), 'Rounding'),
    test_strategy(subtract(45, 8), 'Sliding'),
    test_strategy(subtract(45, 18), 'Chunking A'),
    test_strategy(subtract(52, 27), 'Chunking B'),
    test_strategy(subtract(63, 38), 'Chunking C'),
    writeln(''),
    
    % Test Multiplication Strategies
    writeln('MULTIPLICATION STRATEGIES:'),
    test_strategy(multiply(3, 4), 'C2C'),
    test_strategy(multiply(7, 8), 'CBO'),
    test_strategy(multiply(6, 7), 'Commutative Reasoning'),
    test_strategy(multiply(12, 5), 'DR'),
    writeln(''),
    
    % Test Division Strategies
    writeln('DIVISION STRATEGIES:'),
    test_strategy(divide(12, 3), 'Dealing by Ones'),
    test_strategy(divide(35, 5), 'CBO (Division)'),
    test_strategy(divide(56, 7), 'IDP'),
    test_strategy(divide(84, 12), 'UCR'),
    writeln(''),
    
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Oracle Wiring Test Complete'),
    writeln('═══════════════════════════════════════════════════════════').

test_strategy(Problem, StrategyName) :-
    format('  Testing: ~w with ~w... ', [Problem, StrategyName]),
    catch(
        ( oracle_server:query_oracle(Problem, StrategyName, Result, Interpretation),
          format('✓ Result: ~w~n', [Result])
        ),
        Error,
        format('✗ ERROR: ~w~n', [Error])
    ).

:- initialization(test_oracle_wiring, main).
