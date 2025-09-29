/** <module> ORR Cycle Integration Test
 *
 * This module tests the complete ORR (Observe-Reorganize-Reflect) cycle
 * with our updated system including grounded arithmetic and normative crisis detection.
 *
 * @author UMEDCA System Test
 */
:- module(test_orr_cycle, [test_addition_cycle/0, test_normative_crisis_cycle/0]).

:- use_module(execution_handler).
:- use_module(object_level).
:- use_module(grounded_arithmetic).
:- use_module(incompatibility_semantics).
:- use_module(config).

%!      test_addition_cycle is det.
%
%       Tests the ORR cycle with a simple addition operation.
test_addition_cycle :-
    writeln('=== Testing ORR Cycle with Addition ==='),
    writeln(''),
    
    % Test simple addition using Peano numbers
    writeln('Testing: add(s(s(0)), s(0), Result)'),
    writeln('This should trigger the ORR cycle due to inefficient enumeration.'),
    writeln(''),
    
    catch(
        run_computation(add(s(s(0)), s(0), Result), 15),
        Error,
        (format('Caught error: ~w~n', [Error]), fail)
    ),
    
    format('Addition result: ~w~n', [Result]),
    writeln(''),
    writeln('=== Addition Test Complete ===').

%!      test_normative_crisis_cycle is det.
%
%       Tests the normative crisis detection and context shifting.
test_normative_crisis_cycle :-
    writeln('=== Testing Normative Crisis Detection ==='),
    writeln(''),
    
    % Ensure we start in natural numbers domain
    set_domain(n),
    current_domain(Domain),
    format('Starting domain: ~w~n', [Domain]),
    writeln(''),
    
    % Test operation that should cause normative crisis: 3 - 8
    writeln('Testing: subtract(s(s(s(0))), s(s(s(s(s(s(s(s(0)))))))), Result)'),
    writeln('This should trigger a normative crisis (3 - 8 in natural numbers).'),
    writeln(''),
    
    catch(
        (
            % Convert to grounded representation for normative checking
            integer_to_recollection(3, Three),
            integer_to_recollection(8, Eight),
            check_norms(subtract(Three, Eight, _)),
            writeln('No crisis detected (unexpected)')
        ),
        normative_crisis(Goal, Context),
        (
            format('SUCCESS: Normative crisis detected!~n'),
            format('  Goal: ~w~n', [Goal]),
            format('  Context: ~w~n', [Context]),
            writeln('  System would now initiate context expansion.')
        )
    ),
    
    writeln(''),
    writeln('=== Normative Crisis Test Complete ===').

%!      test_cognitive_cost_accumulation is det.
%
%       Tests cognitive cost accumulation in strategy execution.
test_cognitive_cost_accumulation :-
    writeln('=== Testing Cognitive Cost Accumulation ==='),
    writeln(''),
    
    % Test that our grounded operations incur appropriate costs
    writeln('Testing cost accumulation in grounded operations...'),
    
    integer_to_recollection(5, Five),
    integer_to_recollection(3, Three),
    
    % These operations should incur costs via incur_cost/1 calls
    add_grounded(Five, Three, Sum),
    recollection_to_integer(Sum, SumInt),
    
    format('5 + 3 = ~w (with cognitive cost tracking)~n', [SumInt]),
    
    writeln(''),
    writeln('=== Cognitive Cost Test Complete ===').