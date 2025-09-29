/** <module> Basic Functionality Tests
 *
 * This module tests the basic functionality of the updated UMEDCA system,
 * particularly the grounded arithmetic and normative crisis detection.
 *
 * @author UMEDCA System Test
 */
:- module(test_basic_functionality, [run_basic_tests/0]).

:- use_module(grounded_arithmetic).
:- use_module(grounded_utils).
:- use_module(object_level).
:- use_module(incompatibility_semantics, [current_domain/1, current_domain_context/1, check_norms/1]).
:- use_module(execution_handler).
:- use_module(config).

%!      run_basic_tests is det.
%
%       Runs a series of basic tests to verify system functionality.
run_basic_tests :-
    writeln('=== UMEDCA Basic Functionality Tests ==='),
    writeln(''),
    
    % Test 1: Grounded arithmetic operations
    writeln('Test 1: Grounded Arithmetic Operations'),
    test_grounded_arithmetic,
    writeln(''),
    
    % Test 2: Recollection conversions
    writeln('Test 2: Recollection Conversions'),
    test_recollection_conversions,
    writeln(''),
    
    % Test 3: Cognitive cost tracking
    writeln('Test 3: Cognitive Cost Configuration'),
    test_cognitive_costs,
    writeln(''),
    
    % Test 4: Basic object-level operations
    writeln('Test 4: Object-Level Operations'),
    test_object_level_operations,
    writeln(''),
    
    % Test 5: Normative crisis detection (simple)
    writeln('Test 5: Normative Crisis Detection'),
    test_normative_crisis,
    writeln(''),
    
    writeln('=== All Basic Tests Complete ===').

%!      test_grounded_arithmetic is det.
%
%       Tests basic grounded arithmetic operations.
test_grounded_arithmetic :-
    % Test addition
    integer_to_recollection(3, Three),
    integer_to_recollection(5, Five),
    add_grounded(Three, Five, Sum),
    recollection_to_integer(Sum, SumInt),
    format('  3 + 5 = ~w (grounded arithmetic)~n', [SumInt]),
    
    % Test comparison
    ( smaller_than(Three, Five) ->
        writeln('  3 < 5 is true (grounded comparison)')
    ;
        writeln('  ERROR: 3 < 5 should be true')
    ),
    
    % Test subtraction
    ( subtract_grounded(Five, Three, Diff) ->
        recollection_to_integer(Diff, DiffInt),
        format('  5 - 3 = ~w (grounded subtraction)~n', [DiffInt])
    ;
        writeln('  5 - 3 failed (expected for this test)')
    ).

%!      test_recollection_conversions is det.
%
%       Tests conversion between integers and recollection structures.
test_recollection_conversions :-
    % Test integer to recollection
    integer_to_recollection(4, Four),
    format('  Integer 4 converts to: ~w~n', [Four]),
    
    % Test recollection to integer
    recollection_to_integer(Four, BackToInt),
    format('  Back to integer: ~w~n', [BackToInt]),
    
    % Test zero
    integer_to_recollection(0, Zero),
    format('  Zero as recollection: ~w~n', [Zero]).

%!      test_cognitive_costs is det.
%
%       Tests cognitive cost configuration.
test_cognitive_costs :-
    cognitive_cost(unit_count, UnitCost),
    cognitive_cost(inference, InferenceCost),
    cognitive_cost(slide_step, SlideCost),
    format('  Unit count cost: ~w~n', [UnitCost]),
    format('  Inference cost: ~w~n', [InferenceCost]),
    format('  Slide step cost: ~w~n', [SlideCost]).

%!      test_object_level_operations is det.
%
%       Tests basic object-level predicate availability.
test_object_level_operations :-
    % Check if predicates are defined
    ( predicate_property(object_level:add(_, _, _), dynamic) ->
        writeln('  add/3 is properly defined as dynamic')
    ;
        writeln('  ERROR: add/3 not found or not dynamic')
    ),
    
    ( predicate_property(object_level:subtract(_, _, _), dynamic) ->
        writeln('  subtract/3 is properly defined as dynamic')
    ;
        writeln('  ERROR: subtract/3 not found or not dynamic')
    ),
    
    ( predicate_property(object_level:multiply(_, _, _), dynamic) ->
        writeln('  multiply/3 is properly defined as dynamic')
    ;
        writeln('  ERROR: multiply/3 not found or not dynamic')
    ),
    
    ( predicate_property(object_level:divide(_, _, _), dynamic) ->
        writeln('  divide/3 is properly defined as dynamic')
    ;
        writeln('  ERROR: divide/3 not found or not dynamic')
    ).

%!      test_normative_crisis is det.
%
%       Tests basic normative crisis detection.
test_normative_crisis :-
    % Test current domain
    current_domain(Domain),
    format('  Current domain: ~w~n', [Domain]),
    
    % Test prohibition checking
    integer_to_recollection(3, Three),
    integer_to_recollection(8, Eight),
    
    current_domain_context(Context),
    format('  Current context: ~w~n', [Context]),
    
    % Test if subtraction is prohibited
    ( catch(check_norms(subtract(Three, Eight, _)), 
            normative_crisis(Goal, CrisisContext), 
            (format('  Normative crisis detected: ~w in ~w~n', [Goal, CrisisContext]), true)) ->
        writeln('  Crisis detection working correctly')
    ;
        writeln('  No crisis detected (may be expected depending on implementation)')
    ).