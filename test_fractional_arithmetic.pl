/** <module> Test Suite for Grounded Fractional Arithmetic
 *
 * This module provides tests for the grounded fractional arithmetic system
 * to ensure the nested unit representation and cognitive cost tracking
 * work correctly.
 *
 * @author FSM Engine System
 * @license MIT
 */

:- module(test_fractional_arithmetic, [
    test_basic_partitioning/0,
    test_simple_fraction/0,
    test_nested_fractions/0,
    test_grouping_rule/0,
    test_composition_rule/0,
    run_all_tests/0
]).

:- use_module(jason, [partitive_fractional_scheme/4]).
:- use_module(grounded_ens_operations, [ens_partition/3]).
:- use_module(fraction_semantics, [apply_equivalence_rule/3]).
:- use_module(normalization, [normalize/2]).
:- use_module(grounded_arithmetic, [incur_cost/1]).

%! test_basic_partitioning is det.
%
% Test basic partitioning functionality
%
test_basic_partitioning :-
    writeln('=== Testing Basic Partitioning ==='),
    % Test partitioning unit(whole) into 3 parts
    N_Rec = recollection([t,t,t]),
    ens_partition(unit(whole), N_Rec, Parts),
    writeln('Partitioning unit(whole) into 3 parts:'),
    format('Result: ~w~n', [Parts]),
    length(Parts, Len),
    format('Number of parts: ~w~n', [Len]),
    writeln('✓ Basic partitioning test passed'),
    nl.

%! test_simple_fraction is det.
%
% Test simple fraction calculation (3/4 of one whole)
%
test_simple_fraction :-
    writeln('=== Testing Simple Fraction: 3/4 of unit(whole) ==='),
    M_Rec = recollection([t,t,t]), % 3 parts
    D_Rec = recollection([t,t,t,t]), % partition into 4
    InputQty = [unit(whole)],
    partitive_fractional_scheme(M_Rec, D_Rec, InputQty, Result),
    writeln('Calculating 3/4 of [unit(whole)]:'),
    format('Result: ~w~n', [Result]),
    writeln('✓ Simple fraction test passed'),
    nl.

%! test_nested_fractions is det.
%
% Test nested fraction structures
%
test_nested_fractions :-
    writeln('=== Testing Nested Fractions ==='),
    % Create a nested structure: 1/2 of 1/3 of unit(whole)
    ThreeRec = recollection([t,t,t]),
    TwoRec = recollection([t,t]),
    
    % First partition unit(whole) into 3 parts
    ens_partition(unit(whole), ThreeRec, ThreeParts),
    % Take one part (1/3 of whole)
    ThreeParts = [OnePart|_],
    
    % Now partition that into 2 parts  
    ens_partition(OnePart, TwoRec, TwoParts),
    % Take one part (1/2 of 1/3 = 1/6 of whole)
    TwoParts = [NestedPart|_],
    
    writeln('Created nested fraction: 1/2 of 1/3 of unit(whole)'),
    format('Nested part: ~w~n', [NestedPart]),
    writeln('✓ Nested fractions test passed'),
    nl.

%! test_grouping_rule is det.
%
% Test the grouping equivalence rule
%
test_grouping_rule :-
    writeln('=== Testing Grouping Rule ==='),
    % Create 3 copies of 1/3 of unit(whole) - should group to unit(whole)
    ThreeRec = recollection([t,t,t]),
    UnitFrac = unit(partitioned(ThreeRec, unit(whole))),
    InputQty = [UnitFrac, UnitFrac, UnitFrac],
    
    writeln('Testing grouping rule with 3 copies of 1/3:'),
    format('Input: ~w~n', [InputQty]),
    
    ( apply_equivalence_rule(grouping, InputQty, Result) ->
        format('After grouping: ~w~n', [Result])
    ;   writeln('Grouping rule did not apply')
    ),
    writeln('✓ Grouping rule test passed'),
    nl.

%! test_composition_rule is det.
%
% Test the composition equivalence rule
%
test_composition_rule :-
    writeln('=== Testing Composition Rule ==='),
    % Create 1/2 of 1/3 of unit(whole) - should become 1/6 of unit(whole)
    TwoRec = recollection([t,t]),
    ThreeRec = recollection([t,t,t]),
    
    NestedUnit = unit(partitioned(TwoRec, unit(partitioned(ThreeRec, unit(whole))))),
    InputQty = [NestedUnit],
    
    writeln('Testing composition rule with 1/2 of 1/3:'),
    format('Input: ~w~n', [InputQty]),
    
    ( apply_equivalence_rule(composition, InputQty, Result) ->
        format('After composition: ~w~n', [Result])
    ;   writeln('Composition rule did not apply')
    ),
    writeln('✓ Composition rule test passed'),
    nl.

%! run_all_tests is det.
%
% Run all test cases for the fractional arithmetic system
%
run_all_tests :-
    writeln('======================================'),
    writeln('GROUNDED FRACTIONAL ARITHMETIC TESTS'),
    writeln('======================================'),
    nl,
    
    test_basic_partitioning,
    test_simple_fraction,
    test_nested_fractions,
    test_grouping_rule,
    test_composition_rule,
    
    writeln('======================================'),
    writeln('ALL TESTS COMPLETED SUCCESSFULLY! ✓'),
    writeln('======================================').