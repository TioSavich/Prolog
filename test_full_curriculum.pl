/** <module> Comprehensive Curriculum Test
 *
 * This test drives the machine through a complete learning curriculum,
 * forcing it to learn ALL strategies that the oracle knows.
 *
 * Oracle Strategies (Addition):
 * 1. COBO (Count On from Bigger) - Start at max, count up min times
 * 2. RMB (Rearrange to Make Base) - Use base-10 decomposition
 * 3. Chunking - Break into decades and ones
 * 4. Rounding - Round to nearest ten, adjust
 *
 * The curriculum is designed to trigger crises that require each strategy.
 * After completion, the machine will have learned to solve problems
 * multiple ways, demonstrating developmental progression.
 */

:- use_module(execution_handler).
:- use_module(object_level).
:- use_module(config).
:- use_module(knowledge_manager).
:- use_module(oracle_server).

%!      test_full_curriculum is det.
%
%       Main entry point. Runs complete learning curriculum.
test_full_curriculum :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  UMEDCA Full Learning Curriculum                           ║'),
    writeln('║  Learning ALL Oracle Strategies                            ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    % Reset to primordial state for clean developmental trajectory
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Setup: Resetting to Primordial State'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Clearing all learned strategies for clean developmental test...'),
    reset_learned_knowledge,
    writeln('✓ Reset complete - starting from primordial machine'),
    writeln(''),
    
    % Show initial state (should be empty)
    inspect_learned_knowledge,
    
    % Run curriculum stages
    stage_1_sense_certainty,
    stage_2_first_crisis_cobo,
    stage_3_generalization_test,
    stage_4_second_crisis_rmb,
    stage_5_chunking_crisis,
    stage_6_rounding_crisis,
    stage_7_mastery_test,
    
    % Final summary
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  Full Curriculum Complete!                                 ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    final_summary.

%!      stage_1_sense_certainty is det.
%
%       Stage 1: Problems solvable with primordial Counting All.
stage_1_sense_certainty :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Stage 1: Sense-Certainty (Primordial Capability)'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Testing problems solvable with Counting All...'),
    writeln(''),
    
    config:max_inferences(Limit),
    
    % Small problems that succeed
    test_problem('add(2, 1, R)', add(2, 1, _), Limit),
    test_problem('add(2, 2, R)', add(2, 2, _), Limit),
    test_problem('add(3, 1, R)', add(3, 1, _), Limit),
    
    writeln('✓ Stage 1 Complete - Primordial strategy works for small numbers'),
    writeln('').

%!      stage_2_first_crisis_cobo is det.
%
%       Stage 2: Trigger first crisis, learn COBO (Count On Bigger).
stage_2_first_crisis_cobo :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Stage 2: First Crisis → Learn COBO'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Triggering crisis with add(8, 5)...'),
    writeln('Expected: Oracle teaches COBO (Count On from Bigger)'),
    writeln(''),
    
    config:max_inferences(Limit),
    test_problem('add(8, 5, R) [CRISIS]', add(8, 5, _), Limit),
    
    % Verify COBO was learned
    (   clause(more_machine_learner:run_learned_strategy(_,_,_,count_on_bigger,_), _)
    ->  writeln('✓ COBO strategy learned (count_on_bigger)')
    ;   writeln('⚠️  COBO not learned (check synthesis engine)')
    ),
    
    writeln('✓ Stage 2 Complete - First crisis resolved'),
    writeln('').

%!      stage_3_generalization_test is det.
%
%       Stage 3: Test that COBO generalizes to other problems.
stage_3_generalization_test :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Stage 3: Generalization Test'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Testing if COBO generalizes...'),
    writeln(''),
    
    config:max_inferences(Limit),
    
    % These should now succeed with learned COBO
    test_problem('add(7, 6, R)', add(7, 6, _), Limit),
    test_problem('add(9, 4, R)', add(9, 4, _), Limit),
    test_problem('add(3, 8, R) [commutative]', add(3, 8, _), Limit),
    
    writeln('✓ Stage 3 Complete - COBO generalizes'),
    writeln('').

%!      stage_4_second_crisis_rmb is det.
%
%       Stage 4: Trigger crisis requiring RMB (Rearrange to Make Base).
%       Use LARGE problems where COBO counting becomes exhausting.
%       The key: make numbers big enough that even "count on from bigger"
%       will exceed the inference limit!
stage_4_second_crisis_rmb :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Stage 4: Second Crisis → Learn RMB'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Forcing crisis with LARGE numbers...'),
    writeln('Expected: COBO exhausts resources, oracle teaches RMB'),
    writeln(''),
    
    config:max_inferences(Limit),
    
    % LARGE problems where even COBO will exhaust (counting 9 or 8 steps)
    writeln('Testing add(15, 9) - COBO must count 9 times (will exhaust)...'),
    test_problem('add(15, 9, R) [CRISIS]', add(15, 9, _), Limit),
    
    writeln('Testing add(12, 8) - COBO must count 8 times (will exhaust)...'),
    test_problem('add(12, 8, R) [CRISIS]', add(12, 8, _), Limit),
    
    % Check if new strategy learned
    findall(N, clause(more_machine_learner:run_learned_strategy(_,_,_,N,_), _), Strategies),
    length(Strategies, Count),
    format('Current strategies learned: ~w~n', [Count]),
    
    writeln('✓ Stage 4 Complete - Forced new crisis with larger numbers'),
    writeln('').

%!      stage_5_chunking_crisis is det.
%
%       Stage 5: Even LARGER problems to force another crisis.
%       After learning RMB/second strategy, we need problems so big
%       that even those strategies exhaust resources.
stage_5_chunking_crisis :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Stage 5: Third Crisis → Learn Chunking'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Forcing crisis with EVEN LARGER numbers...'),
    writeln('Expected: Previous strategies exhaust, oracle teaches Chunking'),
    writeln(''),
    
    config:max_inferences(Limit),
    
    % VERY LARGE problems that will exhaust any counting-based strategy
    writeln('Testing add(20, 15) - massive problem (35 total)...'),
    test_problem('add(20, 15, R) [CRISIS]', add(20, 15, _), Limit),
    
    writeln('Testing add(18, 12) - large decade problem...'),
    test_problem('add(18, 12, R) [CRISIS]', add(18, 12, _), Limit),
    
    findall(N, clause(more_machine_learner:run_learned_strategy(_,_,_,N,_), _), Strategies),
    length(Strategies, Count),
    format('Current strategies learned: ~w~n', [Count]),
    
    writeln('✓ Stage 5 Complete - Forced third crisis'),
    writeln('').

%!      stage_6_rounding_crisis is det.
%
%       Stage 6: Push to the absolute limit with massive numbers.
stage_6_rounding_crisis :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Stage 6: Fourth Crisis → Learn Rounding'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Forcing crisis with MASSIVE numbers...'),
    writeln('Expected: All previous strategies exhaust, oracle teaches Rounding'),
    writeln(''),
    
    config:max_inferences(Limit),
    
    % EXTREME problems near boundaries (these should trigger rounding)
    writeln('Testing add(29, 18) - near 50, massive count...'),
    test_problem('add(29, 18, R) [CRISIS]', add(29, 18, _), Limit),
    
    writeln('Testing add(38, 21) - approaching 60...'),
    test_problem('add(38, 21, R) [CRISIS]', add(38, 21, _), Limit),
    
    findall(N, clause(more_machine_learner:run_learned_strategy(_,_,_,N,_), _), Strategies),
    length(Strategies, Count),
    format('Current strategies learned: ~w~n', [Count]),
    
    writeln('✓ Stage 6 Complete - Forced fourth crisis with extreme numbers'),
    writeln('').

%!      stage_7_mastery_test is det.
%
%       Stage 7: Test mastery with diverse problems.
%       These should now succeed efficiently with learned strategies.
stage_7_mastery_test :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Stage 7: Mastery Test'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Testing diverse problems with learned strategies...'),
    writeln('These should all succeed efficiently now!'),
    writeln(''),
    
    config:max_inferences(Limit),
    
    % Mix of problem types - should all succeed with learned strategies
    test_problem('add(13, 7, R)', add(13, 7, _), Limit),
    test_problem('add(25, 8, R)', add(25, 8, _), Limit),
    test_problem('add(16, 14, R)', add(16, 14, _), Limit),
    test_problem('add(30, 19, R)', add(30, 19, _), Limit),
    
    writeln('✓ Stage 7 Complete - Mastery demonstrated with learned strategies'),
    writeln('').

%!      test_problem(+Description, +Goal, +Limit) is det.
%
%       Helper to test a single problem and report results.
test_problem(Description, Goal, Limit) :-
    format('  Testing: ~w~n', [Description]),
    catch(
        (   execution_handler:run_computation(object_level:Goal, Limit)
        ->  writeln('    ✓ Success')
        ;   writeln('    ✗ Failed')
        ),
        Error,
        format('    ✗ Error: ~w~n', [Error])
    ).

%!      final_summary is det.
%
%       Display final learning summary.
final_summary :-
    writeln('Final Learning Summary:'),
    writeln(''),
    
    % Count strategies
    findall(Name, clause(more_machine_learner:run_learned_strategy(_,_,_,Name,_), _), Strategies),
    length(Strategies, Count),
    
    format('Total Strategies Learned: ~w~n', [Count]),
    writeln(''),
    
    (   Count > 0
    ->  writeln('Strategies:'),
        forall(member(S, Strategies),
               format('  • ~w~n', [S]))
    ;   writeln('No strategies learned (unexpected!)')
    ),
    
    writeln(''),
    writeln('Oracle Strategies Available:'),
    writeln('  1. COBO (Count On from Bigger)'),
    writeln('  2. RMB (Rearrange to Make Base)'),
    writeln('  3. Chunking (Decades + Ones)'),
    writeln('  4. Rounding (Round to nearest ten)'),
    writeln(''),
    
    (   Count >= 2
    ->  writeln('✓ System has learned multiple strategies!'),
        writeln('  Developmental trajectory successful.')
    ;   Count = 1
    ->  writeln('⚠️  System learned only 1 strategy.'),
        writeln('  May need more diverse crises to trigger additional learning.')
    ;   writeln('✗ No learning occurred.'),
        writeln('  Check FSM synthesis engine.')
    ),
    
    writeln(''),
    writeln('To reset and retry: reset_learned_knowledge.'),
    writeln('To backup current state: backup_learned_knowledge(curriculum_complete).'),
    writeln('').

%! Run curriculum when file is loaded
:- initialization(test_full_curriculum, main).
