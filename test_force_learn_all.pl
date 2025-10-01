/** <module> Force-Learn All Oracle Strategies
 *
 * This test forces the machine to learn ALL 4 oracle addition strategies
 * by artificially restricting the inference limit and manually directing
 * which oracle strategy to consult.
 *
 * PHILOSOPHICAL NOTE: This is NOT how the machine naturally learns.
 * Natural learning is crisis-driven and emerges from genuine computational
 * need. This curriculum is an ARTIFICIAL acceleration for demonstration
 * purposes - showing that the machine CAN synthesize any strategy the
 * oracle teaches, not that it WOULD naturally learn all of them.
 *
 * Think of this as "teacher-directed learning" vs "discovery learning".
 */

:- use_module(execution_handler).
:- use_module(object_level).
:- use_module(config).
:- use_module(knowledge_manager).
:- use_module(oracle_server).
:- use_module(fsm_synthesis_engine).
:- use_module(more_machine_learner).

%!      force_learn_all_strategies is det.
%
%       Artificially force learning of all 4 oracle strategies.
force_learn_all_strategies :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  FORCED LEARNING: All Oracle Strategies                   ║'),
    writeln('║  (Artificial Acceleration for Demonstration)               ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    % Reset to primordial
    writeln('Resetting to primordial state...'),
    reset_learned_knowledge,
    writeln(''),
    
    % Learn each strategy manually by forcing oracle consultation
    learn_cobo_strategy,
    learn_rmb_strategy,
    learn_chunking_strategy,
    learn_rounding_strategy,
    
    % Final summary
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  All Strategies Learned!                                   ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    inspect_learned_knowledge,
    
    writeln(''),
    writeln('Testing all learned strategies...'),
    test_learned_strategies.

%!      learn_cobo_strategy is det.
%
%       Force learning of Count On from Bigger strategy.
learn_cobo_strategy :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Learning Strategy 1: COBO (Count On from Bigger)'),
    writeln('═══════════════════════════════════════════════════════════'),
    
    % This naturally triggers with add(5, 3) at limit 10
    config:max_inferences(Limit),
    writeln('Forcing crisis with add(5, 3)...'),
    execution_handler:run_computation(object_level:add(s(s(s(s(s(0))))), s(s(s(0))), _), Limit),
    
    (   clause(more_machine_learner:run_learned_strategy(_,_,_,count_on_bigger,_), _)
    ->  writeln('✓ COBO learned successfully')
    ;   writeln('⚠️  COBO not learned')
    ),
    writeln('').

%!      learn_rmb_strategy is det.
%
%       Force learning of Rearrange to Make Base strategy.
learn_rmb_strategy :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Learning Strategy 2: RMB (Rearrange to Make Base)'),
    writeln('═══════════════════════════════════════════════════════════'),
    
    % Manually synthesize from oracle guidance
    writeln('Manually consulting oracle for add(9, 7) with RMB strategy...'),
    oracle_server:query_oracle(add(9, 7), rearrange_make_base, Result, Interpretation),
    format('  Oracle Result: ~w~n', [Result]),
    format('  Oracle Says: "~w"~n', [Interpretation]),
    writeln(''),
    
    writeln('Forcing synthesis from oracle guidance...'),
    SynthesisInput = _{
        goal: add(s(s(s(s(s(s(s(s(s(0))))))))), s(s(s(s(s(s(s(0))))))), _),
        failed_trace: [],
        target_result: Result,
        target_interpretation: Interpretation
    },
    
    (   fsm_synthesis_engine:synthesize_strategy_from_oracle(
            SynthesisInput.goal,
            SynthesisInput.failed_trace,
            SynthesisInput.target_result,
            SynthesisInput.target_interpretation
        )
    ->  writeln('✓ RMB learned successfully')
    ;   writeln('⚠️  RMB synthesis failed - may require different problem structure')
    ),
    writeln('').

%!      learn_chunking_strategy is det.
%
%       Force learning of Chunking (Decades + Ones) strategy.
learn_chunking_strategy :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Learning Strategy 3: Chunking (Decades + Ones)'),
    writeln('═══════════════════════════════════════════════════════════'),
    
    writeln('Manually consulting oracle for add(12, 8) with Chunking strategy...'),
    oracle_server:query_oracle(add(12, 8), chunking, Result, Interpretation),
    format('  Oracle Result: ~w~n', [Result]),
    format('  Oracle Says: "~w"~n', [Interpretation]),
    writeln(''),
    
    writeln('Forcing synthesis from oracle guidance...'),
    SynthesisInput = _{
        goal: add(s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))), s(s(s(s(s(s(s(s(0)))))))), _),
        failed_trace: [],
        target_result: Result,
        target_interpretation: Interpretation
    },
    
    (   fsm_synthesis_engine:synthesize_strategy_from_oracle(
            SynthesisInput.goal,
            SynthesisInput.failed_trace,
            SynthesisInput.target_result,
            SynthesisInput.target_interpretation
        )
    ->  writeln('✓ Chunking learned successfully')
    ;   writeln('⚠️  Chunking synthesis failed - may require different problem structure')
    ),
    writeln('').

%!      learn_rounding_strategy is det.
%
%       Force learning of Rounding strategy.
learn_rounding_strategy :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Learning Strategy 4: Rounding (Round and Adjust)'),
    writeln('═══════════════════════════════════════════════════════════'),
    
    writeln('Manually consulting oracle for add(19, 3) with Rounding strategy...'),
    oracle_server:query_oracle(add(19, 3), rounding, Result, Interpretation),
    format('  Oracle Result: ~w~n', [Result]),
    format('  Oracle Says: "~w"~n', [Interpretation]),
    writeln(''),
    
    writeln('Forcing synthesis from oracle guidance...'),
    SynthesisInput = _{
        goal: add(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0))))))))))))))))))), s(s(s(0))), _),
        failed_trace: [],
        target_result: Result,
        target_interpretation: Interpretation
    },
    
    (   fsm_synthesis_engine:synthesize_strategy_from_oracle(
            SynthesisInput.goal,
            SynthesisInput.failed_trace,
            SynthesisInput.target_result,
            SynthesisInput.target_interpretation
        )
    ->  writeln('✓ Rounding learned successfully')
    ;   writeln('⚠️  Rounding synthesis failed - may require different problem structure')
    ),
    writeln('').

%!      test_learned_strategies is det.
%
%       Test that all learned strategies work correctly.
test_learned_strategies :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Testing Learned Strategies'),
    writeln('═══════════════════════════════════════════════════════════'),
    
    config:max_inferences(Limit),
    
    % These should now use learned strategies
    test_problem('add(7, 6, R)', add(s(s(s(s(s(s(s(0))))))), s(s(s(s(s(s(0)))))), _), Limit),
    test_problem('add(14, 8, R)', add(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))))), s(s(s(s(s(s(s(s(0)))))))), _), Limit),
    test_problem('add(9, 7, R)', add(s(s(s(s(s(s(s(s(s(0))))))))), s(s(s(s(s(s(s(0))))))), _), Limit),
    test_problem('add(18, 5, R)', add(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(0)))))))))))))))))), s(s(s(s(s(0))))), _), Limit),
    
    writeln('✓ All tests complete').

%!      test_problem(+Description, +Goal, +Limit) is det.
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

:- initialization(force_learn_all_strategies, main).
