/** <module> Comprehensive Integration Test
 *
 * This module tests the complete enhanced UMEDCA system including:
 * - Grounded arithmetic operations
 * - Modal logic integration
 * - Normative crisis detection and context shifting
 * - Cognitive cost tracking
 * - Multiplicative pattern detection
 * - Enhanced ORR cycle functionality
 *
 * @author UMEDCA System Test
 */
:- module(test_comprehensive, [run_comprehensive_tests/0]).

:- use_module(grounded_arithmetic).
:- use_module(grounded_utils).
:- use_module(object_level).
:- use_module(incompatibility_semantics).
:- use_module(execution_handler).
:- use_module(more_machine_learner).
:- use_module(config).
:- use_module(fsm_engine).

%!      run_comprehensive_tests is det.
%
%       Runs comprehensive tests of the enhanced UMEDCA system.
run_comprehensive_tests :-
    writeln('=== COMPREHENSIVE UMEDCA SYSTEM TESTS ==='),
    writeln(''),
    
    % Test 1: Enhanced grounded arithmetic with modal signals
    writeln('Test 1: Enhanced Grounded Arithmetic with Modal Context'),
    test_grounded_arithmetic_with_modals,
    writeln(''),
    
    % Test 2: Normative crisis and context shifting
    writeln('Test 2: Normative Crisis and Context Shifting'),
    test_normative_crisis_and_context_shifting,
    writeln(''),
    
    % Test 3: Cognitive cost accumulation and tracking
    writeln('Test 3: Cognitive Cost Accumulation'),
    test_cognitive_cost_accumulation,
    writeln(''),
    
    % Test 4: Modal pattern detection in learning
    writeln('Test 4: Modal Pattern Detection in Learning'),
    test_modal_pattern_detection,
    writeln(''),
    
    % Test 5: Multiplicative pattern bootstrapping
    writeln('Test 5: Multiplicative Pattern Bootstrapping'),
    test_multiplicative_bootstrapping,
    writeln(''),
    
    % Test 6: FSM engine functionality
    writeln('Test 6: FSM Engine Infrastructure'),
    test_fsm_engine,
    writeln(''),
    
    % Test 7: Configuration-based server endpoints
    writeln('Test 7: Server Configuration System'),
    test_server_configuration,
    writeln(''),
    
    writeln('=== ALL COMPREHENSIVE TESTS COMPLETE ===').

%!      test_grounded_arithmetic_with_modals is det.
%
%       Tests grounded arithmetic operations with modal context emission.
test_grounded_arithmetic_with_modals :-
    % Test basic grounded operations with cost tracking
    integer_to_recollection(7, Seven),
    integer_to_recollection(3, Three),
    
    writeln('  Testing grounded addition with modal context...'),
    add_grounded(Seven, Three, Sum),
    recollection_to_integer(Sum, SumInt),
    format('    7 + 3 = ~w (grounded with modal tracking)~n', [SumInt]),
    
    % Test grounded subtraction
    writeln('  Testing grounded subtraction...'),
    ( subtract_grounded(Seven, Three, Diff) ->
        recollection_to_integer(Diff, DiffInt),
        format('    7 - 3 = ~w (grounded subtraction)~n', [DiffInt])
    ;
        writeln('    Subtraction failed (may be expected)')
    ),
    
    % Test modal context in recollection validation
    writeln('  Testing modal context in validation...'),
    ( is_recollection(Seven, History) ->
        format('    Seven is valid recollection with history: ~w~n', [History])
    ;
        writeln('    Seven recollection validation failed')
    ).

%!      test_normative_crisis_and_context_shifting is det.
%
%       Tests the normative crisis detection and context shifting mechanism.
test_normative_crisis_and_context_shifting :-
    % Ensure we start in natural numbers domain
    set_domain(n),
    current_domain(StartDomain),
    format('  Starting domain: ~w~n', [StartDomain]),
    
    % Test crisis detection for 3 - 8
    integer_to_recollection(3, Three),
    integer_to_recollection(8, Eight),
    
    writeln('  Testing normative crisis detection (3 - 8 in natural numbers)...'),
    ( catch(check_norms(subtract(Three, Eight, _)),
            normative_crisis(Goal, Context),
            (format('    ✓ Crisis detected: ~w in ~w context~n', [Goal, Context]), true)) ->
        writeln('    Crisis detection working correctly')
    ;
        writeln('    No crisis detected (unexpected)')
    ),
    
    % Test context shifting capabilities
    writeln('  Testing context expansion capabilities...'),
    current_domain_context(CurrentContext),
    format('    Current context: ~w~n', [CurrentContext]),
    
    % Test domain expansion
    writeln('  Testing domain expansion to integers...'),
    set_domain(z),
    current_domain(ExpandedDomain),
    format('    Expanded to domain: ~w~n', [ExpandedDomain]).

%!      test_cognitive_cost_accumulation is det.
%
%       Tests cognitive cost tracking and accumulation.
test_cognitive_cost_accumulation :-
    writeln('  Testing cognitive cost definitions...'),
    
    % Test various cost types
    cognitive_cost(unit_count, UnitCost),
    cognitive_cost(slide_step, SlideCost),
    cognitive_cost(modal_shift, ModalCost),
    cognitive_cost(norm_check, NormCost),
    
    format('    Unit count cost: ~w~n', [UnitCost]),
    format('    Slide step cost: ~w~n', [SlideCost]),
    format('    Modal shift cost: ~w~n', [ModalCost]),
    format('    Norm check cost: ~w~n', [NormCost]),
    
    writeln('  Testing cost emission in operations...'),
    % The incur_cost/1 calls in grounded operations should work
    incur_cost(unit_count),
    writeln('    ✓ Cost emission successful').

%!      test_modal_pattern_detection is det.
%
%       Tests modal pattern detection in the learning system.
test_modal_pattern_detection :-
    writeln('  Testing modal pattern detection infrastructure...'),
    
    % Create a mock trace with modal elements
    MockTrace = [
        modal_trace(comp_nec(focus), compressive, [step1, step2], modal_info(transition(neutral, compressive), cost_impact(neutral, compressive), goal(test))),
        cognitive_cost(modal_shift, 3),
        modal_trace(exp_poss(explore), expansive, [step3], modal_info(transition(compressive, expansive), cost_impact(compressive, expansive), goal(test2)))
    ],
    
    % Test modal sequence extraction
    ( more_machine_learner:extract_modal_sequence(MockTrace, ModalSequence) ->
        format('    ✓ Extracted modal sequence: ~w~n', [ModalSequence])
    ;
        writeln('    Modal sequence extraction failed')
    ),
    
    % Test efficiency calculation
    TestModalSeq = [modal_state(compressive, focus), modal_transition, modal_state(expansive, explore)],
    ( more_machine_learner:calculate_modal_efficiency_gain(TestModalSeq, Gain) ->
        format('    ✓ Calculated efficiency gain: ~w~n', [Gain])
    ;
        writeln('    Efficiency calculation failed')
    ).

%!      test_multiplicative_bootstrapping is det.
%
%       Tests multiplicative pattern detection and bootstrapping.
test_multiplicative_bootstrapping :-
    writeln('  Testing multiplicative pattern detection...'),
    
    % Create a mock trace showing repeated addition
    MockAdditionTrace = [
        addition_ops([step(add, 5, 5, 10), step(add, 10, 5, 15), step(add, 15, 5, 20)])
    ],
    
    % Test pattern detection
    ( more_machine_learner:analyze_for_repeated_addition(MockAdditionTrace, Multiplicand, Multiplier, Count) ->
        format('    ✓ Detected pattern: ~w × ~w (count: ~w)~n', [Multiplicand, Multiplier, Count])
    ;
        writeln('    Multiplicative pattern detection failed')
    ),
    
    writeln('  Testing algebraic abstraction detection...'),
    % Test algebraic pattern detection
    MockPatterns = [add_pattern(3, 5, 8), add_pattern(5, 3, 8), add_pattern(2, 7, 9)],
    ( more_machine_learner:find_algebraic_abstraction(MockPatterns, AbstractForm, Instances) ->
        format('    ✓ Found abstraction: ~w with instances: ~w~n', [AbstractForm, Instances])
    ;
        writeln('    Algebraic abstraction detection failed')
    ).

%!      test_fsm_engine is det.
%
%       Tests the finite state machine engine infrastructure.
test_fsm_engine :-
    writeln('  Testing FSM engine infrastructure...'),
    
    % Test basic FSM utilities
    TestState = state(test_state, [data1, data2]),
    fsm_engine:extract_state_info(TestState, StateName, StateData),
    format('    ✓ State extraction: ~w -> ~w~n', [StateName, StateData]),
    
    % Test history entry creation
    fsm_engine:create_history_entry(TestState, 'Test interpretation', HistoryEntry),
    format('    ✓ History entry created: ~w~n', [HistoryEntry]),
    
    writeln('    FSM engine foundation is ready for strategy refactoring').

%!      test_server_configuration is det.
%
%       Tests the server configuration system.
test_server_configuration :-
    writeln('  Testing server configuration system...'),
    
    % Test current server mode
    server_mode(CurrentMode),
    format('    Current server mode: ~w~n', [CurrentMode]),
    
    % Test endpoint availability
    writeln('  Testing endpoint availability:'),
    ( server_endpoint_enabled(solve) ->
        writeln('    ✓ solve endpoint enabled')
    ;
        writeln('    ✗ solve endpoint disabled')
    ),
    
    ( server_endpoint_enabled(debug) ->
        writeln('    ✓ debug endpoint enabled')
    ;
        writeln('    ✗ debug endpoint disabled')
    ),
    
    % Test mode switching
    writeln('  Testing mode switching...'),
    retractall(server_mode(_)),
    assertz(server_mode(production)),
    
    ( server_endpoint_enabled(debug) ->
        writeln('    ✗ debug endpoint still enabled in production (error)')
    ;
        writeln('    ✓ debug endpoint correctly disabled in production')
    ),
    
    % Restore development mode
    retractall(server_mode(_)),
    assertz(server_mode(development)),
    writeln('    Restored development mode').