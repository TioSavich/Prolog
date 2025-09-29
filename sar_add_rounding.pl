/** <module> Student Addition Strategy: Rounding and Adjusting
 *
 * This module implements the 'Rounding and Adjusting' strategy for addition,
 * modeled as a multi-phase finite state machine. The strategy involves
 * simplifying an addition problem by rounding one number up to a multiple of 10,
 * performing the addition, and then adjusting the result.
 *
 * The process is as follows:
 * 1.  **Phase 1: Rounding**: Select one number (`Target`) to round up, typically
 *     the one closer to the next multiple of 10. Calculate the amount `K`
 *     needed for rounding.
 * 2.  **Phase 2: Addition**: Add the *rounded* number to the other number. This
 *     is performed using a 'Counting On by Bases and Ones' (COBO) sub-strategy.
 * 3.  **Phase 3: Adjustment**: Adjust the sum from Phase 2 by subtracting `K`
 *     to get the final, correct answer.
 *
 * The state is represented by the complex term:
 * `state(Name, K, A_rounded, TempSum, Result, Target, Other, TargetBase, BaseCounter, OneCounter)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, K, RoundedTarget, TempSum, CurrentResult, Interpretation)`
 *
 * 
 * 
 */
:- module(sar_add_rounding,
          [ run_rounding/4,
            % FSM Engine Interface
            setup_strategy/4,
            transition/3,
            transition/4,
            accept_state/1,
            final_interpretation/2,
            extract_result_from_history/2
          ]).

:- use_module(library(lists)).
:- use_module(fsm_engine, [run_fsm_with_base/5]).
:- use_module(grounded_arithmetic, [incur_cost/1]).
:- use_module(incompatibility_semantics, [s/1, comp_nec/1, exp_poss/1]).

% determine_target/5 is a helper to decide which number to round.
% It selects the number that is closer to the next multiple of the base.
determine_target(A_in, B_in, Base, Target, Other) :-
    A_rem is A_in mod Base,
    B_rem is B_in mod Base,
    (A_rem >= B_rem ->
        (Target = A_in, Other = B_in)
    ;
        (Target = B_in, Other = A_in)
    ).

%!      run_rounding(+A_in:integer, +B_in:integer, -FinalResult:integer, -History:list) is det.
%
%       Executes the 'Rounding and Adjusting' addition strategy for A + B.
%
%       This predicate initializes and runs a state machine that models the
%       three phases of the strategy: rounding, adding, and adjusting.
%       It traces the entire execution, providing a step-by-step history
%       of the cognitive process.
%
%       @param A_in The first addend.
%       @param B_in The second addend.
%       @param FinalResult The resulting sum of A and B.
%       @param History A list of `step/6` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_rounding(A_in, B_in, FinalResult, History) :-
    % Use the FSM engine to run this strategy
    setup_strategy(A_in, B_in, InitialState, Parameters),
    Base = 10,
    run_fsm_with_base(sar_add_rounding, InitialState, Parameters, Base, History),
    extract_result_from_history(History, FinalResult).

%!      setup_strategy(+A, +B, -InitialState, -Parameters) is det.
%
%       Sets up the initial state for the rounding addition strategy.
setup_strategy(A_in, B_in, InitialState, Parameters) :-
    InitialState = state(q_init, 0, 0, 0, 0, 0, 0, 0, 0, 0, A_in, B_in),
    Parameters = [A_in, B_in],
    
    % Emit modal signal for strategy initiation
    s(exp_poss(initiating_rounding_addition_strategy)),
    incur_cost(inference).

%!      transition(+StateNum, -NextStateNum, -Action) is det.
%
%       State transitions for rounding addition FSM.

transition(q_init, q_determine_target, select_rounding_target) :-
    s(comp_nec(transitioning_to_target_determination)),
    incur_cost(state_change).

transition(q_determine_target, q_init_K, initialize_rounding_calculation) :-
    s(exp_poss(preparing_rounding_amount_calculation)),
    incur_cost(preparation).

transition(q_init_K, q_loop_K, begin_rounding_loop) :-
    s(comp_nec(beginning_rounding_count_up)),
    incur_cost(initialization).

transition(q_loop_K, q_init_Add, proceed_to_addition) :-
    s(exp_poss(transitioning_to_addition_phase)),
    incur_cost(phase_transition).

transition(q_init_Add, q_loop_AddBases, begin_cobo_addition) :-
    s(comp_nec(beginning_cobo_base_processing)),
    incur_cost(cobo_initialization).

transition(q_loop_AddBases, q_loop_AddOnes, process_ones_component) :-
    s(exp_poss(transitioning_to_ones_processing)),
    incur_cost(component_transition).

transition(q_loop_AddOnes, q_init_Adjust, prepare_adjustment) :-
    s(exp_poss(preparing_final_adjustment)),
    incur_cost(adjustment_preparation).

transition(q_init_Adjust, q_loop_Adjust, begin_adjustment_loop) :-
    s(comp_nec(beginning_adjustment_countdown)),
    incur_cost(adjustment_initialization).

transition(q_loop_Adjust, q_accept, complete_rounding_strategy) :-
    s(exp_poss(completing_rounding_addition_strategy)),
    incur_cost(completion).

%!      transition(+State, +Base, -NextState, -Interpretation) is det.
%
%       Complete state transitions with full state tracking.

% From q_init, determine target and setup initial values
transition(state(q_init, _, _, _, _, _, _, _, _, _, A_in, B_in), Base,
           state(q_determine_target, 0, 0, 0, 0, Target, Other, 0, 0, 0, A_in, B_in),
           Interpretation) :-
    s(exp_poss(determining_optimal_rounding_target)),
    determine_target(A_in, B_in, Base, Target, Other),
    format(atom(Interpretation), 'Inputs: ~w, ~w. Target for rounding: ~w', [A_in, B_in, Target]),
    incur_cost(target_determination).

% Phase 1: Rounding - Initialize K calculation
transition(state(q_determine_target, _, _, _, _, Target, Other, _, _, _, A_in, B_in), Base,
           state(q_init_K, 0, Target, 0, 0, Target, Other, TargetBase, 0, 0, A_in, B_in),
           Interpretation) :-
    s(comp_nec(calculating_rounding_target_base)),
    (Target =< 0 -> 
        TargetBase = 0 
    ; (Target mod Base =:= 0 -> 
        TargetBase = Target 
    ; 
        TargetBase is ((Target // Base) + 1) * Base)),
    format(atom(Interpretation), 'Initializing K calculation. Counting from ~w to ~w.', [Target, TargetBase]),
    incur_cost(rounding_initialization).

% Phase 1: Rounding - Count up to calculate K
transition(state(q_init_K, K, AR, TS, R, T, O, TB, BC, OC, A_in, B_in), _,
           state(q_loop_K, K, AR, TS, R, T, O, TB, BC, OC, A_in, B_in),
           'Entering K calculation loop.') :-
    s(exp_poss(entering_rounding_calculation_loop)),
    incur_cost(loop_entry).

transition(state(q_loop_K, K, AR, TS, R, T, O, TB, BC, OC, A_in, B_in), _,
           state(q_loop_K, NewK, NewAR, TS, R, T, O, TB, BC, OC, A_in, B_in),
           Interpretation) :-
    AR < TB,
    s(comp_nec(continuing_rounding_count_up)),
    NewK is K + 1, 
    NewAR is AR + 1,
    format(atom(Interpretation), 'Counting Up: ~w, K=~w', [NewAR, NewK]),
    incur_cost(counting_step).

transition(state(q_loop_K, K, AR, TS, R, T, O, TB, BC, OC, A_in, B_in), _,
           state(q_init_Add, K, AR, TS, R, T, O, TB, BC, OC, A_in, B_in),
           Interpretation) :-
    AR >= TB,
    s(exp_poss(completing_rounding_calculation)),
    format(atom(Interpretation), 'K needed is ~w. Target rounded to ~w.', [K, AR]),
    incur_cost(rounding_completion).

% Phase 2: Addition (using COBO sub-strategy)
transition(state(q_init_Add, K, AR, _TS, R, T, O, TB, _BC, _OC, A_in, B_in), Base,
           state(q_loop_AddBases, K, AR, AR, R, T, O, TB, OBC, OOC, A_in, B_in),
           Interpretation) :-
    s(comp_nec(initializing_cobo_addition_substrategy)),
    OBC is O // Base, 
    OOC is O mod Base,
    format(atom(Interpretation), 'Initializing COBO: ~w + ~w. (Bases: ~w, Ones: ~w)', [AR, O, OBC, OOC]),
    incur_cost(cobo_setup).

transition(state(q_loop_AddBases, K, AR, TS, R, T, O, TB, BC, OC, A_in, B_in), Base,
           state(q_loop_AddBases, K, AR, NewTS, R, T, O, TB, NewBC, OC, A_in, B_in),
           Interpretation) :-
    BC > 0,
    s(comp_nec(processing_cobo_base_components)),
    NewTS is TS + Base, 
    NewBC is BC - 1,
    format(atom(Interpretation), 'COBO (Base): ~w', [NewTS]),
    incur_cost(base_addition).

transition(state(q_loop_AddBases, K, AR, TS, R, T, O, TB, 0, OC, A_in, B_in), _,
           state(q_loop_AddOnes, K, AR, TS, R, T, O, TB, 0, OC, A_in, B_in),
           'COBO Bases complete.') :-
    s(exp_poss(completing_cobo_base_processing)),
    incur_cost(base_completion).

transition(state(q_loop_AddOnes, K, AR, TS, R, T, O, TB, BC, OC, A_in, B_in), _,
           state(q_loop_AddOnes, K, AR, NewTS, R, T, O, TB, BC, NewOC, A_in, B_in),
           Interpretation) :-
    OC > 0,
    s(comp_nec(processing_cobo_ones_components)),
    NewTS is TS + 1, 
    NewOC is OC - 1,
    format(atom(Interpretation), 'COBO (One): ~w', [NewTS]),
    incur_cost(ones_addition).

transition(state(q_loop_AddOnes, K, AR, TS, R, T, O, TB, BC, 0, A_in, B_in), _,
           state(q_init_Adjust, K, AR, TS, R, T, O, TB, BC, 0, A_in, B_in),
           Interpretation) :-
    s(exp_poss(completing_cobo_addition_phase)),
    format(atom(Interpretation), '~w + ~w = ~w.', [AR, O, TS]),
    incur_cost(addition_completion).

% Phase 3: Adjustment
transition(state(q_init_Adjust, K, AR, TS, _, T, O, TB, BC, OC, A_in, B_in), _,
           state(q_loop_Adjust, K, AR, TS, TS, T, O, TB, BC, OC, A_in, B_in),
           Interpretation) :-
    s(comp_nec(initializing_final_adjustment_phase)),
    format(atom(Interpretation), 'Initializing Adjustment: Count back K=~w.', [K]),
    incur_cost(adjustment_initialization).

transition(state(q_loop_Adjust, K, AR, TS, R, T, O, TB, BC, OC, A_in, B_in), _,
           state(q_loop_Adjust, NewK, AR, TS, NewR, T, O, TB, BC, OC, A_in, B_in),
           Interpretation) :-
    K > 0,
    s(comp_nec(continuing_adjustment_countdown)),
    NewK is K - 1, 
    NewR is R - 1,
    format(atom(Interpretation), 'Counting Back: ~w', [NewR]),
    incur_cost(adjustment_step).

transition(state(q_loop_Adjust, 0, AR, TS, R, T, _, _, _, _, A_in, B_in), _,
           state(q_accept, 0, AR, TS, R, T, 0, 0, 0, 0, A_in, B_in),
           Interpretation) :-
    s(exp_poss(finalizing_rounding_addition_result)),
    Adj is AR - T,
    format(atom(Interpretation), 'Subtracted Adjustment (~w). Final Result: ~w.', [Adj, R]),
    incur_cost(final_adjustment).

%!      accept_state(+State) is semidet.
%
%       Defines accepting states for the FSM.
accept_state(state(q_accept, _, _, _, _, _, _, _, _, _, _, _)).

%!      final_interpretation(+State, -Interpretation) is det.
%
%       Provides final interpretation of the computation.
final_interpretation(state(q_accept, _, _, _, Result, _, _, _, _, _, _, _), Interpretation) :-
    format(atom(Interpretation), 'Successfully computed sum: ~w via rounding and adjusting strategy', [Result]).

%!      extract_result_from_history(+History, -Result) is det.
%
%       Extracts the final result from the execution history.
extract_result_from_history(History, Result) :-
    last(History, LastStep),
    (LastStep = step(state(q_accept, _, _, _, Result, _, _, _, _, _, _, _), _, _) ->
        true
    ;
        Result = 'error'
    ).
