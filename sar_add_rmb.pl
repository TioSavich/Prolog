/** <module> Student Addition Strategy: Rearranging to Make Bases (RMB)
 *
 * This module implements the 'Rearranging to Make Bases' (RMB) strategy for
 * addition, modeled as a finite state machine. This is a sophisticated
 * strategy where a student rearranges quantities between the two addends
 * to create a "friendly" number (a multiple of 10), simplifying the final calculation.
 *
 * The process is as follows:
 * 1. Identify the larger number (A) and the smaller number (B).
 * 2. Calculate how much A needs to reach the next multiple of 10. This amount is K.
 * 3. "Take" K from B and "give" it to A. This is a decomposition and recombination step.
 * 4. The new problem becomes (A + K) + (B - K).
 * 5. The strategy fails if B is smaller than K.
 *
 * The state is represented by the term:
 * `state(Name, A, B, K, A_temp, B_temp, TargetBase, B_initial)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, A, B, K, A_temp, B_temp, Interpretation)`
 *
 * 
 * 
 */
:- module(sar_add_rmb,
          [ run_rmb/4,
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

%!      run_rmb(+A_in:integer, +B_in:integer, -FinalResult:integer, -History:list) is det.
%
%       Executes the 'Rearranging to Make Bases' (RMB) addition strategy for A + B.
%
%       This predicate initializes and runs a state machine that models the RMB
%       strategy. It first determines the amount `K` needed for the larger number
%       to reach a multiple of 10, then transfers `K` from the smaller number.
%       It traces the execution, providing a step-by-step history.
%
%       @param A_in The first addend.
%       @param B_in The second addend.
%       @param FinalResult The resulting sum of A and B. If the strategy
%       fails (because the smaller addend is less than K), this will be the
%       atom `'error'`.
%       @param History A list of `step/7` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_rmb(A_in, B_in, FinalResult, History) :-
    % Use the FSM engine to run this strategy
    setup_strategy(A_in, B_in, InitialState, Parameters),
    Base = 10,
    run_fsm_with_base(sar_add_rmb, InitialState, Parameters, Base, History),
    extract_result_from_history(History, FinalResult).

%!      setup_strategy(+A, +B, -InitialState, -Parameters) is det.
%
%       Sets up the initial state for the RMB addition strategy.
setup_strategy(A_in, B_in, InitialState, Parameters) :-
    InitialState = state(q_init, A_in, B_in, 0, 0, 0, 0, 0),
    Parameters = [A_in, B_in],
    
    % Emit modal signal for strategy initiation
    s(exp_poss(initiating_rearranging_make_bases_strategy)),
    incur_cost(inference).
%!      transition(+StateNum, -NextStateNum, -Action) is det.
%
%       State transitions for RMB addition FSM.

transition(q_init, q_determine_order, determine_number_ordering) :-
    s(comp_nec(transitioning_to_number_ordering)),
    incur_cost(state_change).

transition(q_determine_order, q_calc_K, calculate_rearrangement_amount) :-
    s(exp_poss(calculating_amount_for_base_creation)),
    incur_cost(calculation).

transition(q_calc_K, q_decompose_B, begin_quantity_transfer) :-
    s(comp_nec(beginning_quantity_decomposition)),
    incur_cost(decomposition_start).

transition(q_decompose_B, q_recombine, complete_decomposition) :-
    s(exp_poss(completing_quantity_rearrangement)),
    incur_cost(recombination_preparation).

transition(q_decompose_B, q_error, decomposition_failure) :-
    s(comp_nec(insufficient_quantity_for_transfer)),
    incur_cost(strategy_failure).

transition(q_recombine, q_accept, finalize_rearrangement) :-
    s(exp_poss(finalizing_rearranged_addition)),
    incur_cost(completion).

transition(q_error, q_error, maintain_error) :-
    s(comp_nec(error_state_is_absorbing)),
    incur_cost(error_handling).

%!      transition(+State, +Base, -NextState, -Interpretation) is det.
%
%       Complete state transitions with full state tracking.

% From q_init, determine larger and smaller numbers
transition(state(q_init, A_in, B_in, _, _, _, _, _), Base,
           state(q_determine_order, A, B, 0, A, B, 0, B),
           Interpretation) :-
    s(exp_poss(determining_optimal_number_ordering)),
    A is max(A_in, B_in),
    B is min(A_in, B_in),
    format(atom(Interpretation), 'Inputs: ~w, ~w. Larger: ~w, Smaller: ~w.', [A_in, B_in, A, B]),
    incur_cost(ordering_determination).

% Prepare to calculate K
transition(state(q_determine_order, A, B, _, _, _, _, _), Base,
           state(q_calc_K, A, B, 0, A, B, TargetBase, B),
           Interpretation) :-
    s(comp_nec(calculating_target_base_for_rearrangement)),
    (A mod Base =:= 0, A =\= 0 -> 
        TargetBase = A 
    ; 
        TargetBase is ((A // Base) + 1) * Base),
    format(atom(Interpretation), 'Target base for A (~w): ~w. Need to calculate K.', [A, TargetBase]),
    incur_cost(target_calculation).

% In q_calc_K, count up from A to the target base to determine K.
transition(state(q_calc_K, A, B, K, AT, BT, TB, B_init), _,
           state(q_calc_K, A, B, NewK, NewAT, BT, TB, B_init),
           Interpretation) :-
    AT < TB,
    s(comp_nec(continuing_k_calculation_count)),
    NewAT is AT + 1,
    NewK is K + 1,
    format(atom(Interpretation), 'Count up: ~w. Distance (K): ~w.', [NewAT, NewK]),
    incur_cost(counting_step).

% Once K is found, transition to q_decompose_B to transfer K from B.
transition(state(q_calc_K, A, B, K, AT, _BT, TB, B_init), _,
           state(q_decompose_B, A, B, K, AT, B, TB, B_init),
           Interpretation) :-
    AT >= TB,
    s(exp_poss(completing_k_calculation_for_transfer)),
    format(atom(Interpretation), 'K needed is ~w. Start counting down K from B.', [K]),
    incur_cost(k_completion).

% In q_decompose_B, "transfer" K from B to A by decrementing both K and a temp copy of B.
transition(state(q_decompose_B, A, B, K, AT, BT, TB, B_init), _,
           state(q_decompose_B, A, B, NewK, AT, NewBT, TB, B_init),
           Interpretation) :-
    K > 0, BT > 0,
    s(comp_nec(continuing_quantity_transfer_operation)),
    NewK is K - 1,
    NewBT is BT - 1,
    format(atom(Interpretation), 'Transferred 1. B remainder: ~w. K remaining: ~w.', [NewBT, NewK]),
    incur_cost(transfer_step).

% Once K is fully transferred (K=0), recombine the numbers.
transition(state(q_decompose_B, _, _, 0, AT, BT, _, _), _,
           state(q_recombine, AT, BT, 0, AT, BT, 0, 0),
           Interpretation) :-
    s(exp_poss(completing_quantity_decomposition)),
    format(atom(Interpretation), 'Decomposition Complete. New state: A=~w, B=~w.', [AT, BT]),
    incur_cost(decomposition_completion).

% If B runs out before K is transferred, the strategy fails.
transition(state(q_decompose_B, _, _, K, _, 0, _, B_init), _,
           state(q_error, 0, 0, 0, 0, 0, 0, 0),
           Interpretation) :-
    K > 0,
    s(comp_nec(detecting_insufficient_quantity_for_transfer)),
    format(atom(Interpretation), 'Strategy Failed. B (~w) is too small to provide K (~w).', [B_init, K]),
    incur_cost(strategy_failure).

% From q_recombine, proceed to the final accept state.
transition(state(q_recombine, A, B, K, AT, BT, _, _), _,
           state(q_accept, A, B, K, AT, BT, 0, 0),
           'Proceed to accept.') :-
    s(exp_poss(proceeding_to_final_acceptance)),
    incur_cost(final_transition).

transition(state(q_error, _, _, _, _, _, _, _), _,
           state(q_error, 0, 0, 0, 0, 0, 0, 0),
           'Error state maintained.') :-
    s(comp_nec(error_state_persistence)),
    incur_cost(error_maintenance).

%!      accept_state(+State) is semidet.
%
%       Defines accepting states for the FSM.
accept_state(state(q_accept, _, _, _, _, _, _, _)).

%!      final_interpretation(+State, -Interpretation) is det.
%
%       Provides final interpretation of the computation.
final_interpretation(state(q_accept, A, B, _, _, _, _, _), Interpretation) :-
    Sum is A + B,
    format(atom(Interpretation), 'Successfully computed sum: ~w via rearranging to make bases strategy', [Sum]).
final_interpretation(state(q_error, _, _, _, _, _, _, _), 'Error: RMB addition failed - insufficient quantity for rearrangement').

%!      extract_result_from_history(+History, -Result) is det.
%
%       Extracts the final result from the execution history.
extract_result_from_history(History, Result) :-
    last(History, LastStep),
    (LastStep = step(state(q_accept, A, B, K, AT, BT, 0, 0), _, _) ->
        Result is A + B
    ;
        Result = 'error'
    ).
