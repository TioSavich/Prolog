/** <module> Student Subtraction Strategy: Double Rounding
 *
 * This module implements a "double rounding" strategy for subtraction (M - S),
 * sometimes used by students to simplify the calculation. It is modeled as a
 * finite state machine.
 *
 * The process is as follows:
 * 1. Round both the minuend (M) and the subtrahend (S) down to the nearest
 *    multiple of 10. Let the rounded values be MR and SR, and the amounts
 *    they were rounded by be KM and KS respectively.
 * 2. Perform a simplified subtraction on the rounded numbers: `TR = MR - SR`.
 * 3. Adjust this temporary result. First, add back the amount M was rounded by: `TR + KM`.
 * 4. Second, subtract the amount S was rounded by: `(TR + KM) - KS`.
 *    This final adjustment is modeled as a chunking/counting-back process.
 * 5. The strategy fails if S > M.
 *
 * The state is represented by the term:
 * `state(Name, K_M, K_S, TempResult, K_S_Rem, Chunk, M, S, MR, SR)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, K_M, K_S, TempResult, K_S_Rem, Interpretation)`
 *
 * 
 * 
 */
:- module(sar_sub_rounding,
          [ run_sub_rounding/4,
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

%!      run_sub_rounding(+M:integer, +S:integer, -FinalResult:integer, -History:list) is det.
%
%       Executes the 'Double Rounding' subtraction strategy for M - S.
%
%       This predicate initializes and runs a state machine that models the
%       double rounding process. It first checks if the subtraction is possible
%       (M >= S). If so, it rounds both numbers down, subtracts them, and then
%       performs two adjustments to arrive at the final answer. It traces
%       the entire execution, providing a step-by-step history.
%
%       @param M The Minuend.
%       @param S The Subtrahend.
%       @param FinalResult The resulting difference (M - S). If S > M, this
%       will be the atom `'error'`.
%       @param History A list of `step/6` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_sub_rounding(M, S, FinalResult, History) :-
    % Use the FSM engine to run this strategy
    setup_strategy(M, S, InitialState, Parameters),
    Base = 10,
    run_fsm_with_base(sar_sub_rounding, InitialState, Parameters, Base, History),
    extract_result_from_history(History, FinalResult).

%!      setup_strategy(+M, +S, -InitialState, -Parameters) is det.
%
%       Sets up the initial state for the double rounding subtraction strategy.
setup_strategy(M, S, InitialState, Parameters) :-
    % Check if subtraction is valid
    (S > M ->
        InitialState = state(q_error, 0, 0, 0, 0, 0, M, S, 0, 0)
    ;
        InitialState = state(q_init, 0, 0, 0, 0, 0, M, S, 0, 0)
    ),
    Parameters = [M, S],
    
    % Emit modal signal for strategy initiation
    s(exp_poss(initiating_double_rounding_subtraction_strategy)),
    incur_cost(inference).

%!      transition(+StateNum, -NextStateNum, -Action) is det.
%
%       State transitions for double rounding subtraction FSM.

transition(q_init, q_round_M, begin_minuend_rounding) :-
    s(comp_nec(transitioning_to_minuend_rounding)),
    incur_cost(state_change).

transition(q_round_M, q_round_S, begin_subtrahend_rounding) :-
    s(exp_poss(proceeding_to_subtrahend_rounding)),
    incur_cost(rounding_transition).

transition(q_round_S, q_subtract, perform_rounded_subtraction) :-
    s(comp_nec(executing_rounded_number_subtraction)),
    incur_cost(computation).

transition(q_subtract, q_adjust_M, begin_minuend_adjustment) :-
    s(exp_poss(beginning_minuend_adjustment_phase)),
    incur_cost(adjustment_preparation).

transition(q_adjust_M, q_init_adjust_S, prepare_subtrahend_adjustment) :-
    s(comp_nec(preparing_subtrahend_adjustment_phase)),
    incur_cost(preparation).

transition(q_init_adjust_S, q_loop_adjust_S, begin_subtrahend_adjustment_loop) :-
    s(exp_poss(entering_subtrahend_adjustment_loop)),
    incur_cost(loop_initialization).

transition(q_loop_adjust_S, q_accept, complete_rounding_strategy) :-
    s(exp_poss(completing_double_rounding_strategy)),
    incur_cost(completion).

transition(q_error, q_error, maintain_error) :-
    s(comp_nec(error_state_is_absorbing)),
    incur_cost(error_handling).

%!      transition(+State, +Base, -NextState, -Interpretation) is det.
%
%       Complete state transitions with full state tracking.

% Initial state, proceeds to rounding the Minuend.
transition(state(q_init, _, _, _, _, _, M, S, _, _), _,
           state(q_round_M, 0, 0, 0, 0, 0, M, S, 0, 0), 
           'Proceed to round M.') :-
    s(exp_poss(initiating_minuend_rounding_process)),
    incur_cost(initialization).

% Round M down and record the amount it was rounded by (KM).
transition(state(q_round_M, _, _, _, _, _, M, S, _, _), Base,
           state(q_round_S, KM, 0, 0, 0, 0, M, S, MR, 0), 
           Interpretation) :-
    s(comp_nec(calculating_minuend_rounding_amount)),
    KM is M mod Base,
    MR is M - KM,
    format(atom(Interpretation), 'Round M down: ~w -> ~w. (K_M = ~w).', [M, MR, KM]),
    incur_cost(minuend_rounding).

% Round S down and record the amount it was rounded by (KS).
transition(state(q_round_S, KM, _, _, _, _, M, S, MR, _), Base,
           state(q_subtract, KM, KS, 0, 0, 0, M, S, MR, SR), 
           Interpretation) :-
    s(comp_nec(calculating_subtrahend_rounding_amount)),
    KS is S mod Base,
    SR is S - KS,
    format(atom(Interpretation), 'Round S down: ~w -> ~w. (K_S = ~w).', [S, SR, KS]),
    incur_cost(subtrahend_rounding).

% Perform the intermediate subtraction with the rounded numbers.
transition(state(q_subtract, KM, KS, _, _, _, M, S, MR, SR), _,
           state(q_adjust_M, KM, KS, TR, 0, 0, M, S, MR, SR), 
           Interpretation) :-
    s(exp_poss(executing_intermediate_subtraction)),
    TR is MR - SR,
    format(atom(Interpretation), 'Intermediate Subtraction: ~w - ~w = ~w.', [MR, SR, TR]),
    incur_cost(intermediate_subtraction).

% First adjustment: Add back the amount M was rounded by (KM).
transition(state(q_adjust_M, KM, KS, TR, _, _, M, S, MR, SR), _,
           state(q_init_adjust_S, KM, KS, NewTR, 0, 0, M, S, MR, SR), 
           Interpretation) :-
    s(comp_nec(applying_minuend_adjustment)),
    NewTR is TR + KM,
    format(atom(Interpretation), 'Adjust for M (Add K_M): ~w + ~w = ~w.', [TR, KM, NewTR]),
    incur_cost(minuend_adjustment).

% Prepare for the second adjustment: subtracting KS.
transition(state(q_init_adjust_S, KM, KS, TR, _, _, M, S, MR, SR), _,
           state(q_loop_adjust_S, KM, KS, TR, KS, 0, M, S, MR, SR), 
           Interpretation) :-
    s(exp_poss(preparing_subtrahend_adjustment_loop)),
    format(atom(Interpretation), 'Begin Adjust for S (Subtract K_S): Need to subtract ~w.', [KS]),
    incur_cost(adjustment_preparation).

% Second adjustment is complete when the remainder (KSR) is zero.
transition(state(q_loop_adjust_S, KM, KS, TR, 0, _, M, S, MR, SR), _,
           state(q_accept, KM, KS, TR, 0, 0, M, S, MR, SR), 
           'Adjustment for S complete.') :-
    s(exp_poss(completing_subtrahend_adjustment)),
    incur_cost(adjustment_completion).

% Perform the second adjustment by subtracting KS in chunks.
transition(state(q_loop_adjust_S, KM, KS, TR, KSR, _, M, S, MR, SR), Base,
           state(q_loop_adjust_S, KM, KS, NewTR, NewKSR, Chunk, M, S, MR, SR), 
           Interpretation) :-
    KSR > 0,
    s(comp_nec(continuing_chunked_subtrahend_adjustment)),
    K_to_prev_base is TR mod Base,
    (K_to_prev_base > 0, KSR >= K_to_prev_base -> 
        Chunk = K_to_prev_base 
    ; 
        Chunk = KSR),
    NewTR is TR - Chunk,
    NewKSR is KSR - Chunk,
    format(atom(Interpretation), 'Chunking Adjustment: ~w - ~w = ~w.', [TR, Chunk, NewTR]),
    incur_cost(chunked_adjustment).

transition(state(q_error, _, _, _, _, _, _, _, _, _), _,
           state(q_error, 0, 0, 0, 0, 0, 0, 0, 0, 0),
           'Error: Invalid subtraction.') :-
    s(comp_nec(error_state_persistence)),
    incur_cost(error_maintenance).

%!      accept_state(+State) is semidet.
%
%       Defines accepting states for the FSM.
accept_state(state(q_accept, _, _, _, _, _, _, _, _, _)).

%!      final_interpretation(+State, -Interpretation) is det.
%
%       Provides final interpretation of the computation.
final_interpretation(state(q_accept, _, _, FinalResult, _, _, _, _, _, _), Interpretation) :-
    format(atom(Interpretation), 'Successfully computed difference: ~w via double rounding strategy', [FinalResult]).
final_interpretation(state(q_error, _, _, _, _, _, _, _, _, _), 'Error: Double rounding subtraction failed').

%!      extract_result_from_history(+History, -Result) is det.
%
%       Extracts the final result from the execution history.
extract_result_from_history(History, Result) :-
    last(History, LastStep),
    (LastStep = step(state(q_accept, _, _, Result, _, _, _, _, _, _), _, _) ->
        true
    ;
        Result = 'error'
    ).
