/** <module> Student Subtraction Strategy: Chunking Backwards to Part
 *
 * This module implements a "counting down" or "take away in chunks" strategy
 * for subtraction (M - S), modeled as a finite state machine. It solves the
 * problem by calculating what needs to be subtracted from M to reach S.
 *
 * The process is as follows:
 * 1. Start at the minuend (M). The goal is to reach the subtrahend (S).
 * 2. Identify a "strategic" chunk to subtract. This could be:
 *    a. The amount `K` needed to get from the current value down to the next
 *       lower multiple of 10 (or 100, etc.).
 *    b. If that's not suitable, the largest possible place-value chunk of the
 *       *remaining distance* to S.
 * 3. Subtract the selected chunk. The size of the chunk is added to a running
 *    total, `Distance`.
 * 4. Repeat until the current value reaches S. The final `Distance` is the
 *    answer to the subtraction problem.
 * 5. The strategy fails if S > M.
 *
 * The state is represented by the term:
 * `state(Name, CurrentValue, Distance, K, TargetBase, InternalTemp, S_target)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, CurrentValue, Distance, K, Interpretation)`
 *
 * 
 * 
 */
:- module(sar_sub_chunking_c,
          [ run_chunking_c/4,
            % FSM Engine Interface
            setup_strategy/4,
            transition/3,
            transition/4,
            accept_state/1,
            final_interpretation/2,
            extract_result_from_history/2
          ]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(fsm_engine, [run_fsm_with_base/5]).
:- use_module(grounded_arithmetic, [incur_cost/1]).
:- use_module(incompatibility_semantics, [s/1, comp_nec/1, exp_poss/1]).

%!      run_chunking_c(+M:integer, +S:integer, -FinalResult:integer, -History:list) is det.
%
%       Executes the 'Chunking Backwards to Part' subtraction strategy for M - S.
%
%       This predicate initializes and runs a state machine that models the
%       "counting down" process. It first checks if the subtraction is possible (M >= S).
%       If so, it calculates the difference by subtracting chunks from M until it reaches S.
%       The sum of these chunks is the result. It traces the entire execution,
%       providing a step-by-step history.
%
%       @param M The Minuend, the number to start counting down from.
%       @param S The Subtrahend, the target number to reach.
%       @param FinalResult The resulting difference (M - S). If S > M, this
%       will be the atom `'error'`.
%       @param History A list of `step/5` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_chunking_c(M, S, FinalResult, History) :-
    % Use the FSM engine to run this strategy
    setup_strategy(M, S, InitialState, Parameters),
    Base = 10,
    run_fsm_with_base(sar_sub_chunking_c, InitialState, Parameters, Base, History),
    extract_result_from_history(History, FinalResult).

%!      setup_strategy(+M, +S, -InitialState, -Parameters) is det.
%
%       Sets up the initial state for the chunking subtraction strategy.
setup_strategy(M, S, InitialState, Parameters) :-
    % Check if subtraction is valid
    (S > M ->
        InitialState = state(q_error, 0, 0, 0, 0, 0, S)
    ;
        InitialState = state(q_init, M, 0, 0, 0, 0, S)
    ),
    Parameters = [M, S],
    
    % Emit modal signal for strategy initiation
    s(exp_poss(initiating_backward_chunking_strategy)),
    incur_cost(inference).

%!      transition(+StateNum, -NextStateNum, -Action) is det.
%
%       State transitions for backward chunking subtraction FSM.

transition(q_init, q_check_status, check_target_reached) :-
    s(comp_nec(transitioning_to_status_check)),
    incur_cost(state_change).

transition(q_check_status, q_init_K, continue_subtraction) :-
    s(exp_poss(continuing_backward_chunking)),
    incur_cost(computation).

transition(q_check_status, q_accept, reach_target) :-
    s(exp_poss(reaching_target_via_backward_counting)),
    incur_cost(completion).

transition(q_error, q_error, maintain_error) :-
    s(comp_nec(error_state_is_absorbing)),
    incur_cost(error_handling).

%!      transition(+State, +Base, -NextState, -Interpretation) is det.
%
%       Complete state transitions with full state tracking.
transition(state(q_init, M, _, _, _, _, S), _,
           state(q_check_status, M, 0, 0, 0, 0, S),
           Interpretation) :-
    s(exp_poss(initializing_backward_chunk_calculation)),
    format(atom(Interpretation), 'Start at M (~w). Target is S (~w).', [M, S]),
    incur_cost(initialization).

transition(state(q_check_status, CV, Dist, _, _, _, S), _,
           state(q_init_K, CV, Dist, 0, 0, CV, S), 
           'Need to subtract more.') :-
    CV > S,
    s(comp_nec(current_value_exceeds_target)),
    incur_cost(comparison).

transition(state(q_check_status, S, Dist, _, _, _, S), _,
           state(q_accept, S, Dist, 0, 0, 0, S), 
           'Target reached.') :-
    s(exp_poss(successfully_reaching_subtraction_target)),
    incur_cost(target_achievement).

transition(state(q_init_K, CV, D, K, _, IT, S), Base,
           state(q_loop_K, CV, D, K, TB, IT, S), 
           Interpretation) :-
    s(exp_poss(calculating_strategic_chunk_size)),
    find_target_base_back(CV, S, Base, 1, TB),
    format(atom(Interpretation), 'Calculating K: Counting back from ~w to ~w.', [CV, TB]),
    incur_cost(chunk_calculation).

transition(state(q_loop_K, CV, D, K, TB, IT, S), _,
           state(q_loop_K, CV, D, NewK, TB, NewIT, S), 
           'Counting down to base.') :-
    IT > TB,
    s(comp_nec(continuing_countdown_to_base)),
    NewIT is IT - 1,
    NewK is K + 1,
    incur_cost(counting_step).

transition(state(q_loop_K, CV, D, K, TB, IT, S), _,
           state(q_sub_chunk, CV, D, K, TB, IT, S), 
           'Ready to subtract chunk.') :-
    IT =< TB,
    s(exp_poss(ready_for_chunk_subtraction)),
    incur_cost(chunk_preparation).

transition(state(q_sub_chunk, CV, D, K, _, _, S), Base,
           state(q_check_status, NewCV, NewD, 0, 0, 0, S), 
           Interpretation) :-
    s(exp_poss(executing_backward_chunk_subtraction)),
    Remaining is CV - S,
    (K > 0, K =< Remaining ->
        Chunk = K,
        format(atom(Interpretation), 'Subtract strategic chunk (-~w) to reach base.', [Chunk]),
        incur_cost(strategic_chunking)
    ;
        (Remaining > 0 ->
            Power is floor(log(Remaining) / log(Base)),
            PowerValue is Base^Power,
            C is floor(Remaining / PowerValue) * PowerValue,
            (C > 0 -> Chunk = C ; Chunk = Remaining),
            format(atom(Interpretation), 'Subtract large/remaining chunk (-~w).', [Chunk]),
            incur_cost(large_chunking)
        )
    ),
    NewCV is CV - Chunk,
    NewD is D + Chunk.

transition(state(q_error, _, _, _, _, _, _), _,
           state(q_error, 0, 0, 0, 0, 0, 0),
           'Error state maintained.') :-
    s(comp_nec(error_state_persistence)),
    incur_cost(error_maintenance).

%!      accept_state(+State) is semidet.
%
%       Defines accepting states for the FSM.
accept_state(state(q_accept, _, _, _, _, _, _)).

%!      final_interpretation(+State, -Interpretation) is det.
%
%       Provides final interpretation of the computation.
final_interpretation(state(q_accept, _, Distance, _, _, _, _), Interpretation) :-
    format(atom(Interpretation), 'Successfully computed difference: ~w via backward chunking', [Distance]).
final_interpretation(state(q_error, _, _, _, _, _, _), 'Error: Backward chunking subtraction failed').

%!      extract_result_from_history(+History, -Result) is det.
%
%       Extracts the final result from the execution history.
extract_result_from_history(History, Result) :-
    last(History, LastStep),
    (LastStep = step(state(q_accept, _, Distance, _, _, _, _), _, _) ->
        Result = Distance
    ;
        Result = 'error'
    ).

% find_target_base_back/5 is a helper to find the next "friendly" number (counting down).
find_target_base_back(CV, S, Base, Power, TargetBase) :-
    BasePower is Base^Power,
    (CV mod BasePower =\= 0 ->
        TargetBase is floor(CV / BasePower) * BasePower
    ;
        (BasePower > CV ->
            TargetBase = CV
        ;
            NewPower is Power + 1,
            find_target_base_back(CV, S, Base, NewPower, TargetBase)
        )
    ).
