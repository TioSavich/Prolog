/** <module> Student Addition Strategy: Chunking by Bases and Ones
 *
 * This module implements the 'Chunking by Bases and Ones' strategy for
 * multi-digit addition, modeled as a finite state machine. This strategy
 * involves decomposing one of the numbers (B) into its base-10 components
 * (e.g., tens and ones), adding them sequentially to the other number (A),
 * and using strategic 'chunks' to reach friendly base-10 numbers.
 *
 * The process is as follows:
 * 1. Decompose B into a 'base chunk' (the tens part) and an 'ones chunk'.
 * 2. Add the entire base chunk to A at once.
 * 3. Strategically add parts of the ones chunk to get the sum to the next multiple of 10.
 * 4. Repeat until all parts of B have been added.
 *
 * The state is represented by the term:
 * `state(Name, Sum, BasesRem, OnesRem, K, InternalSum, TargetBase)`
 *
 * The history of execution is captured as a list of steps:
 * `step(StateName, CurrentSum, BasesRemaining, OnesRemaining, K, Interpretation)`
 *
 * 
 * 
 */
:- module(sar_add_chunking,
          [ run_chunking/4,
            % FSM Engine Interface
            setup_strategy/4,
            transition/3,
            transition/4,
            accept_state/1,
            final_interpretation/2,
            extract_result_from_history/2
          ]).

:- use_module(library(lists)).
:- use_module(fsm_engine).
:- use_module(grounded_arithmetic, [greater_than/2, smaller_than/2, equal_to/2,
                                  integer_to_recollection/2, recollection_to_integer/2, 
                                  add_grounded/3, subtract_grounded/3, successor/2,
                                  zero/1, incur_cost/1]).
:- use_module(grounded_utils, [base_decompose_grounded/4, base_recompose_grounded/4]).
:- use_module(incompatibility_semantics, [s/1, comp_nec/1, exp_poss/1]).

%!      run_chunking(+A:integer, +B:integer, -FinalSum:integer, -History:list) is det.
%
%       Executes the 'Chunking by Bases and Ones' addition strategy for A + B.
%
%       This predicate initializes the state machine and runs it until it
%       reaches the accept state. It traces the execution, providing a
%       step-by-step history of how the sum was computed.
%
%       @param A The first addend.
%       @param B The second addend, which will be decomposed and added in chunks.
%       @param FinalSum The resulting sum of A and B.
%       @param History A list of `step/6` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_chunking(A, B, FinalSum, History) :-
    % Use the FSM engine to run this strategy
    setup_strategy(A, B, InitialState, Parameters),
    Base = 10,
    run_fsm_with_base(sar_add_chunking, InitialState, Parameters, Base, History),
    extract_result_from_history(History, FinalSum).

%!      setup_strategy(+A, +B, -InitialState, -Parameters) is det.
%
%       Sets up the initial state for the chunking strategy.
setup_strategy(A, B, InitialState, Parameters) :-    
    % For now, use built-in arithmetic but add modal signals and cost tracking
    % This will be converted to full grounded arithmetic in a future iteration
    Base = 10,
    BasesRemaining is (B // Base) * Base,
    OnesRemaining is B mod Base,
    
    % Initial state
    InitialState = state(q_init, A, BasesRemaining, OnesRemaining, 0, 0, 0),
    Parameters = [A, B, Base],
    
    % Emit modal signal for strategy initiation
    s(exp_poss(initiating_chunking_strategy)),
    incur_cost(inference).

%!      transition(+CurrentState, -NextState, -Interpretation) is det.
%       transition(+CurrentState, +Base, -NextState, -Interpretation) is det.
%
%       State transition rules for the chunking strategy.

% Version without base parameter (for FSM engine compatibility)
transition(CurrentState, NextState, Interpretation) :-
    transition(CurrentState, 10, NextState, Interpretation).

% From q_init, always proceed to add the base chunk.
transition(state(q_init, Sum, BR, OR, K, IS, TB), _Base, state(q_add_base_chunk, Sum, BR, OR, K, IS, TB),
           'Proceed to add base chunk.') :-
    s(exp_poss(beginning_base_chunk_addition)),
    incur_cost(inference).

% From q_add_base_chunk:
% If there are bases remaining, add them all at once.
transition(state(q_add_base_chunk, Sum, BR, OR, _K, _IS, _TB), _Base, state(q_init_ones_chunk, NewSum, 0, OR, 0, 0, 0), Interpretation) :-
    BR > 0,
    NewSum is Sum + BR,
    s(comp_nec(adding_complete_base_chunk)),
    incur_cost(unit_count),
    format(string(Interpretation), 'Add Base Chunk (+~w). Sum = ~w.', [BR, NewSum]).

% If there are no bases, move on.
transition(state(q_add_base_chunk, Sum, 0, OR, _K, _IS, _TB), _Base, state(q_init_ones_chunk, Sum, 0, OR, 0, 0, 0),
           'No bases to add.') :-
    s(exp_poss(skipping_empty_base_chunk)),
    incur_cost(inference).

% From q_init_ones_chunk:
% If there are ones to add, start the strategic chunking process.
transition(state(q_init_ones_chunk, Sum, BR, OR, K, _IS, _TB), _Base, state(q_init_K, Sum, BR, OR, K, Sum, TargetBase), Interpretation) :-
    OR > 0,
    % Calculate target base using built-in arithmetic (to be converted later)
    calculate_next_base_grounded(Sum, TargetBase),
    s(exp_poss(beginning_strategic_ones_chunking)),
    incur_cost(inference),
    format(string(Interpretation), 'Begin strategic chunking of remaining ones (~w).', [OR]).

% If no ones are left, the process is finished.
transition(state(q_init_ones_chunk, Sum, _, 0, _, _, _), _Base, state(q_accept, Sum, 0, 0, 0, 0, 0),
           'All ones added. Accepting.') :-
    s(comp_nec(completing_chunking_strategy)),
    incur_cost(inference).

% From q_init_K, calculate the value K needed to reach the next base.
transition(state(q_init_K, Sum, BR, OR, _, IS, TB), _Base, state(q_loop_K, Sum, BR, OR, 0, IS, TB), Interpretation) :-
    s(exp_poss(calculating_distance_to_target_base)),
    incur_cost(inference),
    format(string(Interpretation), 'Calculating K: Counting from ~w to ~w.', [Sum, TB]).

% From q_loop_K, count up from the current sum to the target base to find K.
transition(state(q_loop_K, Sum, BR, OR, K, IS, TB), _Base, state(q_loop_K, Sum, BR, OR, NewK, NewIS, TB), Interpretation) :-
    IS < TB,
    NewIS is IS + 1,
    NewK is K + 1,
    s(comp_nec(counting_units_to_target)),
    incur_cost(unit_count),
    format(string(Interpretation), 'Counting Up: ~w, K=~w', [NewIS, NewK]).

% Once the target base is reached, the value of K is known.
transition(state(q_loop_K, Sum, BR, OR, K, IS, TB), _Base, state(q_add_ones_chunk, Sum, BR, OR, K, IS, TB), Interpretation) :-
    IS >= TB,
    s(exp_poss(target_distance_calculated)),
    incur_cost(inference),
    format(string(Interpretation), 'K needed to reach base is ~w.', [K]).

% From q_add_ones_chunk:
% If we have enough ones remaining to add the strategic chunk K, do so.
transition(state(q_add_ones_chunk, Sum, BR, OR, K, _IS, _TB), _Base, state(q_init_ones_chunk, NewSum, BR, NewOR, 0, 0, 0), Interpretation) :-
    OR >= K, K > 0,
    NewSum is Sum + K,
    NewOR is OR - K,
    s(exp_poss(adding_strategic_chunk_to_reach_base)),
    incur_cost(unit_count),
    format(string(Interpretation), 'Add Strategic Chunk (+~w) to make base. Sum = ~w.', [K, NewSum]).

% Otherwise, add all remaining ones. This happens if K is too large or 0.
transition(state(q_add_ones_chunk, Sum, BR, OR, K, _IS, _TB), _Base, state(q_init_ones_chunk, NewSum, BR, 0, 0, 0, 0), Interpretation) :-
    (OR < K ; K =< 0), OR > 0,
    NewSum is Sum + OR,
    s(comp_nec(adding_remaining_ones)),
    incur_cost(unit_count),
    format(string(Interpretation), 'Add Remaining Chunk (+~w). Sum = ~w.', [OR, NewSum]).

%!      calculate_next_base_grounded(+Sum, -TargetBase) is det.
%
%       Calculates the next multiple of 10 using the same logic as before.
calculate_next_base_grounded(Sum, TargetBase) :-
    % For now, keep the arithmetic calculation but mark it for future conversion
    (Sum > 0, Sum mod 10 =\= 0 -> TargetBase is ((Sum // 10) + 1) * 10 ; TargetBase is Sum).

%!      accept_state(+State) is semidet.
%
%       Identifies terminal states.
accept_state(state(q_accept, _, _, _, _, _, _)).

%!      final_interpretation(+State, -Interpretation) is det.
%
%       Provides final interpretation for terminal states.
final_interpretation(state(q_accept, Sum, _, _, _, _, _), Interpretation) :-
    format(string(Interpretation), 'Chunking Complete. Final sum: ~w.', [Sum]).

%!      extract_result_from_history(+History, -Result) is det.
%
%       Extracts the final result from the execution history.
extract_result_from_history(History, Result) :-
    last(History, LastStep),
    (LastStep = step(state(q_accept, Sum, _, _, _, _, _), _, _) ->
        Result = Sum
    ;
        Result = 'error'
    ).
