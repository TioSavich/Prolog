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
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(sar_add_rmb,
          [ run_rmb/4
          ]).

:- use_module(library(lists)).

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
    Base = 10,
    % Ensure A is the larger number and B is the smaller.
    A is max(A_in, B_in),
    B is min(A_in, B_in),

    % Initial state (q_calc_K): Determine K needed to get A to a multiple of 10.
    (A mod Base =:= 0, A =\= 0 -> TargetBase is A ; TargetBase is ((A // Base) + 1) * Base),
    InitialState = state(q_calc_K, A, B, 0, A, 0, TargetBase, B), % B_initial stored for error msg

    InitialInterpretation = 'Start. Determine larger number and target base.',
    InitialHistoryEntry = step(q_start, A, B, 0, 0, 0, InitialInterpretation),

    run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
    reverse(ReversedHistory, History),

    % Check the final state to determine the result.
    (last(History, step(q_accept, FinalA, FinalB, _, _, _, _)) ->
        FinalResult is FinalA + FinalB
    ;
        FinalResult = 'error'
    ).

% run/4 is the main recursive loop of the state machine.

% Base case: Stop when the machine reaches the 'q_accept' state.
run(state(q_accept, A, B, K, AT, BT, _, _), _, Acc, FinalHistory) :-
    Result is A + B,
    format(string(Interpretation), 'Combine rearranged numbers: ~w + ~w = ~w.', [A, B, Result]),
    HistoryEntry = step(q_accept, A, B, K, AT, BT, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

% Recursive step: Perform one transition and continue.
run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, A, B, K, AT, BT, _, _),
    HistoryEntry = step(Name, A, B, K, AT, BT, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% transition/4 defines the logic for moving from one state to the next.

% In q_calc_K, count up from A to the target base to determine K.
transition(state(q_calc_K, A, B, K, AT, BT, TB, B_init), _, state(q_calc_K, A, B, NewK, NewAT, BT, TB, B_init), Interpretation) :-
    AT < TB,
    NewAT is AT + 1,
    NewK is K + 1,
    format(string(Interpretation), 'Count up: ~w. Distance (K): ~w.', [NewAT, NewK]).
% Once K is found, transition to q_decompose_B to transfer K from B.
transition(state(q_calc_K, A, B, K, AT, BT, TB, B_init), _, state(q_decompose_B, A, B, K, AT, B, TB, B_init), Interpretation) :-
    AT >= TB,
    format(string(Interpretation), 'K needed is ~w. Start counting down K from B.', [K]).

% In q_decompose_B, "transfer" K from B to A by decrementing both K and a temp copy of B.
transition(state(q_decompose_B, A, B, K, AT, BT, TB, B_init), _, state(q_decompose_B, A, B, NewK, AT, NewBT, TB, B_init), Interpretation) :-
    K > 0, BT > 0,
    NewK is K - 1,
    NewBT is BT - 1,
    format(string(Interpretation), 'Transferred 1. B remainder: ~w. K remaining: ~w.', [NewBT, NewK]).
% Once K is fully transferred (K=0), recombine the numbers.
transition(state(q_decompose_B, _, _, 0, AT, BT, _, _), _, state(q_recombine, AT, BT, 0, AT, BT, 0, 0), Interpretation) :-
    format(string(Interpretation), 'Decomposition Complete. New state: A=~w, B=~w.', [AT, BT]).
% If B runs out before K is transferred, the strategy fails.
transition(state(q_decompose_B, _, _, K, _, 0, _, B_init), _, state(q_error, 0,0,0,0,0,0,0), Interpretation) :-
    K > 0,
    format(string(Interpretation), 'Strategy Failed. B (~w) is too small to provide K (~w).', [B_init, K]).

% From q_recombine, proceed to the final accept state.
transition(state(q_recombine, A, B, K, AT, BT, _, _), _, state(q_accept, A, B, K, AT, BT, 0, 0), 'Proceed to accept.').
