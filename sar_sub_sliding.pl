/** <module> Student Subtraction Strategy: Sliding (Constant Difference)
 *
 * This module implements the "sliding" or "constant difference" strategy for
 * subtraction (M - S), modeled as a finite state machine.
 *
 * The core idea of this strategy is that the difference between two numbers
 * remains the same if both numbers are shifted by the same amount. The
 * strategy simplifies the problem `M - S` by transforming it into
 * `(M + K) - (S + K)`, where `K` is chosen to make `S + K` a "friendly"
 * number (a multiple of 10).
 *
 * The process is as follows:
 * 1. Determine the amount `K` needed to "slide" the subtrahend (S) up to the
 *    next multiple of 10.
 * 2. Add `K` to both the minuend (M) and the subtrahend (S) to get the new
 *    numbers, `M_adj` and `S_adj`.
 * 3. Perform the simplified subtraction `M_adj - S_adj`.
 * 4. The strategy fails if S > M.
 *
 * The state is represented by the term:
 * `state(Name, K, M_adj, S_adj, TargetBase, TempCounter, M, S)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, K, M_adj, S_adj, Interpretation)`
 *
 * 
 * 
 */
:- module(sar_sub_sliding,
          [ run_sliding/4
          ]).

:- use_module(library(lists)).

%!      run_sliding(+M:integer, +S:integer, -FinalResult:integer, -History:list) is det.
%
%       Executes the 'Sliding' (Constant Difference) subtraction strategy for M - S.
%
%       This predicate initializes and runs a state machine that models the
%       sliding strategy. It first checks if the subtraction is possible (M >= S).
%       If so, it calculates the amount `K` to slide both numbers, performs the
%       adjustment, and then executes the final, simpler subtraction. It
%       traces the entire execution.
%
%       @param M The Minuend.
%       @param S The Subtrahend.
%       @param FinalResult The resulting difference (M - S). If S > M, this
%       will be the atom `'error'`.
%       @param History A list of `step/5` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_sliding(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        (S > 0, S mod Base =\= 0 -> TB is ((S // Base) + 1) * Base ; TB is S),
        InitialState = state(q_init_K, 0, 0, 0, TB, S, M, S),
        InitialHistoryEntry = step(q_start, 0, 0, 0, 'Start.'),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, _, M_adj, S_adj, _)) -> FinalResult is M_adj - S_adj ; FinalResult = 'error')
    ).

% run/4 is the main recursive loop of the state machine.
run(state(q_accept, K, M_adj, S_adj, _, _, _, _), _, Acc, FinalHistory) :-
    Result is M_adj - S_adj,
    format(string(Interpretation), 'Perform Subtraction: ~w - ~w = ~w.', [M_adj, S_adj, Result]),
    HistoryEntry = step(q_accept, K, M_adj, S_adj, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, K, M_adj, S_adj, _, _, _, _),
    HistoryEntry = step(Name, K, M_adj, S_adj, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% transition/4 defines the logic for moving from one state to the next.

% From q_init_K, determine the amount K needed to slide S to a multiple of 10.
transition(state(q_init_K, _, _, _, TB, _, M, S), _, state(q_loop_K, 0, 0, 0, TB, S, M, S), Interp) :-
    format(string(Interp), 'Initializing K calculation: Counting from ~w to ~w.', [S, TB]).

% Loop in q_loop_K to count up from S to the target base, calculating K.
transition(state(q_loop_K, K, M_adj, S_adj, TB, TC, M, S), _, state(q_loop_K, NewK, M_adj, S_adj, TB, NewTC, M, S), Interp) :-
    TC < TB,
    NewTC is TC + 1,
    NewK is K + 1,
    format(string(Interp), 'Counting Up: ~w, K=~w', [NewTC, NewK]).
% Once K is found, transition to q_adjust to apply the slide.
transition(state(q_loop_K, K, _, _, TB, TC, M, S), _, state(q_adjust, K, 0, 0, TB, TC, M, S), Interp) :-
    TC >= TB,
    format(string(Interp), 'K needed to reach base is ~w.', [K]).

% In q_adjust, "slide" both M and S by adding K.
transition(state(q_adjust, K, _, _, _, _, M, S), _, state(q_subtract, K, M_adj, S_adj, 0, 0, M, S), Interp) :-
    S_adj is S + K,
    M_adj is M + K,
    format(string(Interp), 'Sliding both by +~w. New problem: ~w - ~w.', [K, M_adj, S_adj]).

% In q_subtract, the new problem is set up. Proceed to accept to perform the final calculation.
transition(state(q_subtract, K, M_adj, S_adj, _, _, _, _), _, state(q_accept, K, M_adj, S_adj, 0, 0, 0, 0), 'Proceed to accept.').
