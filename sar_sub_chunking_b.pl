/** <module> Student Subtraction Strategy: Chunking Forwards from Part (Missing Addend)
 *
 * This module implements a "counting up" or "missing addend" strategy for
 * subtraction (M - S), modeled as a finite state machine. It solves the
 * problem by calculating what needs to be added to S to reach M.
 *
 * The process is as follows:
 * 1. Start at the subtrahend (S). The goal is to reach the minuend (M).
 * 2. Identify a "strategic" chunk to add. This could be:
 *    a. The amount `K` needed to get from the current value to the next
 *       multiple of 10 (or 100, etc.).
 *    b. If that's not suitable, the largest possible place-value chunk of the
 *       *remaining distance* to M.
 * 3. Add the selected chunk. The size of the chunk is added to a running
 *    total, `Distance`.
 * 4. Repeat until the current value reaches M. The final `Distance` is the
 *    answer to the subtraction problem.
 * 5. The strategy fails if S > M.
 *
 * The state is represented by the term:
 * `state(Name, CurrentValue, Distance, K, TargetBase, InternalTemp, Minuend)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, CurrentValue, Distance, K, Interpretation)`
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(sar_sub_chunking_b,
          [ run_chunking_b/4
          ]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

%!      run_chunking_b(+M:integer, +S:integer, -FinalResult:integer, -History:list) is det.
%
%       Executes the 'Chunking Forwards from Part' (missing addend) subtraction
%       strategy for M - S.
%
%       This predicate initializes and runs a state machine that models the
%       "counting up" process. It first checks if the subtraction is possible (M >= S).
%       If so, it calculates the difference by adding chunks to S until it reaches M.
%       The sum of these chunks is the result. It traces the entire execution,
%       providing a step-by-step history.
%
%       @param M The Minuend, the target number to count up to.
%       @param S The Subtrahend, the number to start counting from.
%       @param FinalResult The resulting difference (M - S). If S > M, this
%       will be the atom `'error'`.
%       @param History A list of `step/5` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_chunking_b(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        InitialState = state(q_init, S, 0, 0, 0, 0, M),
        InitialHistoryEntry = step(q_start, 0, 0, 0, 'Start: Initialize.'),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, _, Dist, _, _)) -> FinalResult = Dist ; FinalResult = 'error')
    ).

% run/4 is the main recursive loop of the state machine.
run(state(q_accept, _, Dist, _, _, _, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Target reached. Result (Distance)=~w.', [Dist]),
    HistoryEntry = step(q_accept, 0, Dist, 0, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, CV, Dist, K, _, _, _),
    HistoryEntry = step(Name, CV, Dist, K, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% transition/4 defines the logic for moving from one state to the next.

% From q_init, proceed to check if we are already at the target.
transition(state(q_init, S, _, _, _, _, M), _, state(q_check_status, S, 0, 0, 0, 0, M), Interp) :-
    format(string(Interp), 'Start at S (~w). Target is M (~w).', [S, M]).

% In q_check_status, decide whether to continue adding or accept the result.
transition(state(q_check_status, CV, Dist, _, _, _, M), _, state(q_init_K, CV, Dist, 0, 0, CV, M), 'Need to add more.') :-
    CV < M.
transition(state(q_check_status, M, Dist, _, _, _, M), _, state(q_accept, M, Dist, 0, 0, 0, M), 'Target reached.').

% In q_init_K, determine the next friendly base number to aim for.
transition(state(q_init_K, CV, D, K, _, IT, M), Base, state(q_loop_K, CV, D, K, TB, IT, M), Interp) :-
    find_target_base(CV, M, Base, 1, TB),
    format(string(Interp), 'Calculating K: Counting from ~w to ~w.', [CV, TB]).

% In q_loop_K, count up to the target base to find the distance K.
transition(state(q_loop_K, CV, D, K, TB, IT, M), _, state(q_loop_K, CV, D, NewK, TB, NewIT, M), _) :-
    IT < TB,
    NewIT is IT + 1,
    NewK is K + 1.
transition(state(q_loop_K, CV, D, K, TB, IT, M), _, state(q_add_chunk, CV, D, K, TB, IT, M), _) :-
    IT >= TB.

% In q_add_chunk, add a strategic chunk or a large place-value chunk.
transition(state(q_add_chunk, CV, D, K, _TB, _IT, M), Base, state(q_check_status, NewCV, NewD, 0, 0, 0, M), Interp) :-
    Remaining is M - CV,
    (K > 0, K =< Remaining ->
        Chunk = K,
        format(string(Interp), 'Add strategic chunk (+~w) to reach base.', [Chunk])
    ;
        (Remaining > 0 ->
            Power is floor(log(Remaining) / log(Base)),
            PowerValue is Base^Power,
            C is floor(Remaining / PowerValue) * PowerValue,
            (C > 0 -> Chunk = C ; Chunk = Remaining),
            format(string(Interp), 'Add large/remaining chunk (+~w).', [Chunk])
        )
    ),
    NewCV is CV + Chunk,
    NewD is D + Chunk.

% find_target_base/5 is a helper to find the next "friendly" number to aim for.
find_target_base(CV, M, Base, Power, TargetBase) :-
    BasePower is Base^Power,
    (CV mod BasePower =\= 0 ->
        TargetBase is (floor(CV / BasePower) + 1) * BasePower
    ;
        (BasePower > M ->
            TargetBase = CV
        ;
            NewPower is Power + 1,
            find_target_base(CV, M, Base, NewPower, TargetBase)
        )
    ).
