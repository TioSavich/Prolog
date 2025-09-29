/** <module> Student Subtraction Strategy: Counting On By Bases and Ones (Missing Addend)
 *
 * This module implements the 'Counting On by Bases and then Ones' (COBO)
 * strategy for subtraction, framed as a "missing addend" problem. It is
 * modeled as a finite state machine. It solves `M - S` by figuring out
 * what number needs to be added to `S` to reach `M`.
 *
 * The process is as follows:
 * 1. Start at the subtrahend (S). The goal is to reach the minuend (M).
 * 2. Count up from S by adding bases (tens) as many times as possible without
 *    exceeding M. The amount added is tracked as `Distance`.
 * 3. Once adding another base would overshoot M, switch to counting up by ones.
 * 4. Continue counting up by ones until M is reached.
 * 5. The total `Distance` accumulated is the result of the subtraction.
 * 6. The strategy fails if S > M.
 *
 * The state of the automaton is represented by the term:
 * `state(Name, CurrentValue, Distance, Target)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, CurrentValue, Distance, Interpretation)`
 *
 * 
 * 
 */
:- module(sar_sub_cobo_missing_addend,
          [ run_cobo_ma/4
          ]).

:- use_module(library(lists)).

%!      run_cobo_ma(+M:integer, +S:integer, -FinalResult:integer, -History:list) is det.
%
%       Executes the 'Counting On by Bases and Ones' (Missing Addend) subtraction
%       strategy for M - S.
%
%       This predicate initializes and runs a state machine that models the
%       COBO "missing addend" strategy. It first checks if the subtraction is
%       possible (M >= S). If so, it finds the difference by counting up from
%       S to M, first by tens and then by ones. The total amount counted up
%       is the result. It traces the entire execution.
%
%       @param M The Minuend, the target number to count up to.
%       @param S The Subtrahend, the number to start counting from.
%       @param FinalResult The resulting difference (M - S). If S > M, this
%       will be the atom `'error'`.
%       @param History A list of `step/4` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_cobo_ma(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        InitialState = state(q_init, S, 0, M),
        format(string(InitialInterpretation), 'Initialize at S (~w). Target is M (~w).', [S, M]),
        InitialHistoryEntry = step(q_start, 0, 0, InitialInterpretation),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, _, Dist, _)) -> FinalResult = Dist ; FinalResult = 'error')
    ).

% run/4 is the main recursive loop of the state machine.
run(state(q_accept, CV, Dist, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Target reached. Result (Distance) = ~w.', [Dist]),
    HistoryEntry = step(q_accept, CV, Dist, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, CV, Dist, _),
    HistoryEntry = step(Name, CV, Dist, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% transition/4 defines the logic for moving from one state to the next.

% From q_init, proceed to add bases (tens).
transition(state(q_init, CV, Dist, T), _, state(q_add_bases, CV, Dist, T),
           'Proceed to add bases.').

% Loop in q_add_bases, counting on by one base (10) at a time, as long as it doesn't overshoot the target.
transition(state(q_add_bases, CV, Dist, T), Base, state(q_add_bases, NewCV, NewDist, T), Interp) :-
    CV + Base =< T,
    NewCV is CV + Base,
    NewDist is Dist + Base,
    format(string(Interp), 'Count on by base (+~w). New Value=~w.', [Base, NewCV]).
% When adding the next base would overshoot, transition to adding ones.
transition(state(q_add_bases, CV, Dist, T), Base, state(q_add_ones, CV, Dist, T),
           'Next base overshoots target. Switching to ones.') :-
    CV + Base > T.

% Loop in q_add_ones, counting on by one at a time until the target is reached.
transition(state(q_add_ones, CV, Dist, T), _, state(q_add_ones, NewCV, NewDist, T), Interp) :-
    CV < T,
    NewCV is CV + 1,
    NewDist is Dist + 1,
    format(string(Interp), 'Count on by one (+1). New Value=~w.', [NewCV]).
% When the target is reached, transition to the final accept state.
transition(state(q_add_ones, T, Dist, T), _, state(q_accept, T, Dist, T),
           'Target reached.') :-
    true.
