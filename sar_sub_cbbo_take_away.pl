/** <module> Student Subtraction Strategy: Counting Back By Bases and Ones (Take Away)
 *
 * This module implements the 'Counting Back by Bases and then Ones' (CBBO)
 * strategy for subtraction, often conceptualized as "taking away". It is
 * modeled as a finite state machine.
 *
 * The process is as follows:
 * 1. The subtrahend (S) is decomposed into its base-10 components (bases/tens and ones).
 * 2. Starting from the minuend (M), the strategy first "takes away" or
 *    counts back by the number of bases (tens).
 * 3. After all bases are subtracted, it counts back by the number of ones.
 * 4. The final value is the result of the subtraction.
 * 5. The strategy fails if the subtrahend is larger than the minuend.
 *
 * The state of the automaton is represented by the term:
 * `state(Name, CurrentValue, BaseCounter, OneCounter)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, CurrentValue, BaseCounter, OneCounter, Interpretation)`
 *
 * 
 * 
 */
:- module(sar_sub_cbbo_take_away,
          [ run_cbbo_ta/4
          ]).

:- use_module(library(lists)).

%!      run_cbbo_ta(+M:integer, +S:integer, -FinalResult:integer, -History:list) is det.
%
%       Executes the 'Counting Back by Bases and Ones' (Take Away) subtraction
%       strategy for M - S.
%
%       This predicate initializes and runs a state machine that models the
%       CBBO strategy. It first checks if the subtraction is possible (M >= S).
%       If so, it decomposes S and simulates the process of counting back from M,
%       first by tens and then by ones. It traces the entire execution,
%       providing a step-by-step history.
%
%       @param M The Minuend, the number to subtract from.
%       @param S The Subtrahend, the number to subtract.
%       @param FinalResult The resulting difference (M - S). If S > M, this
%       will be the atom `'error'`.
%       @param History A list of `step/5` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_cbbo_ta(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        BC is S // Base,
        OC is S mod Base,
        InitialState = state(q_init, M, BC, OC),
        format(string(InitialInterpretation), 'Initialize at M (~w). Decompose S (~w): ~w bases, ~w ones.', [M, S, BC, OC]),
        InitialHistoryEntry = step(q_start, M, 0, 0, InitialInterpretation),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, CV, _, _, _)) ->
            FinalResult = CV
        ;
            FinalResult = 'error'
        )
    ).

% run/4 is the main recursive loop of the state machine.
run(state(q_accept, CV, BC, OC), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Subtraction finished. Result (Final Position) = ~w.', [CV]),
    HistoryEntry = step(q_accept, CV, BC, OC, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, CV, BC, OC),
    HistoryEntry = step(Name, CV, BC, OC, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% transition/4 defines the logic for moving from one state to the next.

% From q_init, proceed to subtract the bases (tens).
transition(state(q_init, CV, BC, OC), _, state(q_sub_bases, CV, BC, OC),
           'Proceed to subtract bases.').

% Loop in q_sub_bases, counting back by one base (10) at a time.
transition(state(q_sub_bases, CV, BC, OC), Base, state(q_sub_bases, NewCV, NewBC, OC), Interp) :-
    BC > 0,
    NewCV is CV - Base,
    NewBC is BC - 1,
    format(string(Interp), 'Count back by base (-~w). New Value=~w.', [Base, NewCV]).
% When all bases are subtracted, transition to q_sub_ones.
transition(state(q_sub_bases, CV, 0, OC), _, state(q_sub_ones, CV, 0, OC),
           'Bases finished. Switching to ones.').

% Loop in q_sub_ones, counting back by one at a time.
transition(state(q_sub_ones, CV, BC, OC), _, state(q_sub_ones, NewCV, BC, NewOC), Interp) :-
    OC > 0,
    NewCV is CV - 1,
    NewOC is OC - 1,
    format(string(Interp), 'Count back by one (-1). New Value=~w.', [NewCV]).
% When all ones are subtracted, transition to the final accept state.
transition(state(q_sub_ones, CV, BC, 0), _, state(q_accept, CV, BC, 0),
           'Subtraction finished.').
