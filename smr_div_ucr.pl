/** <module> Student Division Strategy: Using Commutative Reasoning (Repeated Addition)
 *
 * This module implements a division strategy based on the concept of
 * commutative reasoning, modeled as a finite state machine. It solves a
 * partitive division problem (E items into G groups) by reframing it as a
 * missing factor multiplication problem: `? * G = E`.
 *
 * The process is as follows:
 * 1.  Start with an accumulated total of 0 and a quotient (items per group) of 0.
 * 2.  In each step, simulate adding one item to each of the `G` groups. This
 *     is equivalent to adding `G` to the accumulated total and `1` to the quotient.
 * 3.  Continue this process of repeated addition until the accumulated total
 *     equals the target number of items `E`.
 * 4.  The final quotient represents the number of items that were placed in
 *     each group, which is the answer to the division problem.
 * 5.  This strategy implicitly uses the commutative property by solving
 *     `E / G = ?` as `? * G = E`.
 *
 * The state is represented by the term:
 * `state(Name, Total_Accumulated, Quotient_PerGroup, E_Total, G_Groups)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, Total_Accumulated, Quotient_PerGroup, Interpretation)`
 *
 * 
 * 
 */
:- module(smr_div_ucr,
          [ run_ucr/4
          ]).

:- use_module(library(lists)).

%!      run_ucr(+E:integer, +G:integer, -FinalQuotient:integer, -History:list) is det.
%
%       Executes the 'Using Commutative Reasoning' division strategy for E / G.
%
%       This predicate initializes and runs a state machine that models the
%       process of solving a division problem by finding the missing factor
%       through repeated addition. It traces the entire execution, providing
%       a step-by-step history of how the quotient is built up.
%
%       @param E The Dividend (Total number of items).
%       @param G The Divisor (Number of groups).
%       @param FinalQuotient The result of the division (items per group).
%       @param History A list of `step/4` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_ucr(E, G, FinalQuotient, History) :-
    InitialState = state(q_start, 0, 0, E, G),

    run(InitialState, [], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, _, FinalQuotient, _)) -> true ; FinalQuotient = 'error').

% run/3 is the main recursive loop of the state machine.
run(state(q_accept, _, Q, _, _), Acc, FinalHistory) :-
    format(string(Interpretation), 'Total reached. Problem solved. Output Q=~w.', [Q]),
    HistoryEntry = step(q_accept, 0, Q, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Acc, FinalHistory) :-
    transition(CurrentState, NextState, Interpretation),
    CurrentState = state(Name, T, Q, _, _),
    HistoryEntry = step(Name, T, Q, Interpretation),
    run(NextState, [HistoryEntry | Acc], FinalHistory).

% transition/3 defines the logic for moving from one state to the next.

% From q_start, identify the problem parameters.
transition(state(q_start, T, Q, E, G), state(q_initialize, T, Q, E, G),
           'Identify total items and number of groups.').

% From q_initialize, begin the iterative process.
transition(state(q_initialize, T, Q, E, G), state(q_iterate, T, Q, E, G),
           'Initialize distribution total and count per group.').

% In q_iterate, perform one round of distribution (repeated addition).
transition(state(q_iterate, T, Q, E, G), state(q_check, NewT, NewQ, E, G), Interp) :-
    NewT is T + G,
    NewQ is Q + 1,
    format(string(Interp), 'Distribute round ~w. Total distributed: ~w.', [NewQ, NewT]).

% In q_check, compare the accumulated total to the target total.
transition(state(q_check, T, Q, E, G), state(q_iterate, T, Q, E, G), Interp) :-
    T < E,
    format(string(Interp), 'Check: T (~w) < E (~w); continue distributing.', [T, E]).
transition(state(q_check, E, Q, E, G), state(q_accept, E, Q, E, G), Interp) :-
    format(string(Interp), 'Check: T (~w) == E (~w); total reached.', [E, E]).
transition(state(q_check, T, _, E, G), state(q_error, T, 0, E, G), Interp) :-
    T > E,
    format(string(Interp), 'Error: Accumulated total (~w) exceeded E (~w).', [T, E]).
