/** <module> Student Multiplication Strategy: Distributive Reasoning (DR)
 *
 * This module implements a multiplication strategy based on the distributive
 * property of multiplication over addition, modeled as a finite state machine.
 * It solves `N * S` by breaking `S` into two easier parts (`S1` and `S2`).
 *
 * The process is as follows:
 * 1.  Split the group size `S` into two smaller, more manageable parts,
 *     `S1` and `S2`, using a simple heuristic. For example, 7 might be
 *     split into 2 + 5.
 * 2.  Calculate the first partial product, `P1 = N * S1`, using repeated addition.
 * 3.  Calculate the second partial product, `P2 = N * S2`, also using repeated addition.
 * 4.  Sum the two partial products to get the final answer: `Total = P1 + P2`.
 *     This demonstrates the distributive property: `N * (S1 + S2) = (N * S1) + (N * S2)`.
 *
 * The state is represented by the term:
 * `state(Name, S1, S2, P1, P2, Total, Counter, N_Groups, S_Size)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, S1, S2, P1, P2, Total, Interpretation)`
 *
 * 
 * 
 */
:- module(smr_mult_dr,
          [ run_dr/4
          ]).

:- use_module(library(lists)).

%!      run_dr(+N:integer, +S:integer, -FinalTotal:integer, -History:list) is det.
%
%       Executes the 'Distributive Reasoning' multiplication strategy for N * S.
%
%       This predicate initializes and runs a state machine that models the DR
%       strategy. It heuristically splits the multiplier `S` into two parts,
%       calculates the partial product for each part via repeated addition, and
%       then sums the partial products. It traces the entire execution.
%
%       @param N The number of groups.
%       @param S The size of each group (this is the number that will be split).
%       @param FinalTotal The resulting product of N * S.
%       @param History A list of `step/7` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_dr(N, S, FinalTotal, History) :-
    Base = 10,
    InitialState = state(q_init, 0, 0, 0, 0, 0, 0, N, S),

    run(InitialState, Base, [], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, _, _, _, _, FinalTotal, _)) -> true ; FinalTotal = 'error').

% run/4 is the main recursive loop of the state machine.
run(state(q_accept, _, _, P1, P2, Total, _, _, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Summing partials: ~w + ~w = ~w.', [P1, P2, Total]),
    HistoryEntry = step(q_accept, 0, 0, P1, P2, Total, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, S1, S2, P1, P2, Total, _, _, _),
    HistoryEntry = step(Name, S1, S2, P1, P2, Total, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% transition/4 defines the logic for moving from one state to the next.

% From q_init, proceed to split the group size S.
transition(state(q_init, _, _, _, _, _, _, N, S), _, state(q_split, 0, 0, 0, 0, 0, 0, N, S), Interp) :-
    format(string(Interp), 'Inputs: ~w x ~w.', [N, S]).

% In q_split, split S into two parts, S1 and S2, using a heuristic.
transition(state(q_split, _, _, P1, P2, T, C, N, S), Base, state(q_init_P1, S1, S2, P1, P2, T, C, N, S), Interp) :-
    heuristic_split(S, Base, S1, S2),
    (S2 > 0 -> format(string(Interp), 'Split S (~w) into ~w + ~w.', [S, S1, S2])
    ; format(string(Interp), 'S (~w) is easy. No split needed.', [S])).

% In q_init_P1, prepare to calculate the first partial product (N * S1).
transition(state(q_init_P1, S1, S2, _, P2, T, _, N, S), _, state(q_loop_P1, S1, S2, 0, P2, T, N, N, S), Interp) :-
    format(string(Interp), 'Initializing calculation of P1 (~w x ~w).', [N, S1]).

% In q_loop_P1, calculate P1 using repeated addition.
transition(state(q_loop_P1, S1, S2, P1, P2, T, C, N, S), _, state(q_loop_P1, S1, S2, NewP1, P2, T, NewC, N, S), Interp) :-
    C > 0,
    NewP1 is P1 + S1,
    NewC is C - 1,
    format(string(Interp), 'Iterate P1: Added ~w. P1 = ~w.', [S1, NewP1]).
% After P1 is calculated, decide whether to calculate P2 or just sum.
transition(state(q_loop_P1, S1, 0, P1, _, _, 0, N, S), _, state(q_sum, S1, 0, P1, 0, 0, 0, N, S), Interp) :-
    format(string(Interp), 'P1 complete. P1 = ~w.', [P1]).
transition(state(q_loop_P1, S1, S2, P1, _, _, 0, N, S), _, state(q_init_P2, S1, S2, P1, 0, 0, 0, N, S), Interp) :-
    S2 > 0,
    format(string(Interp), 'P1 complete. P1 = ~w.', [P1]).

% In q_init_P2, prepare to calculate the second partial product (N * S2).
transition(state(q_init_P2, S1, S2, P1, _, T, _, N, S), _, state(q_loop_P2, S1, S2, P1, 0, T, N, N, S), Interp) :-
    format(string(Interp), 'Initializing calculation of P2 (~w x ~w).', [N, S2]).

% In q_loop_P2, calculate P2 using repeated addition.
transition(state(q_loop_P2, S1, S2, P1, P2, T, C, N, S), _, state(q_loop_P2, S1, S2, P1, NewP2, T, NewC, N, S), Interp) :-
    C > 0,
    NewP2 is P2 + S2,
    NewC is C - 1,
    format(string(Interp), 'Iterate P2: Added ~w. P2 = ~w.', [S2, NewP2]).
transition(state(q_loop_P2, S1, S2, P1, P2, _, 0, N, S), _, state(q_sum, S1, S2, P1, P2, 0, 0, N, S), Interp) :-
    format(string(Interp), 'P2 complete. P2 = ~w.', [P2]).

% In q_sum, add the partial products to get the final total.
transition(state(q_sum, _, _, P1, P2, _, _, N, S), _, state(q_accept, 0, 0, P1, P2, Total, 0, N, S), 'Summing partials.') :-
    Total is P1 + P2.

% heuristic_split/4 is a helper to split a number S into two parts, S1 and S2.
% It uses a simple set of rules to find an "easy" part to split off.
heuristic_split(Value, Base, S1, S2) :-
    (Value > Base -> S1 = Base, S2 is Value - Base ;
    (Base mod 2 =:= 0, Value > Base / 2 -> S1 is Base / 2, S2 is Value - S1 ;
    (Value > 2 -> S1 = 2, S2 is Value - 2 ;
    (Value > 1 -> S1 = 1, S2 is Value - 1 ;
    S1 = Value, S2 = 0)))).
