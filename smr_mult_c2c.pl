/** <module> Student Multiplication Strategy: Coordinating Two Counts (C2C)
 *
 * This module implements a foundational multiplication strategy, "Coordinating
 * Two Counts" (C2C), modeled as a finite state machine. This strategy
 * represents a direct modeling approach where a student literally counts every
 * single item across all groups.
 *
 * The cognitive process involves two simultaneous counting acts:
 * 1.  Tracking the number of items counted within the current group.
 * 2.  Tracking which group is currently being counted.
 *
 * This is a direct simulation of `N * S` where the total is found by
 * counting `1` for each item, `S` times for each of the `N` groups.
 *
 * The state is represented by the term:
 * `state(Name, GroupsDone, ItemInGroup, Total, NumGroups, GroupSize)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, GroupsDone, ItemInGroup, Total, Interpretation)`
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(smr_mult_c2c,
          [ run_c2c/4
          ]).

:- use_module(library(lists)).

%!      run_c2c(+N:integer, +S:integer, -FinalTotal:integer, -History:list) is det.
%
%       Executes the 'Coordinating Two Counts' multiplication strategy for N * S.
%
%       This predicate initializes and runs a state machine that models the
%       C2C strategy. It simulates a student counting every item, one by one,
%       across all `N` groups of size `S`. It traces the entire execution,
%       providing a step-by-step history of the two coordinated counts.
%
%       @param N The number of groups.
%       @param S The size of each group (number of items).
%       @param FinalTotal The resulting product of N * S.
%       @param History A list of `step/5` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_c2c(N, S, FinalTotal, History) :-
    InitialState = state(q_init, 0, 0, 0, N, S),

    run(InitialState, [], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, _, _, FinalTotal, _)) -> true ; FinalTotal = 'error').

% run/3 is the main recursive loop of the state machine.
run(state(q_accept, _, _, T, _, _), Acc, FinalHistory) :-
    format(string(Interpretation), 'All groups counted. Result = ~w.', [T]),
    HistoryEntry = step(q_accept, 0, 0, T, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Acc, FinalHistory) :-
    transition(CurrentState, NextState, Interpretation),
    CurrentState = state(Name, G, I, T, _, _),
    HistoryEntry = step(Name, G, I, T, Interpretation),
    run(NextState, [HistoryEntry | Acc], FinalHistory).

% transition/3 defines the logic for moving from one state to the next.

% From q_init, proceed to check the group counter.
transition(state(q_init, G, I, T, N, S), state(q_check_G, G, I, T, N, S), Interp) :-
    format(string(Interp), 'Inputs: ~w groups of ~w. Initialize counters.', [N, S]).

% In q_check_G, decide whether to count another group or finish.
transition(state(q_check_G, G, I, T, N, S), state(q_count_items, G, I, T, N, S), Interp) :-
    G < N,
    G1 is G + 1,
    format(string(Interp), 'G < N. Starting Group ~w.', [G1]).
transition(state(q_check_G, N, _, T, N, S), state(q_accept, N, 0, T, N, S), 'G = N. All groups counted.').

% In q_count_items, count one item and increment the total. Loop until the group is full.
transition(state(q_count_items, G, I, T, N, S), state(q_count_items, G, NewI, NewT, N, S), Interp) :-
    I < S,
    NewI is I + 1,
    NewT is T + 1,
    G1 is G + 1,
    format(string(Interp), 'Count: ~w. (Item ~w in Group ~w).', [NewT, NewI, G1]).
% When the current group is fully counted, move to the next group.
transition(state(q_count_items, G, S, T, N, S), state(q_next_group, G, S, T, N, S), Interp) :-
    G1 is G + 1,
    format(string(Interp), 'Group ~w finished.', [G1]).

% In q_next_group, increment the group counter and reset the item counter, then loop back.
transition(state(q_next_group, G, _, T, N, S), state(q_check_G, NewG, 0, T, N, S), 'Increment G. Reset I.') :-
    NewG is G + 1.
