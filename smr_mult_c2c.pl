% SWI-Prolog code for the 'Coordinating Two Counts' (C2C) multiplication strategy.

:- module(smr_mult_c2c,
          [ run_c2c/4 % N, S, FinalTotal, History
          ]).

:- use_module(library(lists)).

% State: state(Name, G_GroupsDone, I_ItemInGroup, T_Total, N_NumGroups, S_GroupSize)
% History: step(Name, G, I, T, Interpretation)

run_c2c(N, S, FinalTotal, History) :-
    InitialState = state(q_init, 0, 0, 0, N, S),

    run(InitialState, [], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, _, _, FinalTotal, _)) -> true ; FinalTotal = 'error').

run(state(q_accept, _, _, T, _, _), Acc, FinalHistory) :-
    format(string(Interpretation), 'All groups counted. Result = ~w.', [T]),
    HistoryEntry = step(q_accept, 0, 0, T, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Acc, FinalHistory) :-
    transition(CurrentState, NextState, Interpretation),
    CurrentState = state(Name, G, I, T, _, _),
    HistoryEntry = step(Name, G, I, T, Interpretation),
    run(NextState, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init, G, I, T, N, S), state(q_check_G, G, I, T, N, S), Interp) :-
    format(string(Interp), 'Inputs: ~w groups of ~w. Initialize counters.', [N, S]).

transition(state(q_check_G, G, I, T, N, S), state(q_count_items, G, I, T, N, S), Interp) :-
    G < N,
    G1 is G + 1,
    format(string(Interp), 'G < N. Starting Group ~w.', [G1]).
transition(state(q_check_G, N, _, T, N, S), state(q_accept, N, 0, T, N, S), 'G = N. All groups counted.').

transition(state(q_count_items, G, I, T, N, S), state(q_count_items, G, NewI, NewT, N, S), Interp) :-
    I < S,
    NewI is I + 1,
    NewT is T + 1,
    G1 is G + 1,
    format(string(Interp), 'Count: ~w. (Item ~w in Group ~w).', [NewT, NewI, G1]).
transition(state(q_count_items, G, S, T, N, S), state(q_next_group, G, S, T, N, S), Interp) :-
    G1 is G + 1,
    format(string(Interp), 'Group ~w finished.', [G1]).

transition(state(q_next_group, G, _, T, N, S), state(q_check_G, NewG, 0, T, N, S), 'Increment G. Reset I.') :-
    NewG is G + 1.
