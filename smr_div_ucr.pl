% SWI-Prolog code for the 'Using Commutative Reasoning' division strategy.

:- module(smr_div_ucr,
          [ run_ucr/4 % E, G, FinalQuotient, History
          ]).

:- use_module(library(lists)).

% State: state(Name, T_Accumulated, Q_PerGroup, E_Total, G_Groups)
% History: step(Name, T, Q, Interpretation)

run_ucr(E, G, FinalQuotient, History) :-
    InitialState = state(q_start, 0, 0, E, G),

    run(InitialState, [], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, _, FinalQuotient, _)) -> true ; FinalQuotient = 'error').

run(state(q_accept, _, Q, _, _), Acc, FinalHistory) :-
    format(string(Interpretation), 'Total reached. Problem solved. Output Q=~w.', [Q]),
    HistoryEntry = step(q_accept, 0, Q, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Acc, FinalHistory) :-
    transition(CurrentState, NextState, Interpretation),
    CurrentState = state(Name, T, Q, _, _),
    HistoryEntry = step(Name, T, Q, Interpretation),
    run(NextState, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_start, T, Q, E, G), state(q_initialize, T, Q, E, G),
           'Identify total items and number of groups.').

transition(state(q_initialize, T, Q, E, G), state(q_iterate, T, Q, E, G),
           'Initialize distribution total and count per group.').

transition(state(q_iterate, T, Q, E, G), state(q_check, NewT, NewQ, E, G), Interp) :-
    NewT is T + G,
    NewQ is Q + 1,
    format(string(Interp), 'Distribute round ~w. Total distributed: ~w.', [NewQ, NewT]).

transition(state(q_check, T, Q, E, G), state(q_iterate, T, Q, E, G), Interp) :-
    T < E,
    format(string(Interp), 'Check: T (~w) < E (~w); continue distributing.', [T, E]).
transition(state(q_check, E, Q, E, G), state(q_accept, E, Q, E, G), Interp) :-
    format(string(Interp), 'Check: T (~w) == E (~w); total reached.', [E, E]).
transition(state(q_check, T, _, E, G), state(q_error, T, 0, E, G), Interp) :-
    T > E,
    format(string(Interp), 'Error: Accumulated total (~w) exceeded E (~w).', [T, E]).
