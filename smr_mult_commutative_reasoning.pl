% SWI-Prolog code for the 'Commutative Reasoning' multiplication strategy.

:- module(smr_mult_commutative_reasoning,
          [ run_commutative_mult/4 % A, B, FinalTotal, History
          ]).

:- use_module(library(lists)).

% State: state(Name, Gs, Items, Total, Counter)
% History: step(Name, Gs, Items, Total, Interpretation)

run_commutative_mult(A, B, FinalTotal, History) :-
    Groups = A,
    Items = B,
    InitialState = state(q_init_calc, Groups, Items, 0, Groups),
    InitialHistoryEntry = step(q_start, 0, 0, 0, 'Start'),

    run(InitialState, [InitialHistoryEntry], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, _, _, Total, _)) -> FinalTotal = Total ; FinalTotal = 'error').

run(state(q_accept, _, _, Total, _), Acc, FinalHistory) :-
    format(string(Interpretation), 'Calculation complete. Result = ~w.', [Total]),
    HistoryEntry = step(q_accept, 0, 0, Total, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Acc, FinalHistory) :-
    transition(CurrentState, NextState, Interpretation),
    CurrentState = state(Name, Gs, Items, Total, _),
    HistoryEntry = step(Name, Gs, Items, Total, Interpretation),
    run(NextState, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init_calc, Gs, Items, _, _), state(q_loop_calc, Gs, Items, 0, Gs),
           'Initializing iterative calculation.').

transition(state(q_loop_calc, Gs, Items, Total, Counter), state(q_loop_calc, Gs, Items, NewTotal, NewCounter), Interp) :-
    Counter > 0,
    NewTotal is Total + Items,
    NewCounter is Counter - 1,
    format(string(Interp), 'Iterate: Added ~w. Total = ~w.', [Items, NewTotal]).
transition(state(q_loop_calc, _, _, Total, 0), state(q_accept, 0, 0, Total, 0),
           'Calculation complete.').
