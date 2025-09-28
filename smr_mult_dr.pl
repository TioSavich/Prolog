% SWI-Prolog code for the 'Distributive Reasoning' (DR) multiplication strategy.

:- module(smr_mult_dr,
          [ run_dr/4 % N, S, FinalTotal, History
          ]).

:- use_module(library(lists)).

% State: state(Name, S1, S2, P1, P2, Total, Counter, N_Groups, S_Size)
% History: step(Name, S1, S2, P1, P2, Total, Interpretation)

run_dr(N, S, FinalTotal, History) :-
    Base = 10,
    InitialState = state(q_init, 0, 0, 0, 0, 0, 0, N, S),

    run(InitialState, Base, [], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, _, _, _, _, FinalTotal, _)) -> true ; FinalTotal = 'error').

run(state(q_accept, _, _, P1, P2, Total, _, _, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Summing partials: ~w + ~w = ~w.', [P1, P2, Total]),
    HistoryEntry = step(q_accept, 0, 0, P1, P2, Total, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, S1, S2, P1, P2, Total, _, _, _),
    HistoryEntry = step(Name, S1, S2, P1, P2, Total, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init, _, _, _, _, _, _, N, S), _, state(q_split, 0, 0, 0, 0, 0, 0, N, S), Interp) :-
    format(string(Interp), 'Inputs: ~w x ~w.', [N, S]).

transition(state(q_split, _, _, P1, P2, T, C, N, S), Base, state(q_init_P1, S1, S2, P1, P2, T, C, N, S), Interp) :-
    heuristic_split(S, Base, S1, S2),
    (S2 > 0 -> format(string(Interp), 'Split S (~w) into ~w + ~w.', [S, S1, S2])
    ; format(string(Interp), 'S (~w) is easy. No split needed.', [S])).

transition(state(q_init_P1, S1, S2, _, P2, T, _, N, S), _, state(q_loop_P1, S1, S2, 0, P2, T, N, N, S), Interp) :-
    format(string(Interp), 'Initializing calculation of P1 (~w x ~w).', [N, S1]).

transition(state(q_loop_P1, S1, S2, P1, P2, T, C, N, S), _, state(q_loop_P1, S1, S2, NewP1, P2, T, NewC, N, S), Interp) :-
    C > 0,
    NewP1 is P1 + S1,
    NewC is C - 1,
    format(string(Interp), 'Iterate P1: Added ~w. P1 = ~w.', [S1, NewP1]).
transition(state(q_loop_P1, S1, 0, P1, _, _, 0, N, S), _, state(q_sum, S1, 0, P1, 0, 0, 0, N, S), Interp) :-
    format(string(Interp), 'P1 complete. P1 = ~w.', [P1]).
transition(state(q_loop_P1, S1, S2, P1, _, _, 0, N, S), _, state(q_init_P2, S1, S2, P1, 0, 0, 0, N, S), Interp) :-
    S2 > 0,
    format(string(Interp), 'P1 complete. P1 = ~w.', [P1]).

transition(state(q_init_P2, S1, S2, P1, _, T, _, N, S), _, state(q_loop_P2, S1, S2, P1, 0, T, N, N, S), Interp) :-
    format(string(Interp), 'Initializing calculation of P2 (~w x ~w).', [N, S2]).

transition(state(q_loop_P2, S1, S2, P1, P2, T, C, N, S), _, state(q_loop_P2, S1, S2, P1, NewP2, T, NewC, N, S), Interp) :-
    C > 0,
    NewP2 is P2 + S2,
    NewC is C - 1,
    format(string(Interp), 'Iterate P2: Added ~w. P2 = ~w.', [S2, NewP2]).
transition(state(q_loop_P2, S1, S2, P1, P2, _, 0, N, S), _, state(q_sum, S1, S2, P1, P2, 0, 0, N, S), Interp) :-
    format(string(Interp), 'P2 complete. P2 = ~w.', [P2]).

transition(state(q_sum, _, _, P1, P2, _, _, N, S), _, state(q_accept, 0, 0, P1, P2, Total, 0, N, S), 'Summing partials.') :-
    Total is P1 + P2.

% Heuristic helper
heuristic_split(Value, Base, S1, S2) :-
    (Value > Base -> S1 = Base, S2 is Value - Base ;
    (Base mod 2 =:= 0, Value > Base / 2 -> S1 is Base / 2, S2 is Value - S1 ;
    (Value > 2 -> S1 = 2, S2 is Value - 2 ;
    (Value > 1 -> S1 = 1, S2 is Value - 1 ;
    S1 = Value, S2 = 0)))).
