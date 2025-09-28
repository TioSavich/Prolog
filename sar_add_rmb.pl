% SWI-Prolog code for the 'Rearranging to Make Bases' (RMB) strategy.

:- module(sar_add_rmb,
          [ run_rmb/4 % A, B, FinalResult, History
          ]).

:- use_module(library(lists)).

% State: state(Name, A, B, K, A_temp, B_temp)
% History: step(Name, A, B, K, A_temp, B_temp, Interpretation)

run_rmb(A_in, B_in, FinalResult, History) :-
    Base = 10,
    A is max(A_in, B_in),
    B is min(A_in, B_in),

    % Initial state q_calc_K
    (A mod Base =:= 0, A =\= 0 -> TargetBase is A ; TargetBase is ((A // Base) + 1) * Base),
    InitialState = state(q_calc_K, A, B, 0, A, 0, TargetBase, B), % B_initial stored for error msg

    InitialInterpretation = 'Start.',
    InitialHistoryEntry = step(q_start, A, B, 0, 0, 0, InitialInterpretation),

    run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, FinalA, FinalB, _, _, _, _)) ->
        FinalResult is FinalA + FinalB
    ;
        FinalResult = 'error'
    ).


run(state(q_accept, A, B, K, AT, BT, _, _), _, Acc, FinalHistory) :-
    Result is A + B,
    format(string(Interpretation), 'Combine rearranged numbers: ~w + ~w = ~w.', [A, B, Result]),
    HistoryEntry = step(q_accept, A, B, K, AT, BT, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, A, B, K, AT, BT, _, _),
    HistoryEntry = step(Name, A, B, K, AT, BT, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
% q_calc_K loop
transition(state(q_calc_K, A, B, K, AT, BT, TB, B_init), _, state(q_calc_K, A, B, NewK, NewAT, BT, TB, B_init), Interpretation) :-
    AT < TB,
    NewAT is AT + 1,
    NewK is K + 1,
    format(string(Interpretation), 'Count up: ~w. Distance (K): ~w.', [NewAT, NewK]).
% q_calc_K -> q_decompose_B
transition(state(q_calc_K, A, B, K, AT, BT, TB, B_init), _, state(q_decompose_B, A, B, K, AT, B, TB, B_init), Interpretation) :-
    AT >= TB,
    format(string(Interpretation), 'K needed is ~w. Start counting down K from B.', [K]).

% q_decompose_B loop
transition(state(q_decompose_B, A, B, K, AT, BT, TB, B_init), _, state(q_decompose_B, A, B, NewK, AT, NewBT, TB, B_init), Interpretation) :-
    K > 0, BT > 0,
    NewK is K - 1,
    NewBT is BT - 1,
    format(string(Interpretation), 'Transferred 1. B remainder: ~w. K remaining: ~w.', [NewBT, NewK]).
% q_decompose_B -> q_recombine
transition(state(q_decompose_B, _, _, 0, AT, BT, _, _), _, state(q_recombine, AT, BT, 0, AT, BT, 0, 0), Interpretation) :-
    format(string(Interpretation), 'Decomp Complete. New state: A=~w, B=~w.', [AT, BT]).
% q_decompose_B -> q_error
transition(state(q_decompose_B, _, _, K, _, 0, _, B_init), _, state(q_error, 0,0,0,0,0,0,0), Interpretation) :-
    K > 0,
    format(string(Interpretation), 'Strategy Failed. B (~w) is too small to provide K (~w).', [B_init, K]).

% q_recombine -> q_accept
transition(state(q_recombine, A, B, K, AT, BT, _, _), _, state(q_accept, A, B, K, AT, BT, 0, 0), 'Proceed to accept.').
