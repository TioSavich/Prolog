% SWI-Prolog code for the 'Sliding' (Constant Difference) subtraction strategy.

:- module(sar_sub_sliding,
          [ run_sliding/4 % M, S, FinalResult, History
          ]).

:- use_module(library(lists)).

% State: state(Name, K, M_adj, S_adj, TargetBase, TempCounter, M, S)
% History: step(Name, K, M_adj, S_adj, Interpretation)

run_sliding(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        (S > 0, S mod Base =\= 0 -> TB is ((S // Base) + 1) * Base ; TB is S),
        InitialState = state(q_init_K, 0, 0, 0, TB, S, M, S),
        InitialHistoryEntry = step(q_start, 0, 0, 0, 'Start.'),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, _, M_adj, S_adj, _)) -> FinalResult is M_adj - S_adj ; FinalResult = 'error')
    ).

run(state(q_accept, K, M_adj, S_adj, _, _, _, _), _, Acc, FinalHistory) :-
    Result is M_adj - S_adj,
    format(string(Interpretation), 'Perform Subtraction: ~w - ~w = ~w.', [M_adj, S_adj, Result]),
    HistoryEntry = step(q_accept, K, M_adj, S_adj, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, K, M_adj, S_adj, _, _, _, _),
    HistoryEntry = step(Name, K, M_adj, S_adj, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init_K, _, _, _, TB, _, M, S), _, state(q_loop_K, 0, 0, 0, TB, S, M, S), Interp) :-
    format(string(Interp), 'Initializing K calculation: Counting from ~w to ~w.', [S, TB]).

transition(state(q_loop_K, K, M_adj, S_adj, TB, TC, M, S), _, state(q_loop_K, NewK, M_adj, S_adj, TB, NewTC, M, S), Interp) :-
    TC < TB,
    NewTC is TC + 1,
    NewK is K + 1,
    format(string(Interp), 'Counting Up: ~w, K=~w', [NewTC, NewK]).
transition(state(q_loop_K, K, _, _, TB, TC, M, S), _, state(q_adjust, K, 0, 0, TB, TC, M, S), Interp) :-
    TC >= TB,
    format(string(Interp), 'K needed to reach base is ~w.', [K]).

transition(state(q_adjust, K, _, _, _, _, M, S), _, state(q_subtract, K, M_adj, S_adj, 0, 0, M, S), Interp) :-
    S_adj is S + K,
    M_adj is M + K,
    format(string(Interp), 'Sliding both by +~w. New problem: ~w - ~w.', [K, M_adj, S_adj]).

transition(state(q_subtract, K, M_adj, S_adj, _, _, _, _), _, state(q_accept, K, M_adj, S_adj, 0, 0, 0, 0), 'Proceed to accept.').
