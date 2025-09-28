% SWI-Prolog code for Kevin's Double Rounding subtraction strategy.

:- module(sar_sub_rounding,
          [ run_sub_rounding/4 % M, S, FinalResult, History
          ]).

:- use_module(library(lists)).

% State: state(Name, K_M, K_S, TempResult, K_S_Rem, Chunk, M, S, MR, SR)
% History: step(Name, K_M, K_S, TempResult, K_S_Rem, Interpretation)

run_sub_rounding(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        InitialState = state(q_start, 0, 0, 0, 0, 0, M, S, 0, 0),
        InitialHistoryEntry = step(q_start, 0, 0, 0, 0, 'Start.'),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, _, _, TR, _, _)) -> FinalResult = TR ; FinalResult = 'error')
    ).

run(state(q_accept, KM, KS, TR, 0, _, _, _, _, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Adjustment for S complete. Final Result = ~w.', [TR]),
    HistoryEntry = step(q_accept, KM, KS, TR, 0, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, KM, KS, TR, KSR, _, _, _, _, _),
    HistoryEntry = step(Name, KM, KS, TR, KSR, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_start, _, _, _, _, _, M, S, _, _), _, state(q_round_M, 0, 0, 0, 0, 0, M, S, 0, 0), 'Proceed to round M.').

transition(state(q_round_M, _, _, _, _, _, M, S, _, _), Base, state(q_round_S, KM, 0, 0, 0, 0, M, S, MR, 0), Interp) :-
    KM is M mod Base,
    MR is M - KM,
    format(string(Interp), 'Round M down: ~w -> ~w. (K_M = ~w).', [M, MR, KM]).

transition(state(q_round_S, KM, _, _, _, _, M, S, MR, _), Base, state(q_subtract, KM, KS, 0, 0, 0, M, S, MR, SR), Interp) :-
    KS is S mod Base,
    SR is S - KS,
    format(string(Interp), 'Round S down: ~w -> ~w. (K_S = ~w).', [S, SR, KS]).

transition(state(q_subtract, KM, KS, _, _, _, M, S, MR, SR), _, state(q_adjust_M, KM, KS, TR, 0, 0, M, S, MR, SR), Interp) :-
    TR is MR - SR,
    format(string(Interp), 'Intermediate Subtraction: ~w - ~w = ~w.', [MR, SR, TR]).

transition(state(q_adjust_M, KM, KS, TR, _, _, M, S, MR, SR), _, state(q_init_adjust_S, KM, KS, NewTR, 0, 0, M, S, MR, SR), Interp) :-
    NewTR is TR + KM,
    format(string(Interp), 'Adjust for M (Add K_M): ~w + ~w = ~w.', [TR, KM, NewTR]).

transition(state(q_init_adjust_S, KM, KS, TR, _, _, M, S, MR, SR), _, state(q_loop_adjust_S, KM, KS, TR, KS, 0, M, S, MR, SR), Interp) :-
    format(string(Interp), 'Begin Adjust for S (Subtract K_S): Need to subtract ~w.', [KS]).

transition(state(q_loop_adjust_S, KM, KS, TR, 0, _, M, S, MR, SR), _, state(q_accept, KM, KS, TR, 0, 0, M, S, MR, SR), 'Adjustment for S complete.').
transition(state(q_loop_adjust_S, KM, KS, TR, KSR, _, M, S, MR, SR), Base, state(q_loop_adjust_S, KM, KS, NewTR, NewKSR, Chunk, M, S, MR, SR), Interp) :-
    KSR > 0,
    K_to_prev_base is TR mod Base,
    (K_to_prev_base > 0, KSR >= K_to_prev_base -> Chunk = K_to_prev_base ; Chunk = KSR),
    NewTR is TR - Chunk,
    NewKSR is KSR - Chunk,
    format(string(Interp), 'Chunking Adjustment: ~w - ~w = ~w.', [TR, Chunk, NewTR]).
