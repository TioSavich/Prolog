% SWI-Prolog code for Subtraction Chunking Strategy C (Backwards to Part).

:- module(sar_sub_chunking_c,
          [ run_chunking_c/4 % M, S, FinalResult, History
          ]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

% State: state(Name, CV, Dist, K, TargetBase, InternalTemp, S_target)
% History: step(Name, CV, Dist, K, Interpretation)

run_chunking_c(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        InitialState = state(q_init, M, 0, 0, 0, 0, S),
        InitialHistoryEntry = step(q_start, 0, 0, 0, 'Start: Initialize.'),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, _, Dist, _, _)) -> FinalResult = Dist ; FinalResult = 'error')
    ).

run(state(q_accept, _, Dist, _, _, _, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Target reached. Result (Distance)=~w.', [Dist]),
    HistoryEntry = step(q_accept, 0, Dist, 0, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, CV, Dist, K, _, _, _),
    HistoryEntry = step(Name, CV, Dist, K, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init, M, _, _, _, _, S), _, state(q_check_status, M, 0, 0, 0, 0, S), Interp) :-
    format(string(Interp), 'Start at M (~w). Target is S (~w).', [M, S]).

transition(state(q_check_status, CV, Dist, _, _, _, S), _, state(q_init_K, CV, Dist, 0, 0, CV, S), 'Need to subtract more.') :-
    CV > S.
transition(state(q_check_status, S, Dist, _, _, _, S), _, state(q_accept, S, Dist, 0, 0, 0, S), 'Target reached.').

transition(state(q_init_K, CV, D, K, _, IT, S), Base, state(q_loop_K, CV, D, K, TB, IT, S), Interp) :-
    find_target_base_back(CV, S, Base, 1, TB),
    format(string(Interp), 'Calculating K: Counting back from ~w to ~w.', [CV, TB]).

transition(state(q_loop_K, CV, D, K, TB, IT, S), _, state(q_loop_K, CV, D, NewK, TB, NewIT, S), _) :-
    IT > TB,
    NewIT is IT - 1,
    NewK is K + 1.
transition(state(q_loop_K, CV, D, K, TB, IT, S), _, state(q_sub_chunk, CV, D, K, TB, IT, S), _) :-
    IT =< TB.

transition(state(q_sub_chunk, CV, D, K, _, _, S), Base, state(q_check_status, NewCV, NewD, 0, 0, 0, S), Interp) :-
    Remaining is CV - S,
    (K > 0, K =< Remaining ->
        Chunk = K,
        format(string(Interp), 'Subtract strategic chunk (-~w) to reach base.', [Chunk])
    ;
        (Remaining > 0 ->
            Power is floor(log(Remaining) / log(Base)),
            PowerValue is Base^Power,
            C is floor(Remaining / PowerValue) * PowerValue,
            (C > 0 -> Chunk = C ; Chunk = Remaining),
            format(string(Interp), 'Subtract large/remaining chunk (-~w).', [Chunk])
        )
    ),
    NewCV is CV - Chunk,
    NewD is D + Chunk.

% find_target_base_back helper
find_target_base_back(CV, S, Base, Power, TargetBase) :-
    BasePower is Base^Power,
    (CV mod BasePower =\= 0 ->
        TargetBase is floor(CV / BasePower) * BasePower
    ;
        (BasePower > CV -> % a bit different from B
            TargetBase = CV
        ;
            NewPower is Power + 1,
            find_target_base_back(CV, S, Base, NewPower, TargetBase)
        )
    ).
