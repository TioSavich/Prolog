% SWI-Prolog code for Subtraction Chunking Strategy B (Forwards from Part).

:- module(sar_sub_chunking_b,
          [ run_chunking_b/4 % M, S, FinalResult, History
          ]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

% State: state(Name, CV, Dist, K, TargetBase, InternalTemp)
% History: step(Name, CV, Dist, K, Interpretation)

run_chunking_b(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        InitialState = state(q_init, S, 0, 0, 0, 0, M),
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
transition(state(q_init, S, _, _, _, _, M), _, state(q_check_status, S, 0, 0, 0, 0, M), Interp) :-
    format(string(Interp), 'Start at S (~w). Target is M (~w).', [S, M]).

transition(state(q_check_status, CV, Dist, _, _, _, M), _, state(q_init_K, CV, Dist, 0, 0, CV, M), 'Need to add more.') :-
    CV < M.
transition(state(q_check_status, M, Dist, _, _, _, M), _, state(q_accept, M, Dist, 0, 0, 0, M), 'Target reached.').

transition(state(q_init_K, CV, D, K, _, IT, M), Base, state(q_loop_K, CV, D, K, TB, IT, M), Interp) :-
    find_target_base(CV, M, Base, 1, TB),
    format(string(Interp), 'Calculating K: Counting from ~w to ~w.', [CV, TB]).

transition(state(q_loop_K, CV, D, K, TB, IT, M), _, state(q_loop_K, CV, D, NewK, TB, NewIT, M), _) :-
    IT < TB,
    NewIT is IT + 1,
    NewK is K + 1.
transition(state(q_loop_K, CV, D, K, TB, IT, M), _, state(q_add_chunk, CV, D, K, TB, IT, M), _) :-
    IT >= TB.

transition(state(q_add_chunk, CV, D, K, TB, IT, M), Base, state(q_check_status, NewCV, NewD, 0, 0, 0, M), Interp) :-
    Remaining is M - CV,
    (K > 0, K =< Remaining ->
        Chunk = K,
        format(string(Interp), 'Add strategic chunk (+~w) to reach base.', [Chunk])
    ;
        (Remaining > 0 ->
            Power is floor(log(Remaining) / log(Base)),
            PowerValue is Base^Power,
            C is floor(Remaining / PowerValue) * PowerValue,
            (C > 0 -> Chunk = C ; Chunk = Remaining),
            format(string(Interp), 'Add large/remaining chunk (+~w).', [Chunk])
        )
    ),
    NewCV is CV + Chunk,
    NewD is D + Chunk.

% find_target_base helper
find_target_base(CV, M, Base, Power, TargetBase) :-
    BasePower is Base^Power,
    (CV mod BasePower =\= 0 ->
        TargetBase is (floor(CV / BasePower) + 1) * BasePower
    ;
        (BasePower > M ->
            TargetBase = CV % or some other logic, python is a bit ambiguous here
        ;
            NewPower is Power + 1,
            find_target_base(CV, M, Base, NewPower, TargetBase)
        )
    ).
