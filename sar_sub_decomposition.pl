% SWI-Prolog code for the 'Decomposition' (Borrowing) strategy for subtraction.

:- module(sar_sub_decomposition,
          [ run_decomposition/4 % M, S, FinalResult, History
          ]).

:- use_module(library(lists)).

% State: state(StateName, R_T, R_O, S_T, S_O)
% History: step(StateName, R_T, R_O, Interpretation)

run_decomposition(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        % Error condition as specified in the Python __init__
        History = [step(q_error, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        % Initial state setup from execute_q_init
        S_T is S // Base, S_O is S mod Base,
        M_T is M // Base, M_O is M mod Base,

        InitialState = state(q_init, M_T, M_O, S_T, S_O),

        format(string(InitialInterpretation), 'Inputs: M=~w, S=~w. Decompose M (~wT+~wO) and S (~wT+~wO).', [M, S, M_T, M_O, S_T, S_O]),
        InitialHistoryEntry = step(q_start, M_T, M_O, InitialInterpretation),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, RT, RO, _)) ->
            FinalResult is RT * Base + RO
        ;
            FinalResult = 'computation_error'
        )
    ).

run(state(q_accept, R_T, R_O, _, _), Base, AccHistory, FinalHistory) :-
    Result is R_T * Base + R_O,
    format(string(Interpretation), 'Accept. Final Result: ~w.', [Result]),
    HistoryEntry = step(q_accept, R_T, R_O, Interpretation),
    FinalHistory = [HistoryEntry | AccHistory].

run(CurrentState, Base, AccHistory, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, R_T, R_O, _, _),
    HistoryEntry = step(Name, R_T, R_O, Interpretation),
    run(NextState, Base, [HistoryEntry | AccHistory], FinalHistory).

% Transitions
transition(state(q_init, R_T, R_O, S_T, S_O), _Base, state(q_sub_bases, R_T, R_O, S_T, S_O),
           'Proceed to subtract bases.').

transition(state(q_sub_bases, R_T, R_O, S_T, S_O), _Base, state(q_check_ones, New_R_T, R_O, S_T, S_O), Interpretation) :-
    New_R_T is R_T - S_T,
    format(string(Interpretation), 'Subtract Bases: ~wT - ~wT = ~wT.', [R_T, S_T, New_R_T]).

transition(state(q_check_ones, R_T, R_O, S_T, S_O), _Base, state(q_sub_ones, R_T, R_O, S_T, S_O), Interpretation) :-
    R_O >= S_O,
    format(string(Interpretation), 'Sufficient Ones (~w >= ~w). Proceed.', [R_O, S_O]).

transition(state(q_check_ones, R_T, R_O, S_T, S_O), _Base, state(q_decompose, R_T, R_O, S_T, S_O), Interpretation) :-
    R_O < S_O,
    format(string(Interpretation), 'Insufficient Ones (~w < ~w). Need decomposition.', [R_O, S_O]).

transition(state(q_decompose, R_T, R_O, S_T, S_O), Base, state(q_sub_ones, New_R_T, New_R_O, S_T, S_O), Interpretation) :-
    R_T > 0,
    New_R_T is R_T - 1,
    New_R_O is R_O + Base,
    format(string(Interpretation), 'Decomposed 1 Ten. New state: ~wT, ~wO.', [New_R_T, New_R_O]).

transition(state(q_sub_ones, R_T, R_O, S_T, S_O), _Base, state(q_accept, R_T, New_R_O, S_T, S_O), Interpretation) :-
    New_R_O is R_O - S_O,
    format(string(Interpretation), 'Subtract Ones: ~wO - ~wO = ~wO.', [R_O, S_O, New_R_O]).
