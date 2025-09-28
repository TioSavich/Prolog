% SWI-Prolog code for Subtraction Chunking Strategy A (Backwards by Part).

:- module(sar_sub_chunking_a,
          [ run_chunking_a/4 % M, S, FinalResult, History
          ]).

:- use_module(library(lists)).
:- use_module(library(clpfd)). % For log/2

% State: state(Name, CurrentValue, S_Remaining, Chunk)
% History: step(Name, CV, S_Rem, Chunk, Interpretation)

run_chunking_a(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        InitialState = state(q_init, M, S, 0),
        InitialHistoryEntry = step(q_start, 0, 0, 0, 'Start: Initialize.'),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, CV, _, _, _)) -> FinalResult = CV ; FinalResult = 'error')
    ).

run(state(q_accept, CV, 0, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'S fully subtracted. Result=~w.', [CV]),
    HistoryEntry = step(q_accept, CV, 0, 0, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, CV, S_Rem, Chunk),
    HistoryEntry = step(Name, CV, S_Rem, Chunk, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init, M, S, _), _, state(q_identify_chunk, M, S, 0), Interp) :-
    format(string(Interp), 'Set CurrentValue=~w. S_Remaining=~w.', [M, S]).

transition(state(q_identify_chunk, CV, S_Rem, _), Base, state(q_subtract_chunk, CV, S_Rem, Chunk), Interp) :-
    S_Rem > 0,
    Power is floor(log(S_Rem) / log(Base)),
    PowerValue is Base^Power,
    Chunk is floor(S_Rem / PowerValue) * PowerValue,
    format(string(Interp), 'Identified chunk to subtract: ~w.', [Chunk]).
transition(state(q_identify_chunk, CV, 0, _), _, state(q_accept, CV, 0, 0),
           'S fully subtracted.').

transition(state(q_subtract_chunk, CV, S_Rem, Chunk), _, state(q_identify_chunk, NewCV, NewSRem, 0), Interp) :-
    NewCV is CV - Chunk,
    NewSRem is S_Rem - Chunk,
    format(string(Interp), 'Subtracted ~w. New Value=~w.', [Chunk, NewCV]).
