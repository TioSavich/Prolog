/** <module> Student Subtraction Strategy: Chunking Backwards by Place Value
 *
 * This module implements a "chunking" strategy for subtraction, modeled as a
 * finite state machine. The strategy involves subtracting the subtrahend (S)
 * from the minuend (M) in parts, based on place value (hundreds, tens, ones).
 *
 * The process is as follows:
 * 1. Identify the largest place-value chunk of the remaining subtrahend (S).
 *    For example, if S is 234, the first chunk is 200.
 * 2. Subtract this chunk from the current value (which starts at M).
 * 3. Repeat the process with the remainder of S. For S=234, the next chunk
 *    would be 30, then 4.
 * 4. The process ends when the entire subtrahend has been subtracted.
 * 5. The strategy fails if the subtrahend is larger than the minuend.
 *
 * The state of the automaton is represented by the term:
 * `state(Name, CurrentValue, S_Remaining, Chunk)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, CurrentValue, S_Remaining, Chunk, Interpretation)`
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(sar_sub_chunking_a,
          [ run_chunking_a/4
          ]).

:- use_module(library(lists)).
:- use_module(library(clpfd)). % For log/2

%!      run_chunking_a(+M:integer, +S:integer, -FinalResult:integer, -History:list) is det.
%
%       Executes the 'Chunking Backwards by Place Value' subtraction strategy for M - S.
%
%       This predicate initializes and runs a state machine that models the
%       chunking strategy. It first checks if the subtraction is possible (M >= S).
%       If so, it repeatedly identifies the largest place-value component of the
%       remaining subtrahend and subtracts it from the minuend. It traces
%       the entire execution, providing a step-by-step history.
%
%       @param M The Minuend, the number to subtract from.
%       @param S The Subtrahend, the number to subtract in chunks.
%       @param FinalResult The resulting difference (M - S). If S > M, this
%       will be the atom `'error'`.
%       @param History A list of `step/5` terms that describe the state
%       machine's execution path and the interpretation of each step.

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

% run/4 is the main recursive loop of the state machine.
run(state(q_accept, CV, 0, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'S fully subtracted. Result=~w.', [CV]),
    HistoryEntry = step(q_accept, CV, 0, 0, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, CV, S_Rem, Chunk),
    HistoryEntry = step(Name, CV, S_Rem, Chunk, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% transition/4 defines the logic for moving from one state to the next.

% From q_init, proceed to identify the first chunk.
transition(state(q_init, M, S, _), _, state(q_identify_chunk, M, S, 0), Interp) :-
    format(string(Interp), 'Set CurrentValue=~w. S_Remaining=~w.', [M, S]).

% In q_identify_chunk, determine the next chunk of S to subtract.
% The chunk is the largest part of S based on place value (e.g., hundreds, tens).
transition(state(q_identify_chunk, CV, S_Rem, _), Base, state(q_subtract_chunk, CV, S_Rem, Chunk), Interp) :-
    S_Rem > 0,
    Power is floor(log(S_Rem) / log(Base)),
    PowerValue is Base^Power,
    Chunk is floor(S_Rem / PowerValue) * PowerValue,
    format(string(Interp), 'Identified chunk to subtract: ~w.', [Chunk]).
% If no subtrahend remains, the process is finished.
transition(state(q_identify_chunk, CV, 0, _), _, state(q_accept, CV, 0, 0),
           'S fully subtracted.').

% In q_subtract_chunk, perform the subtraction and loop back to identify the next chunk.
transition(state(q_subtract_chunk, CV, S_Rem, Chunk), _, state(q_identify_chunk, NewCV, NewSRem, 0), Interp) :-
    NewCV is CV - Chunk,
    NewSRem is S_Rem - Chunk,
    format(string(Interp), 'Subtracted ~w. New Value=~w.', [Chunk, NewCV]).
