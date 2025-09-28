/** <module> Student Addition Strategy: Chunking by Bases and Ones
 *
 * This module implements the 'Chunking by Bases and Ones' strategy for
 * multi-digit addition, modeled as a finite state machine. This strategy
 * involves decomposing one of the numbers (B) into its base-10 components
 * (e.g., tens and ones), adding them sequentially to the other number (A),
 * and using strategic 'chunks' to reach friendly base-10 numbers.
 *
 * The process is as follows:
 * 1. Decompose B into a 'base chunk' (the tens part) and an 'ones chunk'.
 * 2. Add the entire base chunk to A at once.
 * 3. Strategically add parts of the ones chunk to get the sum to the next multiple of 10.
 * 4. Repeat until all parts of B have been added.
 *
 * The state is represented by the term:
 * `state(Name, Sum, BasesRem, OnesRem, K, InternalSum, TargetBase)`
 *
 * The history of execution is captured as a list of steps:
 * `step(StateName, CurrentSum, BasesRemaining, OnesRemaining, K, Interpretation)`
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(sar_add_chunking,
          [ run_chunking/4
          ]).

:- use_module(library(lists)).

%!      run_chunking(+A:integer, +B:integer, -FinalSum:integer, -History:list) is det.
%
%       Executes the 'Chunking by Bases and Ones' addition strategy for A + B.
%
%       This predicate initializes the state machine and runs it until it
%       reaches the accept state. It traces the execution, providing a
%       step-by-step history of how the sum was computed.
%
%       @param A The first addend.
%       @param B The second addend, which will be decomposed and added in chunks.
%       @param FinalSum The resulting sum of A and B.
%       @param History A list of `step/6` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_chunking(A, B, FinalSum, History) :-
    Base = 10,
    % Initial state (q_init): Decompose B and set the initial sum.
    Sum is A,
    BasesRemaining is (B // Base) * Base,
    OnesRemaining is B mod Base,

    format(string(InitialInterpretation), 'Initialize Sum to ~w. Decompose B: ~w + ~w.', [A, BasesRemaining, OnesRemaining]),
    InitialHistoryEntry = step(q_start, A, 0, 0, 0, InitialInterpretation),

    InitialState = state(q_init, Sum, BasesRemaining, OnesRemaining, 0, 0, 0),

    % Run the state machine.
    run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
    reverse(ReversedHistory, History),

    % Extract the final sum from the last history entry.
    (last(History, step(_, FinalSum, _, _, _, _)) -> true ; FinalSum = A).

% run/4 is the main loop of the state machine. It stops at the q_accept state.
run(state(q_accept, Sum, BR, OR, K, _IS, _TB), _Base, Acc, FinalHistory) :-
    HistoryEntry = step(q_accept, Sum, BR, OR, K, 'Execution finished.'),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, Sum, BR, OR, K, _, _),
    HistoryEntry = step(Name, Sum, BR, OR, K, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% transition/4 defines the state transitions of the finite state machine.

% From q_init, always proceed to add the base chunk.
transition(state(q_init, Sum, BR, OR, K, IS, TB), _Base, state(q_add_base_chunk, Sum, BR, OR, K, IS, TB),
           'Proceed to add base chunk.').

% From q_add_base_chunk:
% If there are bases remaining, add them all at once.
transition(state(q_add_base_chunk, Sum, BR, OR, _K, _IS, _TB), _Base, state(q_init_ones_chunk, NewSum, 0, OR, 0, 0, 0), Interpretation) :-
    BR > 0,
    NewSum is Sum + BR,
    format(string(Interpretation), 'Add Base Chunk (+~w). Sum = ~w.', [BR, NewSum]).
% If there are no bases, move on.
transition(state(q_add_base_chunk, Sum, 0, OR, _K, _IS, _TB), _Base, state(q_init_ones_chunk, Sum, 0, OR, 0, 0, 0),
           'No bases to add.').

% From q_init_ones_chunk:
% If there are ones to add, start the strategic chunking process.
transition(state(q_init_ones_chunk, Sum, BR, OR, K, _IS, _TB), _Base, state(q_init_K, Sum, BR, OR, K, Sum, TargetBase), Interpretation) :-
    OR > 0,
    format(string(Interpretation), 'Begin strategic chunking of remaining ones (~w).', [OR]),
    (Sum > 0, Sum mod 10 =\= 0 -> TargetBase is ((Sum // 10) + 1) * 10 ; TargetBase is Sum).
% If no ones are left, the process is finished.
transition(state(q_init_ones_chunk, Sum, _, 0, _, _, _), _Base, state(q_accept, Sum, 0, 0, 0, 0, 0),
           'All ones added. Accepting.').

% From q_init_K, calculate the value K needed to reach the next base.
transition(state(q_init_K, Sum, BR, OR, _, IS, TB), _Base, state(q_loop_K, Sum, BR, OR, 0, IS, TB), Interpretation) :-
    format(string(Interpretation), 'Calculating K: Counting from ~w to ~w.', [Sum, TB]).

% From q_loop_K, count up from the current sum to the target base to find K.
transition(state(q_loop_K, Sum, BR, OR, K, IS, TB), _Base, state(q_loop_K, Sum, BR, OR, NewK, NewIS, TB), Interpretation) :-
    IS < TB,
    NewIS is IS + 1,
    NewK is K + 1,
    format(string(Interpretation), 'Counting Up: ~w, K=~w', [NewIS, NewK]).
% Once the target base is reached, the value of K is known.
transition(state(q_loop_K, Sum, BR, OR, K, IS, TB), _Base, state(q_add_ones_chunk, Sum, BR, OR, K, IS, TB), Interpretation) :-
    IS >= TB,
    format(string(Interpretation), 'K needed to reach base is ~w.', [K]).

% From q_add_ones_chunk:
% If we have enough ones remaining to add the strategic chunk K, do so.
transition(state(q_add_ones_chunk, Sum, BR, OR, K, _IS, _TB), _Base, state(q_init_ones_chunk, NewSum, BR, NewOR, 0, 0, 0), Interpretation) :-
    OR >= K, K > 0,
    NewSum is Sum + K,
    NewOR is OR - K,
    format(string(Interpretation), 'Add Strategic Chunk (+~w) to make base. Sum = ~w.', [K, NewSum]).
% Otherwise, add all remaining ones. This happens if K is too large or 0.
transition(state(q_add_ones_chunk, Sum, BR, OR, K, _IS, _TB), _Base, state(q_init_ones_chunk, NewSum, BR, 0, 0, 0, 0), Interpretation) :-
    (OR < K ; K =< 0), OR > 0,
    NewSum is Sum + OR,
    format(string(Interpretation), 'Add Remaining Chunk (+~w). Sum = ~w.', [OR, NewSum]).
