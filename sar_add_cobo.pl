/** <module> Student Addition Strategy: Counting On by Bases and Ones (COBO)
 *
 * This module implements the 'Counting On by Bases and then Ones' (COBO)
 * strategy for multi-digit addition, modeled as a finite state machine.
 * This strategy involves decomposing one number (B) into its base-10
 * components and then incrementally counting on from the other number (A).
 *
 * The process is as follows:
 * 1. Decompose B into a number of 'bases' (tens) and 'ones'.
 * 2. Starting with A, count on by ten for each base.
 * 3. After all bases are added, count on by one for each one.
 *
 * The state of the automaton is represented by the term:
 * `state(StateName, Sum, BaseCounter, OneCounter)`
 *
 * The history of execution is captured as a list of steps:
 * `step(StateName, CurrentSum, BaseCounter, OneCounter, Interpretation)`
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(sar_add_cobo,
          [ run_cobo/4
          ]).

:- use_module(library(lists)).

%!      run_cobo(+A:integer, +B:integer, -FinalSum:integer, -History:list) is det.
%
%       Executes the 'Counting On by Bases and Ones' (COBO) addition strategy for A + B.
%
%       This predicate initializes the state machine and runs it until it
%       reaches the accept state. It traces the execution, providing a
%       step-by-step history of how the sum was computed by first counting
%       on by tens, and then by ones.
%
%       @param A The first addend, the number to start counting from.
%       @param B The second addend, which is decomposed into bases and ones.
%       @param FinalSum The resulting sum of A and B.
%       @param History A list of `step/5` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_cobo(A, B, FinalSum, History) :-
    Base = 10,
    % Initial state: Decompose B into base and one counters.
    BaseCounter is B // Base,
    OneCounter is B mod Base,

    InitialState = state(q_initialize, A, BaseCounter, OneCounter),

    % Record the start and the interpretation of the initialization.
    format(string(InitialInterpretation), 'Initialize Sum to ~w. Decompose ~w into ~w Bases, ~w Ones.', [A, B, BaseCounter, OneCounter]),
    InitialHistoryEntry = step(q_start, A, BaseCounter, OneCounter, InitialInterpretation),

    % Run the state machine.
    run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),

    % Reverse the history for correct chronological order.
    reverse(ReversedHistory, History),

    % Extract the final sum from the last history entry.
    (last(History, step(_, FinalSum, _, _, _)) -> true ; FinalSum = A).


% run/4 is the main recursive loop of the state machine.
% It drives the state transitions until the accept state is reached.

% Base case: Stop when the machine reaches the 'q_accept' state.
run(state(q_accept, Sum, BC, OC), _Base, AccHistory, FinalHistory) :-
    Interpretation = 'All ones added. Accept.',
    HistoryEntry = step(q_accept, Sum, BC, OC, Interpretation),
    FinalHistory = [HistoryEntry | AccHistory].

% Recursive step: Perform one transition and continue.
run(CurrentState, Base, AccHistory, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, Sum, BC, OC),
    HistoryEntry = step(Name, Sum, BC, OC, Interpretation),
    run(NextState, Base, [HistoryEntry | AccHistory], FinalHistory).

% transition/4 defines the logic for moving from one state to the next.

% From q_initialize, always transition to q_add_bases to start counting.
transition(state(q_initialize, Sum, BaseCounter, OneCounter), _Base, state(q_add_bases, Sum, BaseCounter, OneCounter), Interpretation) :-
    Interpretation = 'Begin counting on by bases.'.

% Loop in q_add_bases, counting on by one base (10) at a time.
transition(state(q_add_bases, Sum, BaseCounter, OneCounter), Base, state(q_add_bases, NewSum, NewBaseCounter, OneCounter), Interpretation) :-
    BaseCounter > 0,
    NewSum is Sum + Base,
    NewBaseCounter is BaseCounter - 1,
    format(string(Interpretation), 'Count on by base: ~w -> ~w.', [Sum, NewSum]).

% When all bases are added, transition from q_add_bases to q_add_ones.
transition(state(q_add_bases, Sum, 0, OneCounter), _Base, state(q_add_ones, Sum, 0, OneCounter), Interpretation) :-
    Interpretation = 'All bases added. Transition to adding ones.'.

% Loop in q_add_ones, counting on by one at a time.
transition(state(q_add_ones, Sum, BaseCounter, OneCounter), _Base, state(q_add_ones, NewSum, BaseCounter, NewOneCounter), Interpretation) :-
    OneCounter > 0,
    NewSum is Sum + 1,
    NewOneCounter is OneCounter - 1,
    format(string(Interpretation), 'Count on by one: ~w -> ~w.', [Sum, NewSum]).

% When all ones are added, transition from q_add_ones to the final accept state.
transition(state(q_add_ones, Sum, BaseCounter, 0), _Base, state(q_accept, Sum, BaseCounter, 0), Interpretation) :-
    Interpretation = 'All ones added. Final sum reached.'.
