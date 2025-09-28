% SWI-Prolog code for the 'Counting On By Bases and then Ones' (COBO) strategy.

:- module(sar_add_cobo,
          [ run_cobo/4 % A, B, FinalSum, History
          ]).

:- use_module(library(lists)).

% Automaton state is represented by a term:
% state(StateName, Sum, BaseCounter, OneCounter)
% History entry is a term:
% step(StateName, Sum, BaseCounter, OneCounter, Interpretation)

% run_cobo(+A, +B, -FinalSum, -History)
% Main predicate to run the COBO automaton with a fixed base of 10.
run_cobo(A, B, FinalSum, History) :-
    Base = 10,
    % Initial state setup from execute_initialize
    BaseCounter is B // Base,
    OneCounter is B mod Base,

    % The initial state before any transitions
    InitialState = state(q_initialize, A, BaseCounter, OneCounter),

    % Record the start and initialization interpretation
    format(string(InitialInterpretation), 'Initialize Sum to ~w. Decompose ~w into ~w Bases, ~w Ones.', [A, B, BaseCounter, OneCounter]),
    InitialHistoryEntry = step(q_start, A, BaseCounter, OneCounter, InitialInterpretation),

    % Run the automaton from the initialized state
    run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),

    % Reverse the history to get the correct chronological order
    reverse(ReversedHistory, History),

    % Extract the final sum from the last step in the history
    (last(History, step(_, FinalSum, _, _, _)) -> true ; FinalSum = A).

% run(+CurrentState, +Base, +AccumulatedHistory, -FinalHistory)
% Recursive predicate that drives the automaton's state transitions.

% Base case: stop when we reach the accept state.
run(state(q_accept, Sum, BC, OC), _Base, AccHistory, FinalHistory) :-
    Interpretation = 'All ones added. Accept.',
    HistoryEntry = step(q_accept, Sum, BC, OC, Interpretation),
    FinalHistory = [HistoryEntry | AccHistory].

% Recursive step: transition to the next state and continue execution.
run(CurrentState, Base, AccHistory, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, Sum, BC, OC),
    HistoryEntry = step(Name, Sum, BC, OC, Interpretation),
    run(NextState, Base, [HistoryEntry | AccHistory], FinalHistory).

% transition(+CurrentState, +Base, -NextState, -Interpretation)
% Defines the transitions from one state to the next.

% Transition from q_initialize to q_add_bases
transition(state(q_initialize, Sum, BaseCounter, OneCounter), _Base, state(q_add_bases, Sum, BaseCounter, OneCounter), Interpretation) :-
    Interpretation = 'All bases added. Transition to adding ones.'.

% Recursive transition for q_add_bases (looping)
transition(state(q_add_bases, Sum, BaseCounter, OneCounter), Base, state(q_add_bases, NewSum, NewBaseCounter, OneCounter), Interpretation) :-
    BaseCounter > 0,
    NewSum is Sum + Base,
    NewBaseCounter is BaseCounter - 1,
    format(string(Interpretation), 'Count on by base: ~w -> ~w.', [Sum, NewSum]).

% Exit transition from q_add_bases to q_add_ones
transition(state(q_add_bases, Sum, 0, OneCounter), _Base, state(q_add_ones, Sum, 0, OneCounter), Interpretation) :-
    Interpretation = 'All bases added. Transition to adding ones.'.

% Recursive transition for q_add_ones (looping)
transition(state(q_add_ones, Sum, BaseCounter, OneCounter), _Base, state(q_add_ones, NewSum, BaseCounter, NewOneCounter), Interpretation) :-
    OneCounter > 0,
    NewSum is Sum + 1,
    NewOneCounter is OneCounter - 1,
    format(string(Interpretation), 'Count on by one: ~w -> ~w.', [Sum, NewSum]).

% Exit transition from q_add_ones to q_accept
transition(state(q_add_ones, Sum, BaseCounter, 0), _Base, state(q_accept, Sum, BaseCounter, 0), Interpretation) :-
    Interpretation = 'All ones added. Accept.'.
