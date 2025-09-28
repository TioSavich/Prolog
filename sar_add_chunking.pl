% SWI-Prolog code for the 'Chunking by Bases and Ones' strategy.

:- module(sar_add_chunking,
          [ run_chunking/4 % A, B, FinalSum, History
          ]).

:- use_module(library(lists)).

% State: state(Name, Sum, BasesRem, OnesRem, K, InternalSum, TargetBase)
% History: step(Name, Sum, BasesRem, OnesRem, K, Interpretation)

run_chunking(A, B, FinalSum, History) :-
    Base = 10,
    % q_init
    Sum is A,
    BasesRemaining is (B // Base) * Base,
    OnesRemaining is B mod Base,

    format(string(InitialInterpretation), 'Initialize Sum to ~w. Decompose B: ~w + ~w.', [A, BasesRemaining, OnesRemaining]),
    InitialHistoryEntry = step(q_start, A, 0, 0, 0, InitialInterpretation),

    InitialState = state(q_init, Sum, BasesRemaining, OnesRemaining, 0, 0, 0),

    run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(_, FinalSum, _, _, _, _)) -> true ; FinalSum = A).

% Main run loop
run(state(q_accept, Sum, BR, OR, K, IS, TB), _Base, Acc, FinalHistory) :-
    HistoryEntry = step(q_accept, Sum, BR, OR, K, 'Execution finished.'),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, Sum, BR, OR, K, _, _),
    HistoryEntry = step(Name, Sum, BR, OR, K, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init, Sum, BR, OR, K, IS, TB), _Base, state(q_add_base_chunk, Sum, BR, OR, K, IS, TB),
           'Proceed to add base chunk.').

transition(state(q_add_base_chunk, Sum, BR, OR, K, IS, TB), _Base, state(q_init_ones_chunk, NewSum, 0, OR, 0, 0, 0), Interpretation) :-
    BR > 0,
    NewSum is Sum + BR,
    format(string(Interpretation), 'Add Base Chunk (+~w). Sum = ~w.', [BR, NewSum]).
transition(state(q_add_base_chunk, Sum, 0, OR, K, IS, TB), _Base, state(q_init_ones_chunk, Sum, 0, OR, 0, 0, 0),
           'No bases to add.').

transition(state(q_init_ones_chunk, Sum, BR, OR, K, IS, TB), _Base, state(q_init_K, Sum, BR, OR, K, Sum, TargetBase), Interpretation) :-
    OR > 0,
    format(string(Interpretation), 'Begin strategic chunking of remaining ones (~w).', [OR]),
    (Sum > 0, Sum mod 10 =\= 0 -> TargetBase is ((Sum // 10) + 1) * 10 ; TargetBase is Sum).
transition(state(q_init_ones_chunk, Sum, _, 0, _, _, _), _Base, state(q_accept, Sum, 0, 0, 0, 0, 0),
           'All ones added. Accepting.').

transition(state(q_init_K, Sum, BR, OR, _, IS, TB), _Base, state(q_loop_K, Sum, BR, OR, 0, IS, TB), Interpretation) :-
    format(string(Interpretation), 'Calculating K: Counting from ~w to ~w.', [Sum, TB]).

transition(state(q_loop_K, Sum, BR, OR, K, IS, TB), _Base, state(q_loop_K, Sum, BR, OR, NewK, NewIS, TB), Interpretation) :-
    IS < TB,
    NewIS is IS + 1,
    NewK is K + 1,
    format(string(Interpretation), 'Counting Up: ~w, K=~w', [NewIS, NewK]).
transition(state(q_loop_K, Sum, BR, OR, K, IS, TB), _Base, state(q_add_ones_chunk, Sum, BR, OR, K, IS, TB), Interpretation) :-
    IS >= TB,
    format(string(Interpretation), 'K needed to reach base is ~w.', [K]).

transition(state(q_add_ones_chunk, Sum, BR, OR, K, IS, TB), _Base, state(q_init_ones_chunk, NewSum, BR, NewOR, 0, 0, 0), Interpretation) :-
    OR >= K, K > 0,
    NewSum is Sum + K,
    NewOR is OR - K,
    format(string(Interpretation), 'Add Strategic Chunk (+~w) to make base. Sum = ~w.', [K, NewSum]).
transition(state(q_add_ones_chunk, Sum, BR, OR, K, IS, TB), _Base, state(q_init_ones_chunk, NewSum, BR, 0, 0, 0, 0), Interpretation) :-
    (OR < K ; K =< 0), OR > 0,
    NewSum is Sum + OR,
    format(string(Interpretation), 'Add Remaining Chunk (+~w). Sum = ~w.', [OR, NewSum]).
