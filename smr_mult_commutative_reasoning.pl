/** <module> Student Multiplication Strategy: Commutative Reasoning (Repeated Addition)
 *
 * This module implements a multiplication strategy based on repeated addition,
 * modeled as a finite state machine. The name "Commutative Reasoning" implies
 * that a student understands that `A * B` is equivalent to `B * A` and can
 * choose the more efficient path. However, this model directly implements
 * `A * B` as adding `B` to itself `A` times.
 *
 * The process is as follows:
 * 1.  Start with a total of 0.
 * 2.  Repeatedly add the number of items (`B`) to the total.
 * 3.  Use a counter, initialized to the number of groups (`A`), to track
 *     how many times to perform the addition.
 * 4.  The process stops when the counter reaches zero. The accumulated total
 *     is the final product.
 *
 * The state is represented by the term:
 * `state(Name, Groups, Items, Total, Counter)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, Groups, Items, Total, Interpretation)`
 *
 * 
 * 
 */
:- module(smr_mult_commutative_reasoning,
          [ run_commutative_mult/4
          ]).

:- use_module(library(lists)).

%!      run_commutative_mult(+A:integer, +B:integer, -FinalTotal:integer, -History:list) is det.
%
%       Executes the 'Commutative Reasoning' (Repeated Addition) multiplication
%       strategy for A * B.
%
%       This predicate initializes and runs a state machine that models the
%       process of calculating `A * B` by adding `B` to an accumulator `A` times.
%       It traces the entire execution, providing a step-by-step history of
%       the repeated addition.
%
%       @param A The number of groups (effectively, the number of additions).
%       @param B The number of items in each group (the number being added).
%       @param FinalTotal The resulting product of A * B.
%       @param History A list of `step/5` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_commutative_mult(A, B, FinalTotal, History) :-
    Groups = A,
    Items = B,
    InitialState = state(q_init_calc, Groups, Items, 0, Groups),
    InitialHistoryEntry = step(q_start, 0, 0, 0, 'Start'),

    run(InitialState, [InitialHistoryEntry], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, _, _, Total, _)) -> FinalTotal = Total ; FinalTotal = 'error').

% run/3 is the main recursive loop of the state machine.
run(state(q_accept, _, _, Total, _), Acc, FinalHistory) :-
    format(string(Interpretation), 'Calculation complete. Result = ~w.', [Total]),
    HistoryEntry = step(q_accept, 0, 0, Total, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Acc, FinalHistory) :-
    transition(CurrentState, NextState, Interpretation),
    CurrentState = state(Name, Gs, Items, Total, _),
    HistoryEntry = step(Name, Gs, Items, Total, Interpretation),
    run(NextState, [HistoryEntry | Acc], FinalHistory).

% transition/3 defines the logic for moving from one state to the next.

% From q_init_calc, start the iterative calculation loop.
transition(state(q_init_calc, Gs, Items, _, _), state(q_loop_calc, Gs, Items, 0, Gs),
           'Initializing iterative calculation.').

% In q_loop_calc, add the number of items to the total and decrement the counter.
transition(state(q_loop_calc, Gs, Items, Total, Counter), state(q_loop_calc, Gs, Items, NewTotal, NewCounter), Interp) :-
    Counter > 0,
    NewTotal is Total + Items,
    NewCounter is Counter - 1,
    format(string(Interp), 'Iterate: Added ~w. Total = ~w.', [Items, NewTotal]).
% When the counter reaches zero, the calculation is complete.
transition(state(q_loop_calc, _, _, Total, 0), state(q_accept, 0, 0, Total, 0),
           'Calculation complete.').
