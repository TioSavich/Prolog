/** <module> Student Addition Strategy: Rounding and Adjusting
 *
 * This module implements the 'Rounding and Adjusting' strategy for addition,
 * modeled as a multi-phase finite state machine. The strategy involves
 * simplifying an addition problem by rounding one number up to a multiple of 10,
 * performing the addition, and then adjusting the result.
 *
 * The process is as follows:
 * 1.  **Phase 1: Rounding**: Select one number (`Target`) to round up, typically
 *     the one closer to the next multiple of 10. Calculate the amount `K`
 *     needed for rounding.
 * 2.  **Phase 2: Addition**: Add the *rounded* number to the other number. This
 *     is performed using a 'Counting On by Bases and Ones' (COBO) sub-strategy.
 * 3.  **Phase 3: Adjustment**: Adjust the sum from Phase 2 by subtracting `K`
 *     to get the final, correct answer.
 *
 * The state is represented by the complex term:
 * `state(Name, K, A_rounded, TempSum, Result, Target, Other, TargetBase, BaseCounter, OneCounter)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, K, RoundedTarget, TempSum, CurrentResult, Interpretation)`
 *
 * 
 * 
 */
:- module(sar_add_rounding,
          [ run_rounding/4
          ]).

:- use_module(library(lists)).

% determine_target/5 is a helper to decide which number to round.
% It selects the number that is closer to the next multiple of the base.
determine_target(A_in, B_in, Base, Target, Other) :-
    A_rem is A_in mod Base,
    B_rem is B_in mod Base,
    (A_rem >= B_rem ->
        (Target = A_in, Other = B_in)
    ;
        (Target = B_in, Other = A_in)
    ).

%!      run_rounding(+A_in:integer, +B_in:integer, -FinalResult:integer, -History:list) is det.
%
%       Executes the 'Rounding and Adjusting' addition strategy for A + B.
%
%       This predicate initializes and runs a state machine that models the
%       three phases of the strategy: rounding, adding, and adjusting.
%       It traces the entire execution, providing a step-by-step history
%       of the cognitive process.
%
%       @param A_in The first addend.
%       @param B_in The second addend.
%       @param FinalResult The resulting sum of A and B.
%       @param History A list of `step/6` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_rounding(A_in, B_in, FinalResult, History) :-
    Base = 10,
    determine_target(A_in, B_in, Base, Target, Other),

    % Initial state (q_init_K): Determine K and the target base for rounding.
    (Target =< 0 -> TB = 0 ; (Target mod Base =:= 0 -> TB = Target ; TB is ((Target // Base) + 1) * Base)),
    InitialState = state(q_init_K, 0, Target, 0, 0, Target, Other, TB, 0, 0),

    format(string(InitialInterpretation), 'Inputs: ~w, ~w. Target for rounding: ~w', [A_in, B_in, Target]),
    InitialHistoryEntry = step(q_start, 0, 0, 0, 0, InitialInterpretation),

    run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
    reverse(ReversedHistory, History),

    % Extract the final result from the last history entry.
    (last(History, step(q_accept, _, _, _, R, _)) -> FinalResult = R ; FinalResult = 'error').

% run/4 is the main recursive loop of the state machine.
run(state(q_accept, K, AR, TS, Result, _, _, _, _, _), _, Acc, FinalHistory) :-
    HistoryEntry = step(q_accept, K, AR, TS, Result, 'Execution finished.'),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, K, AR, TS, Result, _, _, _, _, _),
    HistoryEntry = step(Name, K, AR, TS, Result, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% transition/4 defines the logic for moving from one state to the next.

% Phase 1: Rounding
transition(state(q_init_K, K, AR, TS, R, T, O, TB, BC, OC), _, state(q_loop_K, K, AR, TS, R, T, O, TB, BC, OC), Interp) :-
    format(string(Interp), 'Initializing K calculation. Counting from ~w to ~w.', [T, TB]).

transition(state(q_loop_K, K, AR, TS, R, T, O, TB, BC, OC), _, state(q_loop_K, NewK, NewAR, TS, R, T, O, TB, BC, OC), Interp) :-
    AR < TB,
    NewK is K + 1, NewAR is AR + 1,
    format(string(Interp), 'Counting Up: ~w, K=~w', [NewAR, NewK]).
transition(state(q_loop_K, K, AR, TS, R, T, O, TB, BC, OC), _, state(q_init_Add, K, AR, TS, R, T, O, TB, BC, OC), Interp) :-
    AR >= TB,
    format(string(Interp), 'K needed is ~w. Target rounded to ~w.', [K, AR]).

% Phase 2: Addition (using COBO sub-strategy)
transition(state(q_init_Add, K, AR, _TS, R, T, O, TB, _BC, _OC), Base, state(q_loop_AddBases, K, AR, AR, R, T, O, TB, OBC, OOC), Interp) :-
    OBC is O // Base, OOC is O mod Base,
    format(string(Interp), 'Initializing COBO: ~w + ~w. (Bases: ~w, Ones: ~w)', [AR, O, OBC, OOC]).

transition(state(q_loop_AddBases, K, AR, TS, R, T, O, TB, BC, OC), Base, state(q_loop_AddBases, K, AR, NewTS, R, T, O, TB, NewBC, OC), Interp) :-
    BC > 0,
    NewTS is TS + Base, NewBC is BC - 1,
    format(string(Interp), 'COBO (Base): ~w', [NewTS]).
transition(state(q_loop_AddBases, K, AR, TS, R, T, O, TB, 0, OC), _, state(q_loop_AddOnes, K, AR, TS, R, T, O, TB, 0, OC),
           'COBO Bases complete.').

transition(state(q_loop_AddOnes, K, AR, TS, R, T, O, TB, BC, OC), _, state(q_loop_AddOnes, K, AR, NewTS, R, T, O, TB, BC, NewOC), Interp) :-
    OC > 0,
    NewTS is TS + 1, NewOC is OC - 1,
    format(string(Interp), 'COBO (One): ~w', [NewTS]).
transition(state(q_loop_AddOnes, K, AR, TS, R, T, O, TB, BC, 0), _, state(q_init_Adjust, K, AR, TS, R, T, O, TB, BC, 0), Interp) :-
    format(string(Interp), '~w + ~w = ~w.', [AR, O, TS]).

% Phase 3: Adjustment
transition(state(q_init_Adjust, K, AR, TS, _, T, O, TB, BC, OC), _, state(q_loop_Adjust, K, AR, TS, TS, T, O, TB, BC, OC), Interp) :-
    format(string(Interp), 'Initializing Adjustment: Count back K=~w.', [K]).

transition(state(q_loop_Adjust, K, AR, TS, R, T, O, TB, BC, OC), _, state(q_loop_Adjust, NewK, AR, TS, NewR, T, O, TB, BC, OC), Interp) :-
    K > 0,
    NewK is K - 1, NewR is R - 1,
    format(string(Interp), 'Counting Back: ~w', [NewR]).
transition(state(q_loop_Adjust, 0, AR, TS, R, T, _, _, _, _), _, state(q_accept, 0, AR, TS, R, T, _, 0, 0, 0), Interp) :-
    Adj is AR - T,
    format(string(Interp), 'Subtracted Adjustment (~w). Final Result: ~w.', [Adj, R]).
