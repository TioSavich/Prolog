/** <module> Student Subtraction Strategy: Double Rounding
 *
 * This module implements a "double rounding" strategy for subtraction (M - S),
 * sometimes used by students to simplify the calculation. It is modeled as a
 * finite state machine.
 *
 * The process is as follows:
 * 1. Round both the minuend (M) and the subtrahend (S) down to the nearest
 *    multiple of 10. Let the rounded values be MR and SR, and the amounts
 *    they were rounded by be KM and KS respectively.
 * 2. Perform a simplified subtraction on the rounded numbers: `TR = MR - SR`.
 * 3. Adjust this temporary result. First, add back the amount M was rounded by: `TR + KM`.
 * 4. Second, subtract the amount S was rounded by: `(TR + KM) - KS`.
 *    This final adjustment is modeled as a chunking/counting-back process.
 * 5. The strategy fails if S > M.
 *
 * The state is represented by the term:
 * `state(Name, K_M, K_S, TempResult, K_S_Rem, Chunk, M, S, MR, SR)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, K_M, K_S, TempResult, K_S_Rem, Interpretation)`
 *
 * 
 * 
 */
:- module(sar_sub_rounding,
          [ run_sub_rounding/4
          ]).

:- use_module(library(lists)).

%!      run_sub_rounding(+M:integer, +S:integer, -FinalResult:integer, -History:list) is det.
%
%       Executes the 'Double Rounding' subtraction strategy for M - S.
%
%       This predicate initializes and runs a state machine that models the
%       double rounding process. It first checks if the subtraction is possible
%       (M >= S). If so, it rounds both numbers down, subtracts them, and then
%       performs two adjustments to arrive at the final answer. It traces
%       the entire execution, providing a step-by-step history.
%
%       @param M The Minuend.
%       @param S The Subtrahend.
%       @param FinalResult The resulting difference (M - S). If S > M, this
%       will be the atom `'error'`.
%       @param History A list of `step/6` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_sub_rounding(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        InitialState = state(q_start, 0, 0, 0, 0, 0, M, S, 0, 0),
        InitialHistoryEntry = step(q_start, 0, 0, 0, 0, 'Start.'),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, _, _, TR, _, _)) -> FinalResult = TR ; FinalResult = 'error')
    ).

% run/4 is the main recursive loop of the state machine.
run(state(q_accept, KM, KS, TR, 0, _, _, _, _, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Adjustment for S complete. Final Result = ~w.', [TR]),
    HistoryEntry = step(q_accept, KM, KS, TR, 0, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, KM, KS, TR, KSR, _, _, _, _, _),
    HistoryEntry = step(Name, KM, KS, TR, KSR, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% transition/4 defines the logic for moving from one state to the next.

% Initial state, proceeds to rounding the Minuend.
transition(state(q_start, _, _, _, _, _, M, S, _, _), _, state(q_round_M, 0, 0, 0, 0, 0, M, S, 0, 0), 'Proceed to round M.').

% Round M down and record the amount it was rounded by (KM).
transition(state(q_round_M, _, _, _, _, _, M, S, _, _), Base, state(q_round_S, KM, 0, 0, 0, 0, M, S, MR, 0), Interp) :-
    KM is M mod Base,
    MR is M - KM,
    format(string(Interp), 'Round M down: ~w -> ~w. (K_M = ~w).', [M, MR, KM]).

% Round S down and record the amount it was rounded by (KS).
transition(state(q_round_S, KM, _, _, _, _, M, S, MR, _), Base, state(q_subtract, KM, KS, 0, 0, 0, M, S, MR, SR), Interp) :-
    KS is S mod Base,
    SR is S - KS,
    format(string(Interp), 'Round S down: ~w -> ~w. (K_S = ~w).', [S, SR, KS]).

% Perform the intermediate subtraction with the rounded numbers.
transition(state(q_subtract, KM, KS, _, _, _, M, S, MR, SR), _, state(q_adjust_M, KM, KS, TR, 0, 0, M, S, MR, SR), Interp) :-
    TR is MR - SR,
    format(string(Interp), 'Intermediate Subtraction: ~w - ~w = ~w.', [MR, SR, TR]).

% First adjustment: Add back the amount M was rounded by (KM).
transition(state(q_adjust_M, KM, KS, TR, _, _, M, S, MR, SR), _, state(q_init_adjust_S, KM, KS, NewTR, 0, 0, M, S, MR, SR), Interp) :-
    NewTR is TR + KM,
    format(string(Interp), 'Adjust for M (Add K_M): ~w + ~w = ~w.', [TR, KM, NewTR]).

% Prepare for the second adjustment: subtracting KS.
transition(state(q_init_adjust_S, KM, KS, TR, _, _, M, S, MR, SR), _, state(q_loop_adjust_S, KM, KS, TR, KS, 0, M, S, MR, SR), Interp) :-
    format(string(Interp), 'Begin Adjust for S (Subtract K_S): Need to subtract ~w.', [KS]).

% Second adjustment is complete when the remainder (KSR) is zero.
transition(state(q_loop_adjust_S, KM, KS, TR, 0, _, M, S, MR, SR), _, state(q_accept, KM, KS, TR, 0, 0, M, S, MR, SR), 'Adjustment for S complete.').
% Perform the second adjustment by subtracting KS in chunks.
transition(state(q_loop_adjust_S, KM, KS, TR, KSR, _, M, S, MR, SR), Base, state(q_loop_adjust_S, KM, KS, NewTR, NewKSR, Chunk, M, S, MR, SR), Interp) :-
    KSR > 0,
    K_to_prev_base is TR mod Base,
    (K_to_prev_base > 0, KSR >= K_to_prev_base -> Chunk = K_to_prev_base ; Chunk = KSR),
    NewTR is TR - Chunk,
    NewKSR is KSR - Chunk,
    format(string(Interp), 'Chunking Adjustment: ~w - ~w = ~w.', [TR, Chunk, NewTR]).
