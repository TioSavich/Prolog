/** <module> Student Division Strategy: Conversion to Groups Other than Bases (CBO)
 *
 * This module implements a sophisticated division strategy, sometimes called
 * "Conversion to Groups Other than Bases," modeled as a finite state machine.
 * It solves a division problem (T / S) by leveraging knowledge of a counting
 * base (e.g., 10).
 *
 * The process is as follows:
 * 1.  Decompose the total (T) into a number of bases (TB) and ones (TO).
 * 2.  Analyze the base itself: determine how many groups of size S can be
 *     made from one base, and what the remainder is. (e.g., "how many 4s in 10?").
 * 3.  Use this knowledge to quickly calculate the quotient and remainder that
 *     result from the "bases" part of the total (TB).
 * 4.  Combine the remainder from the bases with the original "ones" part (TO).
 * 5.  Process this combined final remainder to see how many more groups of
 *     size S can be made.
 * 6.  Sum the quotients from the base and remainder parts to get the final answer.
 * 7.  The strategy fails if the divisor (S) is not positive.
 *
 * The state is represented by the term:
 * `state(Name, T_Bases, T_Ones, Quotient, Remainder, S_in_Base, Rem_in_Base, Total, Divisor)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, Quotient, Remainder, Interpretation)`
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(smr_div_cbo,
          [ run_cbo_div/5
          ]).

:- use_module(library(lists)).

%!      run_cbo_div(+T:integer, +S:integer, +Base:integer, -FinalQuotient:integer, -FinalRemainder:integer) is det.
%
%       Executes the 'Conversion to Groups Other than Bases' division strategy
%       for T / S, using the specified Base.
%
%       This predicate initializes and runs a state machine that models the CBO
%       division strategy. It first checks for a positive divisor. If valid, it
%       decomposes the dividend `T` and uses knowledge about the `Base` to find
%       the quotient and remainder. It traces the entire execution.
%
%       @param T The Dividend (Total).
%       @param S The Divisor (Size of groups).
%       @param Base The numerical base to use for decomposition (e.g., 10).
%       @param FinalQuotient The quotient of the division.
%       @param FinalRemainder The remainder of the division. If S is not
%       positive, this will be the atom `'error'`.

run_cbo_div(T, S, Base, FinalQuotient, FinalRemainder) :-
    (S =< 0 ->
        % History is not exposed, but we could create it here if needed.
        % History = [step(q_error, 0, 0, 'Error: Divisor must be positive.')],
        FinalQuotient = 'error', FinalRemainder = 'error'
    ;
        TB is T // Base,
        TO is T mod Base,
        InitialState = state(q_init, TB, TO, 0, 0, 0, 0, T, S),

        run(InitialState, Base, [], ReversedHistory),
        reverse(ReversedHistory, _History), % History is generated but not returned.

        (last(ReversedHistory, step(q_accept, FinalQuotient, FinalRemainder, _)) -> true ;
         (FinalQuotient = 'error', FinalRemainder = 'error'))
    ).

% run/4 is the main recursive loop of the state machine.
run(state(q_accept, _, _, Q, R, _, _, _, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Finished. Total Quotient = ~w.', [Q]),
    HistoryEntry = step(q_accept, Q, R, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, _, _, Q, R, _, _, _, _),
    HistoryEntry = step(Name, Q, R, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% transition/4 defines the logic for moving from one state to the next.

% From q_init, decompose T and proceed to analyze the base.
transition(state(q_init, TB, TO, Q, R, SiB, RiB, T, S), _, state(q_analyze_base, TB, TO, Q, R, SiB, RiB, T, S), Interp) :-
    format(string(Interp), 'Initialize: ~w/~w. Decompose T: ~w Bases + ~w Ones.', [T, S, TB, TO]).

% In q_analyze_base, determine how many groups of S fit in one Base.
transition(state(q_analyze_base, TB, TO, Q, R, _, _, T, S), Base, state(q_process_bases, TB, TO, Q, R, SiB, RiB, T, S), Interp) :-
    SiB is Base // S,
    RiB is Base mod S,
    format(string(Interp), 'Analyze Base: One Base (~w) = ~w group(s) of ~w + Remainder ~w.', [Base, SiB, S, RiB]).

% In q_process_bases, calculate the quotient and remainder from the "bases" part of T.
transition(state(q_process_bases, TB, TO, _, _, SiB, RiB, T, S), _, state(q_combine_R, TB, TO, NewQ, NewR, SiB, RiB, T, S), Interp) :-
    NewQ is TB * SiB,
    NewR is TB * RiB,
    format(string(Interp), 'Process ~w Bases: Yields ~w groups and ~w remainder.', [TB, NewQ, NewR]).

% In q_combine_R, add the remainder from the bases to the original ones part of T.
transition(state(q_combine_R, _, TO, Q, R, SiB, RiB, T, S), _, state(q_process_R, _, TO, Q, NewR, SiB, RiB, T, S), Interp) :-
    NewR is R + TO,
    format(string(Interp), 'Combine Remainders: ~w (from Bases) + ~w (from Ones) = ~w.', [R, TO, NewR]).

% In q_process_R, find the quotient and remainder from the combined remainder, then accept.
transition(state(q_process_R, _, _, Q, R, _, _, T, S), _, state(q_accept, _, _, NewQ, NewR, _, _, T, S), Interp) :-
    Q_from_R is R // S,
    NewR is R mod S,
    NewQ is Q + Q_from_R,
    format(string(Interp), 'Process Remainder: Yields ~w additional group(s).', [Q_from_R]).
