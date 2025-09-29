/** <module> Student Division Strategy: Inverse of Distributive Property (IDP)
 *
 * This module implements a division strategy based on the inverse of the
 * distributive property, modeled as a finite state machine. It solves a
 * division problem (T / S) by using a knowledge base (KB) of known
 * multiplication facts for the divisor S.
 *
 * The process is as follows:
 * 1.  Given a knowledge base of facts for S (e.g., 2*S, 5*S, 10*S), find the
 *     largest known multiple of S that is less than or equal to the
 *     remaining total (T).
 * 2.  Subtract this multiple from T.
 * 3.  Add the corresponding factor to a running total for the quotient.
 * 4.  Repeat the process with the new, smaller remainder until no more known
 *     multiples can be subtracted.
 * 5.  The final quotient is the sum of the factors, and the final remainder
 *     is what's left of the total.
 * 6.  The strategy fails if the divisor (S) is not positive.
 *
 * The state is represented by the term:
 * `state(Name, Remaining, TotalQuotient, PartialTotal, PartialQuotient, KB, Divisor)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, Remainder, TotalQuotient, PartialTotal, PartialQuotient, Interpretation)`
 *
 * 
 * 
 */
:- module(smr_div_idp,
          [ run_idp/5
          ]).

:- use_module(library(lists)).

%!      run_idp(+T:integer, +S:integer, +KB_in:list, -FinalQuotient:integer, -FinalRemainder:integer) is det.
%
%       Executes the 'Inverse of Distributive Property' division strategy for T / S.
%
%       This predicate initializes and runs a state machine that models the IDP
%       strategy. It first checks for a positive divisor. If valid, it uses the
%       provided knowledge base `KB_in` to repeatedly subtract the largest
%       possible known multiple of `S` from `T`, accumulating the quotient.
%       It traces the entire execution.
%
%       @param T The Dividend (Total).
%       @param S The Divisor.
%       @param KB_in A list of `Multiple-Factor` pairs representing known
%       multiplication facts for `S`. Example: `[20-2, 50-5, 100-10]` for S=10.
%       @param FinalQuotient The calculated quotient of the division.
%       @param FinalRemainder The calculated remainder. If S is not positive,
%       this will be `T`.

run_idp(T, S, KB_in, FinalQuotient, FinalRemainder) :-
    (S =< 0 ->
        % History is not exposed, but we could create it here if needed.
        % History = [step(q_error, T, 0, 0, 0, 'Error: Divisor must be positive.')],
        FinalQuotient = 'error', FinalRemainder = T
    ;
        % Sort KB descending by the multiple (the key) for the greedy search.
        keysort(KB_in, SortedKB_asc),
        reverse(SortedKB_asc, KB),

        InitialState = state(q_init, T, 0, 0, 0, KB, S),

        run(InitialState, [], ReversedHistory),
        reverse(ReversedHistory, _History), % History is generated but not returned.

        (last(ReversedHistory, step(q_accept, FinalRemainder, FinalQuotient, _, _, _)) -> true ;
         (FinalQuotient = 'error', FinalRemainder = 'error'))
    ).

% run/3 is the main recursive loop of the state machine.
run(state(q_accept, Rem, TQ, _, _, _, _), Acc, FinalHistory) :-
    format(string(Interpretation), 'Decomposition complete. Total Quotient = ~w.', [TQ]),
    HistoryEntry = step(q_accept, Rem, TQ, 0, 0, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Acc, FinalHistory) :-
    transition(CurrentState, NextState, Interpretation),
    CurrentState = state(Name, Rem, TQ, PT, PQ, _, _),
    HistoryEntry = step(Name, Rem, TQ, PT, PQ, Interpretation),
    run(NextState, [HistoryEntry | Acc], FinalHistory).

% transition/3 defines the logic for moving from one state to the next.

% From q_init, proceed to search the knowledge base.
transition(state(q_init, T, TQ, PT, PQ, KB, S), state(q_search_KB, T, TQ, PT, PQ, KB, S), Interp) :-
    format(string(Interp), 'Initialize: ~w / ~w. Loaded known facts for ~w.', [T, S, S]).

% In q_search_KB, find the best known multiple to subtract.
transition(state(q_search_KB, Rem, TQ, _, _, KB, S), state(q_apply_fact, Rem, TQ, Multiple, Factor, KB, S), Interp) :-
    find_best_fact(KB, Rem, Multiple, Factor),
    format(string(Interp), 'Found known multiple: ~w (~w x ~w).', [Multiple, Factor, S]).
% If no suitable fact is found, the process is complete.
transition(state(q_search_KB, Rem, TQ, _, _, KB, S), state(q_accept, Rem, TQ, 0, 0, KB, S), 'No suitable fact found.') :-
    \+ find_best_fact(KB, Rem, _, _).

% In q_apply_fact, subtract the found multiple and add the factor to the quotient.
transition(state(q_apply_fact, Rem, TQ, PT, PQ, KB, S), state(q_search_KB, NewRem, NewTQ, 0, 0, KB, S), Interp) :-
    NewRem is Rem - PT,
    NewTQ is TQ + PQ,
    format(string(Interp), 'Applied fact. Subtracted ~w. Added ~w to Quotient.', [PT, PQ]).

% find_best_fact/4 is a helper to greedily find the largest applicable known fact.
% It assumes KB is sorted in descending order of multiples.
find_best_fact([Multiple-Factor | _], Rem, Multiple, Factor) :-
    Multiple =< Rem.
find_best_fact([_ | Rest], Rem, BestMultiple, BestFactor) :-
    find_best_fact(Rest, Rem, BestMultiple, BestFactor).
