% SWI-Prolog code for the 'Inverse of Distributive Reasoning' division strategy.

:- module(smr_div_idp,
          [ run_idp/5 % T, S, KB, FinalQuotient, FinalRemainder
          ]).

:- use_module(library(lists)).

% KB is a list of pairs: [multiple-factor, ...]
% State: state(Name, Remaining, TotalQ, PartialT, PartialQ, KB)
% History: step(Name, Rem, TQ, PT, PQ, Interpretation)

run_idp(T, S, KB_in, FinalQuotient, FinalRemainder) :-
    (S =< 0 ->
        History = [step(q_error, T, 0, 0, 0, 'Error: Divisor must be positive.')],
        FinalQuotient = 'error', FinalRemainder = T
    ;
        % Sort KB descending by multiple
        keysort(KB_in, SortedKB_asc),
        reverse(SortedKB_asc, KB),

        InitialState = state(q_init, T, 0, 0, 0, KB, S),

        run(InitialState, [], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, FinalRemainder, FinalQuotient, _, _, _)) -> true ;
         (FinalQuotient = 'error', FinalRemainder = 'error'))
    ).

run(state(q_accept, Rem, TQ, _, _, _, _), Acc, FinalHistory) :-
    format(string(Interpretation), 'Decomposition complete. Total Quotient = ~w.', [TQ]),
    HistoryEntry = step(q_accept, Rem, TQ, 0, 0, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Acc, FinalHistory) :-
    transition(CurrentState, NextState, Interpretation),
    CurrentState = state(Name, Rem, TQ, PT, PQ, _, _),
    HistoryEntry = step(Name, Rem, TQ, PT, PQ, Interpretation),
    run(NextState, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init, T, TQ, PT, PQ, KB, S), state(q_search_KB, T, TQ, PT, PQ, KB, S), Interp) :-
    format(string(Interp), 'Initialize: ~w / ~w. Loaded known facts for ~w.', [T, S, S]).

transition(state(q_search_KB, Rem, TQ, _, _, KB, S), state(q_apply_fact, Rem, TQ, Multiple, Factor, KB, S), Interp) :-
    find_best_fact(KB, Rem, Multiple, Factor),
    format(string(Interp), 'Found known multiple: ~w (~w x ~w).', [Multiple, Factor, S]).
transition(state(q_search_KB, Rem, TQ, _, _, KB, S), state(q_accept, Rem, TQ, 0, 0, KB, S), 'No suitable fact found.') :-
    \+ find_best_fact(KB, Rem, _, _).

transition(state(q_apply_fact, Rem, TQ, PT, PQ, KB, S), state(q_search_KB, NewRem, NewTQ, 0, 0, KB, S), Interp) :-
    NewRem is Rem - PT,
    NewTQ is TQ + PQ,
    format(string(Interp), 'Applied fact. Subtracted ~w. Added ~w to Quotient.', [PT, PQ]).

% find_best_fact helper
find_best_fact([Multiple-Factor | _], Rem, Multiple, Factor) :-
    Multiple =< Rem.
find_best_fact([_ | Rest], Rem, BestMultiple, BestFactor) :-
    find_best_fact(Rest, Rem, BestMultiple, BestFactor).
