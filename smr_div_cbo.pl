% SWI-Prolog code for the 'Conversion to Groups Other than Bases' division strategy.

:- module(smr_div_cbo,
          [ run_cbo_div/5 % T, S, Base, FinalQuotient, FinalRemainder
          ]).

:- use_module(library(lists)).

% State: state(Name, T_Bases, T_Ones, Q, R, S_in_B, R_in_B, T, S)
% History: step(Name, Q, R, Interpretation)

run_cbo_div(T, S, Base, FinalQuotient, FinalRemainder) :-
    (S =< 0 ->
        History = [step(q_error, 0, 0, 'Error: Divisor must be positive.')],
        FinalQuotient = 'error', FinalRemainder = 'error'
    ;
        TB is T // Base,
        TO is T mod Base,
        InitialState = state(q_init, TB, TO, 0, 0, 0, 0, T, S),

        run(InitialState, Base, [], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, FinalQuotient, FinalRemainder, _)) -> true ;
         (FinalQuotient = 'error', FinalRemainder = 'error'))
    ).

run(state(q_accept, _, _, Q, R, _, _, _, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Finished. Total Quotient = ~w.', [Q]),
    HistoryEntry = step(q_accept, Q, R, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, _, _, Q, R, _, _, _, _),
    HistoryEntry = step(Name, Q, R, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init, TB, TO, Q, R, SiB, RiB, T, S), _, state(q_analyze_base, TB, TO, Q, R, SiB, RiB, T, S), Interp) :-
    format(string(Interp), 'Initialize: ~w/~w. Decompose T: ~w Bases + ~w Ones.', [T, S, TB, TO]).

transition(state(q_analyze_base, TB, TO, Q, R, _, _, T, S), Base, state(q_process_bases, TB, TO, Q, R, SiB, RiB, T, S), Interp) :-
    SiB is Base // S,
    RiB is Base mod S,
    format(string(Interp), 'Analyze Base: One Base (~w) = ~w group(s) of ~w + Remainder ~w.', [Base, SiB, S, RiB]).

transition(state(q_process_bases, TB, TO, _, _, SiB, RiB, T, S), _, state(q_combine_R, TB, TO, NewQ, NewR, SiB, RiB, T, S), Interp) :-
    NewQ is TB * SiB,
    NewR is TB * RiB,
    format(string(Interp), 'Process ~w Bases: Yields ~w groups and ~w remainder.', [TB, NewQ, NewR]).

transition(state(q_combine_R, _, TO, Q, R, SiB, RiB, T, S), _, state(q_process_R, _, TO, Q, NewR, SiB, RiB, T, S), Interp) :-
    NewR is R + TO,
    format(string(Interp), 'Combine Remainders: ~w (from Bases) + ~w (from Ones) = ~w.', [R, TO, NewR]).

transition(state(q_process_R, _, _, Q, R, _, _, T, S), _, state(q_accept, _, _, NewQ, NewR, _, _, T, S), Interp) :-
    Q_from_R is R // S,
    NewR is R mod S,
    NewQ is Q + Q_from_R,
    format(string(Interp), 'Process Remainder: Yields ~w additional group(s).', [Q_from_R]).
