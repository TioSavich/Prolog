% SWI-Prolog code for the 'CBBO (Counting Back - Take Away)' subtraction strategy.

:- module(sar_sub_cbbo_take_away,
          [ run_cbbo_ta/4 % M, S, FinalResult, History
          ]).

:- use_module(library(lists)).

% State: state(Name, CurrentValue, BaseCounter, OneCounter)
% History: step(Name, CV, BC, OC, Interpretation)

run_cbbo_ta(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        BC is S // Base,
        OC is S mod Base,
        InitialState = state(q_init, M, BC, OC),
        format(string(InitialInterpretation), 'Initialize at M (~w). Decompose S (~w): ~w bases, ~w ones.', [M, S, BC, OC]),
        InitialHistoryEntry = step(q_start, M, 0, 0, InitialInterpretation),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, CV, _, _, _)) ->
            FinalResult = CV
        ;
            FinalResult = 'error'
        )
    ).

run(state(q_accept, CV, BC, OC), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Subtraction finished. Result (Final Position) = ~w.', [CV]),
    HistoryEntry = step(q_accept, CV, BC, OC, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, CV, BC, OC),
    HistoryEntry = step(Name, CV, BC, OC, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init, CV, BC, OC), _, state(q_sub_bases, CV, BC, OC),
           'Proceed to subtract bases.').

transition(state(q_sub_bases, CV, BC, OC), Base, state(q_sub_bases, NewCV, NewBC, OC), Interp) :-
    BC > 0,
    NewCV is CV - Base,
    NewBC is BC - 1,
    format(string(Interp), 'Count back by base (-~w). New Value=~w.', [Base, NewCV]).
transition(state(q_sub_bases, CV, 0, OC), _, state(q_sub_ones, CV, 0, OC),
           'Bases finished. Switching to ones.').

transition(state(q_sub_ones, CV, BC, OC), _, state(q_sub_ones, NewCV, BC, NewOC), Interp) :-
    OC > 0,
    NewCV is CV - 1,
    NewOC is OC - 1,
    format(string(Interp), 'Count back by one (-1). New Value=~w.', [NewCV]).
transition(state(q_sub_ones, CV, BC, 0), _, state(q_accept, CV, BC, 0),
           'Subtraction finished.').
