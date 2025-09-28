% SWI-Prolog code for the 'COBO (Counting On - Missing Addend)' subtraction strategy.

:- module(sar_sub_cobo_missing_addend,
          [ run_cobo_ma/4 % M, S, FinalResult, History
          ]).

:- use_module(library(lists)).

% State: state(Name, CurrentValue, Distance, Target)
% History: step(Name, CV, Dist, Interpretation)

run_cobo_ma(M, S, FinalResult, History) :-
    Base = 10,
    (S > M ->
        History = [step(q_error, 0, 0, 'Error: Subtrahend > Minuend.')],
        FinalResult = 'error'
    ;
        InitialState = state(q_init, S, 0, M),
        format(string(InitialInterpretation), 'Initialize at S (~w). Target is M (~w).', [S, M]),
        InitialHistoryEntry = step(q_start, 0, 0, InitialInterpretation),

        run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, _, Dist, _)) -> FinalResult = Dist ; FinalResult = 'error')
    ).

run(state(q_accept, CV, Dist, _), _, Acc, FinalHistory) :-
    format(string(Interpretation), 'Target reached. Result (Distance) = ~w.', [Dist]),
    HistoryEntry = step(q_accept, CV, Dist, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, CV, Dist, _),
    HistoryEntry = step(Name, CV, Dist, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init, CV, Dist, T), _, state(q_add_bases, CV, Dist, T),
           'Proceed to add bases.').

transition(state(q_add_bases, CV, Dist, T), Base, state(q_add_bases, NewCV, NewDist, T), Interp) :-
    CV + Base =< T,
    NewCV is CV + Base,
    NewDist is Dist + Base,
    format(string(Interp), 'Count on by base (+~w). New Value=~w.', [Base, NewCV]).
transition(state(q_add_bases, CV, Dist, T), Base, state(q_add_ones, CV, Dist, T),
           'Next base overshoots target. Switching to ones.') :-
    CV + Base > T.

transition(state(q_add_ones, CV, Dist, T), _, state(q_add_ones, NewCV, NewDist, T), Interp) :-
    CV < T,
    NewCV is CV + 1,
    NewDist is Dist + 1,
    format(string(Interp), 'Count on by one (+1). New Value=~w.', [NewCV]).
transition(state(q_add_ones, T, Dist, T), _, state(q_accept, T, Dist, T),
           'Target reached.') :-
    % CV is equal to T here
    true.
