% SWI-Prolog code for the 'Rounding and Adjusting' strategy.

:- module(sar_add_rounding,
          [ run_rounding/4 % A, B, FinalResult, History
          ]).

:- use_module(library(lists)).

% State: state(Name, K, A_rounded, TempSum, Result, Target, Other, TargetBase, BaseCounter, OneCounter)
% History: step(Name, K, A_rounded, TempSum, Result, Interpretation)

determine_target(A_in, B_in, Base, Target, Other) :-
    A_rem is A_in mod Base,
    B_rem is B_in mod Base,
    (A_rem >= B_rem ->
        (Target = A_in, Other = B_in)
    ;
        (Target = B_in, Other = A_in)
    ).

run_rounding(A_in, B_in, FinalResult, History) :-
    Base = 10,
    determine_target(A_in, B_in, Base, Target, Other),

    % q_init_K
    (Target =< 0 -> TB = 0 ; (Target mod Base =:= 0 -> TB = Target ; TB is ((Target // Base) + 1) * Base)),
    InitialState = state(q_init_K, 0, Target, 0, 0, Target, Other, TB, 0, 0),

    format(string(InitialInterpretation), 'Inputs: ~w, ~w. Target for rounding: ~w', [A_in, B_in, Target]),
    InitialHistoryEntry = step(q_start, 0, 0, 0, 0, InitialInterpretation),

    run(InitialState, Base, [InitialHistoryEntry], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, _, _, _, R, _)) -> FinalResult = R ; FinalResult = 'error').

run(state(q_accept, K, AR, TS, Result, _, _, _, _, _), _, Acc, FinalHistory) :-
    HistoryEntry = step(q_accept, K, AR, TS, Result, 'Execution finished.'),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, K, AR, TS, Result, _, _, _, _, _),
    HistoryEntry = step(Name, K, AR, TS, Result, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init_K, K, AR, TS, R, T, O, TB, BC, OC), _, state(q_loop_K, K, AR, TS, R, T, O, TB, BC, OC), Interp) :-
    format(string(Interp), 'Initializing K calculation. Counting from ~w to ~w.', [T, TB]).

transition(state(q_loop_K, K, AR, TS, R, T, O, TB, BC, OC), _, state(q_loop_K, NewK, NewAR, TS, R, T, O, TB, BC, OC), Interp) :-
    AR < TB,
    NewK is K + 1, NewAR is AR + 1,
    format(string(Interp), 'Counting Up: ~w, K=~w', [NewAR, NewK]).
transition(state(q_loop_K, K, AR, TS, R, T, O, TB, BC, OC), _, state(q_init_Add, K, AR, TS, R, T, O, TB, BC, OC), Interp) :-
    AR >= TB,
    format(string(Interp), 'K needed is ~w. Target rounded to ~w.', [K, AR]).

% Phase 2: Addition (COBO)
transition(state(q_init_Add, K, AR, TS, R, T, O, TB, BC, OC), Base, state(q_loop_AddBases, K, AR, AR, R, T, O, TB, OBC, OOC), Interp) :-
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
