% SWI-Prolog code for the 'Conversion to Bases and Ones' (CBO) multiplication strategy.

:- module(smr_mult_cbo,
          [ run_cbo_mult/5 % N, S, Base, FinalTotal, History
          ]).

:- use_module(library(lists)).

% State: state(Name, Groups, SourceIdx, TargetIdx)
% History: step(Name, Groups, Interpretation)

run_cbo_mult(N, S, Base, FinalTotal, History) :-
    (N > 0 -> length(Groups, N), maplist(=(S), Groups) ; Groups = []),
    (N > 0 -> SourceIdx is N - 1 ; SourceIdx = -1),
    InitialState = state(q_init, Groups, SourceIdx, 0),

    run(InitialState, Base, [], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, FinalGroups, _)),
     calculate_total(FinalGroups, Base, FinalTotal) -> true ; FinalTotal = 'error').

run(state(q_accept, Gs, _, _), Base, Acc, FinalHistory) :-
    calculate_total(Gs, Base, Total),
    format(string(Interpretation), 'Final Tally. Total = ~w.', [Total]),
    HistoryEntry = step(q_accept, Gs, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, Base, Acc, FinalHistory) :-
    transition(CurrentState, Base, NextState, Interpretation),
    CurrentState = state(Name, Gs, _, _),
    HistoryEntry = step(Name, Gs, Interpretation),
    run(NextState, Base, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init, Gs, SourceIdx, TI), _, state(q_select_source, Gs, SourceIdx, TI), 'Initialized groups.').

transition(state(q_select_source, Gs, SourceIdx, TI), _, state(q_init_transfer, Gs, SourceIdx, TI), Interp) :-
    (SourceIdx >= 0 ->
        SI1 is SourceIdx + 1,
        format(string(Interp), 'Selected Group ~w as the source.', [SI1])
    ;
        Interp = 'No groups to process.'
    ).

transition(state(q_init_transfer, Gs, SI, _), _, state(q_loop_transfer, Gs, SI, 0),
           'Starting redistribution loop.').

transition(state(q_loop_transfer, Gs, SI, TI), Base, state(q_loop_transfer, NewGs, SI, NewTI), Interp) :-
    % Conditions for transfer
    nth0(SI, Gs, SourceItems), SourceItems > 0,
    length(Gs, N), TI < N,
    (TI =\= SI ->
        nth0(TI, Gs, TargetItems), TargetItems < Base,
        % Perform transfer
        update_list(Gs, SI, SourceItems - 1, Gs_mid),
        update_list(Gs_mid, TI, TargetItems + 1, NewGs),
        % Check if target is now full
        (TargetItems + 1 =:= Base -> NewTI is TI + 1 ; NewTI is TI),
        format(string(Interp), 'Transferred 1 from ~w to ~w.', [SI+1, TI+1])
    ;
        % Skip source index
        NewTI is TI + 1, NewGs = Gs, Interp = 'Skipping source index.'
    ).
transition(state(q_loop_transfer, Gs, SI, TI), _, state(q_finalize, Gs, SI, TI), 'Redistribution complete.') :-
    % Exit conditions
    (nth0(SI, Gs, 0) ; length(Gs, N), TI >= N).

transition(state(q_finalize, Gs, SI, TI), _, state(q_accept, Gs, SI, TI), 'Finalizing.').

% Helpers
update_list(List, Index, NewVal, NewList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, NewList, NewVal, Rest).

calculate_total([], _, 0).
calculate_total([H|T], Base, Total) :-
    calculate_total(T, Base, RestTotal),
    Total is H + RestTotal.
