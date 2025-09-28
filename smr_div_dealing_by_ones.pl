% SWI-Prolog code for the 'Dealing by Ones' division strategy.

:- module(smr_div_dealing_by_ones,
          [ run_dealing_by_ones/4 % T, N, FinalQuotient, History
          ]).

:- use_module(library(lists)).

% State: state(Name, Remaining, Groups, CurrentIdx)
% History: step(Name, Remaining, Groups, Interpretation)

run_dealing_by_ones(T, N, FinalQuotient, History) :-
    (N =< 0, T > 0 ->
        History = [step(q_error, T, [], 'Error: Cannot divide by N.')],
        FinalQuotient = 'error'
    ;
        % Create a list of N zeros
        length(Groups, N),
        maplist(=(0), Groups),
        InitialState = state(q_init, T, Groups, 0),

        run(InitialState, N, [], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, _, FinalGroups, _)), nth0(0, FinalGroups, FinalQuotient) -> true ; FinalQuotient = 'error')
    ).

run(state(q_accept, 0, Groups, _), _, Acc, FinalHistory) :-
    (nth0(0, Groups, R) -> Result = R ; Result = 0),
    format(string(Interpretation), 'Dealing complete. Result: ~w per group.', [Result]),
    HistoryEntry = step(q_accept, 0, Groups, Interpretation),
    FinalHistory = [HistoryEntry | Acc].

run(CurrentState, N, Acc, FinalHistory) :-
    transition(CurrentState, N, NextState, Interpretation),
    CurrentState = state(Name, Rem, Gs, _),
    HistoryEntry = step(Name, Rem, Gs, Interpretation),
    run(NextState, N, [HistoryEntry | Acc], FinalHistory).

% Transitions
transition(state(q_init, T, Gs, Idx), _, state(q_loop_deal, T, Gs, Idx), Interp) :-
    length(Gs, N),
    format(string(Interp), 'Initialize: ~w items to deal into ~w groups.', [T, N]).

transition(state(q_loop_deal, Rem, Gs, Idx), N, state(q_loop_deal, NewRem, NewGs, NewIdx), Interp) :-
    Rem > 0,
    NewRem is Rem - 1,
    % Increment value in list at index Idx
    nth0(Idx, Gs, OldVal, Rest),
    NewVal is OldVal + 1,
    nth0(Idx, NewGs, NewVal, Rest),
    NewIdx is (Idx + 1) mod N,
    format(string(Interp), 'Dealt 1 item to Group ~w.', [Idx+1]).
transition(state(q_loop_deal, 0, Gs, Idx), _, state(q_accept, 0, Gs, Idx), 'Dealing complete.').
