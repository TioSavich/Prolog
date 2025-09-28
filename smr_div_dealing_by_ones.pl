/** <module> Student Division Strategy: Dealing by Ones
 *
 * This module implements a basic "dealing" or "sharing one by one" strategy
 * for division (T / N), modeled as a finite state machine. It simulates
 * distributing a total number of items (T) one at a time into a number of
 * groups (N) until the items run out.
 *
 * The process is as follows:
 * 1. Initialize N empty groups.
 * 2. Deal one item from the total T to the first group.
 * 3. Deal one item to the second group, and so on, cycling through the groups.
 * 4. Continue until all T items have been dealt.
 * 5. The quotient is the number of items in any one group (assuming fair sharing,
 *    i.e., the remainder is 0). This model does not explicitly calculate a remainder.
 * 6. The strategy fails if the number of groups (N) is not positive.
 *
 * The state is represented by the term:
 * `state(Name, RemainingItems, Groups, CurrentGroupIndex)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, RemainingItems, Groups, Interpretation)`
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(smr_div_dealing_by_ones,
          [ run_dealing_by_ones/4
          ]).

:- use_module(library(lists)).

%!      run_dealing_by_ones(+T:integer, +N:integer, -FinalQuotient:integer, -History:list) is det.
%
%       Executes the 'Dealing by Ones' division strategy for T / N.
%
%       This predicate initializes and runs a state machine that models the
%       process of dealing `T` items one by one into `N` groups. It first
%       checks for a positive number of groups `N`. If valid, it simulates
%       the dealing process and traces the execution. The quotient is the
%       final number of items in one of the groups.
%
%       @param T The Dividend (Total number of items to deal).
%       @param N The Divisor (Number of groups to deal into).
%       @param FinalQuotient The result of the division (items per group).
%       If N is not positive, this will be the atom `'error'`.
%       @param History A list of `step/4` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_dealing_by_ones(T, N, FinalQuotient, History) :-
    (N =< 0, T > 0 ->
        History = [step(q_error, T, [], 'Error: Cannot divide by N.')],
        FinalQuotient = 'error'
    ;
        % Create a list of N zeros to represent the groups.
        length(Groups, N),
        maplist(=(0), Groups),
        InitialState = state(q_init, T, Groups, 0),

        run(InitialState, N, [], ReversedHistory),
        reverse(ReversedHistory, History),

        (last(History, step(q_accept, _, FinalGroups, _)), nth0(0, FinalGroups, FinalQuotient) -> true ; FinalQuotient = 'error')
    ).

% run/4 is the main recursive loop of the state machine.
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

% transition/4 defines the logic for moving from one state to the next.

% From q_init, proceed to the main dealing loop.
transition(state(q_init, T, Gs, Idx), _, state(q_loop_deal, T, Gs, Idx), Interp) :-
    length(Gs, N),
    format(string(Interp), 'Initialize: ~w items to deal into ~w groups.', [T, N]).

% In q_loop_deal, deal one item to the current group and cycle to the next.
transition(state(q_loop_deal, Rem, Gs, Idx), N, state(q_loop_deal, NewRem, NewGs, NewIdx), Interp) :-
    Rem > 0,
    NewRem is Rem - 1,
    % Increment value in the list at the current group index.
    nth0(Idx, Gs, OldVal, Rest),
    NewVal is OldVal + 1,
    nth0(Idx, NewGs, NewVal, Rest),
    NewIdx is (Idx + 1) mod N,
    format(string(Interp), 'Dealt 1 item to Group ~w.', [Idx+1]).
% If no items remain, transition to the accept state.
transition(state(q_loop_deal, 0, Gs, Idx), _, state(q_accept, 0, Gs, Idx), 'Dealing complete.').
