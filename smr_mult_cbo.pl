/** <module> Student Multiplication Strategy: Conversion to Bases and Ones (CBO)
 *
 * This module implements a multiplication strategy based on the physical act
 * of creating groups and then re-grouping (converting) them into a standard
 * base, like 10. It's modeled as a finite state machine.
 *
 * The process is as follows:
 * 1.  Start with `N` groups, each containing `S` items.
 * 2.  Systematically take items from one "source" group and redistribute them
 *     one-by-one into other "target" groups.
 * 3.  The goal of the redistribution is to fill the target groups until they
 *     contain `Base` items (e.g., 10).
 * 4.  This process continues until the source group is empty.
 * 5.  The final total is calculated by summing the items in all the rearranged
 *     groups. This demonstrates the principle of conservation of number, as the
 *     total remains `N * S` despite the redistribution.
 *
 * The state is represented by the term:
 * `state(Name, Groups, SourceIndex, TargetIndex)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, Groups, Interpretation)`
 *
 * 
 * 
 */
:- module(smr_mult_cbo,
          [ run_cbo_mult/5
          ]).

:- use_module(library(lists)).

%!      run_cbo_mult(+N:integer, +S:integer, +Base:integer, -FinalTotal:integer, -History:list) is det.
%
%       Executes the 'Conversion to Bases and Ones' multiplication strategy
%       for N * S, using a target Base for re-grouping.
%
%       This predicate initializes and runs a state machine that models the
%       conceptual process of redistribution. It creates `N` groups of `S` items
%       and then shuffles items between them to form groups of size `Base`.
%       The final total demonstrates that the quantity is conserved.
%
%       @param N The number of initial groups.
%       @param S The size of each initial group.
%       @param Base The target size for the re-grouping.
%       @param FinalTotal The resulting product (N * S).
%       @param History A list of `step/3` terms that describe the state
%       machine's execution path and the interpretation of each step.

run_cbo_mult(N, S, Base, FinalTotal, History) :-
    (N > 0 -> length(Groups, N), maplist(=(S), Groups) ; Groups = []),
    (N > 0 -> SourceIdx is N - 1 ; SourceIdx = -1),
    InitialState = state(q_init, Groups, SourceIdx, 0),

    run(InitialState, Base, [], ReversedHistory),
    reverse(ReversedHistory, History),

    (last(History, step(q_accept, FinalGroups, _)),
     calculate_total(FinalGroups, Base, FinalTotal) -> true ; FinalTotal = 'error').

% run/4 is the main recursive loop of the state machine.
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

% transition/4 defines the logic for moving from one state to the next.

% From q_init, select a source group to begin redistribution.
transition(state(q_init, Gs, SourceIdx, TI), _, state(q_select_source, Gs, SourceIdx, TI), 'Initialized groups.').

% From q_select_source, confirm the source and begin the transfer process.
transition(state(q_select_source, Gs, SourceIdx, TI), _, state(q_init_transfer, Gs, SourceIdx, TI), Interp) :-
    (SourceIdx >= 0 ->
        SI1 is SourceIdx + 1,
        format(string(Interp), 'Selected Group ~w as the source.', [SI1])
    ;
        Interp = 'No groups to process.'
    ).

% From q_init_transfer, start the main redistribution loop.
transition(state(q_init_transfer, Gs, SI, _), _, state(q_loop_transfer, Gs, SI, 0),
           'Starting redistribution loop.').

% In q_loop_transfer, move one item from the source group to a target group.
transition(state(q_loop_transfer, Gs, SI, TI), Base, state(q_loop_transfer, NewGs, SI, NewTI), Interp) :-
    % Conditions for transfer: source has items, target is not full.
    nth0(SI, Gs, SourceItems), SourceItems > 0,
    length(Gs, N), TI < N,
    (TI =\= SI ->
        nth0(TI, Gs, TargetItems), TargetItems < Base,
        % Perform transfer of one item.
        update_list(Gs, SI, SourceItems - 1, Gs_mid),
        update_list(Gs_mid, TI, TargetItems + 1, NewGs),
        % Check if target is now full, if so, advance target index.
        (TargetItems + 1 =:= Base -> NewTI is TI + 1 ; NewTI is TI),
        format(string(Interp), 'Transferred 1 from ~w to ~w.', [SI+1, TI+1])
    ;
        % Skip transferring to the source index itself.
        NewTI is TI + 1, NewGs = Gs, Interp = 'Skipping source index.'
    ).
% Exit the loop when the source is empty or all targets have been considered.
transition(state(q_loop_transfer, Gs, SI, TI), _, state(q_finalize, Gs, SI, TI), 'Redistribution complete.') :-
    (nth0(SI, Gs, 0) ; length(Gs, N), TI >= N).

% From q_finalize, move to the accept state.
transition(state(q_finalize, Gs, SI, TI), _, state(q_accept, Gs, SI, TI), 'Finalizing.').

% update_list/4 is a helper to non-destructively update a list element at an index.
update_list(List, Index, NewVal, NewList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, NewList, NewVal, Rest).

% calculate_total/3 is a helper to sum the elements of the final groups list.
% Note: The Base is not used, as this just verifies the total number of items.
calculate_total([], _, 0).
calculate_total([H|T], Base, Total) :-
    calculate_total(T, Base, RestTotal),
    Total is H + RestTotal.
