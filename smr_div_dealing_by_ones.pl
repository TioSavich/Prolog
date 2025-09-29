/** <module> Student Division Strategy: Dealing by Ones
 *
 * This module implements a basic "dealing" or "sharing one by one" strategy
 * for division (T / N), modeled as a finite state machine using the FSM engine.
 * It simulates distributing a total number of items (T) one at a time into a 
 * number of groups (N) until the items run out.
 *
 * @author Assistant
 * @license MIT
 */

:- module(smr_div_dealing_by_ones,
          [ run_dealing_by_ones/4,
            % FSM Engine Interface
            transition/4,
            accept_state/1, 
            final_interpretation/2, 
            extract_result_from_history/2
          ]).

:- use_module(library(lists)).
:- use_module(fsm_engine, [run_fsm_with_base/5]).
:- use_module(grounded_arithmetic, [incur_cost/1]).
:- use_module(incompatibility_semantics, [s/1, comp_nec/1, exp_poss/1]).

%! run_dealing_by_ones(+T:int, +N:int, -FinalQuotient:int, -History:list) is det.
:- use_module(incompatibility_semantics, [s/1, comp_nec/1, exp_poss/1]).

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
        History = [step(state(q_error, T, [], 0), [], 'Error: Cannot divide by N.')],
        FinalQuotient = 'error'
    ;
        % Create a list of N zeros to represent the groups.
        length(Groups, N),
        maplist(=(0), Groups),
        InitialState = state(q_init, T, Groups, 0),
        Parameters = [T, N],
        ModalCosts = [
            s(initiating_dealing_by_ones_division),
            s(comp_nec(systematic_dealing_process_for_division)),
            s(exp_poss(fair_distribution_of_items_into_groups))
        ],
        incur_cost(ModalCosts),
        
        run_fsm_with_base(smr_div_dealing_by_ones, InitialState, Parameters, _, History),
        extract_result_from_history(History, FinalQuotient)
    ).

% transition/4 defines the FSM engine transitions with modal logic integration.

% From q_init, proceed to the main dealing loop.
transition(state(q_init, T, Gs, Idx), [T, N], state(q_loop_deal, T, Gs, Idx), Interp) :-
    length(Gs, N),
    s(initializing_dealing_by_ones_division),
    format(string(Interp), 'Initialize: ~w items to deal into ~w groups.', [T, N]),
    incur_cost(initialization).

% In q_loop_deal, deal one item to the current group and cycle to the next.
transition(state(q_loop_deal, Rem, Gs, Idx), [T, N], state(q_loop_deal, NewRem, NewGs, NewIdx), Interp) :-
    Rem > 0,
    NewRem is Rem - 1,
    % Increment value in the list at the current group index.
    nth0(Idx, Gs, OldVal, Rest),
    NewVal is OldVal + 1,
    nth0(Idx, NewGs, NewVal, Rest),
    NewIdx is (Idx + 1) mod N,
    s(comp_nec(dealing_one_item_systematically)),
    format(string(Interp), 'Dealt 1 item to Group ~w.', [Idx+1]),
    incur_cost(iteration).
    
% If no items remain, transition to the accept state.
transition(state(q_loop_deal, 0, Gs, Idx), [T, N], state(q_accept, 0, Gs, Idx), Interp) :-
    s(exp_poss(complete_fair_distribution_achieved)),
    Interp = 'Dealing complete.',
    incur_cost(completion).

% Accept state predicate for FSM engine
accept_state(state(q_accept, 0, _, _)).

% Final interpretation predicate
final_interpretation(state(q_accept, 0, Groups, _), Interpretation) :-
    (nth0(0, Groups, Result) -> true ; Result = 0),
    format(string(Interpretation), 'Division complete. Result: ~w per group.', [Result]).

% Extract result from FSM engine history
extract_result_from_history(History, FinalQuotient) :-
    last(History, step(state(q_accept, 0, FinalGroups, _), [], _)),
    (nth0(0, FinalGroups, FinalQuotient) -> true ; FinalQuotient = 0).
