/** <module> Deterministic Pushdown Automaton for Counting
 *
 * This module implements a Deterministic Pushdown Automaton (DPDA) that
 * simulates the cognitive process of counting from 0 up to a specified number.
 * It models how units, tens, and hundreds are incremented and "carry over,"
 * similar to an odometer.
 *
 * The automaton's configuration is represented by `pda(State, Stack)`. The
 * stack is used to store the current count, with separate atoms for the
 * units, tens, and hundreds places (e.g., `['U5', 'T2', 'H1', '#']` for 125).
 * The input to the automaton is a series of `tick` events, each causing the
 * counter to increment by one.
 *
 * 
 * 
 */
:- module(counting2,
          [ run_counter/2
          ]).

:- use_module(library(lists)).

%!      run_counter(+N:integer, -FinalValue:integer) is det.
%
%       Runs the counting automaton for `N` steps and returns the final value.
%
%       This predicate generates an input list of `N` `tick` atoms,
%       initializes the DPDA, runs the simulation, and then converts the
%       final stack configuration back into an integer result.
%
%       @param N The number of times to "tick" the counter, effectively the
%       number to count up to.
%       @param FinalValue The integer value represented by the automaton's
%       stack after `N` increments.
run_counter(N, FinalValue) :-
    % Generate the input sequence of N 'tick' events.
    length(Input, N),
    maplist(=(tick), Input),

    % Initial DPDA configuration: start state with an empty stack marker.
    InitialPDA = pda(q_start, ['#']),

    % Run the DPDA simulation.
    run_pda(InitialPDA, Input, FinalPDA),

    % Convert the final stack configuration to an integer value.
    FinalPDA = pda(_, FinalStack),
    stack_to_int(FinalStack, FinalValue).

% run_pda(+PDA, +Input, -FinalPDA)
%
% The main recursive loop that drives the automaton.
run_pda(PDA, [], PDA).
run_pda(PDA, [Input|Rest], FinalPDA) :-
    transition(PDA, Input, NextPDA),
    run_pda(NextPDA, Rest, FinalPDA).
run_pda(pda(State, Stack), [], pda(FinalState, FinalStack)) :-
    transition(pda(State, Stack), '', pda(FinalState, FinalStack)),
    \+ transition(pda(FinalState, FinalStack), '', _), % ensure it's a final epsilon transition
    !.

% transition(+CurrentPDA, +Input, -NextPDA)
%
% Defines the state transition rules for the counting automaton.

% Epsilon transition from start to initialize the counter stack.
transition(pda(q_start, ['#']), '', pda(q_idle, ['U0', 'T0', 'H0', '#'])).

% --- Unit Transitions ---
% If units are not 9, just increment the unit counter.
transition(pda(q_idle, [U|Rest]), tick, pda(q_idle, [NewU|Rest])) :-
    atom_concat('U', N_str, U), atom_number(N_str, N), N < 9, NewN is N + 1, atom_concat('U', NewN, NewU).
% If units are 9, transition to increment the tens place.
transition(pda(q_idle, ['U9'|Rest]), tick, pda(q_inc_tens, Rest)).

% --- Tens Transitions (Epsilon) ---
% After incrementing units from 9, reset units to 0 and increment tens.
transition(pda(q_inc_tens, [T|Rest]), '', pda(q_idle, ['U0', NewT|Rest])) :-
    atom_concat('T', N_str, T), atom_number(N_str, N), N < 9, NewN is N + 1, atom_concat('T', NewN, NewT).
% If tens are also 9, transition to increment the hundreds place.
transition(pda(q_inc_tens, ['T9'|Rest]), '', pda(q_inc_hundreds, Rest)).

% --- Hundreds Transitions (Epsilon) ---
% After incrementing tens from 9, reset units/tens and increment hundreds.
transition(pda(q_inc_hundreds, [H|Rest]), '', pda(q_idle, ['U0', 'T0', NewH|Rest])) :-
    atom_concat('H', N_str, H), atom_number(N_str, N), N < 9, NewN is N + 1, atom_concat('H', NewN, NewH).
% If hundreds are also 9, we have overflowed; halt.
transition(pda(q_inc_hundreds, ['H9'|Rest]), '', pda(q_halt, ['U0', 'T0', 'H0'|Rest])).


% stack_to_int(+Stack, -Value)
%
% Converts the final stack representation back into an integer.
stack_to_int(['U0', 'T0', 'H0', '#'], 0).
stack_to_int([U, T, H, '#'], Value) :-
    atom_concat('U', U_str, U), atom_number(U_str, U_val),
    atom_concat('T', T_str, T), atom_number(T_str, T_val),
    atom_concat('H', H_str, H), atom_number(H_str, H_val),
    Value is U_val + T_val * 10 + H_val * 100.
