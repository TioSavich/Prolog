/** <module> Bidirectional Counting Automaton (Up and Down)
 *
 * This module implements a Deterministic Pushdown Automaton (DPDA) that
 * simulates counting both forwards and backwards. It extends the functionality
 * of `counting2.pl` by handling two types of input events:
 * - `tick`: Increments the counter by one.
 * - `tock`: Decrements the counter by one.
 *
 * The automaton manages carrying (for `tick`) and borrowing (for `tock`)
 * across units, tens, and hundreds places, which are stored on the stack.
 * This provides a more complex model of cognitive counting processes.
 *
 * 
 * 
 */
:- module(counting_on_back,
          [ run_counter/3
          ]).

:- use_module(library(lists)).

%!      run_counter(+StartN:integer, +Ticks:list, -FinalValue:integer) is det.
%
%       Runs the bidirectional counting automaton.
%
%       This predicate initializes the DPDA's stack to represent `StartN`,
%       then processes a list of `Ticks`, where each element is either `tick`
%       (increment) or `tock` (decrement). Finally, it converts the resulting
%       stack back into an integer.
%
%       @param StartN The integer value to start counting from.
%       @param Ticks A list of `tick` and `tock` atoms.
%       @param FinalValue The final integer value after processing all ticks.
run_counter(StartN, Ticks, FinalValue) :-
    % Set up initial stack from the starting number.
    H is StartN // 100,
    T is (StartN mod 100) // 10,
    U is StartN mod 10,
    atom_concat('U', U, US), atom_concat('T', T, TS), atom_concat('H', H, HS),
    InitialStack = [US, TS, HS, '#'],
    InitialPDA = pda(q_idle, InitialStack),

    % Run the DPDA with the list of ticks/tocks.
    run_pda(InitialPDA, Ticks, FinalPDA),

    % Convert the final stack configuration to an integer.
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
% Defines the state transition rules for the up/down counter.

% --- Unit Transitions ---
% Increment (tick)
transition(pda(q_idle, [U|Rest]), tick, pda(q_idle, [NewU|Rest])) :-
    atom_concat('U', N_str, U), atom_number(N_str, N), N < 9, NewN is N + 1, atom_concat('U', NewN, NewU).
transition(pda(q_idle, ['U9'|Rest]), tick, pda(q_inc_tens, Rest)).
% Decrement (tock)
transition(pda(q_idle, [U|Rest]), tock, pda(q_idle, [NewU|Rest])) :-
    atom_concat('U', N_str, U), atom_number(N_str, N), N > 0, NewN is N - 1, atom_concat('U', NewN, NewU).
transition(pda(q_idle, ['U0'|Rest]), tock, pda(q_dec_tens, Rest)).


% --- Tens Transitions (Epsilon-driven) ---
% Carry from units
transition(pda(q_inc_tens, [T|Rest]), '', pda(q_idle, ['U0', NewT|Rest])) :-
    atom_concat('T', N_str, T), atom_number(N_str, N), N < 9, NewN is N + 1, atom_concat('T', NewN, NewT).
transition(pda(q_inc_tens, ['T9'|Rest]), '', pda(q_inc_hundreds, Rest)).
% Borrow from tens
transition(pda(q_dec_tens, [T|Rest]), '', pda(q_idle, ['U9', NewT|Rest])) :-
    atom_concat('T', N_str, T), atom_number(N_str, N), N > 0, NewN is N - 1, atom_concat('T', NewN, NewT).
transition(pda(q_dec_tens, ['T0'|Rest]), '', pda(q_dec_hundreds, Rest)).


% --- Hundreds Transitions (Epsilon-driven) ---
% Carry from tens
transition(pda(q_inc_hundreds, [H|Rest]), '', pda(q_idle, ['U0', 'T0', NewH|Rest])) :-
    atom_concat('H', N_str, H), atom_number(N_str, N), N < 9, NewN is N + 1, atom_concat('H', NewN, NewH).
transition(pda(q_inc_hundreds, ['H9'|Rest]), '', pda(q_halt, ['U0', 'T0', 'H0'|Rest])).
% Borrow from hundreds
transition(pda(q_dec_hundreds, [H|Rest]), '', pda(q_idle, ['U9', 'T9', NewH|Rest])) :-
    atom_concat('H', N_str, H), atom_number(N_str, N), N > 0, NewN is N - 1, atom_concat('H', NewN, NewH).
transition(pda(q_dec_hundreds, ['H0'|Rest]), '', pda(q_underflow, ['U9', 'T9', 'H9'|Rest])).


% stack_to_int(+Stack, -Value)
%
% Converts the final stack representation back into an integer.
stack_to_int(['U0', 'T0', 'H0', '#'], 0).
stack_to_int([U, T, H, '#'], Value) :-
    atom_concat('U', U_str, U), atom_number(U_str, U_val),
    atom_concat('T', T_str, T), atom_number(T_str, T_val),
    atom_concat('H', H_str, H), atom_number(H_str, H_val),
    Value is U_val + T_val * 10 + H_val * 100.
