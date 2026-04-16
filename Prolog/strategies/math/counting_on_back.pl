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
:- use_module(formalization(grounded_arithmetic), [integer_to_recollection/2,
                                     recollection_to_integer/2, successor/2,
                                     predecessor/2, add_grounded/3,
                                     multiply_grounded/3, greater_than/2,
                                     smaller_than/2]).
:- use_module(formalization(grounded_utils), [base_decompose_grounded/4]).

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
    % Set up initial stack from the starting number using grounded decomposition.
    integer_to_recollection(StartN, RecStart),
    integer_to_recollection(100, Rec100),
    base_decompose_grounded(RecStart, Rec100, RecH, RecRem100),
    recollection_to_integer(RecH, H),
    integer_to_recollection(10, Rec10),
    base_decompose_grounded(RecRem100, Rec10, RecT, RecU),
    recollection_to_integer(RecT, T),
    recollection_to_integer(RecU, U),
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
    atom_concat('U', N_str, U), atom_number(N_str, N),
    integer_to_recollection(N, RecN), integer_to_recollection(9, Rec9),
    smaller_than(RecN, Rec9),
    successor(RecN, RecNewN), recollection_to_integer(RecNewN, NewN),
    atom_concat('U', NewN, NewU).
transition(pda(q_idle, ['U9'|Rest]), tick, pda(q_inc_tens, Rest)).
% Decrement (tock)
transition(pda(q_idle, [U|Rest]), tock, pda(q_idle, [NewU|Rest])) :-
    atom_concat('U', N_str, U), atom_number(N_str, N),
    integer_to_recollection(N, RecN2), integer_to_recollection(0, Rec0),
    greater_than(RecN2, Rec0),
    predecessor(RecN2, RecNewN2), recollection_to_integer(RecNewN2, NewN),
    atom_concat('U', NewN, NewU).
transition(pda(q_idle, ['U0'|Rest]), tock, pda(q_dec_tens, Rest)).


% --- Tens Transitions (Epsilon-driven) ---
% Carry from units
transition(pda(q_inc_tens, [T|Rest]), '', pda(q_idle, ['U0', NewT|Rest])) :-
    atom_concat('T', N_str, T), atom_number(N_str, N),
    integer_to_recollection(N, RecN3), integer_to_recollection(9, Rec9b),
    smaller_than(RecN3, Rec9b),
    successor(RecN3, RecNewN3), recollection_to_integer(RecNewN3, NewN),
    atom_concat('T', NewN, NewT).
transition(pda(q_inc_tens, ['T9'|Rest]), '', pda(q_inc_hundreds, Rest)).
% Borrow from tens
transition(pda(q_dec_tens, [T|Rest]), '', pda(q_idle, ['U9', NewT|Rest])) :-
    atom_concat('T', N_str, T), atom_number(N_str, N),
    integer_to_recollection(N, RecN4), integer_to_recollection(0, Rec0b),
    greater_than(RecN4, Rec0b),
    predecessor(RecN4, RecNewN4), recollection_to_integer(RecNewN4, NewN),
    atom_concat('T', NewN, NewT).
transition(pda(q_dec_tens, ['T0'|Rest]), '', pda(q_dec_hundreds, Rest)).


% --- Hundreds Transitions (Epsilon-driven) ---
% Carry from tens
transition(pda(q_inc_hundreds, [H|Rest]), '', pda(q_idle, ['U0', 'T0', NewH|Rest])) :-
    atom_concat('H', N_str, H), atom_number(N_str, N),
    integer_to_recollection(N, RecN5), integer_to_recollection(9, Rec9c),
    smaller_than(RecN5, Rec9c),
    successor(RecN5, RecNewN5), recollection_to_integer(RecNewN5, NewN),
    atom_concat('H', NewN, NewH).
transition(pda(q_inc_hundreds, ['H9'|Rest]), '', pda(q_halt, ['U0', 'T0', 'H0'|Rest])).
% Borrow from hundreds
transition(pda(q_dec_hundreds, [H|Rest]), '', pda(q_idle, ['U9', 'T9', NewH|Rest])) :-
    atom_concat('H', N_str, H), atom_number(N_str, N),
    integer_to_recollection(N, RecN6), integer_to_recollection(0, Rec0c),
    greater_than(RecN6, Rec0c),
    predecessor(RecN6, RecNewN6), recollection_to_integer(RecNewN6, NewN),
    atom_concat('H', NewN, NewH).
transition(pda(q_dec_hundreds, ['H0'|Rest]), '', pda(q_underflow, ['U9', 'T9', 'H9'|Rest])).


% stack_to_int(+Stack, -Value)
%
% Converts the final stack representation back into an integer using grounded arithmetic.
stack_to_int(['U0', 'T0', 'H0', '#'], 0).
stack_to_int([U, T, H, '#'], Value) :-
    atom_concat('U', U_str, U), atom_number(U_str, U_val),
    atom_concat('T', T_str, T), atom_number(T_str, T_val),
    atom_concat('H', H_str, H), atom_number(H_str, H_val),
    integer_to_recollection(U_val, RecU),
    integer_to_recollection(T_val, RecT),
    integer_to_recollection(H_val, RecH),
    integer_to_recollection(10, Rec10),
    integer_to_recollection(100, Rec100),
    multiply_grounded(RecT, Rec10, RecTens),
    multiply_grounded(RecH, Rec100, RecHundreds),
    add_grounded(RecU, RecTens, RecPartial),
    add_grounded(RecPartial, RecHundreds, RecValue),
    recollection_to_integer(RecValue, Value).
