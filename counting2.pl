% SWI-Prolog code for the 0-999 Counter DPDA.

:- module(counting2,
          [ run_counter/2 % N, FinalValue
          ]).

:- use_module(library(lists)).

% DPDA configuration: pda(State, Stack)
% Stack is a list, with the head being the top of the stack.

run_counter(N, FinalValue) :-
    % Generate the input sequence
    length(Input, N),
    maplist(=(tick), Input),

    % Initial configuration
    InitialPDA = pda(q_start, ['#']),

    % Run the DPDA
    run_pda(InitialPDA, Input, FinalPDA),

    % Convert final stack to integer
    FinalPDA = pda(_, FinalStack),
    stack_to_int(FinalStack, FinalValue).

run_pda(PDA, [], PDA).
run_pda(PDA, [Input|Rest], FinalPDA) :-
    transition(PDA, Input, NextPDA),
    run_pda(NextPDA, Rest, FinalPDA).
run_pda(pda(State, Stack), [], pda(FinalState, FinalStack)) :-
    transition(pda(State, Stack), '', pda(FinalState, FinalStack)),
    \+ transition(pda(FinalState, FinalStack), '', _), % ensure it's a final epsilon transition
    !.

% Transitions
% Epsilon transition from start
transition(pda(q_start, ['#']), '', pda(q_idle, ['U0', 'T0', 'H0', '#'])).

% Unit transitions
transition(pda(q_idle, [U|Rest]), tick, pda(q_idle, [NewU|Rest])) :-
    atom_concat('U', N_str, U), atom_number(N_str, N), N < 9, NewN is N + 1, atom_concat('U', NewN, NewU).
transition(pda(q_idle, ['U9'|Rest]), tick, pda(q_inc_tens, Rest)).

% Tens transitions (epsilon)
transition(pda(q_inc_tens, [T|Rest]), '', pda(q_idle, ['U0', NewT|Rest])) :-
    atom_concat('T', N_str, T), atom_number(N_str, N), N < 9, NewN is N + 1, atom_concat('T', NewN, NewT).
transition(pda(q_inc_tens, ['T9'|Rest]), '', pda(q_inc_hundreds, Rest)).

% Hundreds transitions (epsilon)
transition(pda(q_inc_hundreds, [H|Rest]), '', pda(q_idle, ['U0', 'T0', NewH|Rest])) :-
    atom_concat('H', N_str, H), atom_number(N_str, N), N < 9, NewN is N + 1, atom_concat('H', NewN, NewH).
transition(pda(q_inc_hundreds, ['H9'|Rest]), '', pda(q_halt, ['U0', 'T0', 'H0'|Rest])).


% Stack to Int conversion
stack_to_int(['U0', 'T0', 'H0', '#'], 0).
stack_to_int([U, T, H, '#'], Value) :-
    atom_concat('U', U_str, U), atom_number(U_str, U_val),
    atom_concat('T', T_str, T), atom_number(T_str, T_val),
    atom_concat('H', H_str, H), atom_number(H_str, H_val),
    Value is U_val + T_val * 10 + H_val * 100.
