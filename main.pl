:- use_module(execution_handler).

% The goal is to calculate 5 + 5.
% s(s(s(s(s(0))))) represents 5 in Peano arithmetic.
main :-
    run_query(add(s(s(s(s(s(0))))), s(s(s(s(s(0))))), X)),
    format('Final Result: ~w~n', [X]),
    halt.

% This directive makes it so that running the script from the command line
% will automatically call the main/0 predicate.
:- initialization(main, main).