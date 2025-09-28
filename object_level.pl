:- module(object_level, [add/3]).
% We must declare predicates that will be modified as dynamic.
:- dynamic add/3.

% Helper to force enumeration (consumes inferences proportional to N)
enumerate(0).
enumerate(s(N)) :- enumerate(N).

% Standard recursive addition (the efficient core we will reuse later)
recursive_add(0, B, B).
recursive_add(s(A), B, s(Sum)) :-
    recursive_add(A, B, Sum).

% Initial definition of 'add': The Inefficient Scheme ("Counting-All")
% It forces enumeration of A and B before calculation.
add(A, B, Sum) :-
    enumerate(A),
    enumerate(B),
    recursive_add(A, B, Sum).