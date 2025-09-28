:- module(meta_interpreter, [solve/4]).
:- use_module(object_level). % Ensure we can access the object-level code

% Predicate signature: solve(Goal, InferencesIn, InferencesOut, Trace)
% Trace is a list of events that occurred during the proof search.

% Base case: 'true' succeeds with an empty trace.
solve(true, I, I, []) :- !.

% Conjunction: The trace is the combination of the traces for A and B.
solve((A, B), I_In, I_Out, [trace(A, A_Trace), trace(B, B_Trace)]) :-
    !,
    solve(A, I_In, I_Mid, A_Trace),
    solve(B, I_Mid, I_Out, B_Trace).

% System predicates: Record the call in the trace.
solve(Goal, I_In, I_Out, [call(Goal)]) :-
    predicate_property(Goal, built_in),
    !,
    check_viability(I_In),
    I_Out is I_In - 1,
    call(Goal).

% Object-level execution: Record which clause was used.
% This is the core of the observation mechanism.
solve(Goal, I_In, I_Out, [clause(object_level:(Goal:-Body)), trace(Body, BodyTrace)]) :-
    check_viability(I_In),
    I_Mid is I_In - 1,
    clause(object_level:Goal, Body),
    solve(Body, I_Mid, I_Out, BodyTrace).

% Failure case: If a goal cannot be solved by any clause, record the failure.
% This makes backtracking an observable event in the trace.
solve(Goal, I, I, [fail(Goal)]) :-
    \+ predicate_property(Goal, built_in),
    \+ clause(object_level:Goal, _), !.


% --- Viability Check ---
check_viability(I) :- I > 0, !.
check_viability(_) :-
    % Constraint violated: PERTURBATION DETECTED
    throw(perturbation(resource_exhaustion)).