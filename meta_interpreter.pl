:- module(meta_interpreter, [solve/3]).
:- use_module(object_level). % Ensure we can access the object-level code

% Predicate signature: solve(Goal, InferencesIn, InferencesOut)

% Base case: 'true' consumes no inferences.
solve(true, I, I) :- !.

% Conjunction
solve((A, B), I_In, I_Out) :-
    !,
    solve(A, I_In, I_Mid),
    solve(B, I_Mid, I_Out).

% System predicates (CRITICAL: Must be handled explicitly)
solve(Goal, I_In, I_Out) :-
    predicate_property(Goal, built_in),
    !,
    check_viability(I_In),
    I_Out is I_In - 1, % Assign a cost (e.g., 1)
    call(Goal).

% Object-level execution
solve(Goal, I_In, I_Out) :-
    % Check viability BEFORE the inference step.
    check_viability(I_In),
    I_Mid is I_In - 1,

    % Access object-level schemes. We must specify the module where schemes reside.
    % Assuming schemes are in a module named 'object_level'.
    clause(object_level:Goal, Body),

    solve(Body, I_Mid, I_Out).

% --- Viability Check ---
check_viability(I) :- I > 0, !.
check_viability(_) :-
    % Constraint violated: PERTURBATION DETECTED
    throw(perturbation(resource_exhaustion)).