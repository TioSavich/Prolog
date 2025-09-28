/** <module> Tracing Meta-Interpreter for Observation
 *
 * This module provides the core "Observe" capability of the ORR cycle.
 * It contains a meta-interpreter, `solve/4`, which executes goals defined
 * in the `object_level` module.
 *
 * Instead of just succeeding or failing, this meta-interpreter produces a
 * detailed `Trace` of the execution path. This trace includes which clauses
 * were used, which built-in predicates were called, and where failures
 * occurred. The trace is the primary data source for the `reflective_monitor`.
 *
 * The interpreter also includes a simple resource constraint (a maximum
 * number of inferences) to prevent infinite loops, throwing a `perturbation`
 * exception if the limit is exceeded.
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(meta_interpreter, [solve/4]).
:- use_module(object_level). % Ensure we can access the object-level code

%!      solve(+Goal, +InferencesIn, -InferencesOut, -Trace) is nondet.
%
%       Executes a given `Goal` and produces a `Trace` of the execution.
%       This is the core predicate of the meta-interpreter. It handles
%       different types of goals (conjunctions, built-ins, object-level
%       clauses) and tracks the number of inferences to prevent run-away
%       execution.
%
%       The `Trace` is a list of events, which can be:
%       - `trace(Goal, SubTrace)`: For a subgoal.
%       - `call(Goal)`: For a successful call to a built-in predicate.
%       - `clause(Clause)`: For the successful application of an object-level clause.
%       - `fail(Goal)`: When a goal fails (no matching clauses).
%
%       @param Goal The goal to be solved.
%       @param InferencesIn The initial number of available inference steps.
%       @param InferencesOut The remaining number of inference steps.
%       @param Trace A list representing the execution trace.
%       @error perturbation(resource_exhaustion) if the inference counter drops to zero.

% Base case: `true` always succeeds with an empty trace.
solve(true, I, I, []) :- !.

% Conjunction: Solve `A` then `B`. The trace is a combination of the two sub-traces.
solve((A, B), I_In, I_Out, [trace(A, A_Trace), trace(B, B_Trace)]) :-
    !,
    solve(A, I_In, I_Mid, A_Trace),
    solve(B, I_Mid, I_Out, B_Trace).

% System predicates: Check resources, execute the built-in, and record the call in the trace.
solve(Goal, I_In, I_Out, [call(Goal)]) :-
    predicate_property(Goal, built_in),
    !,
    check_viability(I_In),
    I_Out is I_In - 1,
    call(Goal).

% Object-level predicates: Find a matching clause in the `object_level` module,
% record its use, and recursively solve its body. This is the core of observation.
solve(Goal, I_In, I_Out, [clause(object_level:(Goal:-Body)), trace(Body, BodyTrace)]) :-
    check_viability(I_In),
    I_Mid is I_In - 1,
    clause(object_level:Goal, Body),
    solve(Body, I_Mid, I_Out, BodyTrace).

% Failure case: If a goal is not a built-in and has no matching clauses,
% record the failure in the trace. This makes backtracking an observable event.
solve(Goal, I, I, [fail(Goal)]) :-
    \+ predicate_property(Goal, built_in),
    \+ clause(object_level:Goal, _), !.


% --- Viability Check ---

% check_viability(+Inferences)
%
% Succeeds if the inference counter is positive. Throws a `perturbation`
% exception otherwise, which is caught by the execution handler.
check_viability(I) :- I > 0, !.
check_viability(_) :-
    % Constraint violated: PERTURBATION DETECTED
    throw(perturbation(resource_exhaustion)).