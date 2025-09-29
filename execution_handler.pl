/** <module> ORR Cycle Execution Handler
 *
 * This module serves as the central controller for the cognitive architecture,
 * managing the Observe-Reorganize-Reflect (ORR) cycle. It orchestrates the
 * interaction between the meta-interpreter (Observe), the reflective monitor
 * (Reflect), and the reorganization engine (Reorganize).
 *
 * The primary entry point is `run_query/1`, which initiates the ORR cycle
 * for a given goal.
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(execution_handler, [run_computation/2]).

:- use_module(meta_interpreter).
:- use_module(reorganization_engine).
:- use_module(object_level).
:- use_module(more_machine_learner, [reflect_and_learn/1]).

%!      run_computation(+Goal:term, +Limit:integer) is semidet.
%
%       The main entry point for the self-reorganizing system. It attempts
%       to solve the given `Goal` within the specified `Limit` of
%       computational steps.
%
%       If the computation exceeds the resource limit, it triggers the
%       reorganization process and then retries the goal.
%
%       @param Goal The computational goal to be solved.
%       @param Limit The maximum number of inference steps allowed.
run_computation(Goal, Limit) :-
    catch(
        call_meta_interpreter(Goal, Limit, Trace),
        Error,
        handle_perturbation(Error, Goal, Trace, Limit)
    ).

%!      call_meta_interpreter(+Goal, +Limit, -Trace) is det.
%
%       A wrapper for the `meta_interpreter:solve/4` predicate. It
%       executes the goal and, upon success, reports that the computation
%       is complete.
%
%       @param Goal The goal to be solved.
%       @param Limit The inference limit.
%       @param Trace The resulting execution trace.
call_meta_interpreter(Goal, Limit, Trace) :-
    meta_interpreter:solve(Goal, Limit, _, Trace),
    writeln('Computation successful.'),
    reflect_on_success(Goal, Trace).

%!      normalize_trace(+Trace, -NormalizedTrace) is det.
%
%       Converts different trace formats into a unified dictionary format
%       for the learner. It specifically handles the `arithmetic_trace/3`
%       term, converting it to a `trace{}` dict.
% Case 1: The trace is a list containing a single arithmetic_trace term.
normalize_trace([arithmetic_trace(Strategy, _, Steps)], NormalizedTrace) :-
    !,
    NormalizedTrace = trace{strategy:Strategy, steps:Steps}.
% Case 2: The trace is a bare arithmetic_trace term.
normalize_trace(arithmetic_trace(Strategy, _, Steps), NormalizedTrace) :-
    !,
    NormalizedTrace = trace{strategy:Strategy, steps:Steps}.
% Case 3: Pass through any other format (already normalized dicts, etc.)
normalize_trace(Trace, Trace).

%!      reflect_on_success(+Goal, +Trace) is det.
%
%       After a successful computation, this predicate triggers the
%       reflective learning process. It passes the goal and the resulting
%       trace to the learning module to check for potential optimizations.
reflect_on_success(Goal, Trace) :-
    writeln('--- Proactive Reflection Cycle Initiated (Success) ---'),
    normalize_trace(Trace, NormalizedTrace),
    Result = _{goal:Goal, trace:NormalizedTrace},
    reflect_and_learn(Result),
    writeln('--- Reflection Cycle Complete ---').

%!      handle_perturbation(+Error, +Goal, +Trace, +Limit) is semidet.
%
%       Catches errors from the meta-interpreter and initiates the
%       reorganization process.
%
%       This predicate specifically handles `perturbation(resource_exhaustion)`.
%       Upon catching this error, it logs the event, invokes the
%       `reorganization_engine`, and then recursively retries the original
%       goal with the same limit.
%
%       @param Error The error term thrown by `catch/3`.
%       @param Goal The original goal that was being attempted.
%       @param Trace The execution trace produced before the error occurred.
%       @param Limit The original resource limit.
handle_perturbation(perturbation(resource_exhaustion), Goal, Trace, Limit) :-
    writeln('Resource exhaustion detected. Initiating reorganization...'),
    % First, attempt to learn from the failure trace
    writeln('--- Reflective Cycle Initiated (Failure) ---'),
    normalize_trace(Trace, NormalizedTrace),
    Result = _{goal:Goal, trace:NormalizedTrace},
    reflect_and_learn(Result),
    % Then, proceed with the original reorganization logic
    reorganize_system(Goal, Trace),
    writeln('Reorganization complete. Retrying goal...'),
    run_computation(Goal, Limit).

handle_perturbation(Error, _, _, _) :-
    writeln('An unhandled error occurred:'),
    writeln(Error),
    fail.