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
 * 
 * 
 */
:- module(execution_handler, [run_computation/2]).

:- use_module(meta_interpreter).
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
%       PRIMORDIAL REFACTORING: Proactive reflection on success has been
%       REMOVED. Learning now occurs ONLY through crisis (resource_exhaustion).
%       This enforces the "Built to Break" philosophy - the system only
%       changes when its current way of being is proven inadequate.
%
%       @param Goal The goal to be solved.
%       @param Limit The inference limit.
%       @param Trace The resulting execution trace.
call_meta_interpreter(Goal, Limit, Trace) :-
    meta_interpreter:solve(Goal, Limit, _, Trace),
    writeln('Computation successful.').
    % REMOVED: reflect_on_success(Goal, Trace)
    % Learning is now crisis-driven only

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
%       DEPRECATED in primordial refactoring.
%       
%       After a successful computation, this predicate USED TO trigger
%       reflective learning. This has been REMOVED to enforce crisis-driven
%       learning only. The system must not learn proactively from success;
%       it must only accommodate when forced by failure.
%
%       Kept for backward compatibility but not called.
reflect_on_success(_Goal, _Trace).
    % REMOVED: Proactive learning from success
    % writeln('--- Proactive Reflection Cycle Initiated (Success) ---'),
    % normalize_trace(Trace, NormalizedTrace),
    % Result = _{goal:Goal, trace:NormalizedTrace},
    % reflect_and_learn(Result),
    % writeln('--- Reflection Cycle Complete ---').

%!      handle_perturbation(+Error, +Goal, +Trace, +Limit) is semidet.
%
%       Catches errors from the meta-interpreter and initiates the
%       reorganization process.
%
%       This predicate handles multiple types of perturbations:
%       - perturbation(resource_exhaustion): Computational efficiency crisis
%       - perturbation(normative_crisis(Goal, Context)): Mathematical norm violation
%       - perturbation(incoherence(Commitments)): Logical contradiction
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
    writeln('Reorganization complete. Retrying goal...'),
    run_computation(Goal, Limit).

handle_perturbation(perturbation(normative_crisis(CrisisGoal, Context)), Goal, Trace, Limit) :-
    format('Normative crisis detected: ~w violates norms of ~w context.~n', [CrisisGoal, Context]),
    writeln('Initiating context shift reorganization...'),
    % Handle normative crisis through context expansion
    reorganization_engine:handle_normative_crisis(CrisisGoal, Context),
    writeln('Context shift complete. Retrying goal...'),
    run_computation(Goal, Limit).

handle_perturbation(perturbation(incoherence(Commitments)), Goal, Trace, Limit) :-
    format('Logical incoherence detected in commitments: ~w~n', [Commitments]),
    writeln('Initiating incoherence resolution...'),
    % Handle logical incoherence through belief revision
    reorganization_engine:handle_incoherence(Commitments),
    writeln('Incoherence resolution complete. Retrying goal...'),
    run_computation(Goal, Limit).

handle_perturbation(Error, _, _, _) :-
    writeln('An unhandled error occurred:'),
    writeln(Error),
    fail.