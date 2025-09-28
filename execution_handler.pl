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
:- module(execution_handler, [run_query/1]).

% Load all components of the ORR architecture
:- use_module(meta_interpreter).
:- use_module(reflective_monitor).
:- use_module(reorganization_engine).
:- use_module(reorganization_log).
:- use_module(config).
:- use_module(object_level).

%!      run_query(+Goal:term) is det.
%
%       Initiates the Observe-Reorganize-Reflect (ORR) cycle for a given Goal.
%
%       This predicate serves as the main entry point for the system. It
%       first clears any previous logs and stress maps, then starts the
%       `orr_cycle/2` with the maximum number of retries specified in the
%       configuration.
%
%       @param Goal The initial goal to be solved by the system.
run_query(Goal) :-
    % Clear logs and stress maps for a clean run.
    clear_log,
    reset_stress_map,
    max_retries(MaxRetries),
    orr_cycle(Goal, MaxRetries).

% orr_cycle(+Goal, +RetriesLeft)
% This is the core recursive loop of the architecture. It attempts to solve
% the goal, reflecting on failures and triggering reorganization if necessary.

orr_cycle(Goal, RetriesLeft) :-
    RetriesLeft > 0,
    format('~n--- OBSERVING (Attempt ~w) ---~n', [RetriesLeft]),
    format('Goal: ~w~n', [Goal]),
    log_event(orr_cycle_start(Goal)), % Log start

    max_inferences(MaxI),
    % The catch/3 block handles critical failures (perturbations) from the meta-interpreter.
    catch(
        (
            % 1. OBSERVE: Attempt to solve the goal using the meta-interpreter.
            solve(Goal, MaxI, _, Trace),
            % 2. REFLECT: Analyze the trace for signs of disequilibrium.
            (   reflect(Trace, Trigger)
            ->  % Disequilibrium found, handle it.
                format('~n--- REFLECTION ---~n'),
                format('Disequilibrium detected: ~w~n', [Trigger]),
                log_event(disequilibrium(Trigger)),
                handle_disequilibrium(Trigger, Goal, RetriesLeft)
            ;   % No disequilibrium, goal is solved and coherent.
                format('~n--- EQUILIBRIUM REACHED ---~n'),
                format('Goal succeeded and is coherent.~n'),
                log_event(equilibrium)
            )
        ),
        perturbation(Type),
        % Handle critical perturbations (e.g., infinite loops) caught by solve/4.
        (
            format('~n--- REFLECTION (Critical) ---~n'),
            format('System perturbation detected: ~w~n', [Type]),
            log_event(disequilibrium(perturbation(Type))),
            handle_disequilibrium(perturbation(Type), Goal, RetriesLeft)
        )
    ).

orr_cycle(_, 0) :-
    format('~n--- FAILURE TO EQUILIBRATE ---~n'),
    format('Max reorganization attempts reached. Aborting.~n').

% handle_disequilibrium(+Trigger, +Goal, +RetriesLeft)
% Manages the reorganization process when disequilibrium is detected.

handle_disequilibrium(Trigger, Goal, RetriesLeft) :-
    format('~n--- REORGANIZING ---~n'),
    % 3. REORGANIZE: Attempt to accommodate the trigger.
    (   accommodate(Trigger)
    ->  % Reorganization was successful, retry the cycle.
        format('Reorganization successful. Retrying goal.~n'),
        NextRetries is RetriesLeft - 1,
        orr_cycle(Goal, NextRetries)
    ;   % Reorganization failed, abort the process.
        format('~n--- FAILURE TO REORGANIZE ---~n'),
        format('Accommodation failed for trigger: ~w. Aborting.~n', [Trigger]),
        log_event(reorganization_failure)
    ).