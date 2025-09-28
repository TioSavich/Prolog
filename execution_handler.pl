:- module(execution_handler, [run_query/1]).

% Load all components of the ORR architecture
:- use_module(meta_interpreter).
:- use_module(reflective_monitor).
:- use_module(reorganization_engine).
:- use_module(reorganization_log). % <-- INTEGRATED LOGGER
:- use_module(config).
:- use_module(object_level).

run_query(Goal) :-
    % Clear logs and stress maps for a clean run.
    clear_log,
    reset_stress_map,
    max_retries(MaxRetries),
    orr_cycle(Goal, MaxRetries).

% orr_cycle(+Goal, +RetriesLeft)
% This is the core loop of the architecture.

orr_cycle(Goal, RetriesLeft) :-
    RetriesLeft > 0,
    format('~n--- OBSERVING (Attempt ~w) ---~n', [RetriesLeft]),
    format('Goal: ~w~n', [Goal]),
    log_event(orr_cycle_start(Goal)), % Log start

    max_inferences(MaxI),
    catch(
        (
            solve(Goal, MaxI, _, Trace),
            (   reflect(Trace, Trigger)
            ->  format('~n--- REFLECTION ---~n'),
                format('Disequilibrium detected: ~w~n', [Trigger]),
                log_event(disequilibrium(Trigger)), % Log trigger
                handle_disequilibrium(Trigger, Goal, RetriesLeft)
            ;   format('~n--- EQUILIBRIUM REACHED ---~n'),
                format('Goal succeeded and is coherent.~n'),
                log_event(equilibrium) % Log success
            )
        ),
        perturbation(Type),
        (
            format('~n--- REFLECTION (Critical) ---~n'),
            format('System perturbation detected: ~w~n', [Type]),
            log_event(disequilibrium(perturbation(Type))), % Log critical trigger
            handle_disequilibrium(perturbation(Type), Goal, RetriesLeft)
        )
    ).

orr_cycle(_, 0) :-
    format('~n--- FAILURE TO EQUILIBRATE ---~n'),
    format('Max reorganization attempts reached. Aborting.~n').

handle_disequilibrium(Trigger, Goal, RetriesLeft) :-
    format('~n--- REORGANIZING ---~n'),
    (   accommodate(Trigger)
    ->  % Reorganization success is logged within the engine
        format('Reorganization successful. Retrying goal.~n'),
        NextRetries is RetriesLeft - 1,
        orr_cycle(Goal, NextRetries)
    ;   format('~n--- FAILURE TO REORGANIZE ---~n'),
        format('Accommodation failed for trigger: ~w. Aborting.~n', [Trigger]),
        log_event(reorganization_failure) % Log failure
    ).