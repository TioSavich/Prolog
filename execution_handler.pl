:- module(execution_handler, [run_query/1]).
:- use_module(meta_interpreter).
:- use_module(config).
:- use_module(reorganization_engine).
:- use_module(object_level). % Ensure object level schemes are loaded

run_query(Goal) :-
    format('Attempting Goal: ~w~n', [Goal]),
    catch(
        (
            max_inferences(MaxI),
            solve(Goal, MaxI, Remaining),
            format('Query succeeded (Viable). Remaining Inferences: ~w~n', [Remaining])
        ),
        % If a perturbation occurs:
        perturbation(Type),
        (
            format('Perturbation detected: ~w.~n', [Type]),
            % Attempt to reorganize the schemes related to the failed Goal.
            accommodate(Goal, Type),
            % Retry the goal after accommodation (Equilibration)
            run_query(Goal)
        )
    ).