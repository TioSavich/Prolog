:- use_module(object_level).
:- use_module(execution_handler).
:- use_module(more_machine_learner).

main :-
    Goal = object_level:add(s(s(s(s(s(s(s(s(0)))))))), s(s(s(s(s(0))))), _),
    Limit = 10,
    catch(execution_handler:run_computation(Goal, Limit), _, true),
    writeln('Listing run_learned_strategy:'),
    listing(more_machine_learner:run_learned_strategy).
