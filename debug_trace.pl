:- use_module(object_level).
:- use_module(execution_handler).

main :-
    Goal = object_level:add(s(s(s(s(s(s(s(s(0)))))))), s(s(s(s(s(0))))), _),
    Limit = 10,
    writeln('Running test limit 10 for 8+5:'),
    ( execution_handler:run_computation(Goal, Limit) ->
        writeln('Computation succeeded incorrectly')
    ;
        writeln('Computation failed')
    ),
    writeln('Finished test limits').
