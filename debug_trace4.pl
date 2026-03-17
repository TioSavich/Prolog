:- use_module(object_level).
:- use_module(execution_handler).

main :-
    Limit = 20,
    writeln('Running 2+1'),
    execution_handler:run_computation(object_level:add(s(s(0)), s(0), _), Limit),
    writeln('Running 2+2'),
    execution_handler:run_computation(object_level:add(s(s(0)), s(s(0)), _), Limit),
    writeln('Running 3+1'),
    execution_handler:run_computation(object_level:add(s(s(s(0))), s(0), _), Limit),
    
    writeln('Running 8+5'),
    catch(
        execution_handler:run_computation(object_level:add(s(s(s(s(s(s(s(s(0)))))))), s(s(s(s(s(0))))), _), Limit),
        Error,
        writeln(Error)
    ).
