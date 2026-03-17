:- use_module(object_level).
:- use_module(meta_interpreter).

main :-
    Goal = object_level:add(s(s(s(s(s(s(s(s(0)))))))), s(s(s(s(s(0))))), _),
    Limit = 100,
    meta_interpreter:solve(Goal, Limit, Left, Trace),
    Cost is Limit - Left,
    format('Total inferences used: ~w~n', [Cost]).
