:- begin_tests(full_reorganization_loop).

:- use_module(execution_handler).
:- use_module(object_level).

% Helper to create a Peano number
int_to_peano(0, 0).
int_to_peano(I, s(P)) :-
    I > 0,
    I_prev is I - 1,
    int_to_peano(I_prev, P).

test(reorganization_on_add, [setup(retractall(object_level:add(_,_,_)))]) :-
    % Define an inefficient add rule for the test
    assertz((object_level:add(A, B, Sum) :-
        object_level:enumerate(A),
        object_level:enumerate(B),
        object_level:recursive_add(A, B, Sum))),

    % This goal is inefficient because 3 is smaller than 10.
    % The learner should discover the "Count On Bigger" (COB) strategy.
    int_to_peano(3, PeanoA),
    int_to_peano(10, PeanoB),
    Goal = add(PeanoA, PeanoB, _Result),

    % Set a low limit to ensure the initial attempt fails
    Limit = 15,

    % This should succeed after reorganization
    run_computation(Goal, Limit).

:- end_tests(full_reorganization_loop).