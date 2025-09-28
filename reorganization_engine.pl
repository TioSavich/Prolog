:- module(reorganization_engine, [accommodate/1]).

:- use_module(object_level).
:- use_module(reflective_monitor).
:- use_module(reorganization_log). % <-- INTEGRATED LOGGER

% accommodate(+Trigger)
%
% Dispatches to different handlers based on the disequilibrium trigger.
accommodate(Trigger) :-
    (   (Trigger = goal_failure(_); Trigger = perturbation(_)) ->
        handle_failure(Trigger)
    ;   Trigger = incoherence(Commitments) ->
        handle_incoherence(Commitments)
    ;   format('Unknown trigger type: ~w. Cannot accommodate.~n', [Trigger]),
        fail
    ).

% handle_failure(+Trigger)
%
% Identifies the most stressed predicate and attempts to transform it.
handle_failure(_Trigger) :-
    get_most_stressed_predicate(Signature),
    format('Highest conceptual stress found for predicate: ~w~n', [Signature]),
    log_event(reorganization_start(Signature)), % Log start
    apply_transformation(Signature).

% handle_incoherence(+Commitments)
%
% Placeholder for future implementation.
handle_incoherence(Commitments) :-
    format('Handling incoherence for commitments: ~w~n', [Commitments]),
    format('Incoherence-driven reorganization is not yet implemented.~n'),
    fail.

% get_most_stressed_predicate(-Signature)
%
% Finds the predicate with the highest stress count.
get_most_stressed_predicate(Signature) :-
    get_stress_map(StressMap),
    StressMap \= [],
    find_max_stress(StressMap, stress(_, 0), stress(Signature, _)), !.
get_most_stressed_predicate(_) :-
    format('Could not identify a stressed predicate. Reorganization failed.~n'),
    fail.

find_max_stress([], Max, Max).
find_max_stress([stress(S, C)|Rest], stress(_, MaxC), Max) :-
    C > MaxC, !, find_max_stress(Rest, stress(S, C), Max).
find_max_stress([_|Rest], Max, Result) :- find_max_stress(Rest, Max, Result).

% apply_transformation(+Signature)
%
% Dispatches to a specific transformation strategy.
apply_transformation(add/3) :-
    !, specialize_add_rule.
apply_transformation(Signature) :-
    format('No specific reorganization strategy available for ~w.~n', [Signature]),
    fail.

% --- Transformation Strategies ---

specialize_add_rule :-
    format('Applying "Specialization" strategy to add/3.~n'),
    % Retract old rules and log each one.
    forall(
        clause(object_level:add(A, B, C), Body),
        (   retract(object_level:add(A, B, C) :- Body),
            log_event(retracted((add(A, B, C) :- Body)))
        )
    ),
    % Synthesize and assert the new rule, logging it.
    NewHead = add(A, B, Sum),
    NewBody = recursive_add(A, B, Sum),
    assertz(object_level:(NewHead :- NewBody)),
    log_event(asserted((NewHead :- NewBody))),
    format('Asserted new specialized add/3 clause.~n'),
    % Synthesize and assert helper predicates if they don't exist.
    (   \+ predicate_property(object_level:recursive_add(_,_,_), defined) ->
        assert_and_log((object_level:recursive_add(0, X, X))),
        assert_and_log((object_level:recursive_add(s(N), Y, s(Z)) :- object_level:recursive_add(N, Y, Z))),
        format('Asserted helper predicate recursive_add/3.~n')
    ;   true
    ),
    log_event(reorganization_success). % Log overall success

assert_and_log(Clause) :-
    assertz(Clause),
    log_event(asserted(Clause)).