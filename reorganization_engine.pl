/** <module> Reorganization Engine for Cognitive Accommodation
 *
 * This module implements the "Reorganize" stage of the ORR cycle. It is
 * responsible for `accommodate/1`, the process of modifying the system's
 * own knowledge base (`object_level.pl`) in response to a state of
 * disequilibrium detected by the `reflective_monitor.pl`.
 *
 * The engine currently handles failures by:
 * 1.  Identifying the predicate causing the most "conceptual stress" (i.e.,
 *     the one involved in the most failures).
 * 2.  Applying a predefined transformation strategy to that predicate.
 *
 * The only transformation implemented is `specialize_add_rule`, which
 * replaces a failing `add/3` implementation with a more robust, recursive
 * one based on the Peano axioms.
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(reorganization_engine, [accommodate/1]).

:- use_module(object_level).
:- use_module(reflective_monitor).
:- use_module(reorganization_log).
:- use_module(more_machine_learner).
:- use_module(incompatibility_semantics).
:- use_module(strategies). % Load all defined strategies

:- reexport(learned_knowledge, [learned_rule/1]).

%!      reorganize_system(+Goal:term, +Trace:list) is semidet.
%
%       The main entry point for the reorganization process, triggered when
%       a perturbation (e.g., resource exhaustion) occurs. This predicate
%       orchestrates the analysis, synthesis, validation, and integration of
%       a new, more efficient strategy.
%
%       @param Goal The goal that failed.
%       @param Trace The execution trace leading to the failure.
reorganize_system(Goal, _Trace) :-
    % Deconstruct the goal to get the arguments
    Goal =.. [Pred, A, B, _Result],
    ( (Pred = add ; Pred = multiply) ->
        % Convert Peano numbers to integers for the learner
        peano_to_int(A, IntA),
        peano_to_int(B, IntB),

        writeln('Invoking machine learner to discover new strategies...'),
        % The learner will analyze, validate, and assert the new rule internally
        (   more_machine_learner:discover_strategy(IntA, IntB, StrategyName) ->
            format('Learner discovered and asserted strategy: ~w~n', [StrategyName]),
            more_machine_learner:save_knowledge,
            writeln('New knowledge has been persisted.')
        ;   writeln('Learner did not find a new strategy for this case.'),
            fail
        )
    ;
        format('Reorganization for predicate ~w is not supported.~n', [Pred]),
        fail
    ).

%!      peano_to_int(+Peano, -Int) is det.
%
%       Converts a Peano number (e.g., `s(s(0))`) to an integer.
peano_to_int(0, 0).
peano_to_int(s(N), Int) :-
    peano_to_int(N, SubInt),
    Int is SubInt + 1.

%!      integrate_new_rule(+Rule:term) is det.
%
%       Integrates a validated new rule into the system's knowledge base.
%       It retracts the old, inefficient rule and asserts the new one in
%       the `object_level` module.
integrate_new_rule((Head :- Body)) :-
    functor(Head, Name, Arity),
    retractall(object_level:Name/Arity),
    assertz(object_level:(Head :- Body)),
    log_event(reorganized(from(Name/Arity), to(Head :- Body))).

%!      save_learned_rule(+Rule:term) is det.
%
%       Persists a newly learned rule to the `learned_knowledge.pl` file
%       so that it can be reused across sessions.
save_learned_rule(Rule) :-
    open('learned_knowledge.pl', append, Stream),
    format(Stream, 'learned_rule(~q).~n', [Rule]),
    close(Stream).

%!      accommodate(+Trigger:term) is semidet.
%
%       Attempts to accommodate a state of disequilibrium by modifying the
%       knowledge base. This is the main entry point for the reorganization engine.
%
%       It dispatches to different handlers based on the type of `Trigger`:
%       - `goal_failure` or `perturbation`: Calls `handle_failure/1` to attempt
%         a knowledge repair based on conceptual stress.
%       - `incoherence`: Currently a placeholder; fails as this type of
%         reorganization is not yet implemented.
%
%       Succeeds if a transformation is successfully applied. Fails otherwise.
%
%       @param Trigger The term describing the disequilibrium, provided by the
%       reflective monitor.
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
% Handles disequilibrium caused by goal failure. It identifies the most
% stressed predicate from the conceptual stress map and attempts to apply a
% transformation to repair it.
handle_failure(_Trigger) :-
    get_most_stressed_predicate(Signature),
    format('Highest conceptual stress found for predicate: ~w~n', [Signature]),
    log_event(reorganization_start(Signature)),
    apply_transformation(Signature).

% handle_incoherence(+Commitments)
%
% Placeholder for handling disequilibrium caused by logical contradictions.
% This is a future work area and currently always fails.
handle_incoherence(Commitments) :-
    format('Handling incoherence for commitments: ~w~n', [Commitments]),
    format('Incoherence-driven reorganization is not yet implemented.~n'),
    fail.

% get_most_stressed_predicate(-Signature)
%
% Finds the predicate with the highest stress count in the stress map
% maintained by the reflective monitor.
get_most_stressed_predicate(Signature) :-
    get_stress_map(StressMap),
    StressMap \= [],
    find_max_stress(StressMap, stress(_, 0), stress(Signature, _)), !.
get_most_stressed_predicate(_) :-
    format('Could not identify a stressed predicate. Reorganization failed.~n'),
    fail.

% find_max_stress(+StressMap, +CurrentMax, -Max)
%
% Helper predicate to find the maximum entry in the stress map list.
find_max_stress([], Max, Max).
find_max_stress([stress(S, C)|Rest], stress(_, MaxC), Max) :-
    C > MaxC, !, find_max_stress(Rest, stress(S, C), Max).
find_max_stress([_|Rest], Max, Result) :- find_max_stress(Rest, Max, Result).

% apply_transformation(+Signature)
%
% Dispatches to a specific transformation strategy based on the predicate
% signature. Currently, only a transformation for `add/3` exists.
apply_transformation(add/3) :-
    !, specialize_add_rule.
apply_transformation(Signature) :-
    format('No specific reorganization strategy available for ~w.~n', [Signature]),
    fail.

% --- Transformation Strategies ---

% specialize_add_rule/0
%
% A specific transformation strategy that replaces the existing `add/3` rules
% with a correct, recursive implementation based on Peano arithmetic. This
% represents a form of learning or knowledge repair.
specialize_add_rule :-
    format('Applying "Specialization" strategy to add/3.~n'),
    % Retract all existing rules for add/3 and log each one.
    forall(
        clause(object_level:add(A, B, C), Body),
        (   retract(object_level:add(A, B, C) :- Body),
            log_event(retracted((add(A, B, C) :- Body)))
        )
    ),
    % Synthesize and assert the new, correct rule and log it.
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
    log_event(reorganization_success).

% assert_and_log(+Clause)
%
% Helper to assert a clause and log the assertion event.
assert_and_log(Clause) :-
    assertz(Clause),
    log_event(asserted(Clause)).