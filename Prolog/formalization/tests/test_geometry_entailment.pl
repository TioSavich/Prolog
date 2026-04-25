% Tests for axioms_geometry.pl entailment correctness.
%
% Run: swipl -l paths.pl -l formalization/tests/test_geometry_entailment.pl -g run_tests -t halt
%
% axioms_geometry.pl is not a module; it is included by
% arche-trace/incompatibility_semantics.pl, which re-exports the
% geometric shape predicates. entails_via_incompatibility/2 lives
% inside the incompatibility_semantics module and is called via
% module qualification below.

:- ensure_loaded('../../paths').
:- ensure_loaded(arche_trace(load)).

% Local convenience alias for the module-qualified call (see header).
entails(P, Q) :- incompatibility_semantics:entails_via_incompatibility(P, Q).

run_tests :-
    writeln('=== Geometry Entailment Tests ==='),
    test_square_entails_rectangle,
    test_square_entails_rhombus,
    test_square_entails_parallelogram,
    test_square_entails_trapezoid,
    test_square_entails_quadrilateral,
    test_rectangle_not_entails_square,
    test_rhombus_not_entails_rectangle,
    test_trapezoid_not_entails_parallelogram,
    test_kite_not_entails_rectangle,
    test_everything_entails_quadrilateral,
    writeln(''),
    writeln('=== All Geometry Entailment Tests Passed ===').

test_square_entails_rectangle :-
    ( entails(square, rectangle)
    -> format('  PASS: square entails rectangle~n')
    ;  (format('  FAIL: square should entail rectangle~n'), fail)
    ).

test_square_entails_rhombus :-
    ( entails(square, rhombus)
    -> format('  PASS: square entails rhombus~n')
    ;  (format('  FAIL: square should entail rhombus~n'), fail)
    ).

test_square_entails_parallelogram :-
    ( entails(square, parallelogram)
    -> format('  PASS: square entails parallelogram~n')
    ;  (format('  FAIL: square should entail parallelogram~n'), fail)
    ).

test_square_entails_trapezoid :-
    ( entails(square, trapezoid)
    -> format('  PASS: square entails trapezoid~n')
    ;  (format('  FAIL: square should entail trapezoid~n'), fail)
    ).

test_square_entails_quadrilateral :-
    ( entails(square, quadrilateral)
    -> format('  PASS: square entails quadrilateral~n')
    ;  (format('  FAIL: square should entail quadrilateral~n'), fail)
    ).

test_rectangle_not_entails_square :-
    ( \+ entails(rectangle, square)
    -> format('  PASS: rectangle does NOT entail square~n')
    ;  (format('  FAIL: rectangle should not entail square~n'), fail)
    ).

test_rhombus_not_entails_rectangle :-
    ( \+ entails(rhombus, rectangle)
    -> format('  PASS: rhombus does NOT entail rectangle~n')
    ;  (format('  FAIL: rhombus should not entail rectangle~n'), fail)
    ).

test_trapezoid_not_entails_parallelogram :-
    ( \+ entails(trapezoid, parallelogram)
    -> format('  PASS: trapezoid does NOT entail parallelogram~n')
    ;  (format('  FAIL: trapezoid should not entail parallelogram~n'), fail)
    ).

test_kite_not_entails_rectangle :-
    ( \+ entails(kite, rectangle)
    -> format('  PASS: kite does NOT entail rectangle~n')
    ;  (format('  FAIL: kite should not entail rectangle~n'), fail)
    ).

test_everything_entails_quadrilateral :-
    Shapes = [square, rectangle, rhombus, parallelogram, trapezoid, kite],
    ( forall(member(S, Shapes), entails(S, quadrilateral))
    -> format('  PASS: all shapes entail quadrilateral~n')
    ;  (format('  FAIL: some shape fails to entail quadrilateral~n'), fail)
    ).
