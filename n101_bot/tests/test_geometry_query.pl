% test_geometry_query.pl — plunit suite for the geometry query layer.
%
% Tests the seven query predicates from query.pl plus linked_misconceptions/3
% against the live KB loaded via geometry_bridge.pl.
%
% Run from the repo root:
%   swipl -g 'consult("/Users/tio/Documents/GitHub/Prolog/geometry_bridge.pl"), \
%             load_geometry_kb, \
%             consult("/Users/tio/Documents/GitHub/Prolog/n101_bot/tests/test_geometry_query.pl"), \
%             run_tests, halt(0)' -g 'halt(1)'
%
% Spec: docs/superpowers/specs/2026-05-04-hermes-chatbot-substrate-design.md §6

:- begin_tests(geometry_query).

% ── 1. matching_concepts/3 ──────────────────────────────────────────

test(matching_concepts_finds_square_recognition) :-
    matching_concepts([square, recognition], any, Concepts),
    Concepts \= [],
    member(concept(square_recognition, _Name, _Topic, Score), Concepts),
    Score > 0.

test(matching_concepts_returns_scored_results) :-
    matching_concepts([quadrilateral, classification], any, Concepts),
    Concepts \= [],
    % Every result has a positive score; ordering is up to the impl.
    forall(member(concept(_, _, _, S), Concepts), S > 0).

test(matching_concepts_unrelated_tokens_returns_empty_or_low) :-
    matching_concepts([xyzzyfoobarnotaword], any, Concepts),
    Concepts == [].

% ── 2. applicable_misconceptions/3 ──────────────────────────────────

test(applicable_misconceptions_diamond_phrase) :-
    applicable_misconceptions("that's a diamond, not a square", any, Miscs),
    Miscs \= [].

test(applicable_misconceptions_no_trigger_returns_empty) :-
    applicable_misconceptions("hello world purple monkey dishwasher", any, Miscs),
    Miscs == [].

% ── linked_misconceptions/3 ─────────────────────────────────────────

test(linked_misconceptions_for_orientation_invariant_naming) :-
    linked_misconceptions([orientation_invariant_naming], 3, Miscs),
    length(Miscs, L),
    L >= 1.

test(linked_misconceptions_any_concept) :-
    linked_misconceptions(any, 3, Miscs),
    length(Miscs, L),
    L >= 1.

% ── 3. vh_markers_for/3 ─────────────────────────────────────────────

test(vh_markers_for_square_rectangle_classification_returns_levels) :-
    vh_markers_for(square_rectangle_classification, any, Markers),
    length(Markers, L),
    L >= 1.

test(vh_markers_for_specific_level) :-
    vh_markers_for(square_rectangle_classification, 1, Markers),
    forall(member(marker(Lvl, _, _, _), Markers), Lvl == 1).

% ── 4. bootstraps_for/4 ─────────────────────────────────────────────

test(bootstraps_for_square_rectangle_classification_returns_activities) :-
    bootstraps_for(square_rectangle_classification, any, any, Bootstraps),
    length(Bootstraps, L),
    L >= 1.

test(bootstraps_for_specific_kind) :-
    bootstraps_for(square_rectangle_classification, any, question, Bs),
    forall(member(bs(_, K, _, _, _, _), Bs), K == question).

% ── 5. developmental_arc_for/2 ──────────────────────────────────────

test(developmental_arc_for_trapezoid) :-
    developmental_arc_for(trapezoid_classification_arc, Arc),
    Arc = arc(_, _, _, _).

test(developmental_arc_for_unknown_returns_none) :-
    developmental_arc_for(xyzzy_no_such_arc, Arc),
    Arc == none.

% ── 6. pck_synthesis_for/2 ──────────────────────────────────────────

test(pck_synthesis_for_quadrilateral_hierarchy) :-
    pck_synthesis_for(quadrilateral_hierarchy, Synth),
    ( Synth = pck(_, _, _, _) ; Synth == none ),
    % We accept either; the predicate should at minimum succeed deterministically.
    !.

test(pck_synthesis_for_unknown_returns_none) :-
    pck_synthesis_for(xyzzy_no_such_concept, Synth),
    Synth == none.

% ── 7. standards_bundle_for/3 ───────────────────────────────────────

test(standards_bundle_for_5_g_b_3) :-
    standards_bundle_for(ccss, "5.G.B.3", Bundle),
    Bundle \= not_found,
    Bundle = bundle(_ConceptId, _Concept, _Statement,
                    _Miscs, _Markers, _Bs, _Arc, _Pck).

test(standards_bundle_for_unknown_returns_not_found) :-
    standards_bundle_for(ccss, "99.X.Z.99", Bundle),
    Bundle == not_found.

:- end_tests(geometry_query).
