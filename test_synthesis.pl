% Filename: test_synthesis.pl (Updated and Corrected)
% Load the module under test. Explicitly qualify imports to avoid ambiguity in tests.
:- use_module(incompatibility_semantics, [
    proves/1, incoherent/1, set_domain/1, obj_coll/1, normalize/2
]).
:- use_module(library(plunit)).

% Ensure operators are visible (FIX: Added all necessary operators)
:- op(500, fx, neg).
:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, comp_poss).
:- op(1050, xfy, =>).
:- op(550, xfy, rdiv). % Ensure rdiv is visible for tests

:- begin_tests(unified_synthesis).

% --- Tests for Part 1: Core Logic and Domains ---
test(identity_subjective) :- assertion(proves([s(p)] => [s(p)])).
test(incoherence_subjective) :- assertion(incoherent([s(p), s(neg(p))])).

test(negation_handling_subjective_lem) :-
    assertion(proves([] => [s(p), s(neg(p))])).

% --- Tests for Part 2: Arithmetic Coexistence and Fixes ---

test(arithmetic_commutativity_normative) :-
    assertion(proves([n(plus(2,3,5))] => [n(plus(3,2,5))])).

test(arithmetic_subtraction_limit_n, [setup(set_domain(n))]) :-
    assertion(incoherent([n(obj_coll(minus(3,5,_)))])).

test(arithmetic_subtraction_limit_n_persistence, [setup(set_domain(n))]) :-
    assertion(incoherent([n(obj_coll(minus(3,5,_))), s(p)])).

test(arithmetic_subtraction_limit_z, [setup(set_domain(z))]) :-
    assertion(\+(incoherent([n(obj_coll(minus(3,5,_)))]))).

% --- Tests for Part 3: Embodied Modal Logic (EML) - UPDATED ---
test(eml_dynamic_u_to_a) :- assertion(proves([s(u)] => [s(a)])).
test(eml_dynamic_full_cycle) :- assertion(proves([s(lg)] => [s(a)])).

% New Tests for Tension and Compressive Possibility
test(eml_tension_expansive_poss) :-
    % Commitment 3: Possibility of Release
    assertion(proves([s(a)] => [s(exp_poss lg)])).

test(eml_tension_compressive_poss) :-
    % Commitment 3: Possibility of Fixation (Temptation)
    assertion(proves([s(a)] => [s(comp_poss t)])).

test(eml_tension_conjunction) :-
    % Verify that both possibilities are entailed by Awareness (using conjunction reduction)
    assertion(proves([s(a)] => [s(conj(exp_poss lg, comp_poss t))])).

test(eml_fixation_consequence) :-
    % Commitment 4a: Fixation necessarily leads to a contraction that collapses unity.
    assertion(proves([s(t)] => [s(neg(u))])).

test(hegel_loop_prevention) :-
    assertion(\+(proves([s(t_b)] => [s(x)]))).

% --- Tests for New Feature: Quadrilateral Hierarchy (Chapter 2) ---

test(quad_incompatibility_square_r1) :-
    assertion(incoherent([n(square(x)), n(r1(x))])).

test(quad_compatibility_trapezoid_r1) :-
    assertion(\+(incoherent([n(trapezoid(x)), n(r1(x))]))).

test(quad_incompatibility_persistence) :-
    assertion(incoherent([n(square(x)), n(r1(x)), s(other)])).

test(quad_entailment_square_rectangle) :-
    assertion(proves([n(square(x))] => [n(rectangle(x))])).

test(quad_entailment_rectangle_square_fail) :-
    assertion(\+(proves([n(rectangle(x))] => [n(square(x))]))).

test(quad_entailment_rhombus_kite) :-
    assertion(proves([n(rhombus(x))] => [n(kite(x))])).

test(quad_entailment_transitive) :-
    assertion(proves([n(square(x))] => [n(parallelogram(x))])).

test(quad_projection_contrapositive) :-
    assertion(proves([n(neg(rectangle(x)))] => [n(neg(square(x)))])).

test(quad_projection_inversion_fail) :-
    assertion(\+(proves([n(neg(square(x)))] => [n(neg(rectangle(x)))]))).

% --- Tests for Number Theory (Euclid's Proof) ---

% Test Grounding Helpers
test(euclid_grounding_prime) :-
    assertion(proves([] => [n(prime(7))])),
    assertion(\+ proves([] => [n(prime(6))])).

test(euclid_grounding_composite) :-
    assertion(proves([] => [n(composite(6))])),
    assertion(\+ proves([] => [n(composite(7))])).

% Test Material Inferences (M4 and M5)
test(euclid_material_inference_m5) :-
    % L=[2,3], Product(L)+1 = 7. P=7.
    assertion(proves([n(prime(7)), n(divides(7, 7))] => [n(neg(member(7, [2, 3])))])).

test(euclid_material_inference_m4) :-
    assertion(proves([n(prime(5)), n(neg(member(5, [2, 3])))] => [n(neg(is_complete([2, 3])))] )).

% Test Forward Chaining (Combining M5 and M4)
test(euclid_forward_chaining) :-
    % L=[2,3], N=7, P=7.
    Premises = [n(prime(7)), n(divides(7, 7)), n(is_complete([2, 3]))],
    Conclusion = [n(neg(is_complete([2, 3])))],
    assertion(proves(Premises => Conclusion)).

% Test Case 1 (N is Prime)
test(euclid_case_1_incoherence) :-
    % L=[2,3], N=7.
    assertion(incoherent([n(prime(7)), n(is_complete([2, 3]))])).

% Test Case 2 (N is Composite)
test(euclid_case_2_incoherence) :-
    % L=[2,3,5,7,11,13]. N=30031 (Composite: 59*509).
    L = [2,3,5,7,11,13],
    N = 30031,
    Premises = [n(composite(N)), n(is_complete(L))],
    assertion(incoherent(Premises)).

% Test The Final Theorem (Euclid's Theorem)
test(euclid_theorem_infinitude_of_primes) :-
    L = [2, 5, 11],
    assertion(incoherent([n(is_complete(L))])).

test(euclid_theorem_empty_list) :-
    assertion(incoherent([n(is_complete([]))])).

% --- Tests for Fractions (Jason.pl integration) ---
% FIX: obj_coll/1 and normalize/2 are now imported and visible.

test(fraction_obj_coll_q, [setup(set_domain(q))]) :-
    assertion(obj_coll(1 rdiv 2)),
    assertion(obj_coll(5)),
    assertion(\+ obj_coll(1 rdiv 0)).

test(fraction_obj_coll_n, [setup(set_domain(n))]) :-
    assertion(\+ obj_coll(1 rdiv 2)),
    assertion(obj_coll(5)).

test(fraction_normalization) :-
    assertion(normalize(4 rdiv 8, 1 rdiv 2)),
    assertion(normalize(10 rdiv 2, 5)).

test(fraction_addition_grounding, [setup(set_domain(q))]) :-
    % 1/2 + 1/3 = 5/6
    assertion(proves([] => [o(plus(1 rdiv 2, 1 rdiv 3, 5 rdiv 6))])).

test(fraction_addition_mixed, [setup(set_domain(q))]) :-
    % 2 + 1/4 = 9/4
    assertion(proves([] => [o(plus(2, 1 rdiv 4, 9 rdiv 4))])).

test(fraction_subtraction_grounding, [setup(set_domain(q))]) :-
    % 1/2 - 1/3 = 1/6
    assertion(proves([] => [o(minus(1 rdiv 2, 1 rdiv 3, 1 rdiv 6))])).

% Test subtraction constraints in N with fractions
test(fraction_subtraction_limit_n, [setup(set_domain(n))]) :-
    % 1/3 - 1/2 = -1/6. Incoherent in N.
    assertion(incoherent([n(obj_coll(minus(1 rdiv 3, 1 rdiv 2, _)))])).

test(fraction_iteration_grounding, [setup(set_domain(q))]) :-
    % (1/3) * 4 = 4/3
    assertion(proves([] => [o(iterate(1 rdiv 3, 4, 4 rdiv 3))])).

test(fraction_partition_grounding, [setup(set_domain(q))]) :-
    % (4/3) / 4 = 1/3 (Normalized from 4/12)
    assertion(proves([] => [o(partition(4 rdiv 3, 4, 1 rdiv 3))])).

test(fraction_partition_integer, [setup(set_domain(q))]) :-
    % 5 / 2 = 5/2
    assertion(proves([] => [o(partition(5, 2, 5 rdiv 2))])).

:- end_tests(unified_synthesis).