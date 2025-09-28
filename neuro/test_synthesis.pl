% Filename: test_synthesis.pl (Updated for Neuro-Symbolic Testing)
% Load the core module
:- use_module(incompatibility_semantics, [
    proves/1, incoherent/1, set_domain/1, obj_coll/1, normalize/2
]).
% Load the bridge module to access the learning triggers.
% We must ensure the bridge is loaded so the Priority 5 hook in the prover can find it.
:- use_module(neuro_symbolic_bridge, [learn_euclid_strategy/0]).

:- use_module(library(plunit)).

% Ensure operators are visible
:- op(500, fx, neg).
:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, comp_poss).
:- op(1050, xfy, =>).
:- op(550, xfy, rdiv).

% Helper to clear knowledge for isolated tests
clear_knowledge :-
    retractall(neuro_symbolic_bridge:learned_proof_strategy(_, _)),
    retractall(neuro_symbolic_bridge:run_learned_strategy(_, _, _, _, _)).

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

test(arithmetic_subtraction_limit_z, [setup(set_domain(z))]) :-
    assertion(\+(incoherent([n(obj_coll(minus(3,5,_)))]))).

% --- Tests for Part 3: Embodied Modal Logic (EML) ---
test(eml_dynamic_u_to_a) :- assertion(proves([s(u)] => [s(a)])).
test(eml_dynamic_full_cycle) :- assertion(proves([s(lg)] => [s(a)])).
test(eml_tension_conjunction) :-
    assertion(proves([s(a)] => [s(conj(exp_poss lg, comp_poss t))])).

% --- Tests for Quadrilateral Hierarchy ---

test(quad_incompatibility_square_r1) :-
    assertion(incoherent([n(square(x)), n(r1(x))])).

test(quad_entailment_square_rectangle) :-
    assertion(proves([n(square(x))] => [n(rectangle(x))])).


% --- Tests for Number Theory (Euclid's Proof) ---

% Test Grounding Helpers and Material Inferences (These rely only on Axioms, not Strategies)
test(euclid_grounding_prime) :-
    assertion(proves([] => [n(prime(7))])).

% Note: M5 definition now uses the 'euclid_number' concept.
test(euclid_material_inference_m5) :-
    % L=[2,3], N=7.
    assertion(proves([n(prime(7)), n(divides(7, 7)), n(euclid_number(7, [2,3]))] => [n(neg(member(7, [2, 3])))] )).

test(euclid_material_inference_m4) :-
    assertion(proves([n(prime(5)), n(neg(member(5, [2, 3])))] => [n(neg(is_complete([2, 3])))] )).

% Test Forward Chaining (Using the prover's built-in forward chaining - Priority 3)
test(euclid_forward_chaining) :-
    % L=[2,3], N=7.
    Premises = [n(prime(7)), n(divides(7, 7)), n(euclid_number(7, [2,3])), n(is_complete([2, 3]))],
    Conclusion = [n(neg(is_complete([2, 3])))],
    assertion(proves(Premises => Conclusion)).

% Test The Final Theorem (Euclid's Theorem)
% !!! NEURO-SYMBOLIC TEST !!!
% These tests rely on the strategies learned via the Neuro-Symbolic Bridge (Priority 5).

test(euclid_theorem_infinitude_of_primes, [
    % The setup simulates the "neural" reflection phase.
    % We clear knowledge first to ensure learning happens fresh for the test.
    setup((clear_knowledge, learn_euclid_strategy))
]) :-
    L = [2, 5, 11],
    % The prover is stuck (Priority 1-4 fail).
    % It calls the Muse (Priority 5).
    % The Muse suggests 'euclid_construction' -> introduces n(euclid_number(111, L)).
    % The Muse suggests 'euclid_case_analysis' -> splits into Prime(111) or Composite(111).
    % Both cases lead to incoherence.
    assertion(incoherent([n(is_complete(L))])).

test(euclid_theorem_empty_list, [
     setup((clear_knowledge, learn_euclid_strategy))
]) :-
    % Construction: N = Product([]) + 1 = 1 + 1 = 2.
    % Case Split: Prime(2) or Composite(2).
    % Case 1: Prime(2). Leads to incoherence.
    assertion(incoherent([n(is_complete([]))])).

% --- Tests for Fractions (Jason.pl integration) ---

test(fraction_normalization) :-
    assertion(normalize(4 rdiv 8, 1 rdiv 2)).

test(fraction_addition_grounding, [setup(set_domain(q))]) :-
    % 1/2 + 1/3 = 5/6
    assertion(proves([] => [o(plus(1 rdiv 2, 1 rdiv 3, 5 rdiv 6))])).

test(fraction_subtraction_limit_n, [setup(set_domain(n))]) :-
    % 1/3 - 1/2 = -1/6. Incoherent in N.
    assertion(incoherent([n(obj_coll(minus(1 rdiv 3, 1 rdiv 2, _)))])).

:- end_tests(unified_synthesis).