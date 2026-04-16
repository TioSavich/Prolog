/** <module> Semantic Axioms (Inter-Modal Dynamics)
 *
 *  This module defines the semantic axioms of Polarized Modal Logic (PML).
 *  These axioms govern the vocabulary and the interaction between the modes (S, O, N).
 *  They are defined as material inferences, integrating with the incompatibility_semantics module.
 *
 *  (Synthesis_1, Chapter 3.6 and Chapter 4)
 */
:- module(semantic_axioms, []).

% Import operators - must be declared before use
:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, comp_poss).
:- op(500, fx, neg).

% Note: We do not explicitly use_module(incompatibility_semantics), but we rely on its definition of material_inference/3.
:- use_module(pml_operators). % Import operators for readability

% =================================================================
% Multifile Declarations
% =================================================================
% We extend the material_inference predicate defined in incompatibility_semantics.
:- multifile embodied_prover:material_inference/3.

% =================================================================
% Dialectical Rhythm (Data-Driven)
% =================================================================
% Each fact encodes a row from the Section 5 table in synthesized_paper.md:
% Stage -> Modal transition. The generic material_inference clause below
% keeps the implementation concise and prevents duplicate definitions.

dialectical_transition(u,        comp_nec(a)).        % Emergence of tension from unity
dialectical_transition(u_prime,  comp_nec(a)).        % Re-entry into the next cycle
dialectical_transition(a,        exp_poss(lg)).       % Letting-go option
dialectical_transition(a,        comp_poss(t)).       % Temptation to fixate
dialectical_transition(lg,       exp_nec(u_prime)).   % Sublation / release
dialectical_transition(t,        comp_nec(neg(u))).   % Pathological contraction
dialectical_transition(t_b,      comp_nec(t_n)).      % Bad infinite (Being -> Nothing)
dialectical_transition(t_n,      comp_nec(t_b)).      % Bad infinite (Nothing -> Being)

embodied_prover:material_inference([s(Stage)], s(ModalTerm), true) :-
    dialectical_transition(Stage, ModalTerm).


% =================================================================
% Inter-Modal Dynamics
% =================================================================
% (Synthesis_1, Chapter 3.6)

% --- Principle 2: The Oobleck Dynamic (S-O Transfer) ---

% Box_down_S => Box_down_O (Effort/Force -> Crystallization)
embodied_prover:material_inference([s(comp_nec P)], o(comp_nec P), true).

% Box_up_S => Box_up_O (Release/Openness -> Liquefaction)
embodied_prover:material_inference([s(exp_nec P)], o(exp_nec P), true).

% --- Principle 5: Internalization of Norms (N -> S) ---
% Formulated here as N-N dynamics reflecting the collective rhythm.

% Normative Solidification leading to potential opening
embodied_prover:material_inference([n(comp_nec P)], n(exp_poss P), true).

% Normative Liquefaction leading to potential re-closure
embodied_prover:material_inference([n(exp_nec P)], n(comp_poss P), true).

% =================================================================
% Hylomorphic Shift: O → N (added 2026-04-16 per Phase 5 audit §6)
% =================================================================
% The Phase 5 audit flagged that the three modes — s/1, o/1, n/1 — carry
% no rule-governed handoffs beyond the Oobleck S→O transfer above. Modes
% read as permissive type-tags rather than as inferential registers.
%
% This rule operationalizes the Hegelian dictum "what is rational is
% actual": an objective proof (execution that reaches a formal q_accept)
% licenses a normative commitment. The shift is read directly off
% Brandom's bimodal hylomorphic conceptual realism (ASOT, ch. 1–2): the
% same conceptual content can wear either form; the rule grants the
% practitioner the entitlement to re-wear it normatively once it has
% been objectively made out.
%
% We do not yet add an N → S internalization rule. N → S is named in the
% prose accompanying this framework but the conditions under which
% normative commitment becomes subjectively embraced (internalized)
% depend on trace-grounding via the I-feeling mechanism, which the
% intersubjective praxis module only partly models. That rule awaits
% the next revision.
embodied_prover:material_inference([o(P)], n(P), true).
