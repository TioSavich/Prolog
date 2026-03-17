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
:- multifile incompatibility_semantics:material_inference/3.

% =================================================================
% The Dialectical Engine (The Rhythm of Thought)
% =================================================================
% The fundamental rhythm: U -> Box_down(A) -> Diamond_up(LG) -> Box_up(U')
% (Synthesis_1, Chapter 4.2)

% 2. First Negation (Compression ↓): Emergence of Awareness/Tension (A)
% [s(u)] => [s(comp_nec a)]
incompatibility_semantics:material_inference([s(u)], s(comp_nec a), true).
incompatibility_semantics:material_inference([s(u_prime)], s(comp_nec a), true).

% 4. Choice Point (Possibility ↑): Recognizing the instability.
% [s(a)] => [s(exp_poss lg)] (Possibility of Letting Go)
incompatibility_semantics:material_inference([s(a)], s(exp_poss lg), true).
% [s(a)] => [s(comp_poss t)] (Temptation of Fixation T)
incompatibility_semantics:material_inference([s(a)], s(comp_poss t), true).

% 5. Second Negation/Sublation (Expansion ↑) or Fixation (Pathology)

% 5a. Sublation: Letting go results in necessary release (U')
% [s(lg)] => [s(exp_nec u_prime)]
incompatibility_semantics:material_inference([s(lg)], s(exp_nec u_prime), true).

% 5b. Fixation (Pathology): Deepened Contraction
% [s(t)] => [s(comp_nec neg(u))]
incompatibility_semantics:material_inference([s(t)], s(comp_nec neg(u)), true).


% =================================================================
% The Bad Infinite (Closed Compressive Cycle)
% =================================================================
% Example: Hegel's Being (t_b) and Nothing (t_n) oscillation.
% (Synthesis_1, Chapter 4.4, Definition 1)

incompatibility_semantics:material_inference([s(t_b)], s(comp_nec t_n), true).
incompatibility_semantics:material_inference([s(t_n)], s(comp_nec t_b), true).


% =================================================================
% Inter-Modal Dynamics
% =================================================================
% (Synthesis_1, Chapter 3.6)

% --- Principle 2: The Oobleck Dynamic (S-O Transfer) ---

% Box_down_S => Box_down_O (Effort/Force -> Crystallization)
incompatibility_semantics:material_inference([s(comp_nec P)], o(comp_nec P), true).

% Box_up_S => Box_up_O (Release/Openness -> Liquefaction)
incompatibility_semantics:material_inference([s(exp_nec P)], o(exp_nec P), true).

% --- Principle 5: Internalization of Norms (N -> S) ---
% Formulated here as N-N dynamics reflecting the collective rhythm.

% Normative Solidification leading to potential opening
incompatibility_semantics:material_inference([n(comp_nec P)], n(exp_poss P), true).

% Normative Liquefaction leading to potential re-closure
incompatibility_semantics:material_inference([n(exp_nec P)], n(comp_poss P), true).
