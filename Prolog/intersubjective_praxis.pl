/** <module> Intersubjective Praxis (Multi-agent Dynamics)
 *
 *  This module implements the dynamics of interaction, dialogue, and recognition
 *  between multiple agents. It focuses on the Oobleck Dynamic and the structure
 *  of mutual recognition (Geist).
 *
 *  (Synthesis_1, Chapter 5.3 and 7.3)
 */
:- module(intersubjective_praxis, []).

% Import operators - must be declared before use
:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, comp_poss).
:- op(500, fx, neg).

:- use_module(incompatibility_semantics).
:- use_module(pml_operators).

% =================================================================
% Multifile Declarations
% =================================================================
% Extend the logic engine with intersubjective axioms.
:- multifile incompatibility_semantics:material_inference/3.

% =================================================================
% The Oobleck Dynamic (Inter-Agent S-O Transfer)
% =================================================================
% Principle: Applying compressive force (Box_down_S) by Agent A
% will predictably lead to the crystallization (Box_down_O) of Agent B's position.
% (Synthesis_1, Principle 2 and Chapter 5.3)

% [s(comp_nec(action(A, aggressive)))] => [o(comp_nec(position(B, crystallized)))]
incompatibility_semantics:material_inference(
    [s(comp_nec(action(A, aggressive)))],
    o(comp_nec(position(B, crystallized))),
    (A \= B) % Body ensures agents are distinct
).

% Principle: Introducing subjective expansion (Box_up_S) by Agent A
% will predictably encourage the liquefaction (Box_up_O) of Agent B's position.

% [s(exp_nec(action(A, listening)))] => [o(exp_nec(position(B, liquefied)))]
incompatibility_semantics:material_inference(
    [s(exp_nec(action(A, listening)))],
    o(exp_nec(position(B, liquefied))),
    (A \= B)
).

% =================================================================
% Recognition (Anerkennung)
% =================================================================
% The Grand Sublation: Forgiveness (Mutual Recognition) realizes Geist.
% (Synthesis_1, Chapter 7.3)

% Mutual Confession and Forgiveness leads to necessary normative release (Geist).
% [n(confession(A)), n(confession(B))] => [n(exp_nec(forgiveness(A, B)))]
incompatibility_semantics:material_inference(
    [n(confession(A)), n(confession(B))],
    n(exp_nec(forgiveness(A, B))),
    (A \= B)
).
