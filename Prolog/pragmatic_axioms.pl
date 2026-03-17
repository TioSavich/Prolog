/** <module> Pragmatic Axioms (The Axioms of Praxis)
 *
 *  This module defines the pragmatic axioms governing embodied action,
 *  separated from the semantic rules. These articulate the fundamental
 *  drives and limitations of praxis by integrating them directly into the logic.
 *
 *  (Synthesis_1, Chapter 4.1)
 */
:- module(pragmatic_axioms,
          [
            i_feeling/1,        % I_f (The Elusive Subject)
            identity_claim/1,   % C_Id (The Objectified Self)
            impetus/1           % I (Holistic striving)
          ]).

% Import operators - must be declared before use
:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, comp_poss).
:- op(500, fx, neg).

:- use_module(automata, [generate_trace/1, contains_trace/1]).
:- use_module(incompatibility_semantics).
:- use_module(pml_operators).

% =================================================================
% Multifile Declarations
% =================================================================
% Extend the logic engine with the pragmatic axioms.
:- multifile incompatibility_semantics:material_inference/3.
:- multifile incompatibility_semantics:is_incoherent/1.

% =================================================================
% The Vocabulary of Praxis
% =================================================================

%!  i_feeling(?I_f) is semidet.
%   The I-Feeling Mode (I_f): The singular, unifying aspect of experience; the elusive subject.
%   Implemented using the Arche-Trace to ensure it resists objectification.
i_feeling(I_f) :-
    (var(I_f) -> generate_trace(I_f) ; contains_trace(I_f)).

%!  identity_claim(?C_Id) is semidet.
%   The Identity Claim (C_Id): The articulated, objectified self (the "me").
%   Must be a concrete term (cannot contain the Trace).
identity_claim(C_Id) :-
    \+ contains_trace(C_Id).

%!  impetus(?I) is semidet.
%   The Impetus to Act (I): The holistic, pre-conceptual striving.
%   (Placeholder definition, as its holistic nature resists full formalization).
impetus(holistic_striving).

% =================================================================
% Axiom 1: The Elusive Subject (S-O Inversion)
% =================================================================
% Any attempt to subjectively fixate (Box_down_S) the I-Feeling (I_f)
% results in its necessary objective dissolution (Box_up_O).
% (Synthesis_1, Chapter 3.6.1, Axiom 1)

% Box_down_S(I_f) => Box_up_O(I_f)
incompatibility_semantics:material_inference(
    [s(comp_nec I_f)],
    o(exp_nec I_f),
    i_feeling(I_f) % Body ensures I_f is the Trace
).

% =================================================================
% Axiom 3: The Unsatisfiable Desire
% =================================================================
% The infinite desire for recognition of the "I" (I_f) can never be fully
% satisfied by the recognition of a finite identity claim (C_Id).
% (Synthesis_1, Chapter 4.1.1, Axiom 3)

% This is implemented as an incoherence: It is impossible to simultaneously
% hold that an Identity Claim (C_Id) fully represents the I-Feeling (I_f).

incompatibility_semantics:is_incoherent(X) :-
    member(n(represents(C_Id, I_f)), X),
    identity_claim(C_Id),
    i_feeling(I_f).
