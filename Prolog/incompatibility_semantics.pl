/** <module> Incompatibility Semantics and Embodied Core Prover
 *
 *  This module implements the core of the Brandomian semantic framework.
 *  It provides a sequent calculus-based theorem prover augmented for
 *  Polarized Modal Logic (PML) and the Deconstructive Trace mechanism.
 *
 *  The prover tracks Modal Context (Compressive/Expansive) and Cognitive Resources,
 *  modeling the embodied experience of reasoning.
 *
 *  (Synthesis_1, Chapters 2.3, 3, and 4)
 */
:- module(incompatibility_semantics,
          [ proves/4, % (Sequent, ResourcesIn, ResourcesOut, Proof)
            incoherent/1,
            % Internals exposed for cross-module definitions
            proves_impl/7,
            is_incoherent/1,
            material_inference/3,
            construct_proof/4
          ]).

:- use_module(pml_operators).
:- use_module(utils, [select/3, match_antecedents/2]).
:- use_module(automata, [contains_trace/1]). % For the Erasure mechanism

% =================================================================
% Configuration and Multifile Declarations
% =================================================================

:- discontiguous proves_impl/7.
:- multifile proves_impl/7.
:- discontiguous is_incoherent/1.
:- multifile is_incoherent/1.
% material_inference/3 MUST be multifile and discontiguous to allow extension by semantic_axioms.pl
:- discontiguous material_inference/3.
:- multifile material_inference/3.


% =================================================================
% Part 0: Embodied Cognition Helpers
% =================================================================

%!      get_inference_cost(+ModalContext, -Cost) is det.
%
%       Determines the inference cost based on the current modal context.
get_inference_cost(compressive, 2). % Compressive state (↓) is more taxing.
get_inference_cost(expansive, 1).  % Expansive state (↑) is less taxing.
get_inference_cost(neutral, 1).

%!      check_viability(+Resources, +Cost) is semidet.
%
%       Succeeds if the resources are sufficient. Throws a perturbation if exhausted.
check_viability(R, Cost) :- R >= Cost, !.
check_viability(_, _) :-
    % Constraint violated: PERTURBATION DETECTED
    throw(perturbation(resource_exhaustion)).

%!      determine_modal_context(+ModalOperatorTerm, -Context) is det.
%
%       Maps a PML operator term to its corresponding ModalContext.
determine_modal_context(M_Q, Context) :-
    ( functor(M_Q, comp_nec, 1) ; functor(M_Q, comp_poss, 1) ) -> Context = compressive ;
    ( functor(M_Q, exp_nec, 1) ; functor(M_Q, exp_poss, 1) ) -> Context = expansive.


% =================================================================
% Part 1: Incoherence Definitions
% =================================================================

incoherent(X) :- is_incoherent(X), !.
% For the recursive check, we assume a fixed high budget (e.g., 1000).
incoherent(X) :- proves(X => [], 1000, _, _Proof).

% --- Base Incoherence (LNC) ---
incoherent_base(X) :- member(P, X), member(neg(P), X).
incoherent_base(X) :-
    member(D_P, X), D_P =.. [D, P],
    member(D_NegP, X), D_NegP =.. [D, neg(P)],
    member(D, [s,o,n]).

is_incoherent(Y) :- incoherent_base(Y), !.


% =================================================================
% Part 2: Sequent Calculus Prover (Augmented and Embodied)
% =================================================================

%!      proves(+Sequent, +R_In, -R_Out, -Proof) is semidet.
%
%       The public wrapper for the embodied prover. Initializes Context to `neutral`.
proves(Sequent, R_In, R_Out, Proof) :-
    proves_impl(Sequent, [], neutral, _CtxOut, R_In, R_Out, Proof).


% --- Proof Construction and Erasure ---
% (Independent of embodiment, relies on automata:contains_trace/1)

construct_proof(RuleName, Sequent, SubProofs, Proof) :-
    % 1. Propagation: If any subproof is erased, the whole justification is erased.
    ( member(erasure(_), SubProofs) -> Proof = erasure(propagation) ;
    % 2. Contamination: If the sequent itself contains the Trace Entity.
      ( contains_trace(Sequent) -> Proof = erasure(RuleName)
      ; Proof = proof(RuleName, Sequent, SubProofs)
      )
    ), !.


% =================================================================
% The Prover Implementation (proves_impl/7)
% Signature: (Sequent, History, CtxIn, CtxOut, R_In, R_Out, Proof)
% =================================================================

% --- PRIORITY 1: Identity and Explosion ---

% Axiom of Identity (A |- A)
proves_impl((P => C), _H, Ctx, Ctx, R_In, R_Out, Proof) :-
    member(X, P), member(X, C), !,
    % Embodiment: Deduct cost based on current context.
    get_inference_cost(Ctx, Cost),
    check_viability(R_In, Cost),
    R_Out is R_In - Cost,
    construct_proof(identity, (P => C), [], Proof).

% Explosion (Ex Falso Quodlibet)
proves_impl((P => C), _H, Ctx, Ctx, R_In, R_Out, Proof) :-
    is_incoherent(P), !,
    % Embodiment: Deduct cost.
    get_inference_cost(Ctx, Cost),
    check_viability(R_In, Cost),
    R_Out is R_In - Cost,
    construct_proof(explosion, (P => C), [], Proof).


% --- PRIORITY 2: Structural Rules (PML Dynamics / Dialectical Engine) ---

% This rule drives state transitions AND manages the Modal Context switch.
proves_impl((P => C), H, CtxIn, CtxOut, R_In, R_Out, Proof) :-
    select(s(X), P, RestP),
    \+ member(s(X), H),
    pml_rhythm_axiom(s(X), s(M_Q)),

    % Embodiment: Determine the new context based on the modality (↓ or ↑).
    determine_modal_context(M_Q, CtxNew),

    % Embodiment: Deduct cost for the transition itself, based on the *incoming* context.
    get_inference_cost(CtxIn, Cost),
    check_viability(R_In, Cost),
    R_Mid is R_In - Cost,

    ( ( M_Q =.. [comp_nec, Q] ; M_Q =.. [exp_nec, Q] ) ->
        % Case 1: Necessity drives state transition.
        % The sub-proof is executed in the *new* context (CtxNew).
        proves_impl(([s(Q)|RestP] => C), [s(X)|H], CtxNew, CtxOut, R_Mid, R_Out, SubProof),
        construct_proof(pml_rhythm(s(X) => s(M_Q)), (P => C), [SubProof], Proof)
    ;
        % Case 2: Possibility check.
      (( functor(M_Q, exp_poss, 1) ; functor(M_Q, comp_poss, 1) ),
       (member(s(M_Q), C) ; member(M_Q, C)),
       % No sub-proof, so CtxOut is CtxNew, R_Out is R_Mid.
       CtxOut = CtxNew,
       R_Out = R_Mid,
       construct_proof(pml_possibility_check, (P => C), [], Proof)
      )
    ).


% --- PRIORITY 3: General Structural Rule: Forward Chaining (MMP) ---

proves_impl((P => C), H, CtxIn, CtxOut, R_In, R_Out, Proof) :-
    % 1. Find an applicable material inference rule.
    material_inference(Antecedents, Consequent, Body),
    is_list(Antecedents),

    % Embodiment: Deduct cost for axiom lookup/matching, based on current context.
    get_inference_cost(CtxIn, Cost),
    check_viability(R_In, Cost),
    R_Mid is R_In - Cost,

    % 2. Check antecedents and execute body.
    match_antecedents(Antecedents, P),
    call(Body), % Engine Level Trace check (attr_unify_hook) happens here.
    \+ member(Consequent, P),

    % 3. Continue the proof search. Context flows through the sub-proof.
    proves_impl(([Consequent|P] => C), H, CtxIn, CtxOut, R_Mid, R_Out, SubProof),

    Axiom = (Antecedents => Consequent),
    construct_proof(mmp(Axiom), (P => C), [SubProof], Proof).


% --- PRIORITY 4: Reduction Schemata (Logical Connectives) ---

% Helper macro for single-premise reduction rules (LN, RN, L-Conj)
% Handles boilerplate for cost deduction and context flow.
apply_reduction_single(Sequent, H, CtxIn, CtxOut, R_In, R_Out, RuleName, SubSequent, Proof) :-
    % Embodiment: Deduct cost.
    get_inference_cost(CtxIn, Cost),
    check_viability(R_In, Cost),
    R_Mid is R_In - Cost,
    % Context flows through the sub-proof.
    proves_impl(SubSequent, H, CtxIn, CtxOut, R_Mid, R_Out, SubProof),
    construct_proof(RuleName, Sequent, [SubProof], Proof).

% Left Negation (LN) and (Modal)
proves_impl((P => C), H, CtxIn, CtxOut, R_In, R_Out, Proof) :-
    select(neg(X), P, P1),
    apply_reduction_single((P => C), H, CtxIn, CtxOut, R_In, R_Out, ln, (P1 => [X|C]), Proof).

proves_impl((P => C), H, CtxIn, CtxOut, R_In, R_Out, Proof) :-
    select(D_NegX, P, P1), D_NegX=..[D, neg(X)], member(D,[s,o,n]), D_X=..[D, X],
    apply_reduction_single((P => C), H, CtxIn, CtxOut, R_In, R_Out, ln_modal(D), (P1 => [D_X|C]), Proof).

% Right Negation (RN) and (Modal)
proves_impl((P => C), H, CtxIn, CtxOut, R_In, R_Out, Proof) :-
    select(neg(X), C, C1),
    apply_reduction_single((P => C), H, CtxIn, CtxOut, R_In, R_Out, rn, ([X|P] => C1), Proof).

proves_impl((P => C), H, CtxIn, CtxOut, R_In, R_Out, Proof) :-
    select(D_NegX, C, C1), D_NegX=..[D, neg(X)], member(D,[s,o,n]), D_X=..[D, X],
    apply_reduction_single((P => C), H, CtxIn, CtxOut, R_In, R_Out, rn_modal(D), ([D_X|P] => C1), Proof).

% Conjunction (Left) and (Modal)
proves_impl((P => C), H, CtxIn, CtxOut, R_In, R_Out, Proof) :-
    select(conj(X,Y), P, P1),
    apply_reduction_single((P => C), H, CtxIn, CtxOut, R_In, R_Out, l_conj, ([X,Y|P1] => C), Proof).

proves_impl((P => C), H, CtxIn, CtxOut, R_In, R_Out, Proof) :-
    select(D_Conj, P, P1), D_Conj=..[D, conj(X,Y)], member(D,[s,o,n]), DX=..[D, X], DY=..[D, Y],
    apply_reduction_single((P => C), H, CtxIn, CtxOut, R_In, R_Out, l_conj_modal(D), ([DX,DY|P1] => C), Proof).


% Conjunction (Right) - Branching Rule
% Resource management for branching rules: The remaining resources from the first branch are used for the second.
proves_impl((P => C), H, CtxIn, CtxOut, R_In, R_Out, Proof) :-
    select(conj(X,Y), C, C1),
    get_inference_cost(CtxIn, Cost),
    check_viability(R_In, Cost),
    R_Start is R_In - Cost,

    % Branch A
    proves_impl((P => [X|C1]), H, CtxIn, CtxMid, R_Start, R_Mid, ProofA),
    % Branch B (Starts with resources and context left by Branch A)
    proves_impl((P => [Y|C1]), H, CtxMid, CtxOut, R_Mid, R_Out, ProofB),
    construct_proof(r_conj, (P => C), [ProofA, ProofB], Proof).

% Conjunction (Right, Modal) - Branching Rule
proves_impl((P => C), H, CtxIn, CtxOut, R_In, R_Out, Proof) :-
    select(D_Conj, C, C1), D_Conj=..[D, conj(X,Y)], member(D,[s,o,n]), DX=..[D, X], DY=..[D, Y],
    get_inference_cost(CtxIn, Cost),
    check_viability(R_In, Cost),
    R_Start is R_In - Cost,

    % Branch A
    proves_impl((P => [DX|C1]), H, CtxIn, CtxMid, R_Start, R_Mid, ProofA),
    % Branch B
    proves_impl((P => [DY|C1]), H, CtxMid, CtxOut, R_Mid, R_Out, ProofB),
    construct_proof(r_conj_modal(D), (P => C), [ProofA, ProofB], Proof).


% =================================================================
% Helper Predicates
% =================================================================

% Helper to find applicable PML rhythm axioms defined via material_inference/3.
pml_rhythm_axiom(A, C) :-
    A = s(_),
    % Access axioms defined via material_inference/3 (e.g., in semantic_axioms.pl).
    material_inference([A], C, true),
    is_pml_modality(C).

is_pml_modality(M) :-
    M=..[D, OpTerm], member(D, [s,o,n]),
    OpTerm=..[Op, _], member(Op, [comp_nec, exp_nec, comp_poss, exp_poss]).
