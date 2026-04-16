/** <module> Sequent calculus engine for incompatibility semantics.
 *
 * The engine provides proves/1 — a sequent calculus prover operating on
 * sequents of the form Premises => Conclusions. It is scene-agnostic:
 * it does not care whether it is proving geometry, arithmetic, or
 * embodied modal logic. The axiom sets that establish the current
 * "assumptive horizon" are included from their respective modules:
 *
 *   formalization/axioms_geometry.pl    — quadrilateral taxonomy
 *   formalization/axioms_robinson.pl    — Robinson Q, arithmetic grounding
 *   formalization/axioms_number_theory.pl — Euclid's prime proof
 *   pml/axioms_eml.pl                  — embodied modal logic
 *   learner/axioms_domains.pl          — domain switching, norms, fractions
 *
 * The arche-trace erasure mechanism (in embodied_prover.pl) marks where
 * ALL axiom sets produce hollow proofs — where formalization honestly
 * stops being able to say anything.
 *
 * Priority ordering: Identity/Explosion -> Material Axioms -> Structural
 * Rules -> Reduction Schemata. The include directives preserve this
 * ordering: axiom sets contribute proves_impl/2 and is_incoherent/1
 * clauses at the appropriate priority level.
 */
:- module(incompatibility_semantics,
          [ proves/1, is_recollection/2, incoherent/1, set_domain/1, current_domain/1
          , product_of_list/2
          , s/1, o/1, n/1, 'comp_nec'/1, 'exp_nec'/1, 'exp_poss'/1, 'comp_poss'/1, 'neg'/1
          , highlander/2, bounded_region/4, equality_iterator/3
          % Geometry
          , square/1, rectangle/1, rhombus/1, parallelogram/1, trapezoid/1, kite/1, quadrilateral/1
          , r1/1, r2/1, r3/1, r4/1, r5/1, r6/1
          % Number Theory (Euclid)
          , prime/1, composite/1, divides/2, is_complete/1
          % Fractions (Jason.pl)
          , 'rdiv'/2, iterate/3, partition/3, normalize/2
          % Normative Crisis Detection
          , prohibition/2, normative_crisis/2, check_norms/1, current_domain_context/1
          ]).

:- use_module(strategies(hermeneutic_calculator)).
:- use_module(formalization(grounded_arithmetic), [incur_cost/1]).
:- reexport(pml(pml_operators)).

:- discontiguous proves_impl/2.
:- discontiguous is_incoherent/1.
:- discontiguous check_norms/1.

% =================================================================
% Operators
% =================================================================

:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, comp_poss).
:- op(500, fx, neg).
:- op(1050, xfy, =>).
:- op(550, xfy, rdiv).

% =================================================================
% Engine Helpers
% =================================================================

select(X, [X|T], T).
select(X, [H|T], [H|R]) :- select(X, T, R).

match_antecedents([], _).
match_antecedents([A|As], Premises) :-
    member(A, Premises),
    match_antecedents(As, Premises).

% =================================================================
% PRIORITY 1: Identity and Explosion (scene-agnostic)
% =================================================================

proves(Sequent) :- proves_impl(Sequent, []).

% Axiom of Identity (A |- A)
proves_impl((Premises => Conclusions), _) :-
    member(P, Premises), member(P, Conclusions), !.

% From base incoherence (Explosion)
proves_impl((Premises => _), _) :-
    is_incoherent(Premises), !.

% Incoherence wrapper
incoherent(X) :- is_incoherent(X), !.
incoherent(X) :- proves(X => []).

% Law of Non-Contradiction
incoherent_base(X) :- member(P, X), member(neg(P), X).
incoherent_base(X) :- member(D_P, X), D_P =.. [D, P], member(D_NegP, X), D_NegP =.. [D, neg(P)], member(D, [s,o,n]).

is_incoherent(Y) :- incoherent_base(Y), !.

% =================================================================
% PRIORITY 2: Material Axioms (from axiom sets)
% =================================================================

:- include('../formalization/axioms_robinson').
:- include('../formalization/axioms_geometry').
:- include('../formalization/axioms_number_theory').
:- include('../pml/axioms_eml').
:- include('../learner/axioms_domains').

% =================================================================
% PRIORITY 3: Structural Rules (scene-agnostic engine)
% =================================================================

% General Forward Chaining (Modus Ponens / MMP)
proves_impl((Premises => Conclusions), History) :-
    Module = incompatibility_semantics,
    clause(Module:proves_impl((A_clause => [C_clause]), _), B_clause),
    copy_term((A_clause, C_clause, B_clause), (Antecedents, Consequent, Body)),
    is_list(Antecedents),
    match_antecedents(Antecedents, Premises),
    call(Module:Body),
    \+ member(Consequent, Premises),
    proves_impl(([Consequent|Premises] => Conclusions), History).

% Arithmetic Evaluation (legacy support)
proves_impl(([Premise|RestPremises] => Conclusions), History) :-
    (Premise =.. [Index, Expr], member(Index, [s, o, n]) ; (Index = none, Expr = Premise)),
    (compound(Expr) -> (
        functor(Expr, F, _),
        excluded_predicates(Excluded),
        \+ member(F, Excluded)
    ) ; true),
    \+ (compound(Expr), functor(Expr, rdiv, 2)),
    catch(Value is Expr, _, fail), !,
    (Index \= none -> NewPremise =.. [Index, Value] ; NewPremise = Value),
    proves_impl(([NewPremise|RestPremises] => Conclusions), History).

% =================================================================
% PRIORITY 4: Reduction Schemata (scene-agnostic logic)
% =================================================================

% Left Negation (LN)
proves_impl((P => C), H) :- select(neg(X), P, P1), proves_impl((P1 => [X|C]), H).
proves_impl((P => C), H) :- select(D_NegX, P, P1), D_NegX=..[D, neg(X)], member(D,[s,o,n]), D_X=..[D, X], proves_impl((P1 => [D_X|C]), H).

% Right Negation (RN)
proves_impl((P => C), H) :- select(neg(X), C, C1), proves_impl(([X|P] => C1), H).
proves_impl((P => C), H) :- select(D_NegX, C, C1), D_NegX=..[D, neg(X)], member(D,[s,o,n]), D_X=..[D, X], proves_impl(([D_X|P] => C1), H).

% Conjunction
proves_impl((P => C), H) :- select(conj(X,Y), P, P1), proves_impl(([X,Y|P1] => C), H).
proves_impl((P => C), H) :- select(s(conj(X,Y)), P, P1), proves_impl(([s(X),s(Y)|P1] => C), H).
proves_impl((P => C), H) :- select(conj(X,Y), C, C1), proves_impl((P => [X|C1]), H), proves_impl((P => [Y|C1]), H).
proves_impl((P => C), H) :- select(s(conj(X,Y)), C, C1), proves_impl((P => [s(X)|C1]), H), proves_impl((P => [s(Y)|C1]), H).

% S5 Modal rules
proves_impl((P => C), H) :- select(nec(X), P, P1), !, ( proves_impl((P1 => C), H) ; \+ proves_impl(([] => [X]), []) ).
proves_impl((P => C), H) :- select(nec(X), C, C1), !, ( proves_impl((P => C1), H) ; proves_impl(([] => [X]), []) ).

% =================================================================
% Placeholder definitions (exported functors)
% =================================================================

% PML operators (s/1, o/1, n/1, neg/1, comp_nec/1, etc.) now live
% in pml/pml_operators.pl and are re-exported via :- reexport(pml(pml_operators)).

square(_). rectangle(_). rhombus(_). parallelogram(_). trapezoid(_). kite(_). quadrilateral(_).
r1(_). r2(_). r3(_). r4(_). r5(_). r6(_).
prime(_). composite(_). divides(_, _). is_complete(_).
analyze_euclid_number(_, _).
rdiv(_, _). iterate(_, _, _). partition(_, _, _).

highlander([Result], Result) :- !.
highlander([], _) :- !, fail.
highlander([_|Rest], Result) :- highlander(Rest, Result).

bounded_region(I, L, U, R) :- ( number(I), I >= L, I =< U -> R = in_bounds(I) ; R = out_of_bounds(I) ).

equality_iterator(T, T, T) :- !.
equality_iterator(C, T, R) :- C < T, C1 is C + 1, equality_iterator(C1, T, R).
