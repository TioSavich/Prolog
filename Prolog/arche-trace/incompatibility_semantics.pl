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
          [ proves/1, safe_proves/2, is_recollection/2, incoherent/1, normalize/2
          , set_domain/1, current_domain/1
          , enable_axiom_pack/1, disable_axiom_pack/1, enabled_axiom_pack/1, with_axiom_packs/2
          , product_of_list/2
          , s/1, o/1, n/1, 'comp_nec'/1, 'exp_nec'/1, 'exp_poss'/1, 'comp_poss'/1, 'neg'/1
          , bounded_region/4, equality_iterator/3
          % Normative Crisis Detection
          , prohibition/2, normative_crisis/2, check_norms/1, current_domain_context/1
          ]).

:- use_module(formalization(grounded_arithmetic), [incur_cost/1]).
:- use_module(library(time), [call_with_time_limit/2]).
:- reexport(pml(pml_operators)).

:- discontiguous proves_impl/2.
:- discontiguous is_incoherent/1.
:- discontiguous check_norms/1.
:- dynamic axiom_pack_enabled/1.

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

default_axiom_pack(robinson).
default_axiom_pack(geometry).
default_axiom_pack(number_theory).
default_axiom_pack(eml).
default_axiom_pack(domains).

initialize_axiom_packs :-
    retractall(axiom_pack_enabled(_)),
    forall(default_axiom_pack(Pack),
           assertz(axiom_pack_enabled(Pack))).

known_axiom_pack(Pack) :-
    default_axiom_pack(Pack),
    !.
known_axiom_pack(Pack) :-
    throw(error(domain_error(axiom_pack, Pack), _)).

enable_axiom_pack(Pack) :-
    known_axiom_pack(Pack),
    (   axiom_pack_enabled(Pack)
    ->  true
    ;   assertz(axiom_pack_enabled(Pack))
    ).

disable_axiom_pack(Pack) :-
    known_axiom_pack(Pack),
    retractall(axiom_pack_enabled(Pack)).

enabled_axiom_pack(Pack) :-
    axiom_pack_enabled(Pack).

set_enabled_axiom_packs(Packs) :-
    retractall(axiom_pack_enabled(_)),
    forall(member(Pack, Packs),
           assertz(axiom_pack_enabled(Pack))).

normalize_axiom_packs(all, Packs) :-
    !,
    findall(Pack, default_axiom_pack(Pack), Packs).
normalize_axiom_packs(Packs, Normalized) :-
    is_list(Packs),
    !,
    maplist(known_axiom_pack, Packs),
    sort(Packs, Normalized).
normalize_axiom_packs(Packs, _) :-
    throw(error(type_error(list, Packs), _)).

with_axiom_packs(Packs, Goal) :-
    normalize_axiom_packs(Packs, Normalized),
    findall(Pack, axiom_pack_enabled(Pack), Saved),
    setup_call_cleanup(
        set_enabled_axiom_packs(Normalized),
        call(Goal),
        set_enabled_axiom_packs(Saved)
    ).

option_value(Key, [Option|_], Value) :-
    Option =.. [Key, Value],
    !.
option_value(Key, [_|Rest], Value) :-
    option_value(Key, Rest, Value).

safe_proves(Sequent, Options) :-
    (   option_value(time_limit, Options, TimeLimit)
    ->  true
    ;   TimeLimit = 2
    ),
    (   option_value(packs, Options, Packs)
    ->  SafeGoal = with_axiom_packs(Packs, proves(Sequent))
    ;   SafeGoal = proves(Sequent)
    ),
    catch(call_with_time_limit(TimeLimit, SafeGoal),
          time_limit_exceeded,
          fail).

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

% These predicates are syntactic placeholders only. They must not succeed as
% raw goals, otherwise the engine can "prove" domain facts without going
% through the sequent rules.
square(_) :- fail.
rectangle(_) :- fail.
rhombus(_) :- fail.
parallelogram(_) :- fail.
trapezoid(_) :- fail.
kite(_) :- fail.
quadrilateral(_) :- fail.
r1(_) :- fail.
r2(_) :- fail.
r3(_) :- fail.
r4(_) :- fail.
r5(_) :- fail.
r6(_) :- fail.
prime(_) :- fail.
composite(_) :- fail.
divides(_, _) :- fail.
is_complete(_) :- fail.
analyze_euclid_number(_, _).
rdiv(_, _) :- fail.
iterate(_, _, _) :- fail.
partition(_, _, _) :- fail.

bounded_region(I, L, U, R) :- ( number(I), I >= L, I =< U -> R = in_bounds(I) ; R = out_of_bounds(I) ).

equality_iterator(T, T, T) :- !.
equality_iterator(C, T, R) :- C < T, C1 is C + 1, equality_iterator(C1, T, R).

:- initialization(initialize_axiom_packs).
