% Filename: incompatibility_semantics.pl (Neuro-Symbolic Integration)
:- module(incompatibility_semantics_neuro,
          [ proves/1, is_recollection/2, incoherent/1, set_domain/1, current_domain/1 % obj_coll/1 is deprecated
          , product_of_list/2 % Exported for the learner module
          % Updated exports
          , s/1, o/1, n/1, 'comp_nec'/1, 'exp_nec'/1, 'exp_poss'/1, 'comp_poss'/1, 'neg'/1
          , highlander/2, bounded_region/4, equality_iterator/3
          % Geometry
          , square/1, rectangle/1, rhombus/1, parallelogram/1, trapezoid/1, kite/1, quadrilateral/1
          , r1/1, r2/1, r3/1, r4/1, r5/1, r6/1
          % Number Theory (Euclid)
          , prime/1, composite/1, divides/2, is_complete/1
          % Fractions (Jason.pl)
          , 'rdiv'/2, iterate/3, partition/3, normalize/2
          ]).
% Declare predicates that are defined across different sections.
:- use_module(hermeneutic_calculator). % Added for is_recollection/2

:- discontiguous proves_impl/2.
:- discontiguous is_incoherent/1. % Non-recursive check

% =================================================================
% Part 0: Setup and Configuration
% =================================================================

% Define operators
:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, comp_poss).
:- op(500, fx, neg).
:- op(1050, xfy, =>).
:- op(550, xfy, rdiv).

% =================================================================
% Part 1: Knowledge Domains
% =================================================================

% --- 1.1 Geometry ---
% (Geometry definitions remain the same as the original file)
incompatible_pair(square, r1). incompatible_pair(rectangle, r1). incompatible_pair(rhombus, r1). incompatible_pair(parallelogram, r1). incompatible_pair(kite, r1).
incompatible_pair(square, r2). incompatible_pair(rhombus, r2). incompatible_pair(kite, r2).
incompatible_pair(square, r3). incompatible_pair(rectangle, r3). incompatible_pair(rhombus, r3). incompatible_pair(parallelogram, r3).
incompatible_pair(square, r4). incompatible_pair(rhombus, r4). incompatible_pair(kite, r4).
incompatible_pair(square, r5). incompatible_pair(rectangle, r5). incompatible_pair(rhombus, r5). incompatible_pair(parallelogram, r5). incompatible_pair(trapezoid, r5).
incompatible_pair(square, r6). incompatible_pair(rectangle, r6).

is_shape(S) :- (incompatible_pair(S, _); S = quadrilateral), !.

entails_via_incompatibility(P, Q) :- P == Q, !.
entails_via_incompatibility(_, quadrilateral) :- !.
entails_via_incompatibility(P, Q) :- forall(incompatible_pair(Q, R), incompatible_pair(P, R)).

geometric_predicates([square, rectangle, rhombus, parallelogram, trapezoid, kite, quadrilateral, r1, r2, r3, r4, r5, r6]).

% --- 1.4 Fraction Domain ---
fraction_predicates([rdiv, iterate, partition]).

% --- 1.2 Arithmetic (O/N Domains) ---
% (Arithmetic definitions remain the same as the original file)

:- dynamic current_domain/1.
current_domain(n).

set_domain(D) :-
    ( member(D, [n, z, q]) -> retractall(current_domain(_)), assertz(current_domain(D)) ; true).

% The new core ontological predicate. It succeeds if `Term` is a
% validly constructed number, where `History` is the execution
% trace of the calculation that constructed it. This replaces the
% static `obj_coll/1` check with a dynamic, process-based validation.
is_recollection(0, [axiom(zero)]).
is_recollection(N, History) :-
    integer(N),
    N > 0,
    Prev is N - 1,
    is_recollection(Prev, _), % Foundational check on the predecessor
    hermeneutic_calculator:calculate(Prev, +, 1, _Strategy, N, History).
is_recollection(N, History) :-
    integer(N),
    N < 0,
    is_recollection(0, _), % Grounded in the axiom of zero
    Val is abs(N),
    hermeneutic_calculator:calculate(0, -, Val, _Strategy, N, History).
is_recollection(N rdiv D, [history(rational, from(N, D))]) :-
    % Denominator must be a positive integer. We check its recollection status.
    is_recollection(D, _),
    integer(D), D > 0,
    % Numerator can be any recollected number.
    is_recollection(N, _).

% --- Helpers for Rational Arithmetic ---
gcd(A, 0, A) :- A \= 0, !.
gcd(A, B, G) :- B \= 0, R is A mod B, gcd(B, R, G).

normalize(N, N) :- integer(N), !.
normalize(N rdiv D, R) :-
    (D =:= 1 -> R = N ;
        G is abs(gcd(N, D)),
        SN is N // G,
        SD is D // G,
        (SD =:= 1 -> R = SN ; R = SN rdiv SD)
    ), !.

perform_arith(+, A, B, C) :- C is A + B.
perform_arith(-, A, B, C) :- C is A - B.

arith_op(A, B, Op, C) :-
    member(Op, [+, -]),
    normalize(A, NA), normalize(B, NB),
    (integer(NA), integer(NB) ->
        perform_arith(Op, NA, NB, C_raw)
    ;
        (integer(NA) -> N1=NA, D1=1 ; NA = N1 rdiv D1),
        (integer(NB) -> N2=NB, D2=1 ; NB = N2 rdiv D2),

        D_res is D1 * D2,
        N1_scaled is N1 * D2,
        N2_scaled is N2 * D1,
        
        perform_arith(Op, N1_scaled, N2_scaled, N_res),

        C_raw = N_res rdiv D_res
    ),
    normalize(C_raw, C).

% --- 1.3 Number Theory Domain (Euclid) ---

% Added 'euclid_number' concept, introduced by the neuro-symbolic bridge.
number_theory_predicates([prime, composite, divides, is_complete, member, euclid_number]).

excluded_predicates(AllPreds) :-
    geometric_predicates(G),
    number_theory_predicates(NT),
    fraction_predicates(F),
    append(G, NT, Temp),
    append(Temp, F, DomainPreds),
    append([neg, conj, nec, comp_nec, exp_nec, exp_poss, comp_poss, is_recollection], DomainPreds, AllPreds).

% --- Helpers for Number Theory (Grounded) ---

product_of_list(L, P) :- (is_list(L) -> product_of_list_impl(L, P) ; fail).
product_of_list_impl([], 1).
product_of_list_impl([H|T], P) :- number(H), product_of_list_impl(T, P_tail), P is H * P_tail.

find_prime_factor(N, F) :- number(N), N > 1, find_factor_from(N, 2, F).
find_factor_from(N, D, D) :- N mod D =:= 0, !.
find_factor_from(N, D, F) :-
    D * D =< N,
    (D =:= 2 -> D_next is 3 ; D_next is D + 2),
    find_factor_from(N, D_next, F).
find_factor_from(N, _, N).

is_prime(N) :- number(N), N > 1, find_factor_from(N, 2, F), F =:= N.

% =================================================================
% Part 2: Core Logic Engine
% =================================================================

% Helper predicates
select(X, [X|T], T).
select(X, [H|T], [H|R]) :- select(X, T, R).

match_antecedents([], _).
match_antecedents([A|As], Premises) :-
    member(A, Premises),
    match_antecedents(As, Premises).

% --- 2.1 Incoherence Definitions ---

incoherent(X) :- is_incoherent(X), !.
incoherent(X) :- proves(X => []).

% --- 1. Specific Material Optimizations ---

% Geometric Incompatibility
is_incoherent(X) :-
    member(n(ShapePred), X), ShapePred =.. [Shape, V],
    member(n(RestrictionPred), X), RestrictionPred =.. [Restriction, V],
    ground(Shape), ground(Restriction),
    incompatible_pair(Shape, Restriction), !.

% Arithmetic Incompatibility
is_incoherent(X) :-
    member(n(minus(A,B,_)), X),
    current_domain(n),
    is_recollection(A, _), is_recollection(B, _),
    normalize(A, NA), normalize(B, NB),
    NA < NB, !.

% M6-Case1: Euclid Case 1 Incoherence (Optimization)
is_incoherent(X) :-
    member(n(prime(EF)), X),
    member(n(is_complete(L)), X),
    % Check if the concept was introduced by the Muse, or calculate P+1 if needed.
    (member(n(euclid_number(EF, L)), X) ; (product_of_list(L, DE), EF is DE + 1)).


% --- 2. Base Incoherence (LNC) and Persistence ---
incoherent_base(X) :- member(P, X), member(neg(P), X).
incoherent_base(X) :- member(D_P, X), D_P =.. [D, P], member(D_NegP, X), D_NegP =.. [D, neg(P)], member(D, [s,o,n]).

is_incoherent(Y) :- incoherent_base(Y), !.


% --- 2.2 Sequent Calculus Prover (RESTRUCTURED) ---

proves(Sequent) :- proves_impl(Sequent, []).

% --- PRIORITY 1: Identity and Explosion ---
proves_impl((Premises => Conclusions), _) :-
    member(P, Premises), member(P, Conclusions), !.

proves_impl((Premises => _), _) :-
    is_incoherent(Premises), !.

% --- PRIORITY 2: Material Inferences and Grounding (Axioms) ---

% --- Arithmetic Grounding ---
proves_impl(_ => [o(eq(A,B))], _) :-
    is_recollection(A, _), is_recollection(B, _),
    normalize(A, NA), normalize(B, NB),
    NA == NB.

proves_impl(_ => [o(plus(A,B,C))], _) :-
    is_recollection(A, _), is_recollection(B, _),
    arith_op(A, B, +, C),
    is_recollection(C, _).

proves_impl(_ => [o(minus(A,B,C))], _) :-
    current_domain(D), is_recollection(A, _), is_recollection(B, _),
    arith_op(A, B, -, C),
    normalize(C, NC),
    ((D=n, NC >= 0) ; member(D, [z, q])),
    is_recollection(C, _).

% --- Arithmetic Material Inferences ---
proves_impl([n(plus(A,B,C))] => [n(plus(B,A,C))], _).

% --- EML Material Inferences (Axioms) ---
proves_impl([s(u)] => [s(comp_nec a)], _).
proves_impl([s(u_prime)] => [s(comp_nec a)], _).
proves_impl([s(a)] => [s(exp_poss lg)], _).
proves_impl([s(a)] => [s(comp_poss t)], _).
proves_impl([s(t)] => [s(comp_nec neg(u))], _).
proves_impl([s(lg)] => [s(exp_nec u_prime)], _).
proves_impl([s(t_b)] => [s(comp_nec t_n)], _).
proves_impl([s(t_n)] => [s(comp_nec t_b)], _).

% --- Fraction Grounding ---
proves_impl(([] => [o(iterate(U, M, R))]), _) :-
    is_recollection(U, _), integer(M), M >= 0,
    normalize(U, NU),
    (integer(NU) -> N1=NU, D1=1 ; NU = N1 rdiv D1),
    N_res is N1 * M,
    normalize(N_res rdiv D1, R).

proves_impl(([] => [o(partition(W, N, U))]), _) :-
    is_recollection(W, _), integer(N), N > 0,
    normalize(W, NW),
    (integer(NW) -> N1=NW, D1=1 ; NW = N1 rdiv D1),
    D_res is D1 * N,
    normalize(N1 rdiv D_res, U).

% --- Number Theory Material Inferences (Axioms/Definitions) ---

% M5 (Revised): If a prime G divides the Euclid number N derived from L, then G is not in L.
% This now relies on the concept introduced by the Muse.
proves_impl(( [n(prime(G)), n(divides(G, N)), n(euclid_number(N, L))] => [n(neg(member(G, L)))] ), _).

% M4: If there is a prime G not in L, then L is not complete.
proves_impl(([n(prime(G)), n(neg(member(G, L)))] => [n(neg(is_complete(L)))]), _).

% Grounding Primality
proves_impl(([] => [n(prime(N))]), _) :- is_prime(N).
proves_impl(([] => [n(composite(N))]), _) :- number(N), N > 1, \+ is_prime(N).


% --- PRIORITY 3: Structural Rules (Domain Specific and General) ---

% Geometric Entailment
proves_impl((Premises => Conclusions), _) :-
    member(n(P_pred), Premises), P_pred =.. [P_shape, X], is_shape(P_shape),
    member(n(Q_pred), Conclusions), Q_pred =.. [Q_shape, X], is_shape(Q_shape),
    entails_via_incompatibility(P_shape, Q_shape), !.

% Structural Rule for EML Dynamics
proves_impl((Premises => Conclusions), History) :-
    select(s(P), Premises, RestPremises), \+ member(s(P), History),
    eml_axiom(s(P), s(M_Q)),
    ( (M_Q = comp_nec Q ; M_Q = exp_nec Q) -> proves_impl(([s(Q)|RestPremises] => Conclusions), [s(P)|History])
    ; ((M_Q = exp_poss _ ; M_Q = comp_poss _), (member(s(M_Q), Conclusions) ; member(M_Q, Conclusions)))
    ).

% Structural Rule: Prime Factorization (Existential Instantiation)
% This is a general principle of number theory, so we keep it in the core prover.
proves_impl((Premises => Conclusions), History) :-
    select(n(composite(N)), Premises, RestPremises),
    \+ member(factorization(N), History),
    find_prime_factor(N, G),
    NewPremises = [n(prime(G)), n(divides(G, N))|RestPremises],
    proves_impl((NewPremises => Conclusions), [factorization(N)|History]).

% --- General Structural Rule: Forward Chaining (Modus Ponens / MMP) ---
proves_impl((Premises => Conclusions), History) :-
    Module = incompatibility_semantics,
    clause(Module:proves_impl((A_clause => [C_clause]), _), B_clause),

    copy_term((A_clause, C_clause, B_clause), (Antecedents, Consequent, Body)),
    is_list(Antecedents),

    match_antecedents(Antecedents, Premises),
    call(Module:Body),
    \+ member(Consequent, Premises),
    proves_impl(([Consequent|Premises] => Conclusions), History).


% Arithmetic Evaluation
% (Arithmetic Evaluation remains the same as the original file)
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


% --- PRIORITY 4: Reduction Schemata (Logical Connectives) ---
% (Logical connective rules remain the same as the original file)

% Left Negation (LN)
proves_impl((P => C), H) :- select(neg(X), P, P1), proves_impl((P1 => [X|C]), H).
proves_impl((P => C), H) :- select(D_NegX, P, P1), D_NegX=..[D, neg(X)], member(D,[s,o,n]), D_X=..[D, X], proves_impl((P1 => [D_X|C]), H).

% Right Negation (RN)
proves_impl((P => C), H) :- select(neg(X), C, C1), proves_impl(([X|P] => C1), H).
proves_impl((P => C), H) :- select(D_NegX, C, C1), D_NegX=..[D, neg(X)], member(D,[s,o,n]), D_X=..[D, X], proves_impl(([D_X|P] => C1), H).

% Conjunction (Generalized)
proves_impl((P => C), H) :- select(conj(X,Y), P, P1), proves_impl(([X,Y|P1] => C), H).
proves_impl((P => C), H) :- select(s(conj(X,Y)), P, P1), proves_impl(([s(X),s(Y)|P1] => C), H).

proves_impl((P => C), H) :- select(conj(X,Y), C, C1), proves_impl((P => [X|C1]), H), proves_impl((P => [Y|C1]), H).
proves_impl((P => C), H) :- select(s(conj(X,Y)), C, C1), proves_impl((P => [s(X)|C1]), H), proves_impl((P => [s(Y)|C1]), H).

% S5 Modal rules (Generalized)
proves_impl((P => C), H) :- select(nec(X), P, P1), !, ( proves_impl((P1 => C), H) ; \+ proves_impl(([] => [X]), []) ).
proves_impl((P => C), H) :- select(nec(X), C, C1), !, ( proves_impl((P => C1), H) ; proves_impl(([] => [X]), []) ).

% --- PRIORITY 5: Neuro-Symbolic Integration Point (The "Muse" Hook) ---
% If all standard logical reductions (Priority 1-4) fail, consult the learned strategies.

proves_impl((Premises => Conclusions), History) :-
    % Check if the bridge module is loaded and the predicate exists
    current_predicate(neuro_symbolic_bridge:suggest_strategy/3),
    % Call the bridge to suggest a strategy (The "neural" intuition)
    neuro_symbolic_bridge:suggest_strategy(Premises, Conclusions, Strategy),
    % Apply the suggested strategy (The "symbolic" execution)
    apply_strategy(Strategy, Premises, Conclusions, History).

% --- Strategy Application Helper ---

% Strategy: Introduce Lemma/Construction
apply_strategy(introduce(NewPremise), Premises, Conclusions, History) :-
    \+ member(NewPremise, Premises),
    proves_impl(([NewPremise|Premises] => Conclusions), History).

% Strategy: Case Split
apply_strategy(case_split(Case1, Case2), Premises, Conclusions, History) :-
    proves_impl(([Case1|Premises] => Conclusions), History),
    proves_impl(([Case2|Premises] => Conclusions), History).


% (Helpers for EML Dynamics)
eml_axiom(A, C) :-
    clause(incompatibility_semantics:proves_impl(([A] => [C]), _), true),
    is_eml_modality(C).

is_eml_modality(s(comp_nec _)).
is_eml_modality(s(exp_nec _)).
is_eml_modality(s(exp_poss _)).
is_eml_modality(s(comp_poss _)).

% =================================================================
% Part 4: Automata and Placeholders
% =================================================================
% (Placeholders remain the same as the original file)

highlander([Result], Result) :- !.
highlander([], _) :- !, fail.
highlander([_|Rest], Result) :- highlander(Rest, Result).

bounded_region(I, L, U, R) :- ( number(I), I >= L, I =< U -> R = in_bounds(I) ; R = out_of_bounds(I) ).

equality_iterator(T, T, T) :- !.
equality_iterator(C, T, R) :- C < T, C1 is C + 1, equality_iterator(C1, T, R).

% Placeholder definitions for exported functors
s(_). o(_). n(_). neg(_). comp_nec(_). exp_nec(_). exp_poss(_). comp_poss(_).
square(_). rectangle(_). rhombus(_). parallelogram(_). trapezoid(_). kite(_). quadrilateral(_).
r1(_). r2(_). r3(_). r4(_). r5(_). r6(_).
prime(_). composite(_). divides(_, _). is_complete(_).
rdiv(_, _). iterate(_, _, _). partition(_, _, _).
% Placeholder for the concept introduced by the bridge
euclid_number(_, _).