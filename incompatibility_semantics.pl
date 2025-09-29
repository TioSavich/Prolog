/** <module> Core logic for incompatibility semantics and automated theorem proving.
 *
 *  This module implements Robert Brandom's incompatibility semantics, providing a
 *  sequent calculus-based theorem prover. It integrates multiple knowledge
 *  domains, including geometry, number theory (Euclid's proof of the
 *  infinitude of primes), and arithmetic over natural numbers, integers, and
 *  rational numbers. The prover uses a combination of structural rules,
 *  material inferences (axioms), and reduction schemata to derive conclusions
 *  from premises.
 *
 *  Key features:
 *  - A sequent prover `proves/1` that operates on sequents of the form `Premises => Conclusions`.
 *  - A predicate `incoherent/1` to check if a set of propositions is contradictory.
 *  - Support for multiple arithmetic domains (n, z, q) via `set_domain/1`.
 *  - A rich set of logical operators and domain-specific predicates.
 *
 * 
 * 
 */
:- module(incompatibility_semantics,
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
          % Normative Crisis Detection
          , prohibition/2, normative_crisis/2, check_norms/1, current_domain_context/1
          ]).
% Declare predicates that are defined across different sections.
:- use_module(hermeneutic_calculator).
:- use_module(grounded_arithmetic, [incur_cost/1]).

:- discontiguous proves_impl/2.
:- discontiguous is_incoherent/1. % Non-recursive check
:- discontiguous check_norms/1.

% =================================================================
% Part 0: Setup and Configuration
% =================================================================

% Define operators for modalities, negation, and sequents.
:- op(500, fx, comp_nec). % Compressive Necessity (Box_down)
:- op(500, fx, exp_nec).  % Expansive Necessity (Box_up)
:- op(500, fx, exp_poss). % Expansive Possibility (Diamond_up)
:- op(500, fx, comp_poss).% Compressive Possibility (Diamond_down)
:- op(500, fx, neg).
:- op(1050, xfy, =>).
:- op(550, xfy, rdiv). % Operator for rational numbers

% =================================================================
% Part 1: Knowledge Domains
% =================================================================

% --- 1.1 Geometry (Chapter 2) ---
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

% --- 1.4 Fraction Domain (Jason.pl) ---
fraction_predicates([rdiv, iterate, partition]).

% --- 1.2 Arithmetic (O/N Domains) ---

:- dynamic current_domain/1.
:- dynamic prohibition/2.
:- dynamic normative_crisis/2.

%!      current_domain(?Domain:atom) is nondet.
%
%       Dynamic fact that holds the current arithmetic domain.
%       Possible values are `n` (natural numbers), `z` (integers),
%       or `q` (rational numbers).
%
%       @param Domain The current arithmetic domain.
current_domain(n).

%!      set_domain(+Domain:atom) is det.
%
%       Sets the current arithmetic domain.
%       Retracts the current domain and asserts the new one.
%       Valid domains are `n`, `z`, and `q`.
%
%       @param Domain The new arithmetic domain to set.
set_domain(D) :-
    % Added 'q' (Rationals) as a valid domain.
    ( member(D, [n, z, q]) -> retractall(current_domain(_)), assertz(current_domain(D)) ; true).

% --- Normative Crisis Detection ---

%!      prohibition(+Context:atom, +Goal:term) is semidet.
%
%       Defines prohibited operations within specific mathematical contexts.
%       This implements the UMEDCA thesis that mathematical norms are 
%       revisable and context-dependent, not universal axioms.
%
%       @param Context The mathematical context (natural_numbers, integers, rationals)
%       @param Goal The goal pattern that is prohibited in this context

% Natural numbers context: Cannot subtract larger from smaller
prohibition(natural_numbers, subtract(M, S, _)) :-
    % Use grounded comparison to avoid arithmetic backstop
    current_domain(n),
    is_recollection(M, _),
    is_recollection(S, _),
    grounded_arithmetic:smaller_than(M, S).

% Natural numbers context: Cannot divide when result would not be natural
prohibition(natural_numbers, divide(Dividend, Divisor, _)) :-
    current_domain(n),
    is_recollection(Dividend, _),
    is_recollection(Divisor, _),
    \+ grounded_arithmetic:zero(Divisor),
    % Division would not yield a natural number (simplified check)
    grounded_arithmetic:smaller_than(Dividend, Divisor).

%!      check_norms(+Goal:term) is det.
%
%       Validates a goal against the current mathematical context norms.
%       Throws normative_crisis/2 if the goal violates current prohibitions.
%
%       @param Goal The goal to validate
%       @error normative_crisis(Goal, Context) if goal violates norms
check_norms(Goal) :-
    % Only check norms for core arithmetic operations
    ( is_core_operation(Goal) ->
        current_domain_context(Context),
        ( prohibition(Context, Goal) ->
            throw(normative_crisis(Goal, Context))
        ;
            incur_cost(norm_check)  % Cost of normative validation
        )
    ;
        true  % Non-arithmetic goals pass through
    ).

%!      is_core_operation(+Goal:term) is semidet.
%
%       Identifies core arithmetic operations that require norm checking.
is_core_operation(add(_, _, _)).
is_core_operation(subtract(_, _, _)).
is_core_operation(multiply(_, _, _)).
is_core_operation(divide(_, _, _)).

%!      current_domain_context(-Context:atom) is det.
%
%       Maps the current domain to a context name for prohibition checking.
current_domain_context(Context) :-
    current_domain(Domain),
    domain_to_context(Domain, Context).

domain_to_context(n, natural_numbers).
domain_to_context(z, integers).
domain_to_context(q, rationals).

%!      check_norms(+Goal:term) is det.
%
%       Validates a goal against current mathematical context norms.
%       Throws normative_crisis/2 if the goal violates current norms.
%
%       @param Goal The goal to validate against current norms
check_norms(Goal) :-
    ( is_core_arithmetic_operation(Goal) ->
        current_domain(Domain),
        context_name(Domain, Context),
        ( prohibition(Context, Goal) ->
            throw(normative_crisis(Goal, Context))
        ;
            true
        )
    ;
        true
    ).

%!      is_core_arithmetic_operation(+Goal:term) is semidet.
%
%       Identifies goals that need normative checking.
is_core_arithmetic_operation(subtract(_, _, _)).
is_core_arithmetic_operation(divide(_, _, _)).
is_core_arithmetic_operation(add(_, _, _)).
is_core_arithmetic_operation(multiply(_, _, _)).

%!      context_name(+Domain:atom, -Context:atom) is det.
%
%       Maps domain symbols to context names.
context_name(n, natural_numbers).
context_name(z, integers).  
context_name(q, rationals).


% Deprecated: obj_coll/1. Replaced by is_recollection/2.
% The old obj_coll/1 predicate checked for static, timeless properties.
% The new ontology requires that a number's validity is proven by
% demonstrating a constructive history (an anaphoric recollection).
%
% obj_coll(N) :- current_domain(n), !, integer(N), N >= 0.
% obj_coll(N) :- current_domain(z), !, integer(N).
% obj_coll(X) :- current_domain(q), !,
%     ( integer(X)
%     ; (X = N rdiv D, integer(N), integer(D), D > 0)
%     ).

%!      is_recollection(?Term, ?History) is semidet.
%
%       The new core ontological predicate. It succeeds if `Term` is a
%       validly constructed number, where `History` is the execution
%       trace of the calculation that constructed it. This replaces the
%       static `obj_coll/1` check with a dynamic, process-based validation.
%
%       @param Term The numerical term to be validated (e.g., 5).
%       @param History The constructive trace that proves the term's existence.

% Base case: 0 is axiomatically a number.
is_recollection(0, [axiom(zero)]).

% Support for explicit recollection structures from grounded_arithmetic
is_recollection(recollection(History), [explicit_recollection(History)]) :-
    is_list(History),
    maplist(=(tally), History).

% Recursive case for positive integers: N is a recollection if N-1 is, and we
% can construct N by adding 1 using the hermeneutic calculator.
is_recollection(N, History) :-
    integer(N),
    N > 0,
    Prev is N - 1,
    is_recollection(Prev, _), % Foundational check on the predecessor
    hermeneutic_calculator:calculate(Prev, +, 1, _Strategy, N, History).

% Case for negative integers: A negative number is constructed by subtracting
% its absolute value from 0.
is_recollection(N, History) :-
    integer(N),
    N < 0,
    is_recollection(0, _), % Grounded in the axiom of zero
    Val is abs(N),
    hermeneutic_calculator:calculate(0, -, Val, _Strategy, N, History).

% Case for rational numbers: A rational N/D is a recollection if its
% numerator and denominator are themselves valid recollections.
% The history records this compositional validation.
is_recollection(N rdiv D, [history(rational, from(N, D))]) :-
    % Denominator must be a positive integer. We check its recollection status.
    is_recollection(D, _),
    integer(D), D > 0,
    % Numerator can be any recollected number.
    is_recollection(N, _).


% --- Helpers for Rational Arithmetic ---
gcd(A, 0, A) :- A \= 0, !.
gcd(A, B, G) :- B \= 0, R is A mod B, gcd(B, R, G).

%!      normalize(+Input, -Normalized) is det.
%
%       Normalizes a number. Integers are unchanged. Rational numbers
%       (e.g., `6 rdiv 8`) are reduced to their simplest form (e.g., `3 rdiv 4`).
%       If the denominator is 1, it is converted to an integer.
%
%       @param Input The integer or rational number to normalize.
%       @param Normalized The resulting normalized number.
normalize(N, N) :- integer(N), !.
normalize(N rdiv D, R) :-
    (D =:= 1 -> R = N ;
        G is abs(gcd(N, D)),
        SN is N // G, % Integer division
        SD is D // G,
        (SD =:= 1 -> R = SN ; R = SN rdiv SD)
    ), !.

% Helper for dynamic arithmetic (FIX: Resolve syntax error)
perform_arith(+, A, B, C) :- C is A + B.
perform_arith(-, A, B, C) :- C is A - B.

% Helper for rational addition/subtraction (FIX: Resolve syntax error)
arith_op(A, B, Op, C) :-
    % Ensure Op is a valid arithmetic operator we handle here
    member(Op, [+, -]),
    normalize(A, NA), normalize(B, NB),
    (integer(NA), integer(NB) ->
        % Case 1: Integer Arithmetic
        % Use helper predicate to perform the operation
        perform_arith(Op, NA, NB, C_raw)
    ;
        % Case 2: Rational Arithmetic
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

number_theory_predicates([prime, composite, divides, is_complete, analyze_euclid_number, member]).

% Combined list of excluded predicates for Arithmetic Evaluation
excluded_predicates(AllPreds) :-
    geometric_predicates(G),
    number_theory_predicates(NT),
    fraction_predicates(F),
    append(G, NT, Temp),
    append(Temp, F, DomainPreds),
    append([neg, conj, nec, comp_nec, exp_nec, exp_poss, comp_poss, is_recollection], DomainPreds, AllPreds).

% --- Helpers for Number Theory (Grounded) ---

% Helper: Product of a list
product_of_list(L, P) :- (is_list(L) -> product_of_list_impl(L, P) ; fail).
product_of_list_impl([], 1).
product_of_list_impl([H|T], P) :- number(H), product_of_list_impl(T, P_tail), P is H * P_tail.

% Helper: Find a prime factor
find_prime_factor(N, F) :- number(N), N > 1, find_factor_from(N, 2, F).
find_factor_from(N, D, D) :- N mod D =:= 0, !.
find_factor_from(N, D, F) :-
    D * D =< N,
    (D =:= 2 -> D_next is 3 ; D_next is D + 2),
    find_factor_from(N, D_next, F).
find_factor_from(N, _, N). % N is prime

% Helper: Grounded check for primality
is_prime(N) :- number(N), N > 1, find_factor_from(N, 2, F), F =:= N.

% =================================================================
% Part 2: Core Logic Engine
% =================================================================

% Helper predicates
select(X, [X|T], T).
select(X, [H|T], [H|R]) :- select(X, T, R).

% Helper to match antecedents against premises (Allows unification)
match_antecedents([], _).
match_antecedents([A|As], Premises) :-
    member(A, Premises),
    match_antecedents(As, Premises).

% --- 2.1 Incoherence Definitions (SAFE AND COMPLETE) ---

%!      incoherent(+PropositionSet:list) is semidet.
%
%       Checks if a set of propositions is incoherent (contradictory).
%       A set is incoherent if:
%       1. It contains a direct contradiction (e.g., `P` and `neg(P)`).
%       2. It violates a material incompatibility (e.g., `n(square(a))` and `n(r1(a))`).
%       3. An empty conclusion `[]` can be proven from it, i.e., `proves(PropositionSet => [])`.
%
%       @param PropositionSet A list of propositions.
incoherent(X) :- is_incoherent(X), !.
incoherent(X) :- proves(X => []).

% is_incoherent/1: Non-recursive Incoherence Check

% --- 1. Specific Material Optimizations ---

% Geometric Incompatibility
is_incoherent(X) :-
    member(n(ShapePred), X), ShapePred =.. [Shape, V],
    member(n(RestrictionPred), X), RestrictionPred =.. [Restriction, V],
    ground(Shape), ground(Restriction),
    incompatible_pair(Shape, Restriction), !.

% Arithmetic Incompatibility (Generalized to handle fractions)
% This is incoherent if a norm demands an impossible recollection.
is_incoherent(X) :-
    member(n(minus(A,B,_)), X), % Check for the normative proposition
    current_domain(n),
    is_recollection(A, _), is_recollection(B, _), % Operands must be valid numbers
    normalize(A, NA), normalize(B, NB),
    NA < NB, !.

% M6-Case1: Euclid Case 1 Incoherence
is_incoherent(X) :-
    member(n(prime(EF)), X),
    member(n(is_complete(L)), X),
    product_of_list(L, DE),
    EF is DE + 1.

% --- 2. Base Incoherence (LNC) and Persistence ---

% Law of Non-Contradiction (LNC)
incoherent_base(X) :- member(P, X), member(neg(P), X).
incoherent_base(X) :- member(D_P, X), D_P =.. [D, P], member(D_NegP, X), D_NegP =.. [D, neg(P)], member(D, [s,o,n]).

% Persistence
is_incoherent(Y) :- incoherent_base(Y), !.


% --- 2.2 Sequent Calculus Prover (REORDERED) ---
% Order: Identity/Explosion -> Axioms -> Structural Rules -> Reduction Schemata.

%!      proves(+Sequent) is semidet.
%
%       Attempts to prove a given sequent using the rules of the calculus.
%       A sequent has the form `Premises => Conclusions`, where `Premises`
%       and `Conclusions` are lists of propositions. The predicate succeeds
%       if the conclusions can be derived from the premises.
%
%       The prover uses a recursive, history-tracked implementation (`proves_impl/2`)
%       to apply inference rules and avoid infinite loops.
%
%       @param Sequent The sequent to be proven.
proves(Sequent) :- proves_impl(Sequent, []).

% --- PRIORITY 1: Identity and Explosion ---

% Axiom of Identity (A |- A)
proves_impl((Premises => Conclusions), _) :-
    member(P, Premises), member(P, Conclusions), !.

% From base incoherence (Explosion)
proves_impl((Premises => _), _) :-
    is_incoherent(Premises), !.

% --- PRIORITY 2: Material Inferences and Grounding (Axioms) ---

% --- Arithmetic Grounding (Extended for Q) ---
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
    % Subtraction constraints only apply to N. We must normalize C before comparison.
    normalize(C, NC),
    ((D=n, NC >= 0) ; member(D, [z, q])),
    is_recollection(C, _).

% --- Arithmetic Material Inferences ---
proves_impl([n(plus(A,B,C))] => [n(plus(B,A,C))], _).

% --- EML Material Inferences (Axioms) - UPDATED ---
% Commitment 2: Emergence of Awareness (Temporal Compression)
proves_impl([s(u)] => [s(comp_nec a)], _).
proves_impl([s(u_prime)] => [s(comp_nec a)], _).

% Commitment 3 (Revised): The Tension of Awareness (Choice Point)
proves_impl([s(a)] => [s(exp_poss lg)], _). % Possibility of Release
proves_impl([s(a)] => [s(comp_poss t)], _).  % Possibility of Fixation (Temptation)

% Commitment 4: Dynamics of the Choice
% 4a: Fixation (Deepened Contraction)
proves_impl([s(t)] => [s(comp_nec neg(u))], _).
% 4b: Release (Sublation)
proves_impl([s(lg)] => [s(exp_nec u_prime)], _).

% Hegel's Triad Oscillation:
proves_impl([s(t_b)] => [s(comp_nec t_n)], _).
proves_impl([s(t_n)] => [s(comp_nec t_b)], _).

% --- 3.5 Fraction Grounding (Jason.pl integration) ---

% Grounding: Iterating (Multiplication)
proves_impl(([] => [o(iterate(U, M, R))]), _) :-
    is_recollection(U, _), integer(M), M >= 0,
    % R = U * M
    normalize(U, NU),
    (integer(NU) -> N1=NU, D1=1 ; NU = N1 rdiv D1),
    N_res is N1 * M,
    % D_res = D1,
    normalize(N_res rdiv D1, R).

% Grounding: Partitioning (Division)
proves_impl(([] => [o(partition(W, N, U))]), _) :-
    is_recollection(W, _), integer(N), N > 0,
    % U = W / N
    normalize(W, NW),
    (integer(NW) -> N1=NW, D1=1 ; NW = N1 rdiv D1),
    % N_res = N1,
    D_res is D1 * N,
    normalize(N1 rdiv D_res, U).

% --- Number Theory Material Inferences ---

% M5-Revised: Euclid's Core Argument (For Forward Chaining)
proves_impl(( [n(prime(G)), n(divides(G, N)), n(is_complete(L))] => [n(neg(member(G, L)))] ), _) :-
    product_of_list(L, P),
    N is P + 1.

% M5-Direct: (For Direct proof, where L is bound by the conclusion)
proves_impl(( [n(prime(G)), n(divides(G, N))] => [n(neg(member(G, L)))] ), _) :-
    product_of_list(L, P),
    N is P + 1.

% M4-Revised: Definition of Completeness Violation (For Forward Chaining)
proves_impl(([n(prime(G)), n(neg(member(G, L))), n(is_complete(L))] => [n(neg(is_complete(L)))]), _).

% M4-Direct: (For Direct proof)
proves_impl(([n(prime(G)), n(neg(member(G, L)))] => [n(neg(is_complete(L)))]), _).

% Grounding Primality
proves_impl(([] => [n(prime(N))]), _) :- is_prime(N).
proves_impl(([] => [n(composite(N))]), _) :- number(N), N > 1, \+ is_prime(N).


% --- PRIORITY 3: Structural Rules (Domain Specific and General) ---
% (Structural rules remain the same)

% Geometric Entailment (Inferential Strength)
proves_impl((Premises => Conclusions), _) :-
    member(n(P_pred), Premises), P_pred =.. [P_shape, X], is_shape(P_shape),
    member(n(Q_pred), Conclusions), Q_pred =.. [Q_shape, X], is_shape(Q_shape),
    entails_via_incompatibility(P_shape, Q_shape), !.

% Structural Rule for EML Dynamics - UPDATED
proves_impl((Premises => Conclusions), History) :-
    select(s(P), Premises, RestPremises), \+ member(s(P), History),
    eml_axiom(s(P), s(M_Q)),
    % Case 1: Necessities drive state transition
    ( (M_Q = comp_nec Q ; M_Q = exp_nec Q) -> proves_impl(([s(Q)|RestPremises] => Conclusions), [s(P)|History])
    % Case 2: Possibilities are checked against conclusions (for direct proofs) - Updated
    ; ((M_Q = exp_poss _ ; M_Q = comp_poss _), (member(s(M_Q), Conclusions) ; member(M_Q, Conclusions)))
    ).

% --- Structural Rules for Euclid's Proof ---

% Structural Rule: Euclid's Construction
proves_impl((Premises => Conclusions), History) :-
    member(n(is_complete(L)), Premises),
    \+ member(euclid_construction(L), History),
    product_of_list(L, DE),
    EF is DE + 1,
    NewPremise = n(analyze_euclid_number(EF, L)),
    proves_impl(([NewPremise|Premises] => Conclusions), [euclid_construction(L)|History]).

% Case Analysis Rule (Handles analyze_euclid_number)
proves_impl((Premises => Conclusions), History) :-
    select(n(analyze_euclid_number(EF, L)), Premises, RestPremises),
    EF > 1,
    (member(n(is_complete(L)), Premises) ->
        % Case 1: Assume EF is prime
        proves_impl(([n(prime(EF))|RestPremises] => Conclusions), History),
        % Case 2: Assume EF is composite
        proves_impl(([n(composite(EF))|RestPremises] => Conclusions), History)
    ; fail
    ).

% Structural Rule: Prime Factorization (Existential Instantiation) (Case 2)
proves_impl((Premises => Conclusions), History) :-
    select(n(composite(N)), Premises, RestPremises),
    \+ member(factorization(N), History),
    find_prime_factor(N, G),
    NewPremises = [n(prime(G)), n(divides(G, N))|RestPremises],
    proves_impl((NewPremises => Conclusions), [factorization(N)|History]).

% --- General Structural Rule: Forward Chaining (Modus Ponens / MMP) ---
proves_impl((Premises => Conclusions), History) :-
    Module = incompatibility_semantics,
    % 1. Find an applicable material inference rule (axiom) defined in Priority 2.
    clause(Module:proves_impl((A_clause => [C_clause]), _), B_clause),

    copy_term((A_clause, C_clause, B_clause), (Antecedents, Consequent, Body)),
    is_list(Antecedents), % Handle grounding axioms like [] => P

    % 2. Check if the antecedents are satisfied by the current premises.
    match_antecedents(Antecedents, Premises),
    % 3. Execute the body of the axiom.
    call(Module:Body),
    % 4. Ensure the consequent hasn't already been derived.
    \+ member(Consequent, Premises),
    % 5. Add the consequent to the premises and continue.
    proves_impl(([Consequent|Premises] => Conclusions), History).


% Arithmetic Evaluation (Legacy support for simple integer evaluation in sequents)
proves_impl(([Premise|RestPremises] => Conclusions), History) :-
    (Premise =.. [Index, Expr], member(Index, [s, o, n]) ; (Index = none, Expr = Premise)),
    (compound(Expr) -> (
        functor(Expr, F, _),
        excluded_predicates(Excluded),
        \+ member(F, Excluded)
    ) ; true),
    % Ensure the expression is not a rational structure before using 'is'
    \+ (compound(Expr), functor(Expr, rdiv, 2)),
    catch(Value is Expr, _, fail), !,
    (Index \= none -> NewPremise =.. [Index, Value] ; NewPremise = Value),
    proves_impl(([NewPremise|RestPremises] => Conclusions), History).


% --- PRIORITY 4: Reduction Schemata (Logical Connectives) ---

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

%!      highlander(+List:list, -Result) is semidet.
%
%       Succeeds if the `List` contains exactly one element, which is unified with `Result`.
%       "There can be only one."
%
%       @param List The input list.
%       @param Result The single element of the list.
highlander([Result], Result) :- !.
highlander([], _) :- !, fail.
highlander([_|Rest], Result) :- highlander(Rest, Result).

%!      bounded_region(+I:number, +L:number, +U:number, -R:term) is det.
%
%       Checks if a number `I` is within a given lower `L` and upper `U` bound.
%
%       @param I The number to check.
%       @param L The lower bound.
%       @param U The upper bound.
%       @param R `in_bounds(I)` if `L =< I =< U`, otherwise `out_of_bounds(I)`.
bounded_region(I, L, U, R) :- ( number(I), I >= L, I =< U -> R = in_bounds(I) ; R = out_of_bounds(I) ).

%!      equality_iterator(?C:integer, +T:integer, -R:integer) is nondet.
%
%       Iterates from a counter `C` up to a target `T`.
%       Unifies `R` with `T` when `C` reaches `T`.
%
%       @param C The current value of the counter.
%       @param T The target value.
%       @param R The result, unified with T on success.
equality_iterator(T, T, T) :- !.
equality_iterator(C, T, R) :- C < T, C1 is C + 1, equality_iterator(C1, T, R).

% Placeholder definitions for exported functors
%! s(P) is det.
% Wrapper for subjective propositions.
s(_).
%! o(P) is det.
% Wrapper for objective propositions.
o(_).
%! n(P) is det.
% Wrapper for normative propositions.
n(_).
%! neg(P) is det.
% Wrapper for negation.
neg(_).
%! comp_nec(P) is det.
% Compressive necessity modality.
comp_nec(_).
%! exp_nec(P) is det.
% Expansive necessity modality.
exp_nec(_).
%! exp_poss(P) is det.
% Expansive possibility modality.
exp_poss(_).
%! comp_poss(P) is det.
% Compressive possibility modality.
comp_poss(_).
%! square(X) is det.
% Geometric shape placeholder.
square(_).
%! rectangle(X) is det.
% Geometric shape placeholder.
rectangle(_).
%! rhombus(X) is det.
% Geometric shape placeholder.
rhombus(_).
%! parallelogram(X) is det.
% Geometric shape placeholder.
parallelogram(_).
%! trapezoid(X) is det.
% Geometric shape placeholder.
trapezoid(_).
%! kite(X) is det.
% Geometric shape placeholder.
kite(_).
%! quadrilateral(X) is det.
% Geometric shape placeholder.
quadrilateral(_).
%! r1(X) is det.
% Geometric restriction placeholder.
r1(_).
%! r2(X) is det.
% Geometric restriction placeholder.
r2(_).
%! r3(X) is det.
% Geometric restriction placeholder.
r3(_).
%! r4(X) is det.
% Geometric restriction placeholder.
r4(_).
%! r5(X) is det.
% Geometric restriction placeholder.
r5(_).
%! r6(X) is det.
% Geometric restriction placeholder.
r6(_).
%! prime(N) is det.
% Number theory placeholder for prime numbers.
prime(_).
%! composite(N) is det.
% Number theory placeholder for composite numbers.
composite(_).
%! divides(A, B) is det.
% Number theory placeholder for divisibility.
divides(_, _).
%! is_complete(L) is det.
% Number theory placeholder for a complete list of primes.
is_complete(_).
%! analyze_euclid_number(N, L) is det.
% Placeholder for Euclid's proof step.
analyze_euclid_number(_, _).
%! rdiv(N, D) is det.
% Placeholder for rational number representation (Numerator rdiv Denominator).
rdiv(_, _).
%! iterate(U, M, R) is det.
% Placeholder for iteration/multiplication of fractions.
iterate(_, _, _).
%! partition(W, N, U) is det.
% Placeholder for partitioning/division of fractions.
partition(_, _, _).