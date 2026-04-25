% ===================================================================
% Number Theory Axioms — Euclid's proof of prime infinitude
% ===================================================================
%
% Euclid's proof enters through an incoherence frame:
%   {The list of primes {A, B, C} is complete} in Inc
%
% The claim to completeness defeats itself through construction.
% This is not a contradiction in the logical sense but an
% incoherence — the assumption generates its own refutation.
%
% Interpretive correspondence: the proof is monological (any
% subject can verify it), but the RECOGNITION that completeness
% is self-defeating — that moment of "the assumption was wrong" —
% is where formalization goes hollow. The structural rules below
% execute the proof mechanically; the insight they formalize is
% not itself mechanical.
% ===================================================================

number_theory_predicates([prime, composite, divides, is_complete, analyze_euclid_number, member]).

% --- Helpers ---

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

% --- Euclid Case 1 Incoherence ---
is_incoherent(X) :-
    axiom_pack_enabled(number_theory),
    member(n(prime(EF)), X),
    member(n(is_complete(L)), X),
    product_of_list(L, DE),
    EF is DE + 1.

% --- Material Inferences ---

% M5: Euclid's Core Argument (Forward Chaining)
proves_impl(( [n(prime(G)), n(divides(G, N)), n(is_complete(L))] => [n(neg(member(G, L)))] ), _) :-
    axiom_pack_enabled(number_theory),
    product_of_list(L, P),
    N is P + 1.

% M4: Completeness Violation (Forward Chaining)
proves_impl(([n(prime(G)), n(neg(member(G, L))), n(is_complete(L))] => [n(neg(is_complete(L)))]), _) :-
    axiom_pack_enabled(number_theory).

% M4-Direct
proves_impl(([n(prime(G)), n(neg(member(G, L)))] => [n(neg(is_complete(L)))]), _) :-
    axiom_pack_enabled(number_theory).

% Primality grounding
proves_impl(([] => [n(prime(N))]), _) :-
    axiom_pack_enabled(number_theory),
    is_prime(N).
proves_impl(([] => [n(composite(N))]), _) :-
    axiom_pack_enabled(number_theory),
    number(N), N > 1, \+ is_prime(N).

% --- Structural Rules for Euclid's Proof ---

% Euclid's Construction
proves_impl((Premises => Conclusions), History) :-
    axiom_pack_enabled(number_theory),
    member(n(is_complete(L)), Premises),
    \+ member(euclid_construction(L), History),
    product_of_list(L, DE),
    EF is DE + 1,
    NewPremise = n(analyze_euclid_number(EF, L)),
    proves_impl(([NewPremise|Premises] => Conclusions), [euclid_construction(L)|History]).

% Case Analysis (analyze_euclid_number)
proves_impl((Premises => Conclusions), History) :-
    axiom_pack_enabled(number_theory),
    select(n(analyze_euclid_number(EF, L)), Premises, RestPremises),
    EF > 1,
    (member(n(is_complete(L)), Premises) ->
        proves_impl(([n(prime(EF))|RestPremises] => Conclusions), History),
        proves_impl(([n(composite(EF))|RestPremises] => Conclusions), History)
    ; fail
    ).

% Prime Factorization (Existential Instantiation)
proves_impl((Premises => Conclusions), History) :-
    axiom_pack_enabled(number_theory),
    select(n(composite(N)), Premises, RestPremises),
    \+ member(factorization(N), History),
    find_prime_factor(N, G),
    NewPremises = [n(prime(G)), n(divides(G, N))|RestPremises],
    proves_impl((NewPremises => Conclusions), [factorization(N)|History]).
