% ===================================================================
% Geometry Axioms — Quadrilateral taxonomy via incompatibility
% ===================================================================
%
% These axioms establish geometric knowledge through material
% incompatibility: a shape's identity is determined by what it
% EXCLUDES, not what it contains. A square is not defined by
% "four equal sides and right angles" but by its incompatibility
% with shapes that lack those properties.
%
% Interpretive correspondence: this material may be read alongside
% Carspecken's Scene One (Form and the Flux), where recognizable
% form emerges from a chaotic background through the act of
% perception. Geometric entailment via incompatibility is one
% formal reconstruction of how determinate shape-concepts arise.
% ===================================================================

% --- Incompatibility Pairs ---
% Each pair asserts that a shape is materially incompatible with
% a restriction. r1-r6 represent geometric properties whose
% absence distinguishes shapes from one another.

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

% --- Geometric Incoherence ---
is_incoherent(X) :-
    member(n(ShapePred), X), ShapePred =.. [Shape, V],
    member(n(RestrictionPred), X), RestrictionPred =.. [Restriction, V],
    ground(Shape), ground(Restriction),
    incompatible_pair(Shape, Restriction), !.

% --- Geometric Entailment (Structural Rule) ---
proves_impl((Premises => Conclusions), _) :-
    member(n(P_pred), Premises), P_pred =.. [P_shape, X], is_shape(P_shape),
    member(n(Q_pred), Conclusions), Q_pred =.. [Q_shape, X], is_shape(Q_shape),
    entails_via_incompatibility(P_shape, Q_shape), !.
