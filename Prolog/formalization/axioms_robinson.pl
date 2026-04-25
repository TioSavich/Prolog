% ===================================================================
% Robinson Arithmetic (Q) Axioms and Arithmetic Grounding
% ===================================================================
%
% These axioms establish the connection between the Hermeneutic
% Calculator's grounded arithmetic and Robinson's axiom system Q.
% The claim is not that this system proves incompleteness — it is
% that children's arithmetic strategies, taken seriously, generate
% machinery rich enough to raise the question.
%
% is_recollection/2 is the ontological core: a number exists only
% if there is a constructive history (an anaphoric recollection)
% demonstrating how it was built from counting. "Numerals are
% pronouns" — they refer back to the act of counting that
% produced them.
%
% Interpretive correspondence: this material participates in what
% might be read as a Scene One horizon (objective, monological,
% verifiable by any subject). Robinson's axioms are the kind of
% claim that admits of multiple access — anyone can check Q1-Q7.
% ===================================================================

% --- Ontological Core: is_recollection/2 ---

robinson_pack_enabled :-
    axiom_pack_enabled(robinson).

history_for_nonnegative_integer(0, [axiom(zero)]).
history_for_nonnegative_integer(N, [succ(Prev)|PrevHistory]) :-
    integer(N),
    N > 0,
    Prev is N - 1,
    history_for_nonnegative_integer(Prev, PrevHistory).

% Base case: 0 is axiomatically a number.
is_recollection(0, [axiom(zero)]) :-
    robinson_pack_enabled.

% Support for explicit recollection structures from grounded_arithmetic
is_recollection(recollection(History), [explicit_recollection(History)]) :-
    robinson_pack_enabled,
    is_list(History),
    maplist(=(tally), History).

% Recursive case for positive integers
is_recollection(N, History) :-
    robinson_pack_enabled,
    integer(N),
    N > 0,
    history_for_nonnegative_integer(N, History).

% Case for negative integers
is_recollection(N, [integer_extension(negative, Abs)|AbsHistory]) :-
    robinson_pack_enabled,
    integer(N),
    N < 0,
    Abs is abs(N),
    history_for_nonnegative_integer(Abs, AbsHistory).

% Case for rational numbers
is_recollection(N rdiv D, [history(rational, from(N, D))]) :-
    robinson_pack_enabled,
    integer(D), D > 0,
    integer(N),
    is_recollection(D, _),
    is_recollection(N, _).

% --- Rational Arithmetic Helpers ---
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
perform_arith(*, A, B, C) :- C is A * B.

rational_parts(N, N, 1) :-
    integer(N),
    !.
rational_parts(N rdiv D, N, D).

rational_arith(*, N1, D1, N2, D2, N_res rdiv D_res) :-
    N_res is N1 * N2,
    D_res is D1 * D2.
rational_arith(Op, N1, D1, N2, D2, N_res rdiv D_res) :-
    member(Op, [+, -]),
    D_res is D1 * D2,
    N1_scaled is N1 * D2,
    N2_scaled is N2 * D1,
    perform_arith(Op, N1_scaled, N2_scaled, N_res).

arith_op(A, B, Op, C) :-
    member(Op, [+, -, *]),
    normalize(A, NA), normalize(B, NB),
    (integer(NA), integer(NB) ->
        perform_arith(Op, NA, NB, C_raw)
    ;
        rational_parts(NA, N1, D1),
        rational_parts(NB, N2, D2),
        rational_arith(Op, N1, D1, N2, D2, C_raw)
    ),
    normalize(C_raw, C).

% --- Arithmetic Grounding Rules ---

proves_impl(_ => [o(eq(A,B))], _) :-
    robinson_pack_enabled,
    once(is_recollection(A, _)), once(is_recollection(B, _)),
    normalize(A, NA), normalize(B, NB),
    NA == NB.

proves_impl(_ => [o(plus(A,B,C))], _) :-
    robinson_pack_enabled,
    once(is_recollection(A, _)), once(is_recollection(B, _)),
    arith_op(A, B, +, C),
    once(is_recollection(C, _)).

proves_impl(_ => [o(minus(A,B,C))], _) :-
    robinson_pack_enabled,
    current_domain(D), once(is_recollection(A, _)), once(is_recollection(B, _)),
    arith_op(A, B, -, C),
    normalize(C, NC),
    ((D=n, NC >= 0) ; member(D, [z, q])),
    once(is_recollection(C, _)).

proves_impl([n(plus(A,B,C))] => [n(plus(B,A,C))], _) :-
    robinson_pack_enabled.

proves_impl(_ => [o(mult(A,B,C))], _) :-
    robinson_pack_enabled,
    once(is_recollection(A, _)), once(is_recollection(B, _)),
    arith_op(A, B, *, C),
    once(is_recollection(C, _)).

proves_impl([n(mult(A,B,C))] => [n(mult(B,A,C))], _) :-
    robinson_pack_enabled.

% Successor grounding
proves_impl(_ => [o(eq(succ(X), SX))], _) :-
    robinson_pack_enabled,
    once(is_recollection(X, _)), integer(X),
    SX is X + 1,
    once(is_recollection(SX, _)).

% --- Robinson Axioms Q1-Q7 ---

% Q1: S(x) != 0
is_incoherent([o(eq(succ(X), 0))]) :-
    robinson_pack_enabled,
    integer(X), X >= 0, once(is_recollection(X, _)).

% Q2: S(x) = S(y) -> x = y (injective)
proves_impl([o(eq(succ(X), succ(Y)))] => [o(eq(X, Y))], _) :-
    robinson_pack_enabled,
    once(is_recollection(X, _)), once(is_recollection(Y, _)).

% Q3: x = 0 v exists y (x = S(y))
proves_impl(_ => [o(eq(X, 0))], _) :-
    robinson_pack_enabled,
    once(is_recollection(X, _)), integer(X), X =:= 0.
proves_impl(_ => [o(eq(X, succ(Y)))], _) :-
    robinson_pack_enabled,
    once(is_recollection(X, _)), integer(X), X > 0,
    Y is X - 1, once(is_recollection(Y, _)).

% Q4: x + 0 = x
proves_impl(_ => [o(eq(plus(X, 0), X))], _) :-
    robinson_pack_enabled,
    once(is_recollection(X, _)).

% Q5: x + S(y) = S(x + y)
proves_impl(_ => [o(eq(plus(X, succ(Y)), succ(plus(X, Y))))], _) :-
    robinson_pack_enabled,
    once(is_recollection(X, _)), once(is_recollection(Y, _)),
    integer(X), integer(Y),
    arith_op(X, Y, +, Sum),
    arith_op(Y, 1, +, SY),
    arith_op(X, SY, +, Sum2),
    arith_op(Sum, 1, +, SumPlus1),
    Sum2 =:= SumPlus1.

% Q6: x * 0 = 0
proves_impl(_ => [o(eq(mult(X, 0), 0))], _) :-
    robinson_pack_enabled,
    once(is_recollection(X, _)).

% Q7: x * S(y) = (x * y) + x
proves_impl(_ => [o(eq(mult(X, succ(Y)), plus(mult(X, Y), X)))], _) :-
    robinson_pack_enabled,
    once(is_recollection(X, _)), once(is_recollection(Y, _)),
    integer(X), integer(Y),
    arith_op(X, Y, *, Prod),
    arith_op(Y, 1, +, SY),
    arith_op(X, SY, *, Prod2),
    arith_op(Prod, X, +, ProdPlusX),
    Prod2 =:= ProdPlusX.

% --- Arithmetic Incoherence ---
is_incoherent(X) :-
    robinson_pack_enabled,
    member(n(minus(A,B,_)), X),
    current_domain(n),
    is_recollection(A, _), is_recollection(B, _),
    normalize(A, NA), normalize(B, NB),
    NA < NB, !.
