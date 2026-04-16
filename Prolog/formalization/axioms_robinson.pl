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

% Base case: 0 is axiomatically a number.
is_recollection(0, [axiom(zero)]).

% Support for explicit recollection structures from grounded_arithmetic
is_recollection(recollection(History), [explicit_recollection(History)]) :-
    is_list(History),
    maplist(=(tally), History).

% Recursive case for positive integers
is_recollection(N, History) :-
    integer(N),
    N > 0,
    Prev is N - 1,
    is_recollection(Prev, _),
    hermeneutic_calculator:calculate(Prev, +, 1, _Strategy, N, History).

% Case for negative integers
is_recollection(N, History) :-
    integer(N),
    N < 0,
    is_recollection(0, _),
    Val is abs(N),
    hermeneutic_calculator:calculate(0, -, Val, _Strategy, N, History).

% Case for rational numbers
is_recollection(N rdiv D, [history(rational, from(N, D))]) :-
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

arith_op(A, B, Op, C) :-
    member(Op, [+, -, *]),
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

% --- Arithmetic Grounding Rules ---

proves_impl(_ => [o(eq(A,B))], _) :-
    once(is_recollection(A, _)), once(is_recollection(B, _)),
    normalize(A, NA), normalize(B, NB),
    NA == NB.

proves_impl(_ => [o(plus(A,B,C))], _) :-
    once(is_recollection(A, _)), once(is_recollection(B, _)),
    arith_op(A, B, +, C),
    once(is_recollection(C, _)).

proves_impl(_ => [o(minus(A,B,C))], _) :-
    current_domain(D), once(is_recollection(A, _)), once(is_recollection(B, _)),
    arith_op(A, B, -, C),
    normalize(C, NC),
    ((D=n, NC >= 0) ; member(D, [z, q])),
    once(is_recollection(C, _)).

proves_impl([n(plus(A,B,C))] => [n(plus(B,A,C))], _).

proves_impl(_ => [o(mult(A,B,C))], _) :-
    once(is_recollection(A, _)), once(is_recollection(B, _)),
    arith_op(A, B, *, C),
    once(is_recollection(C, _)).

proves_impl([n(mult(A,B,C))] => [n(mult(B,A,C))], _).

% Successor grounding
proves_impl(_ => [o(eq(succ(X), SX))], _) :-
    once(is_recollection(X, _)), integer(X),
    SX is X + 1,
    once(is_recollection(SX, _)).

% --- Robinson Axioms Q1-Q7 ---

% Q1: S(x) != 0
is_incoherent([o(eq(succ(X), 0))]) :-
    integer(X), X >= 0, once(is_recollection(X, _)).

% Q2: S(x) = S(y) -> x = y (injective)
proves_impl([o(eq(succ(X), succ(Y)))] => [o(eq(X, Y))], _) :-
    once(is_recollection(X, _)), once(is_recollection(Y, _)).

% Q3: x = 0 v exists y (x = S(y))
proves_impl(_ => [o(eq(X, 0))], _) :-
    once(is_recollection(X, _)), integer(X), X =:= 0.
proves_impl(_ => [o(eq(X, succ(Y)))], _) :-
    once(is_recollection(X, _)), integer(X), X > 0,
    Y is X - 1, once(is_recollection(Y, _)).

% Q4: x + 0 = x
proves_impl(_ => [o(eq(plus(X, 0), X))], _) :-
    once(is_recollection(X, _)).

% Q5: x + S(y) = S(x + y)
proves_impl(_ => [o(eq(plus(X, succ(Y)), succ(plus(X, Y))))], _) :-
    once(is_recollection(X, _)), once(is_recollection(Y, _)),
    integer(X), integer(Y),
    arith_op(X, Y, +, Sum),
    arith_op(Y, 1, +, SY),
    arith_op(X, SY, +, Sum2),
    arith_op(Sum, 1, +, SumPlus1),
    Sum2 =:= SumPlus1.

% Q6: x * 0 = 0
proves_impl(_ => [o(eq(mult(X, 0), 0))], _) :-
    once(is_recollection(X, _)).

% Q7: x * S(y) = (x * y) + x
proves_impl(_ => [o(eq(mult(X, succ(Y)), plus(mult(X, Y), X)))], _) :-
    once(is_recollection(X, _)), once(is_recollection(Y, _)),
    integer(X), integer(Y),
    arith_op(X, Y, *, Prod),
    arith_op(Y, 1, +, SY),
    arith_op(X, SY, *, Prod2),
    arith_op(Prod, X, +, ProdPlusX),
    Prod2 =:= ProdPlusX.

% --- Arithmetic Incoherence ---
is_incoherent(X) :-
    member(n(minus(A,B,_)), X),
    current_domain(n),
    is_recollection(A, _), is_recollection(B, _),
    normalize(A, NA), normalize(B, NB),
    NA < NB, !.
