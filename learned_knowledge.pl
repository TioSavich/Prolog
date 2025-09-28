% Automatically generated knowledge base.
:- op(550, xfy, rdiv).
run_learned_strategy(A, B, C, rmb(10), D) :-
    integer(A),
    integer(B),
    A>0,
    A<10,
    E is 10-A,
    B>=E,
    F is B-E,
    C is 10+F,
    D=trace{a_start:A, b_start:B, steps:[step(A, 10), step(10, C)], strategy:rmb(10)}.
run_learned_strategy(A, B, C, doubles, D) :-
    integer(A),
    A==B,
    C is A*2,
    D=trace{a_start:A, b_start:B, steps:[rote(C)], strategy:doubles}.
run_learned_strategy(A, B, C, cob, D) :-
    integer(A),
    integer(B),
    (   A>=B
    ->  E=A,
        F=B,
        G=no_swap
    ;   E=B,
        F=A,
        G=swapped(B, A)
    ),
    (   G=swapped(_, _)
    ->  (   proves(([n(plus(A, B, H))]=>[n(plus(B, A, H))]))
        ->  true
        ;   fail
        )
    ;   true
    ),
    solve_foundationally(E, F, C, I),
    D=trace{a_start:A, b_start:B, steps:[G, inner_trace(I)], strategy:cob}.
