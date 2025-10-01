% Automatically generated knowledge base.
:- op(550, xfy, rdiv).
run_learned_strategy(A, B, C, count_on_bigger, fsm_trace(count_on_bigger, [state(start, compare(3, 2)), state(swap_if_needed, conditional_swap(3, 2)), state(count_on, iterate_successor(3, 2))])) :-
    fsm_synthesis_engine:
    (   peano_to_int(A, D),
        peano_to_int(B, E),
        (   D>=E
        ->  F=D,
            G=E
        ;   F=E,
            G=D
        ),
        C is F+G
    ).
