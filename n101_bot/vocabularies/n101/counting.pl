:- module(term_counting, [term_def/1]).

term_def(
    term(
        counting,
        ['count', 'counts', 'enumerate', 'enumerating'],
        [
            "Counting is one way to determine the value of a quantity.",
            "Children count to determine how much they have of something — it is a way of organizing experience."
        ],
        [quantity, measurement_unit],
        [
            incompat(
                "reciting numbers without reference is not counting",
                [
                    "saying numbers aloud is counting",
                    "reciting the number sequence is counting"
                ],
                "Counting requires referring to units of some quantity. Reciting '1, 2, 3, 4, 5' without counting anything is number-word recitation, not counting."
            )
        ],
        [
            source("N101coursenotes_f24.md", "151-166", "Day 1: counting as way to determine a quantity's value")
        ]
    )
).
