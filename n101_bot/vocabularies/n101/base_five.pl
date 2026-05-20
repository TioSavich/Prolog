:- module(term_base_five, [term_def/1]).

term_def(
    term(
        base_five,
        ['base 5', 'fives', 'grouping by five'],
        [
            "Base five groups tallies in fives rather than tens.",
            "In base five, the numeral 10 means one base (one hand) and zero loose tallies.",
            "Working in base five helps preservice teachers feel what it is like for children to build up a base-ten system for the first time."
        ],
        [base, counting],
        [
            incompat(
                "5 cannot be the digit for the base in base five",
                [
                    "5 represents the base in base five",
                    "the digit 5 is one base",
                    "base five uses the numeral 5 for its base"
                ],
                "In base five the digit '5' is not used. One base (hand) and zero loose tallies is written '10'. The digits available in base five are 0, 1, 2, 3, 4."
            ),
            incompat(
                "the digits in base five do not include 5 through 9",
                [
                    "base five uses digits 0-9",
                    "base five has a 5 digit",
                    "base five uses the digits 5 6 7 8 9"
                ],
                "The digits in base five are 0, 1, 2, 3, 4 only. Getting to five means grouping up to the next place."
            )
        ],
        [
            source("N101coursenotes_f24.md", "521-590", "Day 2: counting in base five, hand naming convention")
        ]
    )
).
