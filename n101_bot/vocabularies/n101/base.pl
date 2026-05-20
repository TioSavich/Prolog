:- module(term_base, [term_def/1]).

term_def(
    term(
        base,
        ['bases', 'base system', 'grouping unit'],
        [
            "A base is a special group — a group used recursively to build numbers.",
            "A base is a way of grouping units into one higher-level unit that can be used to build and measure other numbers.",
            "A base is special because we use it over and over again on itself to build larger numbers."
        ],
        [counting],
        [
            incompat(
                "a base is not just any group",
                [
                    "any group is a base",
                    "every grouping is a base"
                ],
                "A base is a special kind of group: one used recursively. Ten tallies grouped into one ten is a base move; it is re-applied at the next level (ten tens become one hundred), and the next, and the next."
            ),
            incompat(
                "base ten is not the only base",
                [
                    "base means ten",
                    "the base has to be ten",
                    "base and ten are the same"
                ],
                "Ten is one choice of base. Mayan and Babylonian civilizations used other bases. In N101 we work in base five precisely to make this visible."
            )
        ],
        [
            source("N101coursenotes_f24.md", "453-490", "Day 2: base defined via recursive grouping")
        ]
    )
).
