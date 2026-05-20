:- module(term_explanation, [term_def/1]).

term_def(
    term(
        explanation,
        ['explanations', 'explaining', 'justification'],
        [
            "A good explanation describes what you did AND gives reasons for what you did.",
            "Explanations sometimes articulate consequences of a thinking move.",
            "Good explanations do not have to be long; they have to have description plus reasons."
        ],
        [strategy],
        [
            incompat(
                "description alone is not an explanation",
                [
                    "description is the same as explanation",
                    "describing what you did is explaining",
                    "you have explained by describing"
                ],
                "Description tells what you did; explanation adds the reasons WHY. An answer that only describes is not yet an explanation."
            ),
            incompat(
                "because it's easier is not an acceptable reason",
                [
                    "because it is easier",
                    "because its easier",
                    "because that way is easier",
                    "because this approach is easier"
                ],
                "'Because it's easier' is not a reason Amy accepts. A reason has to identify the mathematical structure that makes the move work — e.g., making a whole number of bases, decomposing across a base boundary."
            )
        ],
        [
            source("N101coursenotes_f24.md", "597-620", "Day 2: explanation distinguished from description"),
            source("N101coursenotes_f24.md", "13-16", "Notes: Don't say because it's easier!")
        ]
    )
).
