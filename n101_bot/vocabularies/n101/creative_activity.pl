:- module(term_creative_activity, [term_def/1]).

term_def(
    term(
        creative_activity,
        ['creativity', 'creative experience', 'creative mathematics'],
        [
            "Arithmetic — for yourself and for kids — should be as creative an activity as dancing or playing the piano.",
            "Creative mathematical activity involves visualizing, making decisions, choosing pathways, and sometimes arriving at different outcomes."
        ],
        [strategy],
        [
            incompat(
                "arithmetic is not rote by nature",
                [
                    "arithmetic is just memorization",
                    "arithmetic is rote",
                    "arithmetic is not creative",
                    "math is not a creative subject",
                    "arithmetic is only about getting answers"
                ],
                "Amy's position: arithmetic is creative. Treating it as pure rote recitation of procedures misses the generative, decision-making, pathway-choosing character of doing arithmetic."
            ),
            incompat(
                "drill is not the same as creative activity",
                [
                    "drill is creative activity",
                    "practice worksheets are creative"
                ],
                "Drill practices a fixed procedure. Creative activity generates and chooses among pathways. They are different activities, though both can have a place."
            )
        ],
        [
            source("N101coursenotes_f24.md", "119-148", "Day 1: position that arithmetic is creative")
        ]
    )
).
