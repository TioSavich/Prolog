:- module(term_measurement_process, [term_def/1]).

term_def(
    term(
        measurement_process,
        ['measurement processes', 'measuring', 'process of measurement'],
        [
            "The measurement process subdivides the quantity into measurement units and counts them.",
            "Measurement involves imagining or actually subdividing a quantity into some number of units and counting those units."
        ],
        [quantity, measurement_unit, counting],
        [
            incompat(
                "imagining a value is not a measurement process",
                [
                    "guessing the value is the measurement process",
                    "estimating without counting is a measurement process"
                ],
                "A measurement process subdivides and counts; it is not just naming a number. An estimate can happen without a process, but then there is no measurement — only a guess."
            )
        ],
        [
            source("N101coursenotes_f24.md", "181-188", "Day 1: process is subdivide-and-count")
        ]
    )
).
