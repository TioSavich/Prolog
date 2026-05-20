:- module(term_measurement_unit, [term_def/1]).

term_def(
    term(
        measurement_unit,
        ['measurement units', 'unit of measurement', 'unit'],
        [
            "A measurement unit is the unit with which the measurement process counts.",
            "For height, a measurement unit could be a paperclip or popsicle stick; for counting people, the unit is the discrete person."
        ],
        [quantity, measurement_process],
        [
            incompat(
                "the object being measured is not its unit",
                [
                    "the person is the unit",
                    "the object is the unit of itself"
                ],
                "The measurement unit is distinct from the object being measured. To measure a person's height, the unit is something like inches or paperclips, not the person."
            ),
            incompat(
                "a measurement unit must be iterable",
                [
                    "a single unique measurement is a unit"
                ],
                "A measurement unit must be something you can count repeatedly through the measurement process."
            )
        ],
        [
            source("N101coursenotes_f24.md", "179-182", "Day 1: measurement unit with paperclip/popsicle stick example")
        ]
    )
).
