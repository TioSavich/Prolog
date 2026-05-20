:- module(term_measure, [term_def/1]).

term_def(
    term(
        measure,
        ['value', 'value of a quantity', 'measured value'],
        [
            "The measure of a quantity is the value — the number of measurement units that the measurement process finds.",
            "A measure is a number paired with a unit; '20 hours' is a measure of the quantity 'time per week.'"
        ],
        [quantity, measurement_unit],
        [
            incompat(
                "the measure is not the quantity",
                [
                    "the value is the quantity",
                    "20 hours is the quantity",
                    "the number found is the quantity"
                ],
                "The measure is a number paired with a unit; the quantity is the property being measured. You can discuss a quantity without measuring it."
            )
        ],
        [
            source("N101coursenotes_f24.md", "189-192", "Day 1: distinction between quantity and its value/measure")
        ]
    )
).
