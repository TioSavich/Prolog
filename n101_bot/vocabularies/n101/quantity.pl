:- module(term_quantity, [term_def/1]).

% Drawn from N101coursenotes_f24.md, Day 1 (Tuesday 27 August).
% Amy's definition, lines approximately 167-205.

term_def(
    term(
        quantity,
        ['quantities', 'quantitative'],
        [
            "A quantity is a property of an object that is measurable.",
            "To be measurable means we need a measurement unit and a measurement process.",
            "A quantity can be discussed without a numerical value; the value is separate from the quantity itself."
        ],
        [measurement_unit, measurement_process, measure],
        [
            incompat(
                "a value is not itself the quantity",
                [
                    "value of a quantity is a quantity",
                    "a value is a quantity",
                    "20 hours per week is a quantity",
                    "the number itself is the quantity"
                ],
                "The VALUE of a quantity is called its measure, not the quantity itself. '20 hours per week' is a value of a quantity; the quantity is 'time spent per week on X.'"
            ),
            incompat(
                "a bare number without a property is not a quantity",
                [
                    "a number is a quantity",
                    "any number is a quantity",
                    "numerals are quantities"
                ],
                "A quantity requires an object with a measurable property. A bare number is not a quantity until it names how much of some property an object has."
            ),
            incompat(
                "properties without a measurement process are not quantities",
                [
                    "any property is a quantity",
                    "color is a quantity"
                ],
                "A property is only a quantity when it can be measured, i.e., when a measurement unit and process can be imagined for it."
            )
        ],
        [
            source("N101coursenotes_f24.md", "167-205", "Day 1: introduction of quantity as measurable property")
        ]
    )
).
