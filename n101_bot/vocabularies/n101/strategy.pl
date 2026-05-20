:- module(term_strategy, [term_def/1]).

term_def(
    term(
        strategy,
        ['strategies', 'method', 'approach'],
        [
            "A strategy is a named pattern of reasoning for an operation. In N101 we name strategies for addition and subtraction: Standard Computational Algorithm (SCA), Rearranging to Make Bases (RMB), Chunking, Rounding and Adjusting.",
            "Different strategies can produce the same answer but they are NOT the same strategy.",
            "A strategy uses specific mathematical moves; naming the strategy lets us talk about the moves it uses."
        ],
        [explanation],
        [
            incompat(
                "same answer does not mean same strategy",
                [
                    "two strategies with the same answer are the same",
                    "they got the same answer so it is the same strategy",
                    "same result means same method",
                    "got the same answer so they used the same",
                    "both got 43 so they used the same strategy",
                    "both students got 43 so they used the same strategy",
                    "got the same answer so they used the same strategy",
                    "since they got the same answer, they used the same strategy",
                    "because they got the same answer, they used the same",
                    "same answer means they used the same strategy",
                    "they must have used the same strategy because they got the same answer"
                ],
                "Two students can reach the same answer via very different strategies — counting-on vs. making ten vs. SCA. The answer is not the strategy."
            ),
            incompat(
                "a strategy is not the same as a heuristic guess",
                [
                    "any guess is a strategy"
                ],
                "A strategy has internal structure: named moves done for articulable reasons. A guess without moves-plus-reasons is not a strategy."
            )
        ],
        [
            source("N101coursenotes_f24.md", "286-300", "Day 1: SCA, RMB, Chunking introduced via Matt and Joel")
        ]
    )
).
