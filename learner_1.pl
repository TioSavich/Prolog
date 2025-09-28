% --- A. DATABASE OF STRATEGY OUTPUTS ---
% This section simulates the results from different strategies.
% Format: strategy_output(StrategyName, Operation, InputsList, Result).

% Case 1: Agreement
% Both strategy_a and strategy_b correctly compute 2 + 3.
strategy_output(strategy_a, add, [2, 3], 5).
strategy_output(strategy_b, add, [2, 3], 5).

% Case 2: Incompatibility (Disagreement)
% strategy_a correctly computes 5 - 1, but strategy_c gets it wrong.
strategy_output(strategy_a, subtract, [5, 1], 4).
strategy_output(strategy_c, subtract, [5, 1], 6). % Incorrect result

% Case 3: Single Available Strategy
% Only strategy_b knows how to multiply.
strategy_output(strategy_b, multiply, [10, 2], 20).

% --- B. RULES FOR COMPUTATION ---
% This section implements the logic to compute a final result.

% resolve(ListOfResults, FinalResult).
% This helper predicate applies the semantics to a list of gathered results.

% Rule 1: If the list of results is empty, the answer is 'unknown'.
resolve([], unknown).

% Rule 2: If the list of results contains different values, it's 'incompatible'.
% We convert the list to a set. If the set has more than one member, there was a disagreement.
resolve(ResultsList, incompatible) :-
    list_to_set(ResultsList, Set),
    length(Set, L),
    L > 1.

% Rule 3: If the list of results contains one or more identical values, that is the answer.
% After converting to a set, there will be exactly one element.
resolve(ResultsList, Result) :-
    list_to_set(ResultsList, Set),
    length(Set, 1),
    [Result] = Set.

% compute(Operation, Inputs, FinalResult).
% This is the main predicate to query. ⚙️

compute(Op, Inputs, Result) :-
    % Step 1: Find all results from all available strategies for the given problem.
    findall(R, strategy_output(_, Op, Inputs, R), ResultsList),
    
    % Step 2: Resolve the collected list of results using our semantics.
    resolve(ResultsList, Result).