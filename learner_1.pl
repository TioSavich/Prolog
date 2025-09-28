/** <module> Simple Strategy Resolution Model
 *
 *  This module provides a simple, self-contained model for resolving the
 *  outputs from multiple, potentially conflicting, information sources or
 *  "strategies". It is a conceptual demonstration and is not integrated with
 *  the main ORR cycle or the other learner modules.
 *
 *  The model consists of two parts:
 *  1.  A database of facts (`strategy_output/4`) that simulates the results
 *      produced by different named strategies for various problems.
 *  2.  A `compute/3` predicate that gathers all possible results for a given
 *      problem and uses a `resolve/2` helper to determine the final outcome
 *      based on a simple semantic:
 *      - If all strategies agree, that is the result.
 *      - If strategies disagree, the result is `incompatible`.
 *      - If no strategy can solve the problem, the result is `unknown`.
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(learner_1, [compute/3]).

:- use_module(library(lists)).

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

% resolve(+ListOfResults, -FinalResult)
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

%!      compute(+Op:atom, +Inputs:list, -Result) is det.
%
%       Computes the result for a given operation and inputs by polling all
%       available strategies and resolving their outputs.
%
%       It uses `findall/3` to collect all possible results from the
%       `strategy_output/4` database for the given `Op` and `Inputs`. It then
%       passes this list of results to `resolve/2` to determine the final,
%       semantically coherent result.
%
%       @param Op The operation to perform (e.g., `add`, `subtract`).
%       @param Inputs A list of input numbers for the operation.
%       @param Result The final resolved result, which can be a number,
%       the atom `incompatible`, or the atom `unknown`.
compute(Op, Inputs, Result) :-
    % Step 1: Find all results from all available strategies for the given problem.
    findall(R, strategy_output(_, Op, Inputs, R), ResultsList),

    % Step 2: Resolve the collected list of results using our semantics.
    resolve(ResultsList, Result).