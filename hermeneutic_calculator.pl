/** <module> Hermeneutic Calculator - Strategy Dispatcher
 *
 * This module acts as a high-level dispatcher for the various cognitive
 * strategy models implemented in the `sar_*` and `smr_*` modules. It provides
 * a unified interface to execute a calculation using a specific, named
 * strategy and to list the available strategies for each arithmetic operation.
 *
 * This allows the user interface or other components to abstract away the
 * details of individual strategy modules.
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(hermeneutic_calculator,
          [ calculate/6
          , list_strategies/2
          ]).

% Addition Strategies
:- use_module(sar_add_cobo).
:- use_module(sar_add_chunking).
:- use_module(sar_add_rmb).
:- use_module(sar_add_rounding).

% Subtraction Strategies
:- use_module(sar_sub_cobo_missing_addend).
:- use_module(sar_sub_cbbo_take_away).
:- use_module(sar_sub_decomposition).
:- use_module(sar_sub_rounding).
:- use_module(sar_sub_sliding).
:- use_module(sar_sub_chunking_a).
:- use_module(sar_sub_chunking_b).
:- use_module(sar_sub_chunking_c).

% Multiplication Strategies
:- use_module(smr_mult_c2c).
:- use_module(smr_mult_cbo).
:- use_module(smr_mult_commutative_reasoning).
:- use_module(smr_mult_dr).

% Division Strategies
:- use_module(smr_div_cbo).
:- use_module(smr_div_dealing_by_ones).
:- use_module(smr_div_idp).
:- use_module(smr_div_ucr).

% Counting Automata
:- use_module(counting2).
:- use_module(counting_on_back).

% --- Strategy Lists ---

%!      list_strategies(+Op:atom, -Strategies:list) is nondet.
%
%       Provides a list of available strategy names for a given arithmetic
%       operator.
%
%       @param Op The operator (`+`, `-`, `*`, `/`).
%       @param Strategies A list of atoms representing the names of the
%       strategies available for that operator.
list_strategies(+, [
    'COBO',
    'Chunking',
    'RMB',
    'Rounding'
]).
list_strategies(-, [
    'COBO (Missing Addend)',
    'CBBO (Take Away)',
    'Decomposition',
    'Rounding',
    'Sliding',
    'Chunking A',
    'Chunking B',
    'Chunking C'
]).
list_strategies(*, [
    'C2C',
    'CBO',
    'Commutative Reasoning',
    'DR'
]).
list_strategies(/, [
    'CBO (Division)',
    'Dealing by Ones',
    'IDP',
    'UCR'
]).

% --- Calculator Dispatch ---

%!      calculate(+Num1:integer, +Op:atom, +Num2:integer, +Strategy:atom, -Result:integer, -History:list) is semidet.
%
%       Executes a calculation using a specified cognitive strategy.
%       This predicate acts as a dispatcher, calling the appropriate
%       `run_*` predicate from the various strategy modules based on the
%       `Strategy` name. It now captures and returns the execution trace.
%
%       @param Num1 The first operand.
%       @param Op The arithmetic operator (`+`, `-`, `*`, `/`).
%       @param Num2 The second operand.
%       @param Strategy The name of the strategy to use (must match one from
%       `list_strategies/2`).
%       @param Result The numerical result of the calculation. Fails if the
%       strategy does not complete successfully.
%       @param History A list of terms representing the execution trace of
%       the chosen strategy.
calculate(N1, +, N2, 'COBO', Result, History) :-
    run_cobo(N1, N2, Result, History).
calculate(N1, +, N2, 'Chunking', Result, History) :-
    run_chunking(N1, N2, Result, History).
calculate(N1, +, N2, 'RMB', Result, History) :-
    run_rmb(N1, N2, Result, History).
calculate(N1, +, N2, 'Rounding', Result, History) :-
    run_rounding(N1, N2, Result, History).

calculate(M, -, S, 'COBO (Missing Addend)', Result, History) :-
    run_cobo_ma(M, S, Result, History).
calculate(M, -, S, 'CBBO (Take Away)', Result, History) :-
    run_cbbo_ta(M, S, Result, History).
calculate(M, -, S, 'Decomposition', Result, History) :-
    run_decomposition(M, S, Result, History).
calculate(M, -, S, 'Rounding', Result, History) :-
    run_sub_rounding(M, S, Result, History).
calculate(M, -, S, 'Sliding', Result, History) :-
    run_sliding(M, S, Result, History).
calculate(M, -, S, 'Chunking A', Result, History) :-
    run_chunking_a(M, S, Result, History).
calculate(M, -, S, 'Chunking B', Result, History) :-
    run_chunking_b(M, S, Result, History).
calculate(M, -, S, 'Chunking C', Result, History) :-
    run_chunking_c(M, S, Result, History).

calculate(N, *, S, 'C2C', Result, History) :-
    run_c2c(N, S, Result, History).
calculate(N, *, S, 'CBO', Result, History) :-
    run_cbo_mult(N, S, 10, Result, History).
calculate(N, *, S, 'Commutative Reasoning', Result, History) :-
    run_commutative_mult(N, S, Result, History).
calculate(N, *, S, 'DR', Result, History) :-
    run_dr(N, S, Result, History).

calculate(T, /, S, 'CBO (Division)', Result, History) :-
    run_cbo_div(T, S, 10, Result, History).
calculate(T, /, N, 'Dealing by Ones', Result, History) :-
    run_dealing_by_ones(T, N, Result, History).
calculate(T, /, S, 'IDP', Result, History) :-
    % A default Knowledge Base is provided for demonstration.
    KB = [40-5, 16-2, 8-1],
    run_idp(T, S, KB, Result, History).
calculate(E, /, G, 'UCR', Result, History) :-
    run_ucr(E, G, Result, History).
