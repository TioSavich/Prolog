:- module(hermeneutic_calculator,
          [ calculate/5 % Num1, Op, Num2, Strategy, Result
          , list_strategies/2 % Op, Strategies
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

calculate(N1, +, N2, 'COBO', Result) :-
    run_cobo(N1, N2, Result, _).
calculate(N1, +, N2, 'Chunking', Result) :-
    run_chunking(N1, N2, Result, _).
calculate(N1, +, N2, 'RMB', Result) :-
    run_rmb(N1, N2, Result, _).
calculate(N1, +, N2, 'Rounding', Result) :-
    run_rounding(N1, N2, Result, _).

calculate(M, -, S, 'COBO (Missing Addend)', Result) :-
    run_cobo_ma(M, S, Result, _).
calculate(M, -, S, 'CBBO (Take Away)', Result) :-
    run_cbbo_ta(M, S, Result, _).
calculate(M, -, S, 'Decomposition', Result) :-
    run_decomposition(M, S, Result, _).
calculate(M, -, S, 'Rounding', Result) :-
    run_sub_rounding(M, S, Result, _).
calculate(M, -, S, 'Sliding', Result) :-
    run_sliding(M, S, Result, _).
calculate(M, -, S, 'Chunking A', Result) :-
    run_chunking_a(M, S, Result, _).
calculate(M, -, S, 'Chunking B', Result) :-
    run_chunking_b(M, S, Result, _).
calculate(M, -, S, 'Chunking C', Result) :-
    run_chunking_c(M, S, Result, _).

calculate(N, *, S, 'C2C', Result) :-
    run_c2c(N, S, Result, _).
calculate(N, *, S, 'CBO', Result) :-
    run_cbo_mult(N, S, 10, Result, _).
calculate(N, *, S, 'Commutative Reasoning', Result) :-
    run_commutative_mult(N, S, Result, _).
calculate(N, *, S, 'DR', Result) :-
    run_dr(N, S, Result, _).

calculate(T, /, S, 'CBO (Division)', Result) :-
    run_cbo_div(T, S, 10, Result, _).
calculate(T, /, N, 'Dealing by Ones', Result) :-
    run_dealing_by_ones(T, N, Result, _).
calculate(T, /, S, 'IDP', Result) :-
    % Default KB for now
    KB = [40-5, 16-2, 8-1],
    run_idp(T, S, KB, Result, _).
calculate(E, /, G, 'UCR', Result) :-
    run_ucr(E, G, Result, _).
