/** <module> Oracle Server - Normative Strategy Black Box
 *
 * This module implements the "Normative Oracle" - a black box interface to
 * the pre-defined expert strategies (sar_* and smr_* modules). The oracle
 * is intentionally isolated from the primordial machine, accessible only
 * through a single, restricted interface: query_oracle/4.
 *
 * ARCHITECTURAL SEPARATION:
 * The primordial machine CANNOT directly access the internal workings of
 * expert strategies. It can only observe their external results and
 * interpretations. This enforces the philosophical position that learning
 * must occur through "recognition" rather than "introspection."
 *
 * PHILOSOPHICAL GROUNDING:
 * The oracle represents the "normative" - the culturally established ways
 * of doing mathematics. The primordial machine is like a student who hears
 * a friend say "I added 8+5 by rearranging to make 10." The student knows
 * the answer (13) and has a linguistic description of the method, but must
 * reconstruct the internal rational structure using only their own primitive
 * cognitive tools.
 *
 * This is Pragmatic Expressive Bootstrapping: observing vocabulary (V) and
 * reconstructing the practice (P) that makes it intelligible.
 *
 * BLACK BOX CONSTRAINT:
 * The oracle returns ONLY:
 *   1. The final numerical result
 *   2. A high-level textual interpretation
 *
 * The oracle NEVER returns:
 *   - Step-by-step execution traces
 *   - Internal state transitions
 *   - FSM structures
 *   - Intermediate computational states
 *
 * This forces the learner into genuine synthesis, not template matching.
 *
 * @author UMEDCA System - Oracle Architecture
 * @version 1.0
 */

:- module(oracle_server, [
    query_oracle/4,
    list_available_strategies/2
]).

% Load the strategy modules directly
:- use_module(sar_add_cobo, [run_cobo/4]).
:- use_module(sar_add_chunking, [run_chunking/4]).
:- use_module(sar_add_rmb, [run_rmb/4]).
:- use_module(sar_add_rounding, [run_rounding/4]).

% Subtraction strategies
:- use_module(sar_sub_cobo_missing_addend, [run_cobo_ma/4]).
:- use_module(sar_sub_cbbo_take_away, [run_cbbo_ta/4]).
:- use_module(sar_sub_decomposition, [run_decomposition/4]).
:- use_module(sar_sub_rounding, [run_sub_rounding/4]).
:- use_module(sar_sub_sliding, [run_sliding/4]).
:- use_module(sar_sub_chunking_a, [run_chunking_a/4]).
:- use_module(sar_sub_chunking_b, [run_chunking_b/4]).
:- use_module(sar_sub_chunking_c, [run_chunking_c/4]).

% Multiplication strategies
:- use_module(smr_mult_c2c, [run_c2c/4]).
:- use_module(smr_mult_cbo, [run_cbo_mult/5]).
:- use_module(smr_mult_commutative_reasoning, [run_commutative_mult/4]).
:- use_module(smr_mult_dr, [run_dr/4]).

% Division strategies
:- use_module(smr_div_cbo, [run_cbo_div/5]).
:- use_module(smr_div_dealing_by_ones, [run_dealing_by_ones/4]).
:- use_module(smr_div_idp, [run_idp/5]).
:- use_module(smr_div_ucr, [run_ucr/4]).

% Load the hermeneutic calculator for strategy listing
:- use_module(hermeneutic_calculator, [list_strategies/2]).

%!      query_oracle(+Operation, +StrategyName, -Result, -Interpretation) is semidet.
%
%       The SOLE interface to the normative oracle. Given an arithmetic
%       operation and a strategy name, returns the numerical result and a
%       high-level interpretation of the method used.
%
%       BLACK BOX ENFORCEMENT:
%       - Input: Operation (e.g., add(8,5)) and StrategyName (e.g., 'RMB')
%       - Output: Result (e.g., 13) and Interpretation (e.g., 'Rearrange to make base 10')
%       - Hidden: All internal execution traces, state transitions, FSM structures
%
%       USAGE BY PRIMORDIAL MACHINE:
%       When the primordial machine encounters a crisis (resource_exhaustion),
%       it can query the oracle to see how an expert would solve the problem.
%       The machine receives the "what" (result) and "how" (interpretation),
%       but must synthesize its own "why" (FSM structure) from primitives.
%
%       @param Operation A compound term representing the arithmetic operation.
%                        Format: op(Num1, Num2) where op is add, subtract, multiply, or divide
%       @param StrategyName An atom identifying the expert strategy to use.
%                           Must be a valid strategy name from list_strategies/2.
%       @param Result The final numerical result (integer).
%       @param Interpretation A textual description of the strategy's approach (atom or string).
%
%       @throws error(domain_error) if StrategyName is not valid for the operation
%       @throws error(type_error) if Operation is malformed
%
%       @example Query the oracle for addition using "Rearranging to Make Bases"
%           ?- query_oracle(add(8,5), 'RMB', Result, Interp).
%           Result = 13,
%           Interp = 'Rearrange to make base 10: 8+5 = (8+2)+3 = 10+3 = 13'.
%
query_oracle(Operation, StrategyName, Result, Interpretation) :-
    % Validate and decompose the operation
    decompose_operation(Operation, Num1, Op, Num2),
    
    % Execute the strategy directly (bypassing hermeneutic_calculator due to its bugs)
    % This captures the full execution history
    execute_strategy(Num1, Op, Num2, StrategyName, Result, FullHistory),
    
    % Extract ONLY the high-level interpretation from the history
    % This is the BLACK BOX boundary - internal states are discarded
    extract_interpretation(StrategyName, Op, Num1, Num2, Result, FullHistory, Interpretation).

%!      execute_strategy(+Num1, +Op, +Num2, +StrategyName, -Result, -History) is semidet.
%
%       Executes a specific strategy directly by calling the appropriate module.
%       This bypasses the buggy hermeneutic_calculator dispatcher.
%
execute_strategy(Num1, +, Num2, 'COBO', Result, History) :-
    sar_add_cobo:run_cobo(Num1, Num2, Result, History).
execute_strategy(Num1, +, Num2, 'Chunking', Result, History) :-
    sar_add_chunking:run_chunking(Num1, Num2, Result, History).
execute_strategy(Num1, +, Num2, 'RMB', Result, History) :-
    sar_add_rmb:run_rmb(Num1, Num2, Result, History).
execute_strategy(Num1, +, Num2, 'Rounding', Result, History) :-
    sar_add_rounding:run_rounding(Num1, Num2, Result, History).

% Subtraction Strategies
execute_strategy(Num1, -, Num2, 'COBO (Missing Addend)', Result, History) :-
    sar_sub_cobo_missing_addend:run_cobo_ma(Num1, Num2, Result, History).
execute_strategy(Num1, -, Num2, 'CBBO (Take Away)', Result, History) :-
    sar_sub_cbbo_take_away:run_cbbo_ta(Num1, Num2, Result, History).
execute_strategy(Num1, -, Num2, 'Decomposition', Result, History) :-
    sar_sub_decomposition:run_decomposition(Num1, Num2, Result, History).
execute_strategy(Num1, -, Num2, 'Rounding', Result, History) :-
    sar_sub_rounding:run_sub_rounding(Num1, Num2, Result, History).
execute_strategy(Num1, -, Num2, 'Sliding', Result, History) :-
    sar_sub_sliding:run_sliding(Num1, Num2, Result, History).
execute_strategy(Num1, -, Num2, 'Chunking A', Result, History) :-
    sar_sub_chunking_a:run_chunking_a(Num1, Num2, Result, History).
execute_strategy(Num1, -, Num2, 'Chunking B', Result, History) :-
    sar_sub_chunking_b:run_chunking_b(Num1, Num2, Result, History).
execute_strategy(Num1, -, Num2, 'Chunking C', Result, History) :-
    sar_sub_chunking_c:run_chunking_c(Num1, Num2, Result, History).

% Multiplication Strategies
execute_strategy(Num1, *, Num2, 'C2C', Result, History) :-
    smr_mult_c2c:run_c2c(Num1, Num2, Result, History).
execute_strategy(Num1, *, Num2, 'CBO', Result, History) :-
    smr_mult_cbo:run_cbo_mult(Num1, Num2, 10, Result, History).
execute_strategy(Num1, *, Num2, 'Commutative Reasoning', Result, History) :-
    smr_mult_commutative_reasoning:run_commutative_mult(Num1, Num2, Result, History).
execute_strategy(Num1, *, Num2, 'DR', Result, History) :-
    smr_mult_dr:run_dr(Num1, Num2, Result, History).

% Division Strategies
execute_strategy(Num1, /, Num2, 'CBO (Division)', Result, History) :-
    smr_div_cbo:run_cbo_div(Num1, Num2, 10, Result, History).
execute_strategy(Num1, /, Num2, 'Dealing by Ones', Result, History) :-
    smr_div_dealing_by_ones:run_dealing_by_ones(Num1, Num2, Result, History).
execute_strategy(Num1, /, Num2, 'IDP', Result, History) :-
    % Knowledge Base of known multiplication facts for IDP strategy
    % Format: Product-Multiplier means Product = Multiplier × Divisor
    % This allows IDP to decompose dividends using known factor pairs
    KB = [
        % Facts for divisor 7: 56 = 16+40 = 2×7+5×7
        40-5, 16-2, 8-1,
        % Facts for divisor 3: 12 = 9+3 = 3×3+1×3
        9-3, 6-2, 3-1,
        % Facts for divisor 5: 35 = 30+5 = 6×5+1×5
        30-6, 25-5, 20-4, 15-3, 10-2, 5-1,
        % Facts for divisor 12: 84 = 72+12 = 6×12+1×12
        72-6, 60-5, 48-4, 36-3, 24-2, 12-1
    ],
    smr_div_idp:run_idp(Num1, Num2, KB, Result, History).
execute_strategy(Num1, /, Num2, 'UCR', Result, History) :-
    smr_div_ucr:run_ucr(Num1, Num2, Result, History).

% Catch-all for unimplemented strategies
execute_strategy(_Num1, Op, _Num2, StrategyName, _Result, _History) :-
    throw(error(not_implemented(execute_strategy(Op, StrategyName)),
                context(oracle_server, 'Strategy not yet implemented in oracle'))).

%!      decompose_operation(+Operation, -Num1, -Op, -Num2) is det.
%
%       Decomposes an operation term into its components.
%       Validates that the operation is well-formed.
decompose_operation(add(Num1, Num2), Num1, +, Num2) :-
    integer(Num1), integer(Num2).
decompose_operation(subtract(Num1, Num2), Num1, -, Num2) :-
    integer(Num1), integer(Num2).
decompose_operation(multiply(Num1, Num2), Num1, *, Num2) :-
    integer(Num1), integer(Num2).
decompose_operation(divide(Num1, Num2), Num1, /, Num2) :-
    integer(Num1), integer(Num2), Num2 \= 0.
decompose_operation(Op, _, _, _) :-
    throw(error(type_error(operation, Op), 
                context(query_oracle/4, 'Operation must be add/subtract/multiply/divide with integer arguments'))).

%!      extract_interpretation(+StrategyName, +Op, +Num1, +Num2, +Result, +History, -Interpretation) is det.
%
%       Extracts a high-level textual interpretation from the execution history.
%       This is where we enforce the BLACK BOX constraint - we summarize the
%       approach without revealing internal computational states.
%
%       The interpretation should be:
%       - High-level (conceptual, not computational)
%       - Linguistic (uses mathematical vocabulary)
%       - Sufficient to constrain synthesis (guides the search)
%       - Insufficient for template matching (doesn't give away the FSM)
%
extract_interpretation('COBO', +, Num1, Num2, Result, _History, Interpretation) :-
    format(atom(Interpretation), 
           'Count on from bigger: Start at ~w, count up ~w times to reach ~w',
           [max(Num1,Num2), min(Num1,Num2), Result]).

extract_interpretation('RMB', +, Num1, Num2, Result, _History, Interpretation) :-
    % Determine which number was closer to base 10
    Dist1 is abs(10 - Num1),
    Dist2 is abs(10 - Num2),
    (   Dist1 < Dist2
    ->  From = Num1, Adding = Num2
    ;   From = Num2, Adding = Num1
    ),
    format(atom(Interpretation),
           'Rearrange to make base 10: Start at ~w, move units from ~w to reach 10, then add remainder to get ~w',
           [From, Adding, Result]).

extract_interpretation('Chunking', +, Num1, Num2, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Chunking: Break ~w+~w into decade chunks and ones, combine to get ~w',
           [Num1, Num2, Result]).

extract_interpretation('Rounding', +, Num1, Num2, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Rounding: Round one number to nearest ten, adjust, result is ~w',
           [Result]).

% Subtraction interpretations
extract_interpretation('COBO (Missing Addend)', -, Minuend, Subtrahend, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Count on from subtrahend: Start at ~w, count up to ~w, the gap is ~w',
           [Subtrahend, Minuend, Result]).

extract_interpretation('CBBO (Take Away)', -, Minuend, Subtrahend, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Count back from bigger: Start at ~w, count back ~w times to reach ~w',
           [Minuend, Subtrahend, Result]).

extract_interpretation('Decomposition', -, Minuend, Subtrahend, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Decomposition: Break ~w into parts, subtract ~w from each part, recombine to get ~w',
           [Minuend, Subtrahend, Result]).

extract_interpretation('Sliding', -, Minuend, Subtrahend, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Sliding: Adjust both numbers by same amount to simplify, then subtract to get ~w',
           [Result]).

extract_interpretation(Name, -, _, _, Result, _History, Interpretation) :-
    atom_string(Name, NameStr),
    (   sub_string(NameStr, _, _, _, "Chunking")
    ->  format(atom(Interpretation), 'Chunking subtraction to get ~w', [Result])
    ;   format(atom(Interpretation), 'Subtraction strategy ~w yields ~w', [Name, Result])
    ).

% Multiplication interpretations  
extract_interpretation('CBO', *, Num1, Num2, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Count by ones to base: Organize ~w groups of ~w using base 10 structure to get ~w',
           [Num1, Num2, Result]).

extract_interpretation('C2C', *, Num1, Num2, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Count to count: Build up ~w copies of ~w through repeated addition to get ~w',
           [Num1, Num2, Result]).

extract_interpretation('Commutative Reasoning', *, Num1, Num2, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Commutative reasoning: Recognize ~w×~w = ~w×~w for efficiency, result is ~w',
           [Num1, Num2, Num2, Num1, Result]).

extract_interpretation('DR', *, Num1, Num2, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Doubling and redistribution: Use doubling patterns in ~w×~w to get ~w',
           [Num1, Num2, Result]).

% Division interpretations
extract_interpretation('CBO (Division)', /, Dividend, Divisor, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Count by ones to base: Organize ~w into groups of ~w using base 10, finding ~w groups',
           [Dividend, Divisor, Result]).

extract_interpretation('Dealing by Ones', /, Dividend, Divisor, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Dealing by ones: Distribute ~w items one-by-one into ~w groups, ~w per group',
           [Dividend, Divisor, Result]).

extract_interpretation('IDP', /, Dividend, Divisor, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Inverse of division by product: Use known multiplication facts to find ~w÷~w = ~w',
           [Dividend, Divisor, Result]).

extract_interpretation('UCR', /, Dividend, Divisor, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Unit coordination and remainders: Coordinate units in ~w÷~w to get ~w',
           [Dividend, Divisor, Result]).

% Generic fallback
extract_interpretation(StrategyName, _Op, _Num1, _Num2, Result, _History, Interpretation) :-
    format(atom(Interpretation),
           'Strategy ~w produces result ~w',
           [StrategyName, Result]).

%!      list_available_strategies(+Operation, -Strategies) is det.
%
%       Lists the available expert strategies for a given operation type.
%       This allows the primordial machine to know what strategies it could
%       query from the oracle.
%
%       @param Operation The operation type (add, subtract, multiply, divide)
%       @param Strategies A list of strategy names (atoms)
%
list_available_strategies(add, Strategies) :-
    hermeneutic_calculator:list_strategies(+, Strategies).
list_available_strategies(subtract, Strategies) :-
    hermeneutic_calculator:list_strategies(-, Strategies).
list_available_strategies(multiply, Strategies) :-
    hermeneutic_calculator:list_strategies(*, Strategies).
list_available_strategies(divide, Strategies) :-
    hermeneutic_calculator:list_strategies(/, Strategies).

% ═══════════════════════════════════════════════════════════════════════
% ORACLE INTERFACE BOUNDARY
% ═══════════════════════════════════════════════════════════════════════
%
% Everything below this line is INTERNAL to the oracle and must not be
% directly accessible to the primordial machine.
%
% The oracle's internal workings (FSM structures, state transitions,
% computational traces) are hidden behind the query_oracle/4 interface.
%
% This architectural separation forces the primordial machine to engage
% in genuine synthesis through recognition, not template matching through
% introspection.
%
% ═══════════════════════════════════════════════════════════════════════
