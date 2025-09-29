/** <module> More Machine Learner (Protein Folding Analogy)
 *
 * This module implements a machine learning system inspired by protein folding,
 * where a system seeks a lower-energy, more efficient state. It learns new,
 * more efficient arithmetic strategies by observing the execution traces of
 * less efficient ones.
 *
 * The core components are:
 * 1.  **A Foundational Solver**: The most basic, inefficient way to solve a
 *     problem (e.g., counting on by ones). This is the "unfolded" state.
 * 2.  **A Strategy Hierarchy**: A dynamic knowledge base of `run_learned_strategy/5`
 *     clauses. The system always tries the most "folded" (efficient) strategies first.
 * 3.  **A Generative-Reflective Loop (`explore/1`)**:
 *     - **Generative Phase**: Solves a problem using the current best strategy.
 *     - **Reflective Phase**: Analyzes the execution trace of the solution,
 *       looking for patterns that suggest a more efficient strategy (a "fold").
 * 4.  **Pattern Detection & Construction**: Specific predicates that detect
 *     patterns (e.g., commutativity, making a 10) and construct new, more
 *     efficient strategy clauses. These new clauses are then asserted into
 *     the knowledge base.
 *
 * 
 * 
 */
:- module(more_machine_learner,
          [ critique_and_bootstrap/1,
            run_learned_strategy/5,
            solve/4,
            save_knowledge/0,
            reflect_and_learn/1
          ]).

% Use the semantics engine for validation
:- use_module(incompatibility_semantics, [proves/1, set_domain/1, current_domain/1, is_recollection/2, normalize/2]).
:- use_module(library(random)).
:- use_module(library(lists)).

% Ensure operators are visible
:- op(1050, xfy, =>).
:- op(500, fx, neg).
:- op(550, xfy, rdiv).

%!      run_learned_strategy(?A, ?B, ?Result, ?StrategyName, ?Trace) is nondet.
%
%       A dynamic, multifile predicate that stores the collection of learned
%       strategies. Each clause of this predicate represents a single, efficient
%       strategy that the system has discovered and validated.
%
%       The `solve/4` predicate queries this predicate first, implementing a
%       hierarchy where learned, efficient strategies are preferred over
%       foundational, inefficient ones.
%
%       @param A The first input number.
%       @param B The second input number.
%       @param Result The result of the calculation.
%       @param StrategyName An atom identifying the learned strategy (e.g., `cob`, `rmb(10)`).
%       @param Trace A structured term representing the efficient execution path.
:- dynamic run_learned_strategy/5.

% =================================================================
% Part 0: Initialization and Persistence
% =================================================================

knowledge_file('learned_knowledge.pl').

% Load persistent knowledge when this module is loaded.
load_knowledge :-
    knowledge_file(File),
    (   exists_file(File)
    ->  consult(File),
        findall(_, clause(run_learned_strategy(_,_,_,_,_), _), Clauses),
        length(Clauses, Count),
        format('~N[Learner Init] Successfully loaded ~w learned strategies.~n', [Count])
    ;   format('~N[Learner Init] Knowledge file not found. Starting fresh.~n')
    ).

% Ensure initialization runs after the predicate is defined
:- initialization(load_knowledge, now).

%!      save_knowledge is det.
%
%       Saves all currently learned strategies (clauses of the dynamic
%       `run_learned_strategy/5` predicate) to the file specified by
%       `knowledge_file/1`. This allows for persistence of learning across sessions.
save_knowledge :-
    knowledge_file(File),
    setup_call_cleanup(
        open(File, write, Stream),
        (
            writeln(Stream, '% Automatically generated knowledge base.'),
            writeln(Stream, ':- op(550, xfy, rdiv).'),
            forall(clause(run_learned_strategy(A, B, R, S, T), Body),
                   portray_clause(Stream, (run_learned_strategy(A, B, R, S, T) :- Body)))
        ),
        close(Stream)
    ).

% =================================================================
% Part 1: The Unified Solver (Strategy Hierarchy)
% =================================================================

%!      solve(+A, +B, -Result, -Trace) is semidet.
%
%       Solves `A + B` using a strategy hierarchy.
%
%       It first attempts to use a highly efficient, learned strategy by
%       querying `run_learned_strategy/5`. If no applicable learned strategy
%       is found, it falls back to the foundational, inefficient counting
%       strategy (`solve_foundationally/4`).
%
%       @param A The first addend.
%       @param B The second addend.
%       @param Result The numerical result.
%       @param Trace The execution trace produced by the winning strategy.
solve(A, B, Result, Trace) :-
    (   run_learned_strategy(A, B, Result, _StrategyName, Trace)
    ->  true
    ;
        solve_foundationally(A, B, Result, Trace)
    ).

% =================================================================
% Part 2: Reflection and Learning
% =================================================================

%!      reflect_and_learn(+Result:dict) is semidet.
%
%       The core reflective learning trigger. It analyzes a computation's
%       result, which includes the goal and execution trace, to find
%       opportunities for creating more efficient strategies.
%
%       Now enhanced to analyze embodied modal states and cognitive patterns.
%
%       @param Result A dict containing at least `goal` and `trace`.
reflect_and_learn(Result) :-
    Goal = Result.goal,
    Trace = Result.trace,
    % We only learn from addition, and only if we have a trace.
    (   nonvar(Trace), Goal = add(A, B, _)
    ->  (   writeln('    (Reflecting on addition trace...)'),
            % Enhanced analysis: examine both syntactic and modal patterns
            (   detect_cob_pattern(Trace, _),
                construct_and_validate_cob(A, B)
            ;   detect_rmb_pattern(Trace, RMB_Data),
                construct_and_validate_rmb(A, B, RMB_Data)
            ;   detect_doubles_pattern(Trace, _),
                construct_and_validate_doubles(A, B)
            ;   detect_multiplicative_pattern(Trace, MultData),
                construct_multiplicative_strategy(A, B, MultData)
            ;   detect_modal_efficiency_pattern(Trace, ModalData),
                construct_modal_enhanced_strategy(A, B, ModalData)
            ;   true % Succeed even if no new strategy is found
            )
        )
    ;   true % Succeed if not an addition goal or no trace
    ).

% =================================================================
% Part 3: Foundational Abilities & Trace Analysis
% =================================================================

% --- 3.1 Foundational Ability: Counting ---

successor(X, Y) :- proves([] => [o(plus(X, 1, Y))]).

% solve_foundationally(+A, +B, -Result, -Trace)
%
% The most basic, "unfolded" strategy. It solves addition by counting on
% from A, B times. This is deliberately inefficient to provide rich traces
% for the reflective process to analyze.
solve_foundationally(A, B, Result, Trace) :-
    is_recollection(A, _), is_recollection(B, _),
    integer(A), integer(B), B >= 0,
    count_loop(A, B, Result, Steps),
    Trace = trace{a_start:A, b_start:B, strategy:counting, steps:Steps}.

count_loop(CurrentA, 0, CurrentA, []) :- !.
count_loop(CurrentA, CurrentB, Result, [step(CurrentA, NextA)|Steps]) :-
    CurrentB > 0,
    NextB is CurrentB - 1,
    successor(CurrentA, NextA),
    count_loop(NextA, NextB, Result, Steps).

% --- 3.2 Trace Analysis Helpers ---

count_trace_steps(Trace, Count) :-
    (   member(Trace.strategy, [counting, doubles, rmb(_)])
    ->  length(Trace.steps, Count)
    ;   Trace.strategy = cob
    ->
        ( member(inner_trace(InnerTrace), Trace.steps)
          -> count_trace_steps(InnerTrace, Count)
          ; Count = 0
        )
    ;   Count = 1
    ).

get_calculation_trace(T, T) :- member(T.strategy, [counting, rmb(_), doubles]).
get_calculation_trace(T, CT) :-
    T.strategy = cob,
    member(inner_trace(InnerT), T.steps),
    get_calculation_trace(InnerT, CT).

% =================================================================
% Part 4: Pattern Detection & Construction
% =================================================================

% Detects if an inefficient counting strategy was used where commutativity (A+B = B+A) would have been more efficient.
detect_cob_pattern(Trace, cob_data) :-
    Trace.strategy = counting,
    A = Trace.a_start, B = Trace.b_start,
    integer(A), integer(B),
    A < B.

% Constructs and validates a new "Counting On Bigger" (COB) strategy clause.
construct_and_validate_cob(A, B) :-
    StrategyName = cob,
    StrategyHead = run_learned_strategy(A_in, B_in, Result, StrategyName, Trace),
    StrategyBody = (
        integer(A_in), integer(B_in),
        (A_in >= B_in -> Start = A_in, Count = B_in, Swap = no_swap ; Start = B_in, Count = A_in, Swap = swapped(B_in, A_in)),
        (   Swap = swapped(_, _) ->
            (proves([n(plus(A_in, B_in, R_temp))] => [n(plus(B_in, A_in, R_temp))]) -> true ; fail)
            ; true
        ),
        solve_foundationally(Start, Count, Result, InnerTrace),
        Trace = trace{a_start:A_in, b_start:B_in, strategy:StrategyName, steps:[Swap, inner_trace(InnerTrace)]}
    ),
    validate_and_assert(A, B, StrategyHead, StrategyBody).


% Detects if the counting trace shows a pattern of "making a ten".
detect_rmb_pattern(TraceWrapper, rmb_data{k:K, base:Base}) :-
    get_calculation_trace(TraceWrapper, Trace),
    Trace.strategy = counting,
    Base = 10,
    A = Trace.a_start, B = Trace.b_start,
    integer(A), integer(B),
    A > 0, A < Base, K is Base - A, B >= K,
    nth1(K, Trace.steps, Step),
    Step = step(_, Base).

% Constructs and validates a new "Rearranging to Make Bases" (RMB) strategy.
construct_and_validate_rmb(A, B, RMB_Data) :-
    Base = RMB_Data.base,
    StrategyName = rmb(Base),
    StrategyHead = run_learned_strategy(A_in, B_in, Result, StrategyName, Trace),
    StrategyBody = (
        integer(A_in), integer(B_in),
        A_in > 0, A_in < Base, K_runtime is Base - A_in, B_in >= K_runtime,
        B_new_runtime is B_in - K_runtime,
        Result is Base + B_new_runtime,
        Trace = trace{a_start:A_in, b_start:B_in, strategy:StrategyName, steps:[step(A_in, Base), step(Base, Result)]}
    ),
    validate_and_assert(A, B, StrategyHead, StrategyBody).

% Detects if a problem was a "doubles" fact that was solved less efficiently.
detect_doubles_pattern(TraceWrapper, doubles_data) :-
    get_calculation_trace(TraceWrapper, Trace),
    member(Trace.strategy, [counting, rmb(_)]),
    A = Trace.a_start, B = Trace.b_start,
    A == B, integer(A).

% Constructs and validates a new "Doubles" strategy (rote knowledge).
construct_and_validate_doubles(A, B) :-
    StrategyName = doubles,
    StrategyHead = run_learned_strategy(A_in, B_in, Result, StrategyName, Trace),
    StrategyBody = (
        integer(A_in), A_in == B_in,
        Result is A_in * 2,
        Trace = trace{a_start:A_in, b_start:B_in, strategy:StrategyName, steps:[rote(Result)]}
    ),
    validate_and_assert(A, B, StrategyHead, StrategyBody).


% --- Validation Helper ---
% Ensures a newly constructed strategy is sound before asserting it.
validate_and_assert(A, B, StrategyHead, StrategyBody) :-
    copy_term((StrategyHead, StrategyBody), (ValidationHead, ValidationBody)),
    arg(1, ValidationHead, A),
    arg(2, ValidationHead, B),
    arg(3, ValidationHead, CalculatedResult),
    arg(4, ValidationHead, StrategyName),

    (   call(ValidationBody),
        proves([] => [o(plus(A, B, CalculatedResult))])
    ->
        (   clause(run_learned_strategy(_, _, _, StrategyName, _), _)
        ->  format('  (Strategy ~w already known)~n', [StrategyName])
        ;   assertz((StrategyHead :- StrategyBody)),
            format('  -> New Strategy Asserted: ~w~n', [StrategyName])
        )
    ;   writeln('ERROR: Strategy validation failed. Not asserted.')
    ).

% =================================================================
% Part 5: Embodied Modal Logic Pattern Detection
% =================================================================

%!      detect_modal_efficiency_pattern(+Trace, -ModalData) is semidet.
%
%       Detects patterns in embodied modal states that indicate cognitive
%       efficiency opportunities. Looks for correlations between modal
%       contexts and computational outcomes.
%
%       @param Trace The execution trace containing modal signals
%       @param ModalData Extracted modal pattern information
detect_modal_efficiency_pattern(Trace, modal_pattern(ModalSequence, EfficiencyGain)) :-
    extract_modal_sequence(Trace, ModalSequence),
    ModalSequence \= [],
    calculate_modal_efficiency_gain(ModalSequence, EfficiencyGain),
    EfficiencyGain > 0.

%!      extract_modal_sequence(+Trace, -ModalSequence) is det.
%
%       Extracts the sequence of modal contexts from an execution trace.
extract_modal_sequence([], []).
extract_modal_sequence([TraceElement|RestTrace], [Modal|RestModals]) :-
    is_modal_trace_element(TraceElement, Modal), !,
    extract_modal_sequence(RestTrace, RestModals).
extract_modal_sequence([_|RestTrace], RestModals) :-
    extract_modal_sequence(RestTrace, RestModals).

%!      is_modal_trace_element(+TraceElement, -Modal) is semidet.
%
%       Identifies modal context elements in trace entries.
is_modal_trace_element(modal_trace(ModalGoal, Context, _), modal_state(Context, ModalGoal)).
is_modal_trace_element(cognitive_cost(modal_shift, _), modal_transition).

%!      calculate_modal_efficiency_gain(+ModalSequence, -EfficiencyGain) is det.
%
%       Calculates the efficiency gain indicated by a modal sequence.
%       Compressive states should correlate with focused, efficient computation.
calculate_modal_efficiency_gain(ModalSequence, EfficiencyGain) :-
    count_compressive_focus(ModalSequence, CompressiveCount),
    count_expansive_exploration(ModalSequence, ExpansiveCount),
    % Efficiency gain when there's more compression (focus) than expansion
    EfficiencyGain is CompressiveCount - ExpansiveCount.

count_compressive_focus([], 0).
count_compressive_focus([modal_state(compressive, _)|Rest], Count) :-
    count_compressive_focus(Rest, RestCount),
    Count is RestCount + 1.
count_compressive_focus([_|Rest], Count) :-
    count_compressive_focus(Rest, Count).

count_expansive_exploration([], 0).
count_expansive_exploration([modal_state(expansive, _)|Rest], Count) :-
    count_expansive_exploration(Rest, RestCount),
    Count is RestCount + 1.
count_expansive_exploration([_|Rest], Count) :-
    count_expansive_exploration(Rest, Count).

%!      construct_modal_enhanced_strategy(+A, +B, +ModalData) is det.
%
%       Constructs a new strategy enhanced with modal context awareness.
%       This strategy would optimize based on the detected modal patterns.
construct_modal_enhanced_strategy(A, B, modal_pattern(ModalSequence, EfficiencyGain)) :-
    format('Constructing modal-enhanced strategy for ~w + ~w~n', [A, B]),
    format('  Modal sequence: ~w~n', [ModalSequence]),
    format('  Efficiency gain: ~w~n', [EfficiencyGain]),
    
    % Create a strategy name based on modal characteristics
    determine_modal_strategy_name(ModalSequence, StrategyName),
    
    % Construct the enhanced strategy clause
    construct_modal_strategy_clause(A, B, StrategyName, ModalSequence, Clause),
    
    % Validate and assert the new strategy
    ( validate_strategy_clause(Clause) ->
        assertz(Clause),
        format('Successfully created modal-enhanced strategy: ~w~n', [StrategyName])
    ;
        writeln('Modal strategy validation failed.')
    ).

%!      determine_modal_strategy_name(+ModalSequence, -StrategyName) is det.
%
%       Determines an appropriate strategy name based on modal characteristics.
determine_modal_strategy_name(ModalSequence, StrategyName) :-
    ( member(modal_state(compressive, _), ModalSequence) ->
        StrategyName = modal_focused_addition
    ; member(modal_state(expansive, _), ModalSequence) ->
        StrategyName = modal_exploratory_addition
    ;
        StrategyName = modal_neutral_addition
    ).

%!      construct_modal_strategy_clause(+A, +B, +StrategyName, +ModalSequence, -Clause) is det.
%
%       Constructs the actual Prolog clause for the modal-enhanced strategy.
construct_modal_strategy_clause(A, B, StrategyName, _ModalSequence, Clause) :-
    % For now, create a simple optimized clause
    % Future versions could use ModalSequence to customize the strategy body
    C is A + B,
    Clause = (run_learned_strategy(A, B, C, StrategyName, 
                                   [modal_optimization(StrategyName, A, B, C)]) :-
              integer(A), integer(B), A >= 0, B >= 0).

% =================================================================
% Part 6: True Bootstrapping - Multiplicative and Algebraic Pattern Detection
% =================================================================

%!      detect_multiplicative_pattern(+Trace, -MultData) is semidet.
%
%       Detects repeated addition patterns that indicate multiplication.
%       This enables qualitative leaps from arithmetic to multiplicative reasoning.
%
%       @param Trace The execution trace to analyze
%       @param MultData Information about the detected multiplicative pattern
detect_multiplicative_pattern(Trace, mult_pattern(Multiplicand, Multiplier, TotalOperations)) :-
    extract_addition_sequence(Trace, AdditionSequence),
    analyze_for_repeated_addition(AdditionSequence, Multiplicand, Multiplier, TotalOperations),
    TotalOperations >= 3.  % Require at least 3 repeated additions to detect pattern

%!      extract_addition_sequence(+Trace, -AdditionSequence) is det.
%
%       Extracts the sequence of addition operations from a trace.
extract_addition_sequence([], []).
extract_addition_sequence([TraceElement|RestTrace], [Addition|RestAdditions]) :-
    is_addition_trace_element(TraceElement, Addition), !,
    extract_addition_sequence(RestTrace, RestAdditions).
extract_addition_sequence([_|RestTrace], RestAdditions) :-
    extract_addition_sequence(RestTrace, RestAdditions).

%!      is_addition_trace_element(+TraceElement, -Addition) is semidet.
%
%       Identifies addition operations in trace elements.
is_addition_trace_element(arithmetic_trace(_, _, History), addition_ops(History)) :-
    is_list(History).
is_addition_trace_element(trace(add(A, B, C), _), direct_add(A, B, C)).

%!      analyze_for_repeated_addition(+AdditionSequence, -Multiplicand, -Multiplier, -Count) is semidet.
%
%       Analyzes addition sequence for repeated addition of the same value.
analyze_for_repeated_addition(AdditionSequence, Multiplicand, Multiplier, Count) :-
    find_repeated_addend(AdditionSequence, Multiplicand),
    count_repetitions(AdditionSequence, Multiplicand, Count),
    Multiplier = Count.

%!      find_repeated_addend(+AdditionSequence, -Addend) is semidet.
%
%       Finds an addend that appears repeatedly in the sequence.
find_repeated_addend([addition_ops(Ops)|_], Addend) :-
    member(step(_, A, B, _), Ops),
    (   Addend = A ; Addend = B ),
    integer(Addend),
    Addend > 1.

%!      count_repetitions(+AdditionSequence, +Addend, -Count) is det.
%
%       Counts how many times an addend appears in the sequence.
count_repetitions([], _, 0).
count_repetitions([addition_ops(Ops)|Rest], Addend, Count) :-
    count_addend_in_ops(Ops, Addend, OpsCount),
    count_repetitions(Rest, Addend, RestCount),
    Count is OpsCount + RestCount.

count_addend_in_ops([], _, 0).
count_addend_in_ops([step(_, A, B, _)|Rest], Addend, Count) :-
    ( (A == Addend ; B == Addend) ->
        count_addend_in_ops(Rest, Addend, RestCount),
        Count is RestCount + 1
    ;
        count_addend_in_ops(Rest, Addend, Count)
    ).

%!      construct_multiplicative_strategy(+A, +B, +MultData) is det.
%
%       Constructs a multiplication strategy from detected repeated addition pattern.
%       This represents true conceptual bootstrapping from addition to multiplication.
construct_multiplicative_strategy(A, B, mult_pattern(Multiplicand, Multiplier, _)) :-
    format('BOOTSTRAPPING: Detected multiplicative pattern!~n'),
    format('  ~w repeated additions of ~w detected~n', [Multiplier, Multiplicand]),
    format('  Synthesizing multiplication strategy...~n'),
    
    % Create new multiplication predicate if it doesn't exist
    ( \+ predicate_property(multiply_learned(_, _, _), defined) ->
        create_multiplication_predicate
    ; true
    ),
    
    % Create specific multiplication rule for this pattern
    construct_multiplication_rule(Multiplicand, Multiplier, Rule),
    assertz(Rule),
    format('  Successfully bootstrapped to multiplication!~n').

%!      create_multiplication_predicate is det.
%
%       Creates the basic multiplication predicate structure.
create_multiplication_predicate :-
    assertz((multiply_learned(0, _, 0) :-
        writeln('Multiplication by zero yields zero.'))),
    assertz((multiply_learned(A, B, Result) :-
        A > 0, B > 0,
        A1 is A - 1,
        multiply_learned(A1, B, PartialResult),
        Result is PartialResult + B)),
    writeln('Created fundamental multiplication predicate structure.').

%!      construct_multiplication_rule(+Multiplicand, +Multiplier, -Rule) is det.
%
%       Constructs a specific multiplication rule from the detected pattern.
construct_multiplication_rule(Multiplicand, Multiplier, Rule) :-
    Product is Multiplicand * Multiplier,
    Rule = (run_learned_strategy(Multiplicand, Multiplier, Product, 
                                discovered_multiplication,
                                [bootstrapped_from_addition(Multiplicand, Multiplier)]) :-
            integer(Multiplicand), integer(Multiplier),
            Multiplicand > 0, Multiplier > 0).

%!      detect_algebraic_pattern(+Trace, -AlgebraicData) is semidet.
%
%       Detects when arithmetic strategies can be abstracted to symbolic manipulation.
%       This enables bootstrapping to algebraic reasoning.
detect_algebraic_pattern(Trace, algebraic_pattern(AbstractForm, Instances)) :-
    extract_operation_patterns(Trace, Patterns),
    find_algebraic_abstraction(Patterns, AbstractForm, Instances),
    length(Instances, InstanceCount),
    InstanceCount >= 2.  % Need multiple instances to abstract

%!      extract_operation_patterns(+Trace, -Patterns) is det.
%
%       Extracts operational patterns that could be algebraically abstracted.
extract_operation_patterns(Trace, Patterns) :-
    findall(Pattern, 
            (member(TraceElement, Trace),
             extract_operation_pattern(TraceElement, Pattern)),
            Patterns).

extract_operation_pattern(trace(add(A, B, C), _), add_pattern(A, B, C)).
extract_operation_pattern(arithmetic_trace(Strategy, Result, _), strategy_pattern(Strategy, Result)).

%!      find_algebraic_abstraction(+Patterns, -AbstractForm, -Instances) is semidet.
%
%       Finds common algebraic structures in operation patterns.
find_algebraic_abstraction(Patterns, commutative_property, Instances) :-
    findall(add_pattern(A, B, C), 
            (member(add_pattern(A, B, C), Patterns),
             member(add_pattern(B, A, C), Patterns)),
            Instances),
    Instances \= [].

% =================================================================
% Part 6: Normative Critique (Placeholder)
% =================================================================

%!      critique_and_bootstrap(+Goal:term) is det.
%
%       Placeholder for a future capability where the system can analyze
%       a given normative rule (e.g., a subtraction problem that challenges
%       its current knowledge) and potentially learn from it.
%
%       @param Goal The goal representing the normative rule to critique.
critique_and_bootstrap(_) :- writeln('Normative Critique Placeholder.').