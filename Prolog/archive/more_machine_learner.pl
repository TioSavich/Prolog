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
%       DEPRECATED: Legacy reflective learning trigger.
%       
%       Phase 5 Refactoring: This predicate is now a NO-OP placeholder.
%       All learning happens through FSM synthesis in fsm_synthesis_engine.pl,
%       which is called directly from execution_handler.pl during crisis.
%       
%       This predicate is kept for backward compatibility but does nothing.
%       It may be removed entirely in future versions.
%
%       @param Result A dict containing at least `goal` and `trace`.
reflect_and_learn(_Result) :-
    % NO-OP: All learning now happens via fsm_synthesis_engine
    true.

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
% Part 4: DEPRECATED - Pattern Detection & Construction REMOVED
% =================================================================
%
% Phase 5.1 Refactoring: All pattern-based strategy construction has been
% removed to enforce the emergence principle. The system no longer has
% "innate" knowledge of strategies like COB, RMB, or Doubles.
%
% Learning now occurs exclusively through FSM synthesis in
% fsm_synthesis_engine.pl, which compositionally builds strategies from
% grounded primitives (successor, predecessor, decompose_base10) guided
% by oracle interpretations.
%
% This is computational hermeneutics: the machine reconstructs rational
% structures that make oracle guidance intelligible, rather than matching
% pre-defined templates.
%
% Legacy predicates REMOVED:
% - detect_cob_pattern/2
% - construct_and_validate_cob/2
% - detect_rmb_pattern/2
% - construct_and_validate_rmb/3
% - detect_doubles_pattern/2
% - construct_and_validate_doubles/2
% - validate_and_assert/4
%
% These predicates violated the principle that everything must be learned,
% not given. FSM synthesis replaces all template-based construction.

% =================================================================
% Part 5 & 6: DEPRECATED - Advanced Pattern Detection REMOVED
% =================================================================
%
% Phase 5.1 Refactoring: All advanced pattern detection removed.
%
% These predicates represented hypothetical future capabilities for:
% - Modal efficiency pattern detection
% - Multiplicative pattern bootstrapping  
% - Algebraic abstraction recognition
%
% However, they still violated the emergence principle by providing
% hard-coded pattern templates. If these capabilities are needed in
% the future, they should be implemented through FSM synthesis, not
% pattern matching.
%
% Legacy predicates REMOVED:
% - detect_modal_efficiency_pattern/2
% - construct_modal_enhanced_strategy/3
% - detect_multiplicative_pattern/2
% - construct_multiplicative_strategy/3
% - detect_algebraic_pattern/2
%
% =================================================================
% Part 7: Normative Critique (Placeholder)
% =================================================================

%!      critique_and_bootstrap(+Goal:term) is det.
%
%       Placeholder for a future capability where the system can analyze
%       a given normative rule (e.g., a subtraction problem that challenges
%       its current knowledge) and potentially learn from it.
%
%       @param Goal The goal representing the normative rule to critique.
critique_and_bootstrap(_) :- writeln('Normative Critique Placeholder.').