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
 * @author Tilo Wiedera
 * @license MIT
 */
:- module(more_machine_learner,
          [ critique_and_bootstrap/1,
            explore/1,
            run_learned_strategy/5,
            solve/4,
            save_knowledge/0
          ]).

% Use the semantics engine for validation
:- use_module(incompatibility_semantics, [proves/1, set_domain/1, current_domain/1, obj_coll/1, normalize/2]).
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
% Part 1: The Generative-Reflective Loop (Exploration)
% =================================================================

%!      explore(+Domain:atom) is det.
%
%       Initiates a session of autonomous exploration and learning for a
%       given domain.
%
%       It repeatedly generates random problems, solves them using the current
%       best strategy, and then reflects on the solution trace to discover
%       and assert new, more efficient strategies.
%
%       @param Domain The domain to explore (currently only `addition` is supported).
explore(addition) :-
    writeln('====================================================='),
    writeln('--- Autonomous Exploration Initiated: Addition (Protein Folding) ---'),
    current_domain(D),
    format('Current Semantic Domain: ~w~n', [D]),
    (member(D, [n, z, q]) ->
        explore_addition_loop(50)
    ;
        writeln('Exploration requires domain (n, z, or q).')
    ).

% explore_addition_loop(+N)
% The main loop for the exploration process.
explore_addition_loop(0) :-
    writeln('\nExploration limit reached. Saving knowledge base...'),
    save_knowledge,
    writeln('Knowledge base saved.'),
    writeln('====================================================='), !.
explore_addition_loop(I) :-
    generate_addition_problem(A, B),
    normalize(A, AN), normalize(B, BN),
    format('\n[Cycle ~w] Exploring Problem: ~w + ~w~n', [I, AN, BN]),
    (   discover_strategy(A, B, StrategyName)
    ->  format('-> Strategy Discovery Processed: ~w~n', [StrategyName])
    ;   true
    ),
    NextI is I - 1,
    explore_addition_loop(NextI).

% Problem Generation (Heuristic)
generate_addition_problem(A, B) :-
    random_between(3, 12, A),
    (   random(R), R < 0.3 % 30% chance
    ->  B = A
    ;   random_between(3, 15, B)
    ).

% =================================================================
% Part 2: The Unified Solver (Strategy Hierarchy)
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
% Part 3: Strategy Discovery (The Core Learner)
% =================================================================

% discover_strategy(+A, +B, -StrategyName)
%
% The core of the learning process. It solves a problem, then analyzes the
% trace for patterns that suggest a more efficient strategy could be created.
discover_strategy(A, B, StrategyName) :-
    solve(A, B, Result, Trace),
    count_trace_steps(Trace, TraceLength),
    format('  Solution found via [~w]: ~w. Steps: ~w~n', [Trace.strategy, Result, TraceLength]),
    (   detect_cob_pattern(Trace, _), StrategyName = cob,
        construct_and_validate_cob(A, B)
    ;   detect_rmb_pattern(Trace, RMB_Data), StrategyName = rmb,
        construct_and_validate_rmb(A, B, RMB_Data)
    ;   detect_doubles_pattern(Trace, _), StrategyName = doubles,
        construct_and_validate_doubles(A, B)
    ;   fail
    ).

% --- 3.1 Foundational Ability: Counting ---

successor(X, Y) :- proves([] => [o(plus(X, 1, Y))]).

% solve_foundationally(+A, +B, -Result, -Trace)
%
% The most basic, "unfolded" strategy. It solves addition by counting on
% from A, B times. This is deliberately inefficient to provide rich traces
% for the reflective process to analyze.
solve_foundationally(A, B, Result, Trace) :-
    obj_coll(A), obj_coll(B),
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
% Part 5: Normative Critique (Placeholder)
% =================================================================

%!      critique_and_bootstrap(+Goal:term) is det.
%
%       Placeholder for a future capability where the system can analyze
%       a given normative rule (e.g., a subtraction problem that challenges
%       its current knowledge) and potentially learn from it.
%
%       @param Goal The goal representing the normative rule to critique.
critique_and_bootstrap(_) :- writeln('Normative Critique Placeholder.').