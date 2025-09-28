% Filename: neuro_symbolic_bridge.pl (The Neuro-Symbolic Bridge V4)
:- module(neuro_symbolic_bridge,
          [ explore_calculation/1,
            solve/4,
            suggest_strategy/3, % Export for the prover hook
            learn_euclid_strategy/0 % Export for triggering simulated learning
          ]).

% Use the semantics engine
% Import product_of_list/2, needed for defining the Euclid construction strategy.
:- use_module(incompatibility_semantics, [proves/1, set_domain/1, current_domain/1, obj_coll/1, normalize/2, product_of_list/2]).
:- use_module(library(random)).
:- use_module(library(lists)).

% Ensure operators are visible
:- op(1050, xfy, =>).
:- op(500, fx, neg).
:- op(550, xfy, rdiv).

% Dynamic predicates for learned strategies.
:- dynamic run_learned_strategy/5. % Calculation strategies
:- dynamic learned_proof_strategy/2. % Proof strategies (The "Intuition" Database)

% =================================================================
% Part 0: Initialization and Persistence
% =================================================================

knowledge_file('learned_knowledge_v2.pl').
:- initialization(load_knowledge, now).

load_knowledge :-
    knowledge_file(File),
    (   exists_file(File)
    ->  consult(File),
        format('~N[Bridge Init] Loaded persistent knowledge.~n')
    ;   format('~N[Bridge Init] Knowledge file not found. Starting fresh.~n')
    ).

save_knowledge :-
    knowledge_file(File),
    setup_call_cleanup(
        open(File, write, Stream),
        (
            writeln(Stream, '% Automatically generated knowledge base V2.'),
            writeln(Stream, ':- op(550, xfy, rdiv).'),
            % Save Calculation Strategies
            forall(clause(run_learned_strategy(A, B, R, S, T), Body),
                   portray_clause(Stream, (run_learned_strategy(A, B, R, S, T) :- Body))),
            % Save Proof Strategies
            forall(clause(learned_proof_strategy(GoalPattern, Strategy), Body),
                   portray_clause(Stream, (learned_proof_strategy(GoalPattern, Strategy) :- Body)))
        ),
        close(Stream)
    ).

% =================================================================
% Part 1-4: Calculation Learning (Retained from more_machine_learner.pl)
% =================================================================
% This section retains the functionality for optimizing arithmetic operations.

explore_calculation(addition) :-
    writeln('====================================================='),
    writeln('--- Autonomous Exploration Initiated: Addition ---'),
    current_domain(D),
    (member(D, [n, z, q]) -> explore_addition_loop(50) ; writeln('Requires domain (n, z, or q).')).

explore_addition_loop(0) :- save_knowledge, writeln('====================================================='), !.
explore_addition_loop(I) :-
    generate_addition_problem(A, B),
    format('\n[Cycle ~w] Exploring Problem: ~w + ~w~n', [I, A, B]),
    (discover_strategy(A, B, _) ; true),
    NextI is I - 1,
    explore_addition_loop(NextI).

generate_addition_problem(A, B) :-
    random_between(3, 12, A),
    (   random(R), R < 0.3 -> B = A ; random_between(3, 15, B)).

% --- Solver Hierarchy ---
solve(A, B, Result, Trace) :-
    (   run_learned_strategy(A, B, Result, _StrategyName, Trace) -> true
    ;   solve_foundationally(A, B, Result, Trace)).

% --- Strategy Discovery (Calculation) ---
discover_strategy(A, B, StrategyName) :-
    solve(A, B, Result, Trace),
    count_trace_steps(Trace, TraceLength),
    format('  Solution found via [~w]: ~w. Steps: ~w~n', [Trace.strategy, Result, TraceLength]),
    (   detect_cob_pattern(Trace, _), StrategyName = cob, construct_and_validate_cob(A, B)
    ;   detect_rmb_pattern(Trace, RMB_Data), StrategyName = rmb, construct_and_validate_rmb(A, B, RMB_Data)
    ;   detect_doubles_pattern(Trace, _), StrategyName = doubles, construct_and_validate_doubles(A, B)).

% --- Foundational Ability: Counting ---
successor(X, Y) :- proves([] => [o(plus(X, 1, Y))]).

solve_foundationally(A, B, Result, Trace) :-
    obj_coll(A), obj_coll(B), integer(A), integer(B), B >= 0,
    count_loop(A, B, Result, Steps),
    Trace = trace{a_start:A, b_start:B, strategy:counting, steps:Steps}.

count_loop(CurrentA, 0, CurrentA, []) :- !.
count_loop(CurrentA, CurrentB, Result, [step(CurrentA, NextA)|Steps]) :-
    CurrentB > 0, NextB is CurrentB - 1, successor(CurrentA, NextA),
    count_loop(NextA, CurrentB, Result, Steps).

% (Trace Analysis Helpers)
count_trace_steps(Trace, Count) :-
    (   is_dict(Trace) ->
        (   member(Trace.strategy, [counting, doubles, rmb(_)]) -> length(Trace.steps, Count)
        ;   Trace.strategy = cob -> ( member(inner_trace(InnerTrace), Trace.steps) -> count_trace_steps(InnerTrace, Count) ; Count = 0)
        ;   Count = 1)
    ; Count = 0).

get_calculation_trace(T, T) :- is_dict(T), member(T.strategy, [counting, rmb(_), doubles]).
get_calculation_trace(T, CT) :- is_dict(T), T.strategy = cob, member(inner_trace(InnerT), T.steps), get_calculation_trace(InnerT, CT).

% (Pattern Detection & Construction: COB, RMB, Doubles)
% PATTERN 1: Counting On Bigger (COB)
detect_cob_pattern(Trace, cob_data) :- is_dict(Trace), Trace.strategy = counting, A = Trace.a_start, B = Trace.b_start, integer(A), integer(B), A < B.

construct_and_validate_cob(A, B) :-
    StrategyName = cob,
    StrategyHead = run_learned_strategy(A_in, B_in, Result, StrategyName, Trace),
    StrategyBody = (
        integer(A_in), integer(B_in),
        (A_in >= B_in -> Start = A_in, Count = B_in, Swap = no_swap ; Start = B_in, Count = A_in, Swap = swapped(B_in, A_in)),
        (   Swap = swapped(_, _) -> (proves([n(plus(A_in, B_in, R_temp))] => [n(plus(B_in, A_in, R_temp))]) -> true ; fail) ; true),
        solve_foundationally(Start, Count, Result, InnerTrace),
        Trace = trace{a_start:A_in, b_start:B_in, strategy:StrategyName, steps:[Swap, inner_trace(InnerTrace)]}
    ),
    validate_and_assert(A, B, StrategyHead, StrategyBody).

% PATTERN 2: Rearranging to Make Bases (RMB)
detect_rmb_pattern(TraceWrapper, rmb_data{k:K, base:Base}) :-
    get_calculation_trace(TraceWrapper, Trace), Trace.strategy = counting, Base = 10,
    A = Trace.a_start, B = Trace.b_start, integer(A), integer(B),
    A > 0, A < Base, K is Base - A, B >= K, nth1(K, Trace.steps, Step), Step = step(_, Base).

construct_and_validate_rmb(A, B, RMB_Data) :-
    Base = RMB_Data.base, StrategyName = rmb(Base),
    StrategyHead = run_learned_strategy(A_in, B_in, Result, StrategyName, Trace),
    StrategyBody = (
        integer(A_in), integer(B_in), A_in > 0, A_in < Base, K_runtime is Base - A_in, B_in >= K_runtime,
        B_new_runtime is B_in - K_runtime, Result is Base + B_new_runtime,
        Trace = trace{a_start:A_in, b_start:B_in, strategy:StrategyName, steps:[step(A_in, Base), step(Base, Result)]}
    ),
    validate_and_assert(A, B, StrategyHead, StrategyBody).

% PATTERN 3: Doubles
detect_doubles_pattern(TraceWrapper, doubles_data) :-
    get_calculation_trace(TraceWrapper, Trace), member(Trace.strategy, [counting, rmb(_)]),
    A = Trace.a_start, B = Trace.b_start, A == B, integer(A).

construct_and_validate_doubles(A, B) :-
    StrategyName = doubles,
    StrategyHead = run_learned_strategy(A_in, B_in, Result, StrategyName, Trace),
    StrategyBody = (
        integer(A_in), A_in == B_in, Result is A_in * 2,
        Trace = trace{a_start:A_in, b_start:B_in, strategy:StrategyName, steps:[rote(Result)]}
    ),
    validate_and_assert(A, B, StrategyHead, StrategyBody).

% Validation Helper
validate_and_assert(A, B, StrategyHead, StrategyBody) :-
    copy_term((StrategyHead, StrategyBody), (ValidationHead, ValidationBody)),
    arg(1, ValidationHead, A), arg(2, ValidationHead, B), arg(3, ValidationHead, CalculatedResult), arg(4, ValidationHead, StrategyName),
    (   call(ValidationBody), proves([] => [o(plus(A, B, CalculatedResult))])
    ->  (   clause(run_learned_strategy(_, _, _, StrategyName, _), _) -> format('  (Strategy ~w already known)~n', [StrategyName])
        ;   assertz((StrategyHead :- StrategyBody)), format('  -> New Strategy Asserted: ~w~n', [StrategyName]))
    ;   writeln('ERROR: Strategy validation failed. Not asserted.')).


% =================================================================
% Part 5: Neuro-Symbolic Proof Strategy Integration (The "Muse")
% =================================================================

% suggest_strategy(+Premises, +Conclusions, -Strategy)
% This is the hook called by the prover when it is stuck (PRIORITY 5).
suggest_strategy(Premises, Conclusions, Strategy) :-
    % 1. Identify the Goal Pattern (Optional, useful for goal-directed strategies)
    (   Conclusions = [] -> Goal = incoherent(Premises)
    ;   member(C, Conclusions), Goal = proves(Premises => [C])
    ),

    % 2. Consult Learned Strategies (The "Intuition Database")
    % Use findall and then select to allow backtracking through different suggestions if the first fails.
    findall(S, consult_learned_proof_strategies(Premises, Goal, S), Strategies),
    member(Strategy, Strategies).

% consult_learned_proof_strategies(+Premises, +Goal, -Strategy)
consult_learned_proof_strategies(Premises, _Goal, Strategy) :-
    % Iterate through learned strategies. The associated Body is executed here by clause/2 and call/1.
    clause(learned_proof_strategy(GoalPattern, StrategyTemplate), Body),

    % Check if the current premises match the required context for the strategy.
    % This binds variables in GoalPattern (like L) to the actual values in the proof state.
    match_context(GoalPattern.context, Premises),
    
    % Execute the body (e.g., to calculate constructions like N=P+1).
    % This binds variables used in the calculation (like N).
    call(Body),
    
    % Instantiate the strategy template with the bound variables.
    instantiate_strategy(StrategyTemplate, GoalPattern.vars, Strategy).

% Helper to check context and bind variables
match_context([], _).
match_context([P|Ps], Premises) :-
    % Use member/2 for unification, binding variables in P (like L in n(is_complete(L)))
    member(P, Premises),
    match_context(Ps, Premises).

% Helper to instantiate the strategy
instantiate_strategy(Template, Vars, Strategy) :-
    % Ensures variables bound during match_context and the body execution are propagated.
    copy_term((Template, Vars), (Strategy, _)).

% =================================================================
% Part 6: The Learning/Reflection Process (The "Critique")
% =================================================================

% This section simulates the "neural" process of analyzing a domain and discovering a strategy.

learn_euclid_strategy :-
    writeln('\n--- Neuro-Symbolic Reflection Initiated: Euclid Domain (The "Muse") ---'),
    % 1. Analyze the Domain (Simulated Intuition)
    % The "Muse" recognizes that to disprove completeness, one needs a construction and subsequent analysis.

    % 2. Formulate the Strategy

    % Strategy 1: Euclid Construction
    % "When assuming is_complete(L), construct the Euclid number N."
    Pattern1 = goal{
        context: [n(is_complete(L))],
        vars: [L, N] % Variables involved (L and N are unbound here)
    },
    % Action: Introduce the constructed number concept
    StrategyTemplate1 = introduce(n(euclid_number(N, L))), 
    % Preconditions/Calculations: How to instantiate N based on L.
    Body1 = (
        % We must qualify the call as product_of_list resides in the other module.
        incompatibility_semantics:product_of_list(L, P),
        N is P + 1,
        N > 1 % Prerequisite for prime analysis
    ),
    assert_proof_strategy(Pattern1, StrategyTemplate1, Body1, 'euclid_construction'),

    % Strategy 2: Case Analysis
    % "When analyzing a constructed Euclid number N, consider if it is prime or composite."
    Pattern2 = goal{
        context: [n(euclid_number(N, L))],
        vars: [N, L]
    },
    StrategyTemplate2 = case_split(n(prime(N)), n(composite(N))),
    Body2 = true, % Conditions (N>1) are checked in the construction phase

    assert_proof_strategy(Pattern2, StrategyTemplate2, Body2, 'euclid_case_analysis'),

    save_knowledge,
    writeln('--- Reflection Complete. Knowledge base updated. ---').

% Helper to assert a new proof strategy if not already known
assert_proof_strategy(GoalPattern, StrategyTemplate, Body, Name) :-
    % We assert the strategy with its body, so the body is executed when the strategy is consulted.
    (   clause(learned_proof_strategy(GP, ST), B),
        % Check if a strategy with the same structure already exists (variant check)
        variant((GP, ST, B), (GoalPattern, StrategyTemplate, Body))
    ->  format('  (Proof strategy ~w already known)~n', [Name])
    ;   % Assert the clause: (learned_proof_strategy(GoalPattern, StrategyTemplate) :- Body).
        assertz((learned_proof_strategy(GoalPattern, StrategyTemplate) :- Body)),
        format('  -> New Proof Strategy Asserted: ~w~n', [Name])
    ).