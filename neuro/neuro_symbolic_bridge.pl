% Filename: neuro_symbolic_bridge.pl (The Neuro-Symbolic Bridge V4)
:- module(neuro_symbolic_bridge,
          [ suggest_strategy/3, % Export for the prover hook
            learn_euclid_strategy/0 % Export for triggering simulated learning
          ]).

% Use the semantics engine
% Import product_of_list/2, needed for defining the Euclid construction strategy.
:- use_module('../incompatibility_semantics.pl', [proves/1, set_domain/1, current_domain/1, is_recollection/2, normalize/2, product_of_list/2]).
:- use_module(library(random)).
:- use_module(library(lists)).

% Ensure operators are visible
:- op(1050, xfy, =>).
:- op(500, fx, neg).
:- op(550, xfy, rdiv).

% Dynamic predicates for learned strategies.
:- dynamic learned_proof_strategy/2. % Proof strategies (The "Intuition" Database)

% =================================================================
% Part 0: Initialization and Persistence
% =================================================================

knowledge_file('learned_knowledge_v2.pl').
%:- initialization(load_knowledge, now).

load_knowledge :-
    knowledge_file(File),
    (   exists_file(File)
    ->  consult(File),
        format('~N[Bridge Init] Loaded persistent knowledge.~n')
    ;   format('~N[Bridge Init] Knowledge file not found. Starting fresh.~n')
    ).

% Ensure initialization runs after the predicate is defined
:- initialization(load_knowledge, now).

save_knowledge :-
    knowledge_file(File),
    setup_call_cleanup(
        open(File, write, Stream),
        (
            writeln(Stream, '% Automatically generated knowledge base V2.'),
            writeln(Stream, ':- op(550, xfy, rdiv).'),
            % Save Proof Strategies
            forall(clause(learned_proof_strategy(GoalPattern, Strategy), Body),
                   portray_clause(Stream, (learned_proof_strategy(GoalPattern, Strategy) :- Body)))
        ),
        close(Stream)
    ).


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