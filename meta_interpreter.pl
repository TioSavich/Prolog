/** <module> Embodied Tracing Meta-Interpreter
 *
 * This module provides the core "Observe" capability of the ORR cycle.
 * It contains a stateful meta-interpreter, `solve/4`, which executes goals
 * defined in the `object_level` module.
 *
 * This version is "embodied": it maintains a `ModalContext` (e.g., neutral,
 * compressive, expansive) that alters its reasoning behavior. For example,
 * in a `compressive` context, the cost of inferences increases, simulating
 * cognitive tension and narrowing the search. This context is switched when
 * the interpreter encounters modal operators defined in `incompatibility_semantics`.
 *
 * It produces a detailed `Trace` of the execution, which is the primary
 * data source for the `reflective_monitor`.
 *
 * 
 * 
 */
:- module(meta_interpreter, [solve/4]).
:- use_module(object_level). % Ensure we can access the object-level code
:- use_module(hermeneutic_calculator). % For strategic choice
:- use_module(incompatibility_semantics, [s/1, 'comp_nec'/1, 'comp_poss'/1, 'exp_nec'/1, 'exp_poss'/1, check_norms/1]). % For modal operators and norm checking
:- use_module(grounded_arithmetic). % For cognitive cost tracking
:- use_module(config). % For cognitive cost lookup

% Note: is_list/1 is a built-in, no need to import from library(lists).

% --- Embodied Cognition Helpers ---

%!      is_modal_operator(?Goal, ?ModalContext) is semidet.
%
%       Identifies an embodied modal operator and maps it to a context.
is_modal_operator(comp_nec(_), compressive).
is_modal_operator(comp_poss(_), compressive).
is_modal_operator(exp_nec(_), expansive).
is_modal_operator(exp_poss(_), expansive).

%!      get_inference_cost(+ModalContext, -Cost) is det.
%
%       Determines the inference cost based on the current modal context.
%       - `compressive`: Cost is 2 (cognitive narrowing).
%       - `neutral`, `expansive`: Cost is 1.
get_inference_cost(compressive, 2).
get_inference_cost(expansive, 1).
get_inference_cost(neutral, 1).


% --- Arithmetic Goal Handling ---

%!      is_arithmetic_goal(?Goal, ?Op) is semidet.
%
%       Identifies arithmetic goals and maps them to standard operators.
%       This allows the meta-interpreter to intercept these goals and
%       handle them with the Hermeneutic Calculator instead of the
%       inefficient object-level definitions.
is_arithmetic_goal(add(_,_,_), +).
is_arithmetic_goal(multiply(_,_,_), *).
% Add other operations like subtract/3, divide/3 here if needed.


%!      peano_to_int(?Peano, ?Int) is det.
%
%       Converts a Peano number (s(s(0))) to an integer.
peano_to_int(0, 0).
peano_to_int(s(P), I) :-
    peano_to_int(P, I_prev),
    I is I_prev + 1.

%!      int_to_peano(?Int, ?Peano) is det.
%
%       Converts an integer to a Peano number.
int_to_peano(0, 0).
int_to_peano(I, s(P)) :-
    I > 0,
    I_prev is I - 1,
    int_to_peano(I_prev, P).


%!      solve(+Goal, +InferencesIn, -InferencesOut, -Trace) is nondet.
%
%       Public wrapper for the stateful meta-interpreter.
%       Initializes the `ModalContext` to `neutral` and calls the
%       internal `solve/6` predicate.
solve(Goal, I_In, I_Out, Trace) :-
    solve(Goal, neutral, _, I_In, I_Out, Trace).


%!      solve(+Goal, +CtxIn, -CtxOut, +I_In, -I_Out, -Trace) is nondet.
%
%       The core stateful, embodied meta-interpreter.
%
%       @param Goal The goal to be solved.
%       @param CtxIn The current `ModalContext`.
%       @param CtxOut The `ModalContext` after the goal is solved.
%       @param I_In The initial number of available inference steps.
%       @param I_Out The remaining number of inference steps.
%       @param Trace A list representing the execution trace.
%       @error perturbation(resource_exhaustion) if inference counter drops to zero.

% Base case: `true` always succeeds. Context is unchanged.
solve(true, Ctx, Ctx, I, I, []) :- !.

% Cognitive Cost Tracking: Intercept cost signals for embodied learning
solve(incur_cost(Action), Ctx, Ctx, I_In, I_Out, [cognitive_cost(Action, Cost)]) :-
    !,
    ( config:cognitive_cost(Action, Cost) -> true ; Cost = 0 ),
    check_viability(I_In, Cost),
    I_Out is I_In - Cost.

% Modal Operator: Detect a modal operator, switch context for the sub-proof,
% and restore it upon completion. Enhanced to capture detailed modal information.
solve(s(ModalGoal), CtxIn, CtxIn, I_In, I_Out, [modal_trace(ModalGoal, Ctx, SubTrace, ModalInfo)]) :-
    is_modal_operator(ModalGoal, Ctx),
    !,
    ModalGoal =.. [_, InnerGoal],
    % Record modal transition information
    ModalInfo = modal_info(
        transition(CtxIn, Ctx),
        cost_impact(CtxIn, Ctx),
        goal(InnerGoal)
    ),
    % The context is switched for the InnerGoal, but restored to CtxIn afterward.
    solve(InnerGoal, Ctx, _, I_In, I_Out, SubTrace).

% Conjunction: Solve `A` then `B`. The context flows from `A` to `B`.
solve((A, B), CtxIn, CtxOut, I_In, I_Out, [trace(A, A_Trace), trace(B, B_Trace)]) :-
    !,
    solve(A, CtxIn, CtxMid, I_In, I_Mid, A_Trace),
    solve(B, CtxMid, CtxOut, I_Mid, I_Out, B_Trace).

% System predicates: Use context-dependent cost. Context is unchanged.
solve(Goal, Ctx, Ctx, I_In, I_Out, [call(Goal)]) :-
    predicate_property(Goal, built_in),
    !,
    get_inference_cost(Ctx, Cost),
    check_viability(I_In, Cost),
    I_Out is I_In - Cost,
    call(Goal).

% DISABLED: Arithmetic handler (forces arithmetic through object-level predicates for crisis testing)
% solve(Goal, Ctx, Ctx, I_In, I_Out, [arithmetic_trace(Strategy, Result, History)]) :-
%     is_arithmetic_goal(Goal, Op),
%     !,
%     get_inference_cost(Ctx, Cost),
%     check_viability(I_In, Cost),
%     I_Out is I_In - Cost,
%     Goal =.. [_, Peano1, Peano2, PeanoResult],
%     peano_to_int(Peano1, N1),
%     peano_to_int(Peano2, N2),
%     list_strategies(Op, Strategies),
%     ( is_list(Strategies), Strategies = [Strategy|_] -> true ; throw(error(no_strategy_found(Op), _)) ),
%     calculate(N1, Op, N2, Strategy, Result, History),
%     int_to_peano(Result, PeanoResult).

% Object-level predicates: Use context-dependent cost. Context flows through sub-proof.
solve(Goal, CtxIn, CtxOut, I_In, I_Out, [clause(object_level:(Goal:-Body)), trace(Body, BodyTrace)]) :-
    % NORMATIVE CHECKING: Validate goal against current mathematical context
    catch(check_norms(Goal), normative_crisis(CrisisGoal, Context), 
          throw(perturbation(normative_crisis(CrisisGoal, Context)))),
    
    get_inference_cost(CtxIn, Cost),
    check_viability(I_In, Cost),
    I_Mid is I_In - Cost,
    clause(object_level:Goal, Body),
    solve(Body, CtxIn, CtxOut, I_Mid, I_Out, BodyTrace).

% Failure case: If a goal is not a built-in and has no matching clauses,
% record the failure. Context is unchanged.
solve(Goal, Ctx, Ctx, I, I, [fail(Goal)]) :-
    \+ predicate_property(Goal, built_in),
    \+ (Goal = s(_), functor(Goal, s, 1)), % Don't fail on modal operators here
    \+ clause(object_level:Goal, _), !.


% --- Viability Check ---

% check_viability(+Inferences, +Cost)
%
% Succeeds if the inference counter is sufficient for the next step's cost.
check_viability(I, Cost) :- I >= Cost, !.
check_viability(_, _) :-
    % Constraint violated: PERTURBATION DETECTED
    throw(perturbation(resource_exhaustion)).