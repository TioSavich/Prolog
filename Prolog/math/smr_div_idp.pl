/** <module> Student Division Strategy: Inverse of Distributive Property (IDP)
 *
 * This module implements a division strategy based on the inverse of the
 * distributive property, modeled as a finite state machine. It solves a
 * division problem (T / S) by using a knowledge base (KB) of known
 * multiplication facts for the divisor S.
 *
 * The process is as follows:
 * 1.  Given a knowledge base of facts for S (e.g., 2*S, 5*S, 10*S), find the
 *     largest known multiple of S that is less than or equal to the
 *     remaining total (T).
 * 2.  Subtract this multiple from T.
 * 3.  Add the corresponding factor to a running total for the quotient.
 * 4.  Repeat the process with the new, smaller remainder until no more known
 *     multiples can be subtracted.
 * 5.  The final quotient is the sum of the factors, and the final remainder
 *     is what's left of the total.
 * 6.  The strategy fails if the divisor (S) is not positive.
 *
 * The state is represented by the term:
 * `state(Name, Remaining, TotalQuotient, PartialTotal, PartialQuotient, KB, Divisor)`
 *
 * The history of execution is captured as a list of steps:
 * `step(Name, Remainder, TotalQuotient, PartialTotal, PartialQuotient, Interpretation)`
 *
 * 
 * 
 */
:- module(smr_div_idp,
          [ run_idp/5,
            % FSM Engine Interface
            setup_strategy/5,
            transition/3,
            transition/4,
            accept_state/1,
            final_interpretation/2,
            extract_result_from_history/2
          ]).

:- use_module(library(lists)).
:- use_module(fsm_engine, [run_fsm_with_base/5]).
:- use_module(grounded_arithmetic, [incur_cost/1]).
:- use_module(incompatibility_semantics, [s/1, comp_nec/1, exp_poss/1]).

%!      run_idp(+T:integer, +S:integer, +KB_in:list, -FinalQuotient:integer, -FinalRemainder:integer) is det.
%
%       Executes the 'Inverse of Distributive Property' division strategy for T / S.
%
%       This predicate initializes and runs a state machine that models the IDP
%       strategy. It first checks for a positive divisor. If valid, it uses the
%       provided knowledge base `KB_in` to repeatedly subtract the largest
%       possible known multiple of `S` from `T`, accumulating the quotient.
%       It traces the entire execution.
%
%       @param T The Dividend (Total).
%       @param S The Divisor.
%       @param KB_in A list of `Multiple-Factor` pairs representing known
%       multiplication facts for `S`. Example: `[20-2, 50-5, 100-10]` for S=10.
%       @param FinalQuotient The calculated quotient of the division.
%       @param FinalRemainder The calculated remainder. If S is not positive,
%       this will be `T`.

run_idp(T, S, KB_in, FinalQuotient, FinalRemainder) :-
    % Check if division is valid first
    (S =< 0 ->
        FinalQuotient = 'error', FinalRemainder = T
    ;
        % Try to extract learned multiplication facts for divisor S
        extract_learned_multiplication_facts(S, LearnedKB),
        
        % If no learned facts available, strategy cannot proceed
        (LearnedKB = [] ->
            format(atom(Reason), 'No learned multiplication facts for divisor ~w', [S]),
            FinalQuotient = unavailable(Reason),
            FinalRemainder = T
        ;
            % Use learned knowledge (not hardcoded facts)
            append(KB_in, LearnedKB, CombinedKB),
            
            % Sort KB descending by multiple (like original)
            keysort(CombinedKB, SortedKB_asc),
            reverse(SortedKB_asc, KB),
            
            % Use the FSM engine to run this strategy
            setup_strategy(T, S, KB, InitialState, Parameters),
            Base = 10,
            run_fsm_with_base(smr_div_idp, InitialState, Parameters, Base, History),
            extract_result_from_history(History, [FinalQuotient, FinalRemainder])
        )
    ).

%!      setup_strategy(+T, +S, +KB, -InitialState, -Parameters) is det.
%
%       Sets up the initial state for the IDP division strategy.
setup_strategy(T, S, KB, InitialState, Parameters) :-
    % Initialize with T as remaining, 0 as total quotient, KB, and S as divisor
    % State format: state(StateName, Remaining, TotalQuotient, PartialT, PartialQ, KB, Divisor)
    InitialState = state(q_init, T, 0, 0, 0, KB, S),
    Parameters = [T, S, KB],
    
    % Emit modal signal for strategy initiation
    s(exp_poss(initiating_inverse_distributive_property_strategy)),
    incur_cost(inference).
%!      transition(+StateNum, -NextStateNum, -Action) is det.
%
%       State transitions for IDP division FSM.

transition(q_init, q_search_KB, search_knowledge_base) :-
    s(comp_nec(transitioning_to_knowledge_base_search)),
    incur_cost(state_change).

transition(q_search_KB, q_apply_fact, apply_found_fact) :-
    s(exp_poss(applying_discovered_multiplication_fact)),
    incur_cost(fact_application).

transition(q_search_KB, q_accept, complete_decomposition) :-
    s(exp_poss(completing_inverse_distributive_decomposition)),
    incur_cost(completion).

transition(q_apply_fact, q_search_KB, continue_search) :-
    s(comp_nec(continuing_iterative_decomposition)),
    incur_cost(iteration).

transition(q_error, q_error, maintain_error) :-
    s(comp_nec(error_state_is_absorbing)),
    incur_cost(error_handling).

%!      transition(+State, +Base, -NextState, -Interpretation) is det.
%
%       Complete state transitions with full state tracking.

% From q_init, proceed to search the knowledge base.
transition(state(q_init, T, TQ, PT, PQ, KB, S), _,
           state(q_search_KB, T, TQ, PT, PQ, KB, S), 
           Interpretation) :-
    s(exp_poss(initializing_knowledge_base_search)),
    format(atom(Interpretation), 'Initialize: ~w / ~w. Loaded known facts for ~w.', [T, S, S]),
    incur_cost(initialization).

% In q_search_KB, find the best known multiple to subtract.
transition(state(q_search_KB, Rem, TQ, _, _, KB, S), _,
           state(q_apply_fact, Rem, TQ, Multiple, Factor, KB, S), 
           Interpretation) :-
    find_best_fact(KB, Rem, Multiple, Factor),
    s(exp_poss(discovering_applicable_multiplication_fact)),
    format(atom(Interpretation), 'Found known multiple: ~w (~w x ~w).', [Multiple, Factor, S]),
    incur_cost(fact_discovery).

% If no suitable fact is found, the process is complete.
transition(state(q_search_KB, Rem, TQ, _, _, KB, S), _,
           state(q_accept, Rem, TQ, 0, 0, KB, S), 
           'No suitable fact found.') :-
    \+ find_best_fact(KB, Rem, _, _),
    s(exp_poss(exhausting_knowledge_base_options)),
    incur_cost(exhaustion).

% In q_apply_fact, subtract the found multiple and add the factor to the quotient.
transition(state(q_apply_fact, Rem, TQ, PT, PQ, KB, S), _,
           state(q_search_KB, NewRem, NewTQ, 0, 0, KB, S), 
           Interpretation) :-
    s(comp_nec(applying_multiplication_fact_decomposition)),
    NewRem is Rem - PT,
    NewTQ is TQ + PQ,
    format(atom(Interpretation), 'Applied fact. Subtracted ~w. Added ~w to Quotient.', [PT, PQ]),
    incur_cost(fact_application).

transition(state(q_error, _, _, _, _, _, _), _,
           state(q_error, 0, 0, 0, 0, [], 0),
           'Error: Invalid divisor.') :-
    s(comp_nec(error_state_persistence)),
    incur_cost(error_maintenance).

%!      accept_state(+State) is semidet.
%
%       Defines accepting states for the FSM.
accept_state(state(q_accept, _, _, _, _, _, _)).

%!      final_interpretation(+State, -Interpretation) is det.
%
%       Provides final interpretation of the computation.
final_interpretation(state(q_accept, Remainder, Quotient, _, _, _, _), Interpretation) :-
    format(atom(Interpretation), 'Successfully computed division: Quotient=~w, Remainder=~w via IDP strategy', [Quotient, Remainder]).
final_interpretation(state(q_error, _, _, _, _, _, _), 'Error: IDP division failed - invalid divisor').

%!      extract_result_from_history(+History, -Result) is det.
%
%       Extracts the final result from the execution history.
extract_result_from_history(History, [Quotient, Remainder]) :-
    last(History, LastStep),
    (LastStep = step(state(q_accept, Remainder, Quotient, _, _, _, _), _, _) ->
        true
    ;
        Quotient = error,
        Remainder = error
    ).

% find_best_fact/4 is a helper to greedily find the largest applicable known fact.
% It assumes KB is sorted in descending order of multiples.
find_best_fact([Multiple-Factor | _], Rem, Multiple, Factor) :-
    Multiple =< Rem.
find_best_fact([_ | Rest], Rem, BestMultiple, BestFactor) :-
    find_best_fact(Rest, Rem, BestMultiple, BestFactor).

%!      extract_learned_multiplication_facts(+Divisor, -LearnedKB) is det.
%
%       Extracts multiplication facts for Divisor from the learned knowledge system.
%       Returns facts in Multiple-Factor format that the system has genuinely learned.
extract_learned_multiplication_facts(Divisor, LearnedKB) :-
    % Query the learned knowledge system for multiplication strategies involving Divisor
    findall(Multiple-Factor, 
        learned_multiplication_fact(Divisor, Factor, Multiple), 
        LearnedKB).

%!      learned_multiplication_fact(+Divisor, -Factor, -Multiple) is nondet.
%
%       Checks if the system has learned a multiplication fact: Divisor * Factor = Multiple
learned_multiplication_fact(Divisor, Factor, Multiple) :-
    % Check if there's a learned strategy that demonstrates this multiplication
    % Look for strategies that use this specific multiplication relationship
    (   % Check if learned knowledge contains this multiplication fact
        catch((
            consult(learned_knowledge),
            run_learned_strategy(Divisor, Factor, Multiple, multiplication, _)
        ), _, fail)
    ;   % Or check if we can derive it from learned addition patterns
        catch((
            consult(learned_knowledge),
            run_learned_strategy(Partial, Partial, Multiple, doubles, _),
            Factor = 2,
            Partial is Divisor * Factor,
            Multiple = Partial
        ), _, fail)
    ;   % For now, no learned multiplication facts available
        fail
    ).
