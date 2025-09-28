%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reflective Pushdown Automaton for A+B using Rearranging to Make Bases %
% Author: Theodore M. Savich (Concept), Revised Implementation (AI Assist)%
% Date: 2023-11-16 (Corrected Version)                                    %
%                                                                         %
% Description:                                                            %
% Implements a PDA that processes input "A+B".                            %
% Primary strategy: Rearranging to Make Bases (RMB) for A+B.              %
% Special Feature: If reflection is enabled AND A=4, B>=6 (for base 10),  %
% the automaton enters a reflective state (q6) which repeatedly applies   %
% "+6 mod 10" to the stack representation of B. This state loops          %
% indefinitely unless the value becomes 0, demonstrating emergence.       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(refrmb_corrected, [run/4]).
:- use_module(library(lists)).

% --- Dynamic Predicates for State ---
:- dynamic stored_A/1.          % Stores decoded value of A
:- dynamic stored_B/1.          % Stores decoded value of B
:- dynamic transition/5.        % Stores transitions: transition(From, Sym, To, Action, IsReflective)
:- dynamic stack_item/1.        % Represents the current stack contents (list managed externally)
:- dynamic reflection_enabled/1.% Flag: y/n
:- dynamic decision_made/1.     % Tracks if decision phase at q3 has occurred for the current run

% --- Configuration ---
base(10). % Base for arithmetic

% --- Define valid digits ---
digit(D) :- member(D, [0,1,2,3,4,5,6,7,8,9]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Main Entry Point           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(Start, Input, Result, ReflectFlag) :-
    % --- Cleanup from any previous run ---
    retractall(stored_A(_)),
    retractall(stored_B(_)),
    retractall(reflection_enabled(_)),
    retractall(stack_item(_)),
    retractall(transition(_,_,_,_,_)), % Clear ALL dynamic transitions
    retractall(decision_made(_)),

    % --- Setup for new run ---
    assertz(reflection_enabled(ReflectFlag)),
    set_global_stack([]),        % Initialize empty stack
    setup_base_transitions,      % Setup only static, non-conditional transitions
    write('Starting run with reflection='), write(ReflectFlag), nl, nl,

    % --- Start processing ---
    step(Start, Input, [], Result). % Initial call to step predicate

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Main Processing Step          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step(State, Input, Stack, Result) :-
    print_config(State, Input, Stack), % Display current stack state (uses passed Stack)

    % --- Handle Terminal States ---
    ( State == 4 -> % Accept state
        Result = accept,
        write('*** ACCEPT reached. ***'), nl
    ; State == 5 -> % Error state
        Result = error,
        write('*** ERROR reached. ***'), nl

    % --- Handle State 3: Decision Phase ---
    ; State == 3, \+ decision_made(_) -> % Check if decision needed and not yet made
        !, % Prevent backtracking once decision logic starts
        make_decision_at_q3(Stack, Decision),
        assertz(decision_made(Decision)), % Mark decision as made for this run
        setup_q3_transition(Decision), % Assert the chosen transition FROM q3
        step(State, Input, Stack, Result) % Re-call step to take the newly added transition

    % --- Handle State 6: Reflection Loop ---
    ; State == 6 ->
        handle_reflection_state(State, Input, Stack, Result)

    % --- Default Transition Handling ---
    ; select_transition(State, Input, Stack, NextState, NextInput, NextStack, Action) ->
        % Note: select_transition now handles applying the action and updating stack
        print_transition(State, Input, Action, NextState), % Print applied transition
        step(NextState, NextInput, NextStack, Result)

    % --- No Transition Found ---
    ; write('*** ERROR: No transition found from state '), print_state(State),
      write(' with input '), write(Input), write(' and stack '), write(Stack), nl,
      Result = error
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       State-Specific Logic         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --- State 3: Decision Making & Transition Setup ---
make_decision_at_q3(Stack, Decision) :-
    decode_stack_final(Stack, A, B, K, Possible), % Decode A, B, calculate K
    ( Possible == error ->
        write('Decision@q3: Stack format error.'), nl,
        Decision = error
    ; reflection_enabled(RF), RF == y, base(Base), A =:= (Base - 6), B >= 6 -> % Generalized reflection trigger (k=6)
        write('Decision@q3: Conditions met for Reflection (k=6).'), nl,
        Decision = reflect
    ; B >= K -> % Standard rearrangement condition
        write('Decision@q3: Conditions met for Rearrangement (Accept).'), nl,
        Decision = accept
    ; % B < K and not reflection case
        write('Decision@q3: B < K, cannot rearrange standardly. Error.'), nl,
        Decision = error
    ).

% Assert the single transition leading OUT of q3 based on the decision
setup_q3_transition(accept) :-
    assertz(transition(3, epsilon, 7, rearrange_action, no)), % Go to q7 (rearrange)
    print_dynamic_transition(3, epsilon, 7, rearrange_action).
setup_q3_transition(error) :-
    assertz(transition(3, epsilon, 5, noop, no)), % Go to q5 (error)
    print_dynamic_transition(3, epsilon, 5, noop).
setup_q3_transition(reflect) :-
    assertz(transition(3, epsilon, 6, setup_reflect_stack, no)), % Go to q6 (reflect)
    print_dynamic_transition(3, epsilon, 6, setup_reflect_stack).

% --- State 6: Reflection Loop Handling ---
handle_reflection_state(State, Input, Stack, Result) :-
    Stack = [CurrentBmodBase | _RestStack], % Peek at stack top (must be B mod Base)
    ( CurrentBmodBase == 0 ->
        % HALT CONDITION MET: B mod Base is 0
        write('State q6: Halt condition met (Stack top == 0). Transitioning to Accept (q4).'), nl,
        NextState = 4,
        NextInput = Input, % Input unchanged (epsilon transition conceptually)
        NextStack = Stack, % Stack unchanged for this pseudo-transition
        print_pseudo_transition(State, 'halt_check', NextState), % Log the decision
        % Proceed directly to the accept state by calling step/4
        step(NextState, NextInput, NextStack, Result)
    ;
        % HALT CONDITION NOT MET: Continue looping
        write('State q6: Continuing reflection loop...'), nl,
        Action = reflect_add_6_step,
        apply_action(Action, Stack, NextStack), % Apply the +6 mod 10 update (this now updates global stack too)
        NextState = 6, % Loop back to self
        NextInput = Input, % Input unchanged (epsilon transition)
        print_pseudo_transition(State, Action, NextState), % Log the loop step
        step(NextState, NextInput, NextStack, Result) % Continue loop
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Transition Selection          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Select transition based on input symbol
% Modified to apply action and return the resulting NextStack
select_transition(State, [Sym|RestInput], Stack, NextState, RestInput, NextStack, Action) :-
    transition(State, Sym, NextState, Action, _), % Match on Sym
    !, % Commit to the first matching transition
    apply_action(Action, Stack, NextStack). % Apply action, get new stack

% Select epsilon transition if no symbol match
% Modified to apply action and return the resulting NextStack
select_transition(State, Input, Stack, NextState, Input, NextStack, Action) :-
    transition(State, epsilon, NextState, Action, _), % Match on epsilon
    !, % Commit to the first matching epsilon transition
    apply_action(Action, Stack, NextStack). % Apply action, get new stack


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Action Handlers            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dispatcher for actions - Actions NOW update global stack if they modify it

apply_action(noop, Stack, Stack). % No operation - doesn't change stack

apply_action(push(X), Stack, NewStack) :- % Push symbol X onto stack
    (digit(X) ; X == '#'), !, % Ensure valid symbol is pushed
    NewStack = [X|Stack],
    set_global_stack(NewStack). % Update global stack

apply_action(pop, [_|Stack], NewStack) :- !, % Pop from stack
    NewStack = Stack,
    set_global_stack(NewStack). % Update global stack
apply_action(pop, [], []) :- !, % Pop from empty stack is noop
    write('Warning: Pop attempted on empty stack.'), nl.
    % No need to set stack if it was already empty

apply_action(rearrange_action, InitialStack, FinalStack) :-
    write('Action: Performing RMB rearrangement...'), nl,
    rearrange_stack(InitialStack, FinalStack), % This predicate now updates global stack internally
    !. % Commit to this action if rearrangement succeeds

apply_action(setup_reflect_stack, Stack, NewStack) :-
    write('Action: Setting up stack for reflection state q6...'), nl,
    split_at_hash(Stack, APart, BPart), % Original stack: [DigitsB | ['#' | DigitsA]]
    digits_to_num(BPart, B),
    base(Base),
    BmodBase is B mod Base,
    append(['#'], APart, RestOfStack), % Keep A part and separator
    NewStack = [BmodBase | RestOfStack], % New stack: [BmodBase, '#', DigitsA...]
    write(' -> New stack top for B (mod Base): '), write(BmodBase), nl,
    set_global_stack(NewStack),!. % Update global stack state

apply_action(reflect_add_6_step, Stack, NewStack) :-
    Stack = [CurrentB | Rest], !, % CurrentB is B mod Base from previous step
    base(Base),
    K_reflect is 6, % The "self" value being added
    NewB is (CurrentB + K_reflect) mod Base,
    write('Action: Reflection step: '),
    write(CurrentB), write(' + '), write(K_reflect), write(' mod '), write(Base), write(' = '), write(NewB), nl,
    NewStack = [NewB | Rest], % Push the new value back
    set_global_stack(NewStack). % Update global stack state

apply_action(Action, Stack, Stack) :- % Default: if action unknown, do nothing
    write('Warning: Unknown action encountered: '), write(Action), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      RMB Rearrangement Logic         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Modified to use InitialStack argument instead of current_stack
rearrange_stack(InitialStack, FinalStack) :-
    decode_stack_final(InitialStack, A, B, K, Possible), % Use argument stack for decoding
    ( Possible == ok, B >= K -> % Ensure it's possible and B is large enough
        base(Base),
        Anew is A + K, % Should always equal Base
        Bnew is B - K,
        write(' -> Rearranging: A='), write(A), write(', B='), write(B),
        write(', K='), write(K), nl,
        write('    -> New A=(A+K)='), write(Anew),
        write(', New B=(B-K)='), write(Bnew), nl,
        num_to_digits(Anew, AnewDigits),
        num_to_digits(Bnew, BnewDigits),
        reverse(BnewDigits, RevB),
        reverse(AnewDigits, RevA),
        append(RevB, ['#'|RevA], NewStackReversed), % Build new stack content
        reverse(NewStackReversed, FinalStack),
        set_global_stack(FinalStack), % Update global stack - *THIS* is the action's effect
        write(' -> Rearrangement complete. New stack: '), write(FinalStack), nl
    ; % Condition not met or error during decode
      write('Error: Rearrange action called inappropriately or decode failed.'), nl,
      FinalStack = InitialStack % Return original stack on failure
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Stack & Arithmetic          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Decode stack into A, B, K. Returns 'ok' or 'error' in Possible.
% Operates purely on the input Stack argument.
decode_stack_final(Stack, A, B, K, Possible) :-
    ( member('#', Stack) ->
        split_at_hash(Stack, APart, BPart),
        ( digits_to_num(APart, A), digits_to_num(BPart, B) -> % Ensure conversion works
            retractall(stored_A(_)), retractall(stored_B(_)), % Clear old stored values
            assertz(stored_A(A)), assertz(stored_B(B)), % Store for potential later use
            base(Base),
            ( A =< Base -> K is Base - A, Possible = ok % Calculate K if A is valid
            ; write('Error: Decoded A > Base.'), nl, Possible = error % A is already >= Base? Error case.
            )
        ; write('Error: Failed to convert digits to numbers.'), nl, Possible = error, A = -1, B = -1, K = -1
        )
    ; % Stack doesn't contain '#', invalid format
        write('Error: Stack missing "#" separator.'), nl,
        Possible = error, A = -1, B = -1, K = -1 % Assign dummy values
    ).

% Split stack list at '#' marker
split_at_hash(Stack, APart, BPart) :-
    reverse(Stack, RevStack), % Example: [5, '#', 8] -> [8, '#', 5] (A=8, B=5)
    append(RevA, ['#'|RevB], RevStack), !, % RevA=[8], RevB=[5] ; Use cut as only one solution expected
    reverse(RevA, APart), % APart=[8]
    reverse(RevB, BPart). % BPart=[5]

% Convert list of digits to number
digits_to_num(Digs, N) :-
    foldl(add_digit, Digs, 0, N).
add_digit(D, Acc, Val) :- Val is Acc*10 + D.

% Convert number to list of digits
num_to_digits(0, [0]) :- !.
num_to_digits(N, Digs) :- N > 0, num_to_digits_acc(N, [], Digs).
num_to_digits_acc(0, Acc, Acc) :- !. % Cut for termination
num_to_digits_acc(N, Acc, Digs) :-
    N > 0,
    D is N mod 10,
    N1 is N // 10,
    num_to_digits_acc(N1, [D|Acc], Digs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Global Stack Access          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Update the global stack representation (used by actions that modify stack)
set_global_stack(NewStack) :-
    retractall(stack_item(_)),
    forall(member(E, NewStack), assertz(stack_item(E))).

% Retrieve the current global stack (ONLY for external query/debug if needed)
% Note: Main logic should rely on stack passed through step/4 arguments.
current_stack_global(Stack) :-
    findall(X, stack_item(X), S),
    reverse(S, Stack). % Stack items are asserted head first, so reverse to get logical order

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Static Transition Setup          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_base_transitions :-
    % q1: reading A until '+'
    forall(digit(D), assertz(transition(1, D, 1, push(D), no))),
    assertz(transition(1, '+', 2, push('#'), no)), % Push separator on '+'
    % q2: reading B digits until end of input
    forall(digit(D), assertz(transition(2, D, 2, push(D), no))),
    assertz(transition(2, epsilon, 3, noop, no)), % End of input -> goto decision q3
    % q7: after successful rearranging, go to q4 (accept)
    assertz(transition(7, epsilon, 4, noop, no)).
    % Transitions FROM q3 and the loop/halt FROM q6 are handled dynamically.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       Printing & Debug Helpers       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Modified to print the Stack argument passed to it.
print_config(State, Input, Stack) :-
    write('--------------------------------------'), nl,
    write('State: '), print_state(State),
    write(' | Input: '), write(Input),
    write(' | Stack: '), write(Stack), nl. % USE THE ARGUMENT Stack

print_state(S) :- write('q'), write(S).

% Print standard transitions found via transition/5
print_transition(SFrom, Input, Action, STo) :-
    ( Input == [] -> InputSym = 'epsilon'
    ; Input = [InputSym|_]
    ),
    write('Transition: '), print_state(SFrom),
    write(' --['), write(InputSym), write(':'), write(Action), write(']--> '),
    print_state(STo), nl.

% Print dynamically added transitions from q3
print_dynamic_transition(SFrom, Sym, STo, Action) :-
     write('Dynamically Added Transition: '), print_state(SFrom),
     write(' --['), write(Sym), write(':'), write(Action), write(']--> '),
     print_state(STo), nl.

% Print pseudo-transitions decided within state 6 logic
print_pseudo_transition(SFrom, ActionOrCheck, STo) :-
     write('State q6 Logic: '), print_state(SFrom),
     write(' --['), write('epsilon'), write(':'), write(ActionOrCheck), write(']--> '),
     print_state(STo), nl.