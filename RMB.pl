/** <module> Reflective Pushdown Automaton for RMB Strategy
 *
 * This module provides a detailed, low-level simulation of the 'Rearranging
 * to Make Bases' (RMB) addition strategy, implemented as a Pushdown
 * Automaton (PDA).
 *
 * **Note:** This appears to be an older or more experimental implementation
 * compared to `sar_add_rmb.pl`. It includes a unique "reflective" state
 * (`q6`) that demonstrates emergent behavior under specific conditions, which
 * is not present in the simplified `sar` models.
 *
 * The automaton processes an input string like `[4, '+', 8]` and uses a
 * stack to manipulate the numbers. The core logic involves deciding whether
 * a standard RMB rearrangement is possible or if a special reflective loop
 * should be entered.
 *
 * The main entry point is `run/4`.
 *
 * @author Theodore M. Savich (Concept), Revised Implementation (AI Assist)
 * @license Unknown
 */
:- module(refrmb_corrected, [run/4]).
:- use_module(library(lists)).

% --- Dynamic Predicates for State ---
:- dynamic stored_A/1.
:- dynamic stored_B/1.
:- dynamic transition/5.
:- dynamic stack_item/1.
:- dynamic reflection_enabled/1.
:- dynamic decision_made/1.

% --- Configuration ---
base(10).

% --- Define valid digits ---
digit(D) :- member(D, [0,1,2,3,4,5,6,7,8,9]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Main Entry Point           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!      run(+Start:integer, +Input:list, -Result:atom, +ReflectFlag:atom) is det.
%
%       Runs the Reflective Pushdown Automaton simulation.
%
%       This is the main entry point for the module. It initializes the
%       automaton's state by clearing any dynamic facts from previous runs,
%       setting up the initial stack, and defining the static transitions.
%       It then starts the simulation process by calling `step/4`.
%
%       @param Start The initial state of the automaton (e.g., `1`).
%       @param Input The input string to be processed, as a list of atoms
%       and numbers (e.g., `[4, '+', 8]`).
%       @param Result The final result of the run, either `accept` or `error`.
%       @param ReflectFlag A flag (`y` or `n`) to enable or disable the
%       special reflective behavior of the automaton.
run(Start, Input, Result, ReflectFlag) :-
    % --- Cleanup from any previous run ---
    retractall(stored_A(_)),
    retractall(stored_B(_)),
    retractall(reflection_enabled(_)),
    retractall(stack_item(_)),
    retractall(transition(_,_,_,_,_)),
    retractall(decision_made(_)),

    % --- Setup for new run ---
    assertz(reflection_enabled(ReflectFlag)),
    set_global_stack([]),
    setup_base_transitions,
    write('Starting run with reflection='), write(ReflectFlag), nl, nl,

    % --- Start processing ---
    step(Start, Input, [], Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Main Processing Step          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% step(+State, +Input, +Stack, -Result)
%
% The main recursive predicate that drives the PDA. In each step, it
% determines the next action based on the current state, input, and stack,
% then calls itself with the updated parameters. It handles terminal states,
% special decision points, and the reflective loop.
step(State, Input, Stack, Result) :-
    print_config(State, Input, Stack),

    % --- Handle Terminal States ---
    ( State == 4 ->
        Result = accept,
        write('*** ACCEPT reached. ***'), nl
    ; State == 5 ->
        Result = error,
        write('*** ERROR reached. ***'), nl

    % --- Handle State 3: Decision Phase ---
    ; State == 3, \+ decision_made(_) ->
        !,
        make_decision_at_q3(Stack, Decision),
        assertz(decision_made(Decision)),
        setup_q3_transition(Decision),
        step(State, Input, Stack, Result)

    % --- Handle State 6: Reflection Loop ---
    ; State == 6 ->
        handle_reflection_state(State, Input, Stack, Result)

    % --- Default Transition Handling ---
    ; select_transition(State, Input, Stack, NextState, NextInput, NextStack, Action) ->
        print_transition(State, Input, Action, NextState),
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
% make_decision_at_q3(+Stack, -Decision)
%
% Determines the next step from state q3. It decodes the numbers A and B
% from the stack, calculates the required transfer amount K, and then decides
% whether to (1) rearrange, (2) enter the reflective state, or (3) error out.
make_decision_at_q3(Stack, Decision) :-
    decode_stack_final(Stack, A, B, K, Possible),
    ( Possible == error ->
        write('Decision@q3: Stack format error.'), nl,
        Decision = error
    ; reflection_enabled(RF), RF == y, base(Base), A =:= (Base - 6), B >= 6 ->
        write('Decision@q3: Conditions met for Reflection (k=6).'), nl,
        Decision = reflect
    ; B >= K ->
        write('Decision@q3: Conditions met for Rearrangement (Accept).'), nl,
        Decision = accept
    ;
        write('Decision@q3: B < K, cannot rearrange standardly. Error.'), nl,
        Decision = error
    ).

% setup_q3_transition(+Decision)
%
% Dynamically asserts the transition rule leading out of state q3 based on
% the decision made by make_decision_at_q3/2.
setup_q3_transition(accept) :-
    assertz(transition(3, epsilon, 7, rearrange_action, no)),
    print_dynamic_transition(3, epsilon, 7, rearrange_action).
setup_q3_transition(error) :-
    assertz(transition(3, epsilon, 5, noop, no)),
    print_dynamic_transition(3, epsilon, 5, noop).
setup_q3_transition(reflect) :-
    assertz(transition(3, epsilon, 6, setup_reflect_stack, no)),
    print_dynamic_transition(3, epsilon, 6, setup_reflect_stack).

% --- State 6: Reflection Loop Handling ---
% handle_reflection_state(+State, +Input, +Stack, -Result)
%
% Manages the logic for the special reflective state q6. It checks the top of
% the stack. If it's 0, the loop halts and transitions to the accept state.
% Otherwise, it applies a "reflect_add_6_step" action to the stack and loops
% back to q6.
handle_reflection_state(State, Input, Stack, Result) :-
    Stack = [CurrentBmodBase | _RestStack],
    ( CurrentBmodBase == 0 ->
        write('State q6: Halt condition met (Stack top == 0). Transitioning to Accept (q4).'), nl,
        NextState = 4,
        NextInput = Input,
        NextStack = Stack,
        print_pseudo_transition(State, 'halt_check', NextState),
        step(NextState, NextInput, NextStack, Result)
    ;
        write('State q6: Continuing reflection loop...'), nl,
        Action = reflect_add_6_step,
        apply_action(Action, Stack, NextStack),
        NextState = 6,
        NextInput = Input,
        print_pseudo_transition(State, Action, NextState),
        step(NextState, NextInput, NextStack, Result)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Transition Selection          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Select transition based on input symbol
% Modified to apply action and return the resulting NextStack
select_transition(State, [Sym|RestInput], Stack, NextState, RestInput, NextStack, Action) :-
    transition(State, Sym, NextState, Action, _),
    !,
    apply_action(Action, Stack, NextStack).

% Select epsilon transition if no symbol match
% Modified to apply action and return the resulting NextStack
select_transition(State, Input, Stack, NextState, Input, NextStack, Action) :-
    transition(State, epsilon, NextState, Action, _),
    !,
    apply_action(Action, Stack, NextStack).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Action Handlers            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dispatcher for actions - Actions NOW update global stack if they modify it

apply_action(noop, Stack, Stack).

apply_action(push(X), Stack, NewStack) :-
    (digit(X) ; X == '#'), !,
    NewStack = [X|Stack],
    set_global_stack(NewStack).

apply_action(pop, [_|Stack], NewStack) :- !,
    NewStack = Stack,
    set_global_stack(NewStack).
apply_action(pop, [], []) :- !,
    write('Warning: Pop attempted on empty stack.'), nl.

apply_action(rearrange_action, InitialStack, FinalStack) :-
    write('Action: Performing RMB rearrangement...'), nl,
    rearrange_stack(InitialStack, FinalStack),
    !.

apply_action(setup_reflect_stack, Stack, NewStack) :-
    write('Action: Setting up stack for reflection state q6...'), nl,
    split_at_hash(Stack, APart, BPart),
    digits_to_num(BPart, B),
    base(Base),
    BmodBase is B mod Base,
    append(['#'], APart, RestOfStack),
    NewStack = [BmodBase | RestOfStack],
    write(' -> New stack top for B (mod Base): '), write(BmodBase), nl,
    set_global_stack(NewStack),!.

apply_action(reflect_add_6_step, Stack, NewStack) :-
    Stack = [CurrentB | Rest], !,
    base(Base),
    K_reflect is 6,
    NewB is (CurrentB + K_reflect) mod Base,
    write('Action: Reflection step: '),
    write(CurrentB), write(' + '), write(K_reflect), write(' mod '), write(Base), write(' = '), write(NewB), nl,
    NewStack = [NewB | Rest],
    set_global_stack(NewStack).

apply_action(Action, Stack, Stack) :-
    write('Warning: Unknown action encountered: '), write(Action), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      RMB Rearrangement Logic         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Modified to use InitialStack argument instead of current_stack
rearrange_stack(InitialStack, FinalStack) :-
    decode_stack_final(InitialStack, A, B, K, Possible),
    ( Possible == ok, B >= K ->
        base(Base),
        Anew is A + K,
        Bnew is B - K,
        write(' -> Rearranging: A='), write(A), write(', B='), write(B),
        write(', K='), write(K), nl,
        write('    -> New A=(A+K)='), write(Anew),
        write(', New B=(B-K)='), write(Bnew), nl,
        num_to_digits(Anew, AnewDigits),
        num_to_digits(Bnew, BnewDigits),
        reverse(BnewDigits, RevB),
        reverse(AnewDigits, RevA),
        append(RevB, ['#'|RevA], NewStackReversed),
        reverse(NewStackReversed, FinalStack),
        set_global_stack(FinalStack),
        write(' -> Rearrangement complete. New stack: '), write(FinalStack), nl
    ;
      write('Error: Rearrange action called inappropriately or decode failed.'), nl,
      FinalStack = InitialStack
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Stack & Arithmetic          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Decode stack into A, B, K. Returns 'ok' or 'error' in Possible.
% Operates purely on the input Stack argument.
decode_stack_final(Stack, A, B, K, Possible) :-
    ( member('#', Stack) ->
        split_at_hash(Stack, APart, BPart),
        ( digits_to_num(APart, A), digits_to_num(BPart, B) ->
            retractall(stored_A(_)), retractall(stored_B(_)),
            assertz(stored_A(A)), assertz(stored_B(B)),
            base(Base),
            ( A =< Base -> K is Base - A, Possible = ok
            ; write('Error: Decoded A > Base.'), nl, Possible = error
            )
        ; write('Error: Failed to convert digits to numbers.'), nl, Possible = error, A = -1, B = -1, K = -1
        )
    ;
        write('Error: Stack missing "#" separator.'), nl,
        Possible = error, A = -1, B = -1, K = -1
    ).

% Split stack list at '#' marker
split_at_hash(Stack, APart, BPart) :-
    reverse(Stack, RevStack),
    append(RevA, ['#'|RevB], RevStack), !,
    reverse(RevA, APart),
    reverse(RevB, BPart).

% Convert list of digits to number
digits_to_num(Digs, N) :-
    foldl(add_digit, Digs, 0, N).
add_digit(D, Acc, Val) :- Val is Acc*10 + D.

% Convert number to list of digits
num_to_digits(0, [0]) :- !.
num_to_digits(N, Digs) :- N > 0, num_to_digits_acc(N, [], Digs).
num_to_digits_acc(0, Acc, Acc) :- !.
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
    reverse(S, Stack).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Static Transition Setup          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_base_transitions :-
    % q1: reading A until '+'
    forall(digit(D), assertz(transition(1, D, 1, push(D), no))),
    assertz(transition(1, '+', 2, push('#'), no)),
    % q2: reading B digits until end of input
    forall(digit(D), assertz(transition(2, D, 2, push(D), no))),
    assertz(transition(2, epsilon, 3, noop, no)),
    % q7: after successful rearranging, go to q4 (accept)
    assertz(transition(7, epsilon, 4, noop, no)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       Printing & Debug Helpers       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Modified to print the Stack argument passed to it.
print_config(State, Input, Stack) :-
    write('--------------------------------------'), nl,
    write('State: '), print_state(State),
    write(' | Input: '), write(Input),
    write(' | Stack: '), write(Stack), nl.

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