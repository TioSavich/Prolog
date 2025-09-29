/** <module> Grounded Arithmetic Operations
 *
 * This module implements arithmetic operations without relying on Prolog's
 * built-in arithmetic operators. All operations are grounded in embodied
 * practice and work with recollection structures that represent the history
 * of counting actions.
 *
 * This implements the UMEDCA thesis that "Numerals are Pronouns" - numbers
 * are anaphoric recollections of the act of counting, not abstract entities.
 * 
 * All operations emit cognitive cost signals to support embodied learning.
 *
 * @author UMEDCA System
 * 
 */
:- module(grounded_arithmetic, [
    % Core grounded operations
    add_grounded/3,
    subtract_grounded/3,
    multiply_grounded/3,
    divide_grounded/3,
    
    % Comparison operations
    smaller_than/2,
    greater_than/2,
    equal_to/2,
    
    % Utility predicates
    successor/2,
    predecessor/2,
    zero/1,
    
    % Conversion predicates (for interfacing with existing code during transition)
    integer_to_recollection/2,
    recollection_to_integer/2,
    
    % Cognitive cost support
    incur_cost/1
]).

% --- Core Representations ---

%!      zero(?Recollection) is det.
%
%       Defines the recollection structure for zero - an empty counting history.
zero(recollection([])).

%!      successor(+Recollection, -NextRecollection) is det.
%
%       Implements the successor operation by adding one more tally to the history.
%       This is the embodied act of counting one more.
successor(recollection(History), recollection([tally|History])) :-
    incur_cost(unit_count).

%!      predecessor(+Recollection, -PrevRecollection) is det.
%
%       Implements the predecessor operation by removing one tally.
%       Fails for zero (cannot count backwards from nothing).
predecessor(recollection([tally|History]), recollection(History)) :-
    incur_cost(unit_count).

% --- Comparison Operations ---

%!      smaller_than(+A, +B) is semidet.
%
%       A is smaller than B if A's history is a proper prefix of B's history.
%       This captures the embodied intuition of "having counted fewer times."
smaller_than(recollection(HistoryA), recollection(HistoryB)) :-
    append(HistoryA, Suffix, HistoryB),
    Suffix \= [],
    incur_cost(inference).

%!      greater_than(+A, +B) is semidet.
%
%       A is greater than B if B is smaller than A.
greater_than(A, B) :-
    smaller_than(B, A).

%!      equal_to(+A, +B) is semidet.
%
%       Two recollections are equal if they have the same counting history.
equal_to(recollection(History), recollection(History)) :-
    incur_cost(inference).

% --- Core Arithmetic Operations ---

%!      add_grounded(+A, +B, -Sum) is det.
%
%       Addition is the concatenation of two counting histories.
%       This represents the embodied act of "counting on" from A by B more.
add_grounded(recollection(HistoryA), recollection(HistoryB), recollection(HistorySum)) :-
    incur_cost(inference),
    append(HistoryA, HistoryB, HistorySum).

%!      subtract_grounded(+Minuend, +Subtrahend, -Difference) is semidet.
%
%       Subtraction removes a counting history from another.
%       Fails if trying to subtract more than is present (embodied constraint).
subtract_grounded(recollection(HistoryM), recollection(HistoryS), recollection(HistoryDiff)) :-
    incur_cost(inference),
    append(HistoryDiff, HistoryS, HistoryM).

%!      multiply_grounded(+A, +B, -Product) is det.
%
%       Multiplication is repeated addition - adding A to itself B times.
%       This captures the embodied understanding of multiplication as iteration.
multiply_grounded(A, recollection([]), Zero) :-
    zero(Zero),
    incur_cost(inference).

multiply_grounded(A, B, Product) :-
    B \= recollection([]),
    predecessor(B, BPrev),
    multiply_grounded(A, BPrev, PartialProduct),
    add_grounded(PartialProduct, A, Product).

%!      divide_grounded(+Dividend, +Divisor, -Quotient) is semidet.
%
%       Division is repeated subtraction - how many times can we subtract Divisor from Dividend.
%       Fails if Divisor is zero (embodied constraint).
divide_grounded(Dividend, Divisor, Quotient) :-
    \+ zero(Divisor),
    divide_helper(Dividend, Divisor, recollection([]), Quotient).

% Helper for division by repeated subtraction
divide_helper(Remainder, Divisor, AccQuotient, Quotient) :-
    ( subtract_grounded(Remainder, Divisor, NewRemainder) ->
        successor(AccQuotient, NewAccQuotient),
        divide_helper(NewRemainder, Divisor, NewAccQuotient, Quotient)
    ;
        Quotient = AccQuotient
    ).

% --- Conversion Utilities (for transition period) ---

%!      integer_to_recollection(+Integer, -Recollection) is det.
%
%       Converts a Prolog integer to a recollection structure.
%       Used during the transition period to interface with existing code.
integer_to_recollection(0, recollection([])) :- !.
integer_to_recollection(N, recollection(History)) :-
    N > 0,
    length(History, N),
    maplist(=(tally), History).

%!      recollection_to_integer(+Recollection, -Integer) is det.
%
%       Converts a recollection structure back to a Prolog integer.
%       Used during the transition period for compatibility.
recollection_to_integer(recollection(History), Integer) :-
    length(History, Integer).

% --- Cognitive Cost Support ---

%!      incur_cost(+Action) is det.
%
%       Records the cognitive cost of an embodied action.
%       This will be intercepted by the meta-interpreter to track computational effort.
incur_cost(_Action) :-
    true.  % Simple implementation - meta-interpreter will intercept this