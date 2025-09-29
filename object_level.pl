/** <module> Object-Level Knowledge Base
 *
 * This module represents the "object level" of the cognitive architecture.
 * It contains the initial, and potentially flawed, knowledge base that the
 * system reasons with. The predicates defined in this module are the ones
 * that are observed by the meta-interpreter and modified by the
 * reorganization engine.
 *
 * The key predicate `add/3` is declared as `dynamic` because it is the
 * target of learning and reorganization. Its initial implementation is
 * deliberately inefficient to create opportunities for the system to detect
 * disequilibrium and self-improve.
 *
 * 
 * 
 */
:- module(object_level, [add/3, subtract/3, multiply/3, divide/3]).

:- use_module(grounded_arithmetic).

:- dynamic add/3.
:- dynamic subtract/3.
:- dynamic multiply/3.
:- dynamic divide/3.

% enumerate/1
% Helper to force enumeration of a Peano number. Its primary purpose
% in this context is to consume inference steps in the meta-interpreter,
% making the initial `add/3` implementation inefficient and prone to
% resource exhaustion, which acts as a trigger for reorganization.
enumerate(0).
enumerate(s(N)) :- enumerate(N).

% recursive_add/3
% This is the standard, efficient, recursive definition of addition for
% Peano numbers. It serves as the "correct" implementation that the
% reorganization engine will synthesize and assert when the initial,
% inefficient `add/3` rule is retracted.
recursive_add(0, B, B).
recursive_add(s(A), B, s(Sum)) :-
    recursive_add(A, B, Sum).

%!      add(?A, ?B, ?Sum) is nondet.
%
%       The initial, inefficient definition of addition.
%       This predicate is designed to simulate a "counting-all" strategy. It
%       works by first completely grounding the two inputs `A` and `B` by
%       recursively calling `enumerate/1`. This process is computationally
%       expensive and is intended to fail (by resource exhaustion) for larger
%       numbers, thus triggering the ORR learning cycle.
%
%       This predicate is declared `dynamic` and will be replaced by a more
%       efficient version by the `reorganization_engine`.
%
%       @param A A Peano number representing the first addend.
%       @param B A Peano number representing the second addend.
%       @param Sum The Peano number representing the sum of A and B.
add(A, B, Sum) :-
    enumerate(A),
    enumerate(B),
    recursive_add(A, B, Sum).

%!      multiply(?A, ?B, ?Product) is nondet.
%
%       The initial, inefficient definition of multiplication.
%       This predicate is designed to simulate multiplication via repeated
%       addition. It is computationally expensive and intended to trigger
%       reorganization for larger numbers.
%
%       This predicate is declared `dynamic` and will be replaced by a more
%       efficient version by the `reorganization_engine`.
multiply(A, B, Product) :-
    enumerate(A),
    enumerate(B),
    recursive_multiply(A, B, Product).

% recursive_multiply/3
% This is the standard, efficient, recursive definition of multiplication.
recursive_multiply(0, _, 0).
recursive_multiply(s(A), B, Product) :-
    recursive_multiply(A, B, PartialProduct),
    add(PartialProduct, B, Product).

% recursive_subtract/3
% The standard, efficient recursive definition of subtraction for Peano numbers.
% This will be synthesized by the reorganization engine.
recursive_subtract(A, 0, A).
recursive_subtract(s(A), s(B), Difference) :-
    recursive_subtract(A, B, Difference).

%!      subtract(?Minuend, ?Subtrahend, ?Difference) is nondet.
%
%       The initial, inefficient definition of subtraction.
%       Like add/3, this deliberately enumerates both inputs to trigger
%       reorganization. It uses the grounded arithmetic to avoid the
%       Prolog arithmetic backstop.
%
%       @param Minuend A Peano number to subtract from.
%       @param Subtrahend A Peano number to subtract.
%       @param Difference The result of Minuend - Subtrahend.
subtract(Minuend, Subtrahend, Difference) :-
    enumerate(Minuend),
    enumerate(Subtrahend),
    recursive_subtract(Minuend, Subtrahend, Difference).

% recursive_divide/3  
% The standard definition of division for Peano numbers via repeated subtraction.
recursive_divide(Dividend, Divisor, Quotient) :-
    recursive_divide_helper(Dividend, Divisor, 0, Quotient).

recursive_divide_helper(Remainder, Divisor, AccQuotient, Quotient) :-
    ( recursive_subtract(Remainder, Divisor, NewRemainder) ->
        recursive_add(AccQuotient, s(0), NewAccQuotient),
        recursive_divide_helper(NewRemainder, Divisor, NewAccQuotient, Quotient)
    ;
        Quotient = AccQuotient
    ).

%!      divide(?Dividend, ?Divisor, ?Quotient) is nondet.
%
%       The initial, inefficient definition of division.
%       Enumerates inputs and uses repeated subtraction to compute quotient.
%
%       @param Dividend A Peano number to be divided.
%       @param Divisor A Peano number to divide by.
%       @param Quotient The result of Dividend / Divisor.
divide(Dividend, Divisor, Quotient) :-
    enumerate(Dividend),
    enumerate(Divisor),
    \+ (Divisor = 0),  % Prevent division by zero
    recursive_divide(Dividend, Divisor, Quotient).