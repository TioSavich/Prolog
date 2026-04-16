/** <module> Standard 1.CA.1 — Addition/subtraction fluency within 20
 *
 * Indiana: 1.CA.1 — "Demonstrate fluency with addition facts and the
 *          corresponding subtraction facts within 20. Use strategies
 *          such as counting on; making ten; decomposing a number
 *          leading to a 10; using the relationship between addition
 *          and subtraction; and creating equivalent but easier or
 *          known sums." (E)
 * CCSS:    1.OA.C.6 — "Add and subtract within 20, demonstrating
 *                      fluency for addition and subtraction within 10."
 *
 * VPV MAPPING:
 *   V  (target vocabulary): addition/subtraction facts within 20,
 *      "makes ten", "doubles", "near doubles", "fact family"
 *   P  (practices): counting-on; making ten (8+6 = 8+2+4 = 10+4 = 14);
 *      decomposing to ten (13-4 = 13-3-1 = 10-1 = 9);
 *      fact families (8+4=12 → 12-8=4); doubles/near-doubles
 *   V' (metavocabulary): "what strategy did you use?", "is there
 *      a faster way?", "how does knowing 8+2=10 help with 8+6?"
 *
 * CONNECTION TO EXISTING AUTOMATA:
 *   This standard names the strategies that the prolog/Prolog/math/
 *   automata formalize at a more general level:
 *   - counting_on → sar_add_counting_on.pl
 *   - making_ten  → related to sar_add_rmb.pl (round-make-base)
 *   - decomposing → sar_sub_decomposition.pl
 *   - fact_family → sar_sub_cobo_missing_addend.pl
 *
 * BRANDOM CONNECTION: The transition from "I can add 8+6" (by
 *   counting all) to "I can add 8+6 by making ten" is a genuine
 *   algorithmic elaboration. The making-ten strategy deploys the
 *   place-value vocabulary (K.NS.7, 1.NS.2) within the addition
 *   practice. The learner who makes ten is using a STRONGER
 *   vocabulary than one who counts all — they see 8+6 as
 *   8+(2+4) = (8+2)+4 = 10+4 = 14. Each rewrite step is a
 *   material inference licensed by the practices mastered so far.
 *
 * CRISIS CONNECTION: The efficiency gap drives strategy adoption:
 *   - counting_all: O(A+B) — adequate for small numbers
 *   - counting_on: O(B) — halves the work
 *   - making_ten: O(1) plus decomposition — near-constant for facts
 *   When counting_on is too slow (8+6 = 14 steps from 0), the
 *   system enters crisis and must find a better strategy.
 *
 * LIMITATIONS:
 *   - This module provides the strategies as standalone predicates.
 *     Wiring them into the ORR cycle (crisis → strategy selection)
 *     is a separate task.
 *   - "Fluency" (speed + accuracy) is not modeled. The module
 *     tests correctness only.
 */

:- module(standard_1_ca_1, [
    add_counting_on/3,     % +A, +B, -Sum
    add_making_ten/3,      % +A, +B, -Sum
    sub_decompose_to_ten/3,% +Minuend, +Subtrahend, -Difference
    fact_family/4,         % +A, +B, -Sum, -Facts
    add_doubles_near/3     % +A, +B, -Sum
]).

:- use_module(formalization(grounded_arithmetic), [
    zero/1,
    successor/2,
    predecessor/2,
    equal_to/2,
    smaller_than/2,
    greater_than/2,
    add_grounded/3,
    subtract_grounded/3,
    integer_to_recollection/2,
    recollection_to_integer/2,
    incur_cost/1
]).

% ============================================================
% Strategy 1: Counting on (from K.NS.1, deployed for addition)
% ============================================================

%!  add_counting_on(+A, +B, -Sum) is det.
%
%   Add A + B by counting on B times from A.
%   Cost: O(B) successor operations.
%   This is K.NS.1's count_on_from applied to addition.

add_counting_on(A, B, Sum) :-
    incur_cost(inference),
    count_on_(A, B, Sum).

count_on_(Current, Remaining, Current) :-
    zero(Zero),
    equal_to(Remaining, Zero), !.
count_on_(Current, Remaining, Sum) :-
    successor(Current, Next),
    predecessor(Remaining, NewRemaining),
    count_on_(Next, NewRemaining, Sum).


% ============================================================
% Strategy 2: Making ten (8+6 = 8+2+4 = 10+4 = 14)
% ============================================================

%!  add_making_ten(+A, +B, -Sum) is semidet.
%
%   Add A + B by first completing A to 10, then adding the rest.
%   Requires: A < 10, A + B >= 10.
%
%   Steps:
%   1. Find complement: need = 10 - A
%   2. Split B: B = need + rest
%   3. Sum = 10 + rest
%
%   This deploys K.CA.3 (complement to 10) and K.NS.7 (place value)
%   within the addition practice.

add_making_ten(A, B, Sum) :-
    incur_cost(inference),
    integer_to_recollection(10, Ten),
    % Step 1: complement to ten
    subtract_grounded(Ten, A, Need),
    % Step 2: split B into need + rest
    subtract_grounded(B, Need, Rest),
    % Step 3: sum is 10 + rest
    add_grounded(Ten, Rest, Sum).


% ============================================================
% Strategy 3: Decompose to ten (13-4 = 13-3-1 = 10-1 = 9)
% ============================================================

%!  sub_decompose_to_ten(+Minuend, +Subtrahend, -Difference) is semidet.
%
%   Subtract by first subtracting down to 10, then subtracting
%   the rest. Useful when Minuend > 10 > Minuend - Subtrahend.
%
%   Steps:
%   1. Find how far Minuend is above 10: above = Minuend - 10
%   2. Split Subtrahend: Subtrahend = above + rest
%   3. Difference = 10 - rest

sub_decompose_to_ten(Minuend, Subtrahend, Difference) :-
    incur_cost(inference),
    integer_to_recollection(10, Ten),
    % Step 1: how far above ten?
    subtract_grounded(Minuend, Ten, Above),
    % Step 2: split subtrahend
    subtract_grounded(Subtrahend, Above, Rest),
    % Step 3: subtract rest from ten
    subtract_grounded(Ten, Rest, Difference).


% ============================================================
% Strategy 4: Fact families (relationship between add/sub)
% ============================================================

%!  fact_family(+A, +B, -Sum, -Facts) is det.
%
%   Given two addends, produce the complete fact family:
%   A+B=Sum, B+A=Sum, Sum-A=B, Sum-B=A
%
%   This models the "relationship between addition and subtraction"
%   strategy: knowing 8+4=12 means knowing 12-8=4.

fact_family(A, B, Sum, Facts) :-
    incur_cost(inference),
    add_grounded(A, B, Sum),
    Facts = [
        add(A, B, Sum),
        add(B, A, Sum),
        sub(Sum, A, B),
        sub(Sum, B, A)
    ].


% ============================================================
% Strategy 5: Doubles and near-doubles
% ============================================================

%!  add_doubles_near(+A, +B, -Sum) is det.
%
%   Add using doubles/near-doubles strategy:
%   If A = B: Sum = A + A (double)
%   If |A - B| = 1: use the double of the smaller, add 1
%
%   Falls back to counting-on if not a doubles case.

add_doubles_near(A, B, Sum) :-
    incur_cost(inference),
    integer_to_recollection(1, One),
    (   equal_to(A, B)
    ->  % Doubles: A + A
        add_grounded(A, A, Sum)
    ;   % Check near-doubles
        (   subtract_grounded(B, A, Diff),
            equal_to(Diff, One)
        ->  % B = A + 1, so A + B = A + A + 1
            add_grounded(A, A, Double),
            successor(Double, Sum)
        ;   subtract_grounded(A, B, Diff2),
            equal_to(Diff2, One)
        ->  % A = B + 1, so A + B = B + B + 1
            add_grounded(B, B, Double),
            successor(Double, Sum)
        ;   % Not a doubles case, fall back
            add_counting_on(A, B, Sum)
        )
    ).
