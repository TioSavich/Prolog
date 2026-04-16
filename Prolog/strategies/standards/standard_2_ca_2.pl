/** <module> Standard 2.CA.2 — Add/subtract within 1000 (place value)
 *
 * Indiana: 2.CA.2 — "Using number sense and place value strategies,
 *          add and subtract within 1,000, including composing and
 *          decomposing tens and hundreds."
 * CCSS:    2.NBT.B.7 — "Add and subtract within 1000, using concrete
 *                       models or drawings and strategies based on
 *                       place value..."
 *
 * VPV MAPPING:
 *   V  (target vocabulary): multi-digit sums/differences, "regroup",
 *      "compose a ten/hundred", "decompose a ten/hundred"
 *   P  (practices): place-value addition with cascading regrouping;
 *      place-value subtraction with borrowing; decomposing
 *      hundreds into tens when borrowing
 *   V' (metavocabulary): "add the ones, add the tens, add the
 *      hundreds", "do I need to regroup?", "I need to break apart
 *      a hundred into ten tens"
 *
 * CONNECTION TO EXISTING AUTOMATA:
 *   This standard is the home of the strategy automata in
 *   prolog/Prolog/math/:
 *
 *   Addition strategies:
 *   - sar_add_counting_on.pl  — counting on (O(B), base case)
 *   - sar_add_cobo.pl         — COBO: add bases, then ones
 *   - sar_add_rmb.pl          — Round to Make Base: round up, adjust
 *   - sar_add_chunking.pl     — Chunk by place value, add in parts
 *   - sar_add_rounding.pl     — Round both, adjust
 *
 *   Subtraction strategies:
 *   - sar_sub_counting_back.pl      — count back (O(B))
 *   - sar_sub_decomposition.pl      — decompose subtrahend
 *   - sar_sub_cobo_missing_addend.pl — COBO: missing addend
 *   - sar_sub_chunking_a/b/c.pl     — chunk by place value
 *   - sar_sub_sliding.pl            — slide both numbers
 *   - sar_sub_cbbo_take_away.pl     — count back bases/ones
 *
 *   The standard describes what children DO; the automata formalize
 *   the specific strategies as FSMs with cost tracking.
 *
 * BRANDOM CONNECTION: The transition from two-digit to three-digit
 *   arithmetic is where algorithmic elaboration becomes recursive.
 *   The same decompose-add-regroup pattern applies at hundreds,
 *   tens, and ones — the practice is the same, applied at different
 *   levels. This recursive application of a practice is what
 *   Brandom calls "algorithmic decomposition": the complex
 *   practice (three-digit addition) decomposes into applications
 *   of a simpler practice (single-place addition + regrouping).
 */

:- module(standard_2_ca_2, [
    add_three_digit/3,     % +A, +B, -Sum
    sub_three_digit/3,     % +A, +B, -Difference
    add_cobo_style/3,      % +A, +B, -Sum (COBO strategy)
    sub_decompose_style/3  % +A, +B, -Difference (decomposition)
]).

:- use_module(formalization(grounded_arithmetic), [
    zero/1,
    equal_to/2,
    smaller_than/2,
    add_grounded/3,
    subtract_grounded/3,
    integer_to_recollection/2,
    recollection_to_integer/2,
    incur_cost/1
]).

:- use_module(standard_2_ns_2_4, [
    decompose_three_digit/4,
    compose_three_digit/4
]).

% ============================================================
% Place-value addition within 1000
% ============================================================

%!  add_three_digit(+A, +B, -Sum) is det.
%
%   Add two numbers using three-place decomposition with
%   cascading regrouping (ones→tens→hundreds).

add_three_digit(A, B, Sum) :-
    incur_cost(inference),
    decompose_three_digit(A, HA, TA, OA),
    decompose_three_digit(B, HB, TB, OB),
    integer_to_recollection(10, Ten),
    % Add ones, possibly regroup
    add_grounded(OA, OB, OnesSum),
    regroup_(OnesSum, Ten, FinalOnes, OnesCarry),
    % Add tens + carry, possibly regroup
    add_grounded(TA, TB, TensPartial),
    add_grounded(TensPartial, OnesCarry, TensSum),
    regroup_(TensSum, Ten, FinalTens, TensCarry),
    % Add hundreds + carry
    add_grounded(HA, HB, HundredsPartial),
    add_grounded(HundredsPartial, TensCarry, FinalHundreds),
    % Compose result
    compose_three_digit(FinalHundreds, FinalTens, FinalOnes, Sum).

%% Regroup: if Value ≥ Limit, subtract Limit and carry 1
regroup_(Value, Limit, Remainder, Carry) :-
    (   smaller_than(Value, Limit)
    ->  Remainder = Value, zero(Carry)
    ;   equal_to(Value, Limit)
    ->  zero(Remainder), integer_to_recollection(1, Carry)
    ;   subtract_grounded(Value, Limit, Remainder),
        integer_to_recollection(1, Carry)
    ).


% ============================================================
% Place-value subtraction within 1000
% ============================================================

%!  sub_three_digit(+A, +B, -Difference) is semidet.
%
%   Subtract B from A using place-value decomposition with
%   borrowing (decomposing a ten into ones, a hundred into tens).
%   Handles cascade borrowing: if ones needs to borrow but tens
%   is 0, borrows from hundreds first to populate tens.

sub_three_digit(A, B, Difference) :-
    incur_cost(inference),
    decompose_three_digit(A, HA, TA, OA),
    decompose_three_digit(B, HB, TB, OB),
    integer_to_recollection(10, Ten),
    zero(Zero),
    % Normalize for ones subtraction: if OA < OB, need to borrow
    (   smaller_than(OA, OB)
    ->  % Need to borrow for ones
        (   equal_to(TA, Zero)
        ->  % Tens is 0: cascade borrow from hundreds first
            predecessor_rec(HA, HA1),
            add_grounded(TA, Ten, TA1),  % tens becomes 10
            predecessor_rec(TA1, TA2),   % borrow from tens: 10→9
            add_grounded(OA, Ten, OA2)   % ones gets +10
        ;   % Tens has value, borrow directly
            HA1 = HA,
            predecessor_rec(TA, TA2),
            add_grounded(OA, Ten, OA2)
        )
    ;   % No borrow needed for ones
        HA1 = HA, TA2 = TA, OA2 = OA
    ),
    subtract_grounded(OA2, OB, FinalOnes),
    % Now handle tens subtraction: if TA2 < TB, borrow from hundreds
    (   smaller_than(TA2, TB)
    ->  predecessor_rec(HA1, HA2),
        add_grounded(TA2, Ten, TA3),
        subtract_grounded(TA3, TB, FinalTens)
    ;   subtract_grounded(TA2, TB, FinalTens),
        HA2 = HA1
    ),
    % Subtract hundreds
    subtract_grounded(HA2, HB, FinalHundreds),
    compose_three_digit(FinalHundreds, FinalTens, FinalOnes, Difference).

predecessor_rec(recollection([_|H]), recollection(H)).


% ============================================================
% COBO-style addition (bases first, then ones)
% ============================================================

%!  add_cobo_style(+A, +B, -Sum) is det.
%
%   Add using COBO (Count On Bases and Ones) strategy:
%   1. Add the hundreds of B to A
%   2. Add the tens of B
%   3. Add the ones of B
%   This matches the pattern in sar_add_cobo.pl.

add_cobo_style(A, B, Sum) :-
    incur_cost(inference),
    decompose_three_digit(B, HB, TB, OB),
    % Add hundreds of B
    integer_to_recollection(100, Hundred),
    multiply_and_add_(A, HB, Hundred, Step1),
    % Add tens of B
    integer_to_recollection(10, Ten),
    multiply_and_add_(Step1, TB, Ten, Step2),
    % Add ones of B
    integer_to_recollection(1, One),
    multiply_and_add_(Step2, OB, One, Sum).

multiply_and_add_(Base, Count, Unit, Result) :-
    zero(Zero),
    (   equal_to(Count, Zero)
    ->  Result = Base
    ;   add_grounded(Base, Unit, Next),
        predecessor_rec(Count, NewCount),
        multiply_and_add_(Next, NewCount, Unit, Result)
    ).


% ============================================================
% Decomposition-style subtraction
% ============================================================

%!  sub_decompose_style(+A, +B, -Difference) is det.
%
%   Subtract using decomposition: break subtrahend into parts
%   and subtract each part separately.
%   This matches sar_sub_decomposition.pl pattern.

sub_decompose_style(A, B, Difference) :-
    incur_cost(inference),
    decompose_three_digit(B, HB, TB, OB),
    % Subtract hundreds of B
    integer_to_recollection(100, Hundred),
    subtract_units_(A, HB, Hundred, Step1),
    % Subtract tens of B
    integer_to_recollection(10, Ten),
    subtract_units_(Step1, TB, Ten, Step2),
    % Subtract ones of B
    integer_to_recollection(1, One),
    subtract_units_(Step2, OB, One, Difference).

subtract_units_(Base, Count, Unit, Result) :-
    zero(Zero),
    (   equal_to(Count, Zero)
    ->  Result = Base
    ;   subtract_grounded(Base, Unit, Next),
        predecessor_rec(Count, NewCount),
        subtract_units_(Next, NewCount, Unit, Result)
    ).
