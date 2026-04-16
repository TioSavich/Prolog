/** <module> Standard 3.NS.2 — Unit fractions and non-unit fractions
 *
 * Indiana: 3.NS.2 — "Model unit fractions as the quantity formed by
 *          1 part when a whole is partitioned into equal parts; model
 *          non-unit fractions as the quantity formed by iterations of
 *          unit fractions. [Denominators limited to 2,3,4,6,8.]" (E)
 * CCSS:    3.NF.A.1 — "Understand a fraction 1/b as the quantity formed
 *                      by 1 part when a whole is partitioned into b
 *                      equal parts."
 *
 * VPV MAPPING:
 *   V  (target vocabulary): "one half", "one third", "three fourths",
 *      "numerator", "denominator", "unit fraction"
 *   P  (practices): partitioning a whole into equal parts; iterating
 *      a unit fraction to build non-unit fractions; identifying the
 *      unit fraction from a partition
 *   V' (metavocabulary): "how many equal parts?", "how many parts
 *      are shaded?", "what is one part called?"
 *
 * THIS IS THE FRACTION CRISIS BOUNDARY.
 *
 * See FRACTION_CRISIS_ASSESSMENT.md for the full analysis. The
 * fundamental problem: fractions introduce a VARIABLE BASE. Whole
 * numbers always operate in base 10 (or base 1 for tallies).
 * Fractions operate in base b where b is the denominator. This
 * means:
 *   1. The partitioning operation is not fixed — it changes with
 *      each fraction.
 *   2. Unit coordination goes from two levels (ones and tens) to
 *      three levels (unit fractions, wholes, and groups of wholes).
 *   3. The FSM synthesis engine cannot generate fraction strategies
 *      from whole-number primitives because the base is a parameter,
 *      not a constant.
 *
 * BRANDOM CONNECTION: Fractions require a vocabulary that is
 *   incommensurable with whole-number vocabulary. "Three fourths"
 *   is not three of anything in the whole-number sense — it is
 *   three iterations of "one fourth", which is itself the result
 *   of partitioning a whole into four equal parts. The
 *   algorithmic elaboration from whole numbers to fractions is
 *   NOT a smooth extension — it is a genuine crisis that requires
 *   reorganization (Steffe's accommodation, not assimilation).
 *
 * REPRESENTATION:
 *   Fractions are represented as fraction(Numerator, Denominator)
 *   where both are recollection structures. This is NOT a ratio
 *   representation — it is a partitioning record: "the whole was
 *   partitioned into Denominator parts, and we have Numerator
 *   of those parts."
 *
 * LIMITATIONS:
 *   - Partitioning is modeled structurally, not spatially. The
 *     embodied act of dividing a physical whole into equal parts
 *     (folding paper, cutting shapes) is absent.
 *   - Denominators limited to {2,3,4,6,8} per the standard.
 *   - The module does not model the crisis itself — only the
 *     fraction concepts. The crisis would appear when the ORR
 *     cycle encounters a fraction problem and the synthesis
 *     engine fails to generate a strategy.
 */

:- module(standard_3_ns_2, [
    make_unit_fraction/2,      % +Denominator, -Fraction
    iterate_unit_fraction/3,   % +UnitFraction, +Count, -Fraction
    partition_whole/2,         % +NumParts, -Parts
    fraction_from_partition/3, % +NumParts, +NumShaded, -Fraction
    valid_denominator/1,       % +Denominator
    fraction_to_pair/3         % +Fraction, -NumInt, -DenInt
]).

:- use_module(formalization(grounded_arithmetic), [
    zero/1,
    successor/2,
    equal_to/2,
    smaller_than/2,
    integer_to_recollection/2,
    recollection_to_integer/2,
    incur_cost/1
]).

%% Valid denominators for Grade 3
valid_denominator(D) :-
    recollection_to_integer(D, DI),
    member(DI, [2, 3, 4, 6, 8]).

% ============================================================
% Unit fractions: 1/b
% ============================================================

%!  make_unit_fraction(+Denominator, -Fraction) is semidet.
%
%   Create a unit fraction 1/b. The whole is partitioned into
%   Denominator equal parts; the fraction represents one part.
%   Fails if Denominator is not a valid Grade 3 denominator.

make_unit_fraction(Denominator, fraction(One, Denominator)) :-
    incur_cost(inference),
    valid_denominator(Denominator),
    integer_to_recollection(1, One).

% ============================================================
% Non-unit fractions by iteration: a/b = a × (1/b)
% ============================================================

%!  iterate_unit_fraction(+UnitFraction, +Count, -Fraction) is semidet.
%
%   Build a non-unit fraction by iterating a unit fraction.
%   3/4 = three iterations of 1/4.
%   Count must be ≤ Denominator (improper fractions not in Grade 3).

iterate_unit_fraction(fraction(One, Den), Count, fraction(Count, Den)) :-
    incur_cost(inference),
    integer_to_recollection(1, ExpectedOne),
    equal_to(One, ExpectedOne),
    % Count must not exceed denominator (no improper fractions yet)
    (   smaller_than(Count, Den) -> true
    ;   equal_to(Count, Den) -> true
    ).

% ============================================================
% Partitioning: divide a whole into equal parts
% ============================================================

%!  partition_whole(+NumParts, -Parts) is det.
%
%   Partition a whole into NumParts equal parts.
%   Returns a list of unit_part atoms.

partition_whole(NumParts, Parts) :-
    incur_cost(inference),
    recollection_to_integer(NumParts, N),
    length(Parts, N),
    maplist(=(unit_part), Parts).

%!  fraction_from_partition(+NumParts, +NumShaded, -Fraction) is semidet.
%
%   Given a whole partitioned into NumParts parts with NumShaded
%   parts selected, produce the fraction representation.

fraction_from_partition(NumParts, NumShaded, fraction(NumShaded, NumParts)) :-
    incur_cost(inference),
    valid_denominator(NumParts),
    (   smaller_than(NumShaded, NumParts) -> true
    ;   equal_to(NumShaded, NumParts) -> true
    ).

% ============================================================
% Utility
% ============================================================

%!  fraction_to_pair(+Fraction, -NumInt, -DenInt) is det.
fraction_to_pair(fraction(Num, Den), NumInt, DenInt) :-
    recollection_to_integer(Num, NumInt),
    recollection_to_integer(Den, DenInt).
