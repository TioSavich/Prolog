/** <module> Standard K.NS.7 — Place value: ten as a group of ten ones
 *
 * Indiana: K.NS.7 — "Define and model a 'ten' as a group of ten ones.
 *          Model equivalent forms of whole numbers from 10 to 20 as
 *          groups of tens and ones using objects and drawings." (E)
 * CCSS:    K.NBT.A.1 — "Compose and decompose numbers from 11 to 19
 *                       into ten ones and some further ones."
 *
 * VPV MAPPING:
 *   V  (target vocabulary): "ten", "ones", "a ten and ___ ones",
 *      place-value decomposition language (e.g., "14 is one ten and
 *      four ones")
 *   P  (practices): grouping ten objects into a unit ("making a ten");
 *      decomposing a teen number into a ten-group and leftover ones;
 *      composing a teen number from a ten-group and ones
 *   V' (metavocabulary): "group them by tens", "how many tens?",
 *      "how many ones left over?", "make a ten"
 *
 * LEARNING COMPONENTS: No direct LearningCommons decomposition for
 *   K.NBT.A.1. The standard itself is the component.
 *
 * BRANDOM CONNECTION: Place value is the first genuine algorithmic
 *   elaboration beyond counting. The vocabulary "one ten and four
 *   ones" is STRONGER than "fourteen" — it makes explicit the
 *   internal structure. The practice of grouping (P) transforms
 *   the flat counting vocabulary into the structured place-value
 *   vocabulary. This is PP-sufficiency: mastering the grouping
 *   practice deploys the place-value vocabulary.
 *
 *   This connects to the DPDA carry mechanism in counting2.pl:
 *   when the ones place overflows (9→10), a carry event produces
 *   a new place. K.NS.7 is where the learner discovers that this
 *   carry event has MEANING — it creates a "ten."
 *
 * LIMITATIONS:
 *   - Only handles 10-20 (per the standard). Extension to larger
 *     numbers is 1.NBT / 2.NBT territory.
 *   - "Grouping" is modeled as list partitioning, not physical
 *     manipulation of objects (bundling connecting cubes, etc.).
 *   - The module uses grounded_utils:decompose_base10 for the
 *     structural decomposition but presents results in the
 *     curriculum's "tens and ones" vocabulary.
 *   - The connection to counting2.pl carry events is documented
 *     but not wired — that requires the reflection mechanism.
 */

:- module(standard_k_ns_7, [
    make_ten/2,            % +Ones, -TenGroup
    decompose_teen/3,      % +Number, -Tens, -Ones
    compose_teen/3,        % +Tens, +Ones, -Number
    describe_place_value/2 % +Number, -Description
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

% ============================================================
% Making a ten: group ten ones into a unit
% ============================================================

%!  make_ten(+Ones, -TenGroup) is semidet.
%
%   Given a list of at least 10 tally items, group the first
%   10 into a ten_group and return the remainder.
%   TenGroup = ten_group(TenRec, RemainderRec)
%   where TenRec is recollection of 10 tallies and
%   RemainderRec is whatever is left.
%
%   Fails if fewer than 10 items available.

make_ten(Ones, ten_group(TenRec, RemainderRec)) :-
    incur_cost(inference),
    integer_to_recollection(10, TenRec),
    % Check that Ones has at least 10 tallies
    smaller_than(TenRec, Ones),
    subtract_grounded(Ones, TenRec, RemainderRec).

make_ten(Ones, ten_group(TenRec, RemainderRec)) :-
    incur_cost(inference),
    integer_to_recollection(10, TenRec),
    equal_to(Ones, TenRec),
    zero(RemainderRec).


% ============================================================
% Decompose: teen number → tens and ones
% ============================================================

%!  decompose_teen(+Number, -TensCount, -OnesCount) is semidet.
%
%   Decompose a number (10-20) into its tens and ones.
%   TensCount is the number of complete tens (0 or 1 for 10-20).
%   OnesCount is the remainder.
%
%   Example: decompose_teen(14) → TensCount=1, OnesCount=4
%   (where counts are recollection structures)
%
%   This models "14 is one ten and four ones."

decompose_teen(Number, TensCount, OnesCount) :-
    incur_cost(inference),
    integer_to_recollection(10, Ten),
    (   smaller_than(Number, Ten)
    ->  % Less than 10: zero tens, all ones
        zero(TensCount),
        OnesCount = Number
    ;   % 10 or more: subtract ten, one ten-group
        subtract_grounded(Number, Ten, OnesCount),
        integer_to_recollection(1, TensCount)
    ).


% ============================================================
% Compose: tens and ones → teen number
% ============================================================

%!  compose_teen(+TensCount, +OnesCount, -Number) is det.
%
%   Compose a number from tens-count and ones-count.
%   TensCount should be 0 or 1 (for numbers 0-20).
%
%   Example: compose_teen(1, 4) → 14

compose_teen(TensCount, OnesCount, Number) :-
    incur_cost(inference),
    integer_to_recollection(10, Ten),
    zero(Zero),
    (   equal_to(TensCount, Zero)
    ->  Number = OnesCount
    ;   add_grounded(Ten, OnesCount, Number)
    ).


% ============================================================
% Description: produce place-value description
% ============================================================

%!  describe_place_value(+Number, -Description) is semidet.
%
%   Produce a structured description of the place-value
%   decomposition. Returns a term of the form:
%     place_value(TensInt, OnesInt)
%
%   Example: describe_place_value(rec(14)) → place_value(1, 4)

describe_place_value(Number, place_value(TensInt, OnesInt)) :-
    decompose_teen(Number, TensCount, OnesCount),
    recollection_to_integer(TensCount, TensInt),
    recollection_to_integer(OnesCount, OnesInt).
