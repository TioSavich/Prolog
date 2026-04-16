/** <module> Standards K.NS.5-6 — Comparing groups and numerals
 *
 * Indiana: K.NS.5 — "Identify whether the number of objects in one group
 *          is greater than, less than, or equal to the number of objects
 *          in another group (e.g., by using matching and counting)."
 *          K.NS.6 — "Compare the values of two numbers from 1 to 20
 *          presented as written numerals."
 * CCSS:    K.CC.C.6 — compare groups by matching and counting
 *          K.CC.C.7 — compare two written numerals (1-10)
 *
 * VPV MAPPING:
 *   V  (target vocabulary): "greater than", "less than", "equal to",
 *      "more", "fewer", "the same number"
 *   P  (practices): matching strategy (pair objects one-to-one, check
 *      for leftovers); counting strategy (count both, compare counts);
 *      numeral comparison (compare written numerals via their quantities)
 *   V' (metavocabulary): "which group has more?", "are they the same?",
 *      "which number is bigger?", "how do you know?"
 *
 * BRANDOM CONNECTION: Comparison introduces incompatibility into the
 *   number vocabulary. "Five is greater than three" is a material
 *   inference with exclusionary force: if five > three, then NOT
 *   five < three, and NOT five = three. This three-way partition
 *   (greater/less/equal) is an XOR structure in the meaning field
 *   (design/01). The learner who masters comparison has acquired
 *   the incompatibility relations that structure the number line.
 *
 * LIMITATIONS:
 *   - The matching strategy (physical pairing) is modeled as
 *     list-structural comparison, not spatial arrangement.
 *   - The module uses grounded_arithmetic comparisons directly.
 *     A more faithful model would have the learner discover
 *     that counting + comparing is equivalent to matching.
 */

:- module(standard_k_ns_5_6, [
    compare_groups/3,      % +GroupA, +GroupB, -Result
    compare_by_matching/3, % +GroupA, +GroupB, -Result
    compare_by_counting/3, % +GroupA, +GroupB, -Result
    compare_numerals/3     % +NameA, +NameB, -Result
]).

:- use_module(formalization(grounded_arithmetic), [
    smaller_than/2,
    greater_than/2,
    equal_to/2,
    incur_cost/1
]).

:- use_module(standard_k_ns_3, [
    count_collection/3
]).

:- use_module(standard_k_ns_2, [
    read_numeral/2
]).

% ============================================================
% K.NS.5: Compare groups of objects
% ============================================================

%!  compare_groups(+GroupA, +GroupB, -Result) is det.
%
%   Compare two groups of objects. Returns one of:
%     greater_than — GroupA has more objects
%     less_than    — GroupA has fewer objects
%     equal_to     — same number of objects
%
%   Uses counting strategy (count both, compare counts).

compare_groups(GroupA, GroupB, Result) :-
    compare_by_counting(GroupA, GroupB, Result).

%!  compare_by_matching(+GroupA, +GroupB, -Result) is det.
%
%   Compare by one-to-one matching: pair objects from each group
%   until one runs out. If A runs out first, A < B. If B runs
%   out first, A > B. If both run out together, A = B.
%
%   This is the concrete matching strategy children use before
%   they can count reliably.

compare_by_matching([], [], equal_to) :-
    incur_cost(inference).
compare_by_matching([], [_|_], less_than) :-
    incur_cost(inference).
compare_by_matching([_|_], [], greater_than) :-
    incur_cost(inference).
compare_by_matching([_|RestA], [_|RestB], Result) :-
    incur_cost(inference),
    compare_by_matching(RestA, RestB, Result).

%!  compare_by_counting(+GroupA, +GroupB, -Result) is det.
%
%   Compare by counting both groups and comparing the counts.
%   More sophisticated than matching — requires cardinality
%   understanding (K.NS.3).

compare_by_counting(GroupA, GroupB, Result) :-
    incur_cost(inference),
    count_collection(GroupA, CountA, _),
    count_collection(GroupB, CountB, _),
    (   equal_to(CountA, CountB)
    ->  Result = equal_to
    ;   smaller_than(CountA, CountB)
    ->  Result = less_than
    ;   Result = greater_than
    ).


% ============================================================
% K.NS.6: Compare written numerals
% ============================================================

%!  compare_numerals(+NameA, +NameB, -Result) is semidet.
%
%   Compare two number words by resolving them to their
%   recollection structures and comparing. Fails if either
%   name is unknown.
%
%   This models K.NS.6: comparing written numerals requires
%   knowing what quantities they represent (K.NS.2) and
%   being able to compare quantities (K.NS.5).

compare_numerals(NameA, NameB, Result) :-
    incur_cost(inference),
    read_numeral(NameA, RecA),
    read_numeral(NameB, RecB),
    (   equal_to(RecA, RecB)
    ->  Result = equal_to
    ;   smaller_than(RecA, RecB)
    ->  Result = less_than
    ;   Result = greater_than
    ).
