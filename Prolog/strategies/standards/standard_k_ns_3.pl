/** <module> Standard K.NS.3 — One-to-one correspondence and cardinality
 *
 * Indiana: K.NS.3 — "Say the number names in standard order when counting
 *          objects, pairing each object with one and only one number name
 *          and each number name with one and only one object. Understand
 *          that the last number name said describes the number of objects
 *          counted and that the number of objects is the same regardless
 *          of their arrangement or the order in which they were counted.
 *          Count out the number of objects, given a number from 1 to 20."
 * CCSS:    K.CC.B.4 — "Understand the relationship between numbers and
 *                      quantities; connect counting to cardinality."
 *          K.CC.B.4.a — one-to-one correspondence
 *          K.CC.B.4.b — last number = count (cardinality principle)
 *          K.CC.B.4.c — each successive number is one larger
 *          K.CC.B.5 — "Count to answer 'how many?' questions..."
 *
 * VPV MAPPING:
 *   V  (target vocabulary): "how many", cardinal answers ("there are five"),
 *      "the same number", arrangement-independent quantity
 *   P  (practices): one-to-one correspondence (pairing each object with
 *      exactly one count); cardinality principle (last count = total);
 *      counting out (given a number, produce that many objects);
 *      order-independence (re-counting produces same result)
 *   V' (metavocabulary): "the last number you said tells how many",
 *      "one for each", "count out five for me", "does it matter
 *      what order you count them?"
 *
 * LEARNING COMPONENTS (from LearningCommons KG v1.7.0):
 *   K.CC.B.4:
 *   - Connect counting to cardinality
 *   - Connect numbers to quantities or amounts
 *   K.CC.B.5:
 *   - Count "how many?" for 20 things in a line/array/circle
 *   - Count "how many?" for 10 things scattered
 *   - Given a number 1-20, count out that many objects
 *
 * BRANDOM CONNECTION: The cardinality principle is a paradigmatic
 *   material inference — from "I said 'five' last while counting"
 *   to "there are five objects." This inference is not formally
 *   valid (it depends on one-to-one correspondence holding), but
 *   it is materially good: anyone who has mastered counting is
 *   entitled to make it. The order-independence claim is that
 *   this inference is invariant under permutation of the
 *   counting sequence — a symmetry property the learner must
 *   discover.
 *
 * LIMITATIONS:
 *   - "Arrangement" (line, array, circle, scattered) is not modeled.
 *     The automaton counts lists regardless of how objects are
 *     conceptually arranged. The spatial dimension of counting
 *     (tracking which objects have been counted) is absent.
 *   - Order-independence is verified by re-counting a permuted list,
 *     but the philosophical point — that the learner discovers this
 *     invariance through experience — is not captured. The module
 *     checks it; the learner would need to be surprised by it.
 */

:- module(standard_k_ns_3, [
    count_collection/3,    % +Objects, -Count, -Pairing
    how_many/2,            % +Objects, -Name
    count_out/2,           % +Name, -Objects
    cardinality/2,         % +Trace, -Count
    verify_order_independence/2  % +Objects, -Result
]).

:- use_module(formalization(grounded_arithmetic), [
    successor/2,
    zero/1,
    equal_to/2,
    integer_to_recollection/2,
    recollection_to_integer/2,
    incur_cost/1
]).

:- use_module(standard_k_ns_1, [
    count_by_ones/3
]).

:- use_module(standard_k_ns_2, [
    write_numeral/2,
    read_numeral/2,
    numeral_known/2
]).

% ============================================================
% One-to-one correspondence: pair each object with a count
% ============================================================

%!  count_collection(+Objects, -Count, -Pairing) is det.
%
%   Count a list of objects by pairing each with a successive
%   number. Returns the final count (recollection) and the
%   pairing as a list of pair(Object, Recollection) terms.
%
%   This models one-to-one correspondence: each object gets
%   exactly one number, each number gets exactly one object.

count_collection(Objects, Count, Pairing) :-
    incur_cost(inference),
    zero(Start),
    pair_objects(Objects, Start, Count, Pairing).

pair_objects([], Count, Count, []).
pair_objects([Obj|Rest], Current, FinalCount, [pair(Obj, Next)|Pairs]) :-
    successor(Current, Next),
    pair_objects(Rest, Next, FinalCount, Pairs).


% ============================================================
% Cardinality principle: last count = "how many"
% ============================================================

%!  how_many(+Objects, -Name) is semidet.
%
%   The "how many?" question. Counts the objects and returns
%   the number word for the total. This IS the cardinality
%   principle in action: count, take the last number, name it.
%
%   Fails if the count exceeds the taught naming range (the
%   learner cannot answer "how many?" for numbers they cannot
%   name — a genuine developmental constraint).

how_many(Objects, Name) :-
    incur_cost(inference),
    count_collection(Objects, Count, _Pairing),
    write_numeral(Count, Name).

%!  cardinality(+Trace, -Count) is det.
%
%   Extract the cardinality from a counting trace (as produced
%   by count_by_ones). The last state in the trace holds the
%   final count. This makes explicit what the cardinality
%   principle claims: the last number said IS the answer.

cardinality(Trace, Count) :-
    last(Trace, state(Count, _)).


% ============================================================
% Count out: given a number, produce objects
% ============================================================

%!  count_out(+Name, -Objects) is semidet.
%
%   Given a number word, produce a list of that many objects.
%   The inverse of how_many: "give me five" → [o,o,o,o,o].
%
%   Objects are represented as atoms (placeholder tokens).
%   The standard says "count out the number of objects" which
%   is a production task, not just recognition.

count_out(Name, Objects) :-
    incur_cost(inference),
    read_numeral(Name, Count),
    recollection_to_integer(Count, N),
    length(Objects, N),
    maplist(=(object), Objects).


% ============================================================
% Order independence
% ============================================================

%!  verify_order_independence(+Objects, -Result) is det.
%
%   Count the objects in original order and in reversed order.
%   If both counts are equal, Result = same(Count).
%   This is a verification, not a discovery — the learner
%   would need to be surprised by the result.

verify_order_independence(Objects, Result) :-
    incur_cost(inference),
    count_collection(Objects, Count1, _),
    reverse(Objects, Reversed),
    count_collection(Reversed, Count2, _),
    (   equal_to(Count1, Count2)
    ->  Result = same(Count1)
    ;   Result = different(Count1, Count2)  % should never happen
    ).
