/** <module> Standards K.CA.1-3 — Addition, subtraction, decomposition within 10
 *
 * Indiana: K.CA.1 — "Solve real-world problems that involve addition and
 *          subtraction within 10 using modeling with objects or drawings."
 *          K.CA.2 — "Use objects or drawings to model the decomposition of
 *          numbers less than 10 into pairs in more than one way."
 *          K.CA.3 — "Find the number that makes 10 when added to the given
 *          number for any number from 1 to 9."
 * CCSS:    K.OA.A.1-4
 *
 * VPV MAPPING:
 *   V  (target vocabulary): "add", "subtract", "put together",
 *      "take apart", "makes ten", "how many more to make ten"
 *   P  (practices): counting-all for addition (count both groups
 *      together); counting-all for subtraction (remove and recount);
 *      decomposition (find all pairs summing to N); complement
 *      finding (what + given = 10)
 *   V' (metavocabulary): "how many altogether?", "how many are left?",
 *      "what are all the ways to break apart 5?", "what do you add
 *      to 7 to make 10?"
 *
 * BRANDOM CONNECTION: Addition and subtraction are the first operations
 *   that deploy the counting vocabulary inferentially. "3 + 2 = 5"
 *   is a material inference: anyone who has mastered counting (K.NS.1)
 *   and cardinality (K.NS.3) is entitled to conclude that combining
 *   groups of 3 and 2 produces a group of 5. Decomposition makes
 *   this bidirectional: 5 can be decomposed into 3+2 OR 4+1 OR 5+0.
 *   "Makes ten" is the first constraint-based problem — the learner
 *   must find a specific complement, not just combine or decompose.
 *
 * CONNECTION TO EXISTING AUTOMATA:
 *   The addition here is "counting all" — the predecessor to
 *   sar_add_counting_on.pl. Counting-all enumerates both addends
 *   from scratch (cost: O(A+B)). Counting-on (K.NS.1's count_on_from)
 *   starts from A (cost: O(B)). The crisis that drives the transition
 *   from counting-all to counting-on is the efficiency gap.
 *
 * LIMITATIONS:
 *   - "Real-world problems" (K.CA.1) are not modeled. The module
 *     handles the arithmetic, not the problem comprehension.
 *   - "Objects or drawings" is modeled as list manipulation.
 *   - Decomposition uses a generate-and-test approach that is
 *     complete for numbers ≤ 10 but would not scale to larger
 *     numbers. For K standards, this is appropriate.
 */

:- module(standard_k_ca_1_3, [
    add_objects/3,         % +GroupA, +GroupB, -Total
    subtract_objects/3,    % +Group, +Remove, -Remaining
    decompose_pairs/2,     % +Number, -Pairs
    find_complement_to_ten/2  % +Given, -Complement
]).

:- use_module(formalization(grounded_arithmetic), [
    zero/1,
    successor/2,
    predecessor/2,
    equal_to/2,
    smaller_than/2,
    add_grounded/3,
    subtract_grounded/3,
    integer_to_recollection/2,
    recollection_to_integer/2,
    incur_cost/1
]).

:- use_module(standard_k_ns_3, [
    count_collection/3
]).

% ============================================================
% K.CA.1: Addition within 10 (counting-all strategy)
% ============================================================

%!  add_objects(+GroupA, +GroupB, -Total) is det.
%
%   Add two groups of objects by combining them and counting
%   the total. This is the counting-all strategy: physically
%   put both groups together, then count everything.
%
%   GroupA and GroupB are lists of objects.
%   Total is the count as a recollection.

add_objects(GroupA, GroupB, Total) :-
    incur_cost(inference),
    append(GroupA, GroupB, Combined),
    count_collection(Combined, Total, _).


% ============================================================
% K.CA.1: Subtraction within 10 (take-away strategy)
% ============================================================

%!  subtract_objects(+Group, +Remove, -Remaining) is semidet.
%
%   Subtract by removing objects from a group and counting
%   what remains. Remove is a count (recollection) of how
%   many to take away.
%
%   Fails if trying to remove more than available.

subtract_objects(Group, Remove, Remaining) :-
    incur_cost(inference),
    count_collection(Group, GroupCount, _),
    subtract_grounded(GroupCount, Remove, Remaining).


% ============================================================
% K.CA.2: Decomposition into pairs
% ============================================================

%!  decompose_pairs(+Number, -Pairs) is det.
%
%   Find all ways to decompose Number (a recollection) into
%   pairs (A, B) where A + B = Number and A ≤ B.
%   Returns a list of pair(A, B) terms.
%
%   Example: decompose_pairs(5) → [pair(0,5), pair(1,4), pair(2,3)]

decompose_pairs(Number, Pairs) :-
    incur_cost(inference),
    recollection_to_integer(Number, N),
    findall(
        pair(RecA, RecB),
        (   between(0, N, A),
            B is N - A,
            A =< B,
            integer_to_recollection(A, RecA),
            integer_to_recollection(B, RecB)
        ),
        Pairs
    ).


% ============================================================
% K.CA.3: Find complement to make 10
% ============================================================

%!  find_complement_to_ten(+Given, -Complement) is semidet.
%
%   Given a number from 1 to 9, find what must be added
%   to make 10. This is the first constraint-satisfaction
%   problem: solve Given + ? = 10.
%
%   Fails if Given ≥ 10 or Given ≤ 0.

find_complement_to_ten(Given, Complement) :-
    incur_cost(inference),
    integer_to_recollection(10, Ten),
    % Given must be between 1 and 9
    zero(Zero),
    \+ equal_to(Given, Zero),
    smaller_than(Given, Ten),
    % Complement = 10 - Given
    subtract_grounded(Ten, Given, Complement).
