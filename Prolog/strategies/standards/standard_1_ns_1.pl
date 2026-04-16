/** <module> Standard 1.NS.1 — Count to 120 by ones, fives, tens
 *
 * Indiana: 1.NS.1 — "Count to at least 120 by ones, fives, and tens
 *          from any given number. In this range, read and write numerals
 *          and represent a number of objects with a written numeral." (E)
 * CCSS:    1.NBT.A.1 — "Count to 120, starting at any number less than
 *                       120. In this range, read and write numerals and
 *                       represent a number of objects with a written
 *                       numeral."
 *
 * VPV MAPPING:
 *   V  (target vocabulary): number words to 120; decade words to 120;
 *      "skip count by fives"
 *   P  (practices): forward counting by 1s/5s/10s from any start;
 *      backward counting by 1s and 10s; reading and writing numerals
 *      beyond 20
 *   V' (metavocabulary): "count by fives", "count backward", "start
 *      at ___ and count to ___", "what comes before/after"
 *
 * LEARNING COMPONENTS (from LearningCommons KG v1.7.0):
 *   - Count up from zero by 1s to 120
 *   - Count up from zero by 10s to 120
 *   - Count up by 1s from non-zero to 120
 *   - Count up by 10s from non-zero to 120
 *   - Count down by 1s within 120
 *   - Count down by 10s within 120
 *   - Read/write numerals to 120
 *   - Represent objects up to 120 with numeral
 *
 * BUILDS UPON: K.NS.1 (counting to 100), K.NS.2 (naming to 20)
 * BUILDS TOWARD: 1.NBT.B.2 (two-digit place value)
 *
 * BRANDOM CONNECTION: Extending the counting range from 100 to 120
 *   crosses the century boundary — the first time the learner
 *   experiences the place-value system recycling (100→101 mirrors
 *   0→1). Counting by fives introduces a new skip-counting practice
 *   that deploys the "five" vocabulary differently from subitizing
 *   (K.NS.4). Backward counting is genuinely new: predecessor
 *   iteration is a distinct practice from successor iteration, and
 *   the discovery that they are inverses is non-trivial.
 *
 * LIMITATIONS:
 *   - The numeral extension (21-120) uses a simple integer→atom
 *     mapping. English number words have irregular structure
 *     (twenty-one vs. *two-ten-one) that is not modeled.
 *   - Count backward by fives is not in the standard but could
 *     be added. Only backward by 1s and 10s are implemented.
 */

:- module(standard_1_ns_1, [
    count_by_fives/3,          % +From, +To, -Trace
    count_backward_by_ones/3,  % +From, +To, -Trace
    count_backward_by_tens/3,  % +From, +To, -Trace
    teach_numerals_to_120/0
]).

:- use_module(formalization(grounded_arithmetic), [
    successor/2,
    predecessor/2,
    zero/1,
    equal_to/2,
    smaller_than/2,
    greater_than/2,
    add_grounded/3,
    subtract_grounded/3,
    integer_to_recollection/2,
    recollection_to_integer/2,
    incur_cost/1
]).

:- use_module(standard_k_ns_1, [
    stored_trace/4
]).

:- use_module(standard_k_ns_2, [
    learn_numeral/2
]).

:- dynamic stored_trace_1/4.
%% stored_trace_1(+From, +To, +Direction, +Trace)

% ============================================================
% Count by fives (new for Grade 1)
% ============================================================

%!  count_by_fives(+From, +To, -Trace) is semidet.
%
%   Skip-count from From to To by fives. Each step adds five
%   using grounded addition. Fails if To is not reachable
%   from From by exact steps of five.

count_by_fives(From, To, Trace) :-
    incur_cost(inference),
    integer_to_recollection(5, Five),
    count_fives_(From, To, Five, [state(From, start)], RevTrace),
    reverse(RevTrace, Trace),
    assertz(stored_trace_1(From, To, forward_by_fives, Trace)).

count_fives_(Current, Target, _Five, Acc, Acc) :-
    equal_to(Current, Target), !.
count_fives_(Current, Target, Five, Acc, Trace) :-
    smaller_than(Current, Target),
    add_grounded(Current, Five, Next),
    count_fives_(Next, Target, Five, [state(Next, five_step)|Acc], Trace).


% ============================================================
% Count backward by ones (new practice)
% ============================================================

%!  count_backward_by_ones(+From, +To, -Trace) is semidet.
%
%   Count backward from From to To by predecessor iteration.
%   From must be greater than or equal to To.
%   This is a genuinely distinct practice from forward counting.

count_backward_by_ones(From, To, Trace) :-
    incur_cost(inference),
    count_back_ones_(From, To, [state(From, start)], RevTrace),
    reverse(RevTrace, Trace),
    assertz(stored_trace_1(From, To, backward, Trace)).

count_back_ones_(Current, Target, Acc, Acc) :-
    equal_to(Current, Target), !.
count_back_ones_(Current, Target, Acc, Trace) :-
    greater_than(Current, Target),
    predecessor(Current, Prev),
    count_back_ones_(Prev, Target, [state(Prev, predecessor)|Acc], Trace).


% ============================================================
% Count backward by tens
% ============================================================

%!  count_backward_by_tens(+From, +To, -Trace) is semidet.
%
%   Count backward from From to To by subtracting ten at each
%   step. Fails if To is not reachable by exact decade steps.

count_backward_by_tens(From, To, Trace) :-
    incur_cost(inference),
    integer_to_recollection(10, Ten),
    count_back_tens_(From, To, Ten, [state(From, start)], RevTrace),
    reverse(RevTrace, Trace),
    assertz(stored_trace_1(From, To, backward_by_tens, Trace)).

count_back_tens_(Current, Target, _Ten, Acc, Acc) :-
    equal_to(Current, Target), !.
count_back_tens_(Current, Target, Ten, Acc, Trace) :-
    greater_than(Current, Target),
    subtract_grounded(Current, Ten, Prev),
    count_back_tens_(Prev, Target, Ten, [state(Prev, decade_back)|Acc], Trace).


% ============================================================
% Extended numeral teaching (21-120)
% ============================================================

%!  teach_numerals_to_120 is det.
%
%   Extend the numeral naming table from 21 to 120.
%   Uses a systematic naming rule for compositionality
%   (twenty-one, twenty-two, ..., one hundred twenty).

teach_numerals_to_120 :-
    forall(
        between(21, 120, N),
        (   integer_to_recollection(N, Rec),
            make_number_word(N, Word),
            learn_numeral(Rec, Word)
        )
    ).

%% Systematic English number word generation
make_number_word(N, Word) :-
    N >= 100, !,
    Ones is N - 100,
    (   Ones =:= 0
    ->  Word = 'one hundred'
    ;   make_number_word(Ones, OnesWord),
        atomic_list_concat(['one hundred', OnesWord], ' ', Word)
    ).
make_number_word(N, Word) :-
    N >= 20, !,
    Tens is N // 10,
    Ones is N mod 10,
    decade_word(Tens, DecWord),
    (   Ones =:= 0
    ->  Word = DecWord
    ;   ones_word(Ones, OnesWord),
        atomic_list_concat([DecWord, '-', OnesWord], Word)
    ).
make_number_word(N, Word) :-
    ones_word(N, Word).

decade_word(2, twenty).
decade_word(3, thirty).
decade_word(4, forty).
decade_word(5, fifty).
decade_word(6, sixty).
decade_word(7, seventy).
decade_word(8, eighty).
decade_word(9, ninety).

ones_word(1, one).
ones_word(2, two).
ones_word(3, three).
ones_word(4, four).
ones_word(5, five).
ones_word(6, six).
ones_word(7, seven).
ones_word(8, eight).
ones_word(9, nine).
ones_word(10, ten).
ones_word(11, eleven).
ones_word(12, twelve).
ones_word(13, thirteen).
ones_word(14, fourteen).
ones_word(15, fifteen).
ones_word(16, sixteen).
ones_word(17, seventeen).
ones_word(18, eighteen).
ones_word(19, nineteen).
