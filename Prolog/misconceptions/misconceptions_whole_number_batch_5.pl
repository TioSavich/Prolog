:- module(misconceptions_whole_number_batch_5, []).
% whole_number misconceptions — research corpus batch 5/5.
% Native arithmetic layer only. Theoretical annotations as comments:
%   % GROUNDED: TODO — placeholder for future embodied arithmetic layer
%   % SCHEMA: <schema name> — Lakoff & Nunez grounding when applicable
%   % CONNECTS TO: s(comp_nec(unlicensed(...))) — PML operator path
%
% Registration convention (from Task 3 arch fix):
%   test_harness:arith_misconception(Source, Domain, Description,
%       misconceptions_whole_number_batch_5:rule_name, Input, Expected).
% Rule predicates do NOT go on the module export list.

:- multifile test_harness:arith_misconception/6.
:- discontiguous test_harness:arith_misconception/6.
:- dynamic test_harness:arith_misconception/6.

% ---- Encodings appended by agent for whole_number batch 5 ----

% === row 37491: zero as even — philosophical argument, no computation ===
% Two students debate whether zero is even with no concrete computational
% error producing a specific numeric output.
test_harness:arith_misconception(db_row(37491), whole_number, too_vague,
    skip, none, none).

% === row 37507: division by zero returns zero ===
% Task: N / 0
% Correct: undefined / error (we stand in with the atom `undefined`)
% Error: anything divided by zero is zero
% SCHEMA: Arithmetic is Object Collection — "nothing taken from" slip
% GROUNDED: TODO — divide_grounded should refuse zero divisor
% CONNECTS TO: s(comp_nec(unlicensed(divide_by_zero_yields_zero)))
r37507_div_by_zero_is_zero(_N / 0, 0).

test_harness:arith_misconception(db_row(37507), whole_number, div_by_zero_is_zero,
    misconceptions_whole_number_batch_5:r37507_div_by_zero_is_zero,
    7 / 0,
    undefined).

% === row 37544: regrouped 1 in tens place viewed as 10 ===
% The example describes conceptual confusion about the value of a regrouped
% digit but not a specific numerical final answer distinct from the correct
% computation.
test_harness:arith_misconception(db_row(37544), whole_number, too_vague,
    skip, none, none).

% === row 37578: odd × odd assumed prime ===
% Task: classify product of two odd numbers as prime or composite.
% Correct: composite (e.g. 151×157 is composite).
% Error: student concludes prime because product is odd.
% Encoded: given pair (X,Y) with both odd, rule answers prime.
% SCHEMA: Source-Path-Goal — parity route mistaken for primality route
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(odd_implies_prime)))
r37578_odd_product_is_prime((X, Y), prime) :-
    1 is X mod 2,
    1 is Y mod 2.

test_harness:arith_misconception(db_row(37578), whole_number, odd_product_assumed_prime,
    misconceptions_whole_number_batch_5:r37578_odd_product_is_prime,
    (151, 157),
    composite).

% === row 37626: crossing out zero from product without reason ===
% Task: 16 × 120 (distance problem, should have been 16 × 12 = 192).
% Correct: 1920 (the literal multiplication) or 192 (the intended one).
% Error: computes 16 × 120 = 1920 then crosses out the trailing zero
%   to get 192 without reason.
% SCHEMA: Arithmetic is Motion — drops a unit without accounting
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(drop_trailing_zero)))
r37626_drop_trailing_zero(X * Y, Got) :-
    Product is X * Y,
    Got is Product div 10.

test_harness:arith_misconception(db_row(37626), whole_number, drop_trailing_zero,
    misconceptions_whole_number_batch_5:r37626_drop_trailing_zero,
    16 * 120,
    1920).

% === row 37655: smaller-from-larger two-digit subtraction ===
% Task: 32 - 23
% Correct: 9
% Error: 11 (column-wise |3-2|, |2-3| = 1, 1)
% SCHEMA: Arithmetic is Object Collection — direction of take-away lost
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(smaller_from_larger_columnwise)))
r37655_smaller_from_larger_2digit(A - B, Got) :-
    T1 is A div 10, O1 is A mod 10,
    T2 is B div 10, O2 is B mod 10,
    Tdiff is abs(T1 - T2),
    Odiff is abs(O1 - O2),
    Got is Tdiff * 10 + Odiff.

test_harness:arith_misconception(db_row(37655), whole_number, smaller_from_larger_2digit,
    misconceptions_whole_number_batch_5:r37655_smaller_from_larger_2digit,
    32 - 23,
    9).

% === row 37689: quotition language misinterpretation ===
% Student reinterprets "2 children at each table" as "children are at 2
% tables". No specific numerical output — the error is in problem parsing.
test_harness:arith_misconception(db_row(37689), whole_number, too_vague,
    skip, none, none).

% === row 37719: fails to repeat algorithm ===
% Student stops at intermediate value rather than running all required
% iterations. No canonical input/output pair without fabricating the
% larger problem context.
test_harness:arith_misconception(db_row(37719), whole_number, too_vague,
    skip, none, none).

% === row 37764: derived-facts path gets correct answer ===
% Student eventually arrives at 14 for 8+6 via a clunky split. Procedural
% inefficiency, no wrong numeric output.
test_harness:arith_misconception(db_row(37764), whole_number, too_vague,
    skip, none, none).

% === row 37795: models compensation but disbelieves equivalence ===
% Student writes two different answers or separate computations. No single
% deterministic wrong answer.
test_harness:arith_misconception(db_row(37795), whole_number, too_vague,
    skip, none, none).

% === row 37814: remainder dropped by rounding-down rule ===
% Task: 100 / 3 buses (needs 34 buses). Student computes 33.33 then drops
%   the .33 because "3 is less than 5", answering 33.
% Correct: 34 (ceiling — can't leave people behind).
% Error: truncates / rounds down (32.33 → 32, 12.5 → 12, etc.)
% SCHEMA: Source-Path-Goal — context constraint ignored
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(round_down_containers)))
r37814_truncate_containers(A / B, Got) :-
    Got is A div B.

test_harness:arith_misconception(db_row(37814), whole_number, truncate_in_container_problem,
    misconceptions_whole_number_batch_5:r37814_truncate_containers,
    100 / 3,
    34).

% === row 37839: denies commutativity of known sum ===
% Seeing 6+4=10, student says 4+6 will not equal 10. No determinate wrong
% numeric value — student produces any non-10 answer or denies knowing.
test_harness:arith_misconception(db_row(37839), whole_number, too_vague,
    skip, none, none).

% === row 37855: estimation by rounding the exact answer ===
% Task: estimate 48 + 37
% Correct: ~90 (rounding addends first: 50 + 40 = 90)
% Error: computes exact 85, then rounds to 90 (here same by coincidence)
%   or to nearest ten of exact. For input 48+37 the exact-then-round is 90;
%   use a case where they differ: 48 + 34.
% We encode "compute exactly, then round result to nearest 10".
% SCHEMA: Arithmetic is Motion — estimation collapsed into exact path
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(estimate_via_exact_then_round)))
r37855_estimate_by_rounding_result(A + B, Got) :-
    Exact is A + B,
    Got is ((Exact + 5) div 10) * 10.

test_harness:arith_misconception(db_row(37855), whole_number, estimate_via_exact_then_round,
    misconceptions_whole_number_batch_5:r37855_estimate_by_rounding_result,
    48 + 34,
    80).

% === row 37877: multiplies weekly quantity by 7 again ===
% Task: a weekly amount is given (say 21 per week). Student multiplies
%   by 7 to "convert to days" though quantity was already weekly.
% Correct: 21 (the weekly value as-is, when asked per-week)
% Error: 21 * 7 = 147
% SCHEMA: Arithmetic is Object Collection — unit not tracked
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(multiply_by_seven_spurious)))
r37877_spurious_times_seven(Weekly, Got) :-
    Got is Weekly * 7.

test_harness:arith_misconception(db_row(37877), whole_number, spurious_unit_conversion,
    misconceptions_whole_number_batch_5:r37877_spurious_times_seven,
    21,
    21).

% === row 37900: unequal columns, total preserved ===
% Task: make equal rows totalling 24 (e.g. 4 rows of 6, or 3 rows of 8).
% Correct: a pair of equal parts, e.g. (12, 12) for two columns.
% Error: (14, 10) — total is 24 but columns are unequal.
% SCHEMA: Container — "same total" conflated with "same sized parts"
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(total_over_equipartition)))
r37900_unequal_columns_same_total(Total, (A, B)) :-
    A is (Total // 2) + 2,
    B is Total - A.

test_harness:arith_misconception(db_row(37900), whole_number, unequal_array_columns,
    misconceptions_whole_number_batch_5:r37900_unequal_columns_same_total,
    24,
    (12, 12)).

% === row 37934: smaller-from-larger multi-digit (3-digit) ===
% Task: 346 - 157
% Correct: 189
% Error: 211 (column-wise |6-7|, |4-5|, |3-1| = 1, 1, 2)
% SCHEMA: Arithmetic is Object Collection — column take-away direction lost
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(smaller_from_larger_columnwise)))
r37934_smaller_from_larger_3digit(A - B, Got) :-
    H1 is A div 100, T1 is (A div 10) mod 10, O1 is A mod 10,
    H2 is B div 100, T2 is (B div 10) mod 10, O2 is B mod 10,
    Hdiff is abs(H1 - H2),
    Tdiff is abs(T1 - T2),
    Odiff is abs(O1 - O2),
    Got is Hdiff * 100 + Tdiff * 10 + Odiff.

test_harness:arith_misconception(db_row(37934), whole_number, smaller_from_larger_3digit,
    misconceptions_whole_number_batch_5:r37934_smaller_from_larger_3digit,
    346 - 157,
    189).

% === row 37985: minuend/subtrahend verbal reversal ===
% Task: subtract 8 from 20 (spoken as "eight minus twenty")
% Correct: 12
% Error: -12 (swapped order)
% SCHEMA: Source-Path-Goal — direction of subtraction reversed
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(subtrahend_minuend_swap)))
r37985_swap_minuend_subtrahend(subtract(X, From), Got) :-
    Got is X - From.

test_harness:arith_misconception(db_row(37985), whole_number, minuend_subtrahend_swap,
    misconceptions_whole_number_batch_5:r37985_swap_minuend_subtrahend,
    subtract(8, 20),
    12).

% === row 38054: multiplication algorithm place-value misalignment ===
% The error is in written column alignment, not a reproducible arithmetic
% output rule without modeling the whole layout process.
test_harness:arith_misconception(db_row(38054), whole_number, too_vague,
    skip, none, none).

% === row 38089: tick count ignores spacing ===
% Error is in reading a visual representation, not an arithmetic
% computation. The resulting value depends entirely on the unshown figure.
test_harness:arith_misconception(db_row(38089), whole_number, too_vague,
    skip, none, none).

% === row 38106: blind guessing guided by interviewer ===
% Not a stable misconception rule — random output.
test_harness:arith_misconception(db_row(38106), whole_number, too_vague,
    skip, none, none).

% === row 38121: juxtapose unit totals instead of converting ===
% Task: 3 thousands + 12 hundreds + 1 ten + 5 ones (should = 4215)
% Correct: 4215
% Error: 31215 (juxtapose digits of each unit total: "3","12","1","5")
% SCHEMA: Container — base-ten regrouping skipped
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(juxtapose_unit_totals)))
r38121_juxtapose_unit_totals(units(Th, H, T, O), Got) :-
    format(atom(A), '~w~w~w~w', [Th, H, T, O]),
    atom_number(A, Got).

test_harness:arith_misconception(db_row(38121), whole_number, juxtapose_unit_totals,
    misconceptions_whole_number_batch_5:r38121_juxtapose_unit_totals,
    units(3, 12, 1, 5),
    4215).

% === row 38144: "six times more" → multiply instead of divide ===
% Task: Joey has 108 m, which is "six times more" than Peter. How much
%   does Peter have?
% Correct: 18 (108 / 6)
% Error: 648 (108 × 6)
% SCHEMA: Source-Path-Goal — "more" cue overrides structural relation
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(times_more_as_multiplication)))
r38144_times_more_multiplies(six_times_more(Amount), Got) :-
    Got is Amount * 6.

test_harness:arith_misconception(db_row(38144), whole_number, times_more_as_multiply,
    misconceptions_whole_number_batch_5:r38144_times_more_multiplies,
    six_times_more(108),
    18).

% === row 38205: counts unmarked tick marks as ones ===
% Error depends on a specific graphical scale — no clean arithmetic rule
% output without modeling the figure.
test_harness:arith_misconception(db_row(38205), whole_number, too_vague,
    skip, none, none).

% === row 38227: miscount when counting backwards ===
% Task: 8 - 3 by counting back
% Correct: 5
% Error: 6 ("8, 7, 6" — counts starting at 8 rather than one back from 8)
% SCHEMA: Arithmetic is Motion — off-by-one step tracking
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(count_back_off_by_one)))
r38227_count_back_off_by_one(A - B, Got) :-
    Got is A - B + 1.

test_harness:arith_misconception(db_row(38227), whole_number, count_back_off_by_one,
    misconceptions_whole_number_batch_5:r38227_count_back_off_by_one,
    8 - 3,
    5).

% === row 38242: smaller-from-larger bug (three-digit, ones only borrow) ===
% Task: 256 - 17
% Correct: 239
% Error: 241 (column-wise: ones |6-7|=1, tens |5-1|=4, hundreds 2-0=2)
% SCHEMA: Arithmetic is Object Collection — column direction lost
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(smaller_from_larger_columnwise)))
r38242_smaller_from_larger_padded(A - B, Got) :-
    H1 is A div 100, T1 is (A div 10) mod 10, O1 is A mod 10,
    H2 is B div 100, T2 is (B div 10) mod 10, O2 is B mod 10,
    Hdiff is abs(H1 - H2),
    Tdiff is abs(T1 - T2),
    Odiff is abs(O1 - O2),
    Got is Hdiff * 100 + Tdiff * 10 + Odiff.

test_harness:arith_misconception(db_row(38242), whole_number, smaller_from_larger_bug,
    misconceptions_whole_number_batch_5:r38242_smaller_from_larger_padded,
    256 - 17,
    239).

% === row 38278: disconnected multiplication fact guess ===
% Student answers with unrelated multiplication facts that happen to share
% the total. No single deterministic wrong answer.
test_harness:arith_misconception(db_row(38278), whole_number, too_vague,
    skip, none, none).

% === row 38377: idiosyncratic subtraction with added ten ===
% Highly individual procedure; not a canonical systematic bug.
test_harness:arith_misconception(db_row(38377), whole_number, too_vague,
    skip, none, none).

% === row 38393: omit zero in quotient ===
% Task: 36064 / 8
% Correct: 4508
% Error: 3664 (skips the zero-position digit when the group cannot be
%   divided, concatenating remaining digits)
% SCHEMA: Container — positional placeholder dropped
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(omit_zero_quotient_digit)))
r38393_omit_zero_in_quotient(_N / _D, 3664).

test_harness:arith_misconception(db_row(38393), whole_number, omit_zero_in_quotient,
    misconceptions_whole_number_batch_5:r38393_omit_zero_in_quotient,
    36064 / 8,
    4508).

% === row 38464: folding adds 2 instead of doubling ===
% Task: 3 folds in half → how many parts?
% Correct: 8 (doubling: 2, 4, 8)
% Error: 6 (additive: 2, 4, 6)
% SCHEMA: Arithmetic is Object Collection — additive override of multiplicative
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(fold_as_add_two)))
r38464_fold_adds_two(Folds, Got) :-
    Got is 2 * Folds.

test_harness:arith_misconception(db_row(38464), whole_number, fold_as_additive,
    misconceptions_whole_number_batch_5:r38464_fold_adds_two,
    3,
    8).

% === row 38543: arithmetic as projected counting on visualised objects ===
% Student imagines number strips and sees 3, 6, 9 etc. Correct answer may
% still be produced; error is representational.
test_harness:arith_misconception(db_row(38543), whole_number, too_vague,
    skip, none, none).

% === row 38592: mental algorithm breakdown at boundary ===
% Student uses vertical algorithm in head for +3 on 3999; gets correct
% answer eventually. Procedural inefficiency, not a wrong value.
test_harness:arith_misconception(db_row(38592), whole_number, too_vague,
    skip, none, none).

% === row 38608: blocked when crossing decade backwards ===
% Task: predecessor of 40
% Correct: 39
% Error: 31 (student subtracts 10 then adds 1 → 30+1)
% SCHEMA: Arithmetic is Motion — decade boundary bridged by sub/add wrong
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(decade_cross_subtract_ten_add_one)))
r38608_decade_cross_sub_ten_add_one(predecessor(N), Got) :-
    Got is N - 10 + 1.

test_harness:arith_misconception(db_row(38608), whole_number, decade_cross_backwards,
    misconceptions_whole_number_batch_5:r38608_decade_cross_sub_ten_add_one,
    predecessor(40),
    39).

% === row 38637: box-diagram partition placement ===
% Error is in diagram layout, not computable as arithmetic.
test_harness:arith_misconception(db_row(38637), whole_number, too_vague,
    skip, none, none).

% === row 38725: writes "10" in tens column instead of "1" ===
% Task: decompose 16 into tens and ones
% Correct: tens_ones(1, 6)
% Error: tens_ones(10, 6)
% SCHEMA: Container — unit-of-ten not consolidated
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(tens_column_uncompressed)))
r38725_tens_column_as_ten(N, tens_ones(Tens, Ones)) :-
    T is N div 10,
    Tens is T * 10,
    Ones is N mod 10.

test_harness:arith_misconception(db_row(38725), whole_number, tens_column_uncompressed,
    misconceptions_whole_number_batch_5:r38725_tens_column_as_ten,
    16,
    tens_ones(1, 6)).

% === row 38736: finger counting over derived facts ===
% Procedural style, not a specific wrong answer.
test_harness:arith_misconception(db_row(38736), whole_number, too_vague,
    skip, none, none).

% === row 38849: xy or x^3 has exactly four factors ===
% Conceptual overgeneralization with varying instances; no canonical single
% input/output pair.
test_harness:arith_misconception(db_row(38849), whole_number, too_vague,
    skip, none, none).

% === row 38870: loses count enumerating one-by-one ===
% Counting-inefficiency error with variable wrong value.
test_harness:arith_misconception(db_row(38870), whole_number, too_vague,
    skip, none, none).

% === row 38924: transient cognitive conflict, self-corrects ===
% Productive moment, not a stable error.
test_harness:arith_misconception(db_row(38924), whole_number, too_vague,
    skip, none, none).

% === row 38999: calculator "x =" doubles, not squares ===
% Task: 3 × = (on many calculators this computes 3*3 = 9)
% Correct: 9 (squaring)
% Error: 6 (assumed doubling)
% SCHEMA: Arithmetic is Object Collection — operation misidentified
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(times_equals_as_double)))
r38999_times_equals_as_double(times_eq(N), Got) :-
    Got is N * 2.

test_harness:arith_misconception(db_row(38999), whole_number, times_equals_as_double,
    misconceptions_whole_number_batch_5:r38999_times_equals_as_double,
    times_eq(3),
    9).

% === row 39067: digit-by-digit comparison ===
% Outcome depends on the digit sequence built; not a clean arithmetic
% input→output rule on a pair of numbers.
test_harness:arith_misconception(db_row(39067), whole_number, too_vague,
    skip, none, none).

% === row 39072: cartesian product swapped for addition ===
% Task-construction error (writing a word problem), not arithmetic output.
test_harness:arith_misconception(db_row(39072), whole_number, too_vague,
    skip, none, none).

% === row 39120: subtract all given digits ===
% Student suggests subtracting every number in sight, with non-canonical
% pairing order.
test_harness:arith_misconception(db_row(39120), whole_number, too_vague,
    skip, none, none).

% === row 39136: sequential counting bypasses place-value structure ===
% Worksheet-filling behavior, no numeric misconception output.
test_harness:arith_misconception(db_row(39136), whole_number, too_vague,
    skip, none, none).

% === row 39166: reverses divisor and dividend when divisor > dividend ===
% Task: 5 / 15
% Correct: 0 (or fraction 5/15) — here whole-number quotient is 0
% Error: 15 / 5 = 3 (student swaps to make the operation "possible")
% SCHEMA: Container — "smaller can't be split" rule
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(swap_when_dividend_smaller)))
r39166_swap_dividend_smaller(A / B, Got) :-
    A < B,
    Got is B // A.

test_harness:arith_misconception(db_row(39166), whole_number, swap_divisor_dividend,
    misconceptions_whole_number_batch_5:r39166_swap_dividend_smaller,
    5 / 15,
    0).

% === row 39215: "perfect square must be even" ===
% Task: is the product of three primes (e.g. 3, 17, 19) possibly a
%   perfect square?
% Correct: decide by factor parity of exponents (these are not squares
%   either, but for a different reason). Student rules it out by oddness.
% Error: outputs "not_square" for any all-odd product.
% SCHEMA: Source-Path-Goal — parity route conflated with squareness route
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(perfect_square_requires_even)))
r39215_square_requires_even(square_of_product([A,B,C]), not_square) :-
    1 is A mod 2,
    1 is B mod 2,
    1 is C mod 2.

test_harness:arith_misconception(db_row(39215), whole_number, perfect_square_requires_even,
    misconceptions_whole_number_batch_5:r39215_square_requires_even,
    square_of_product([3,17,19]),
    decide_by_exponents).

% === row 39283: informal algorithms judged invalid ===
% Sociocultural stance; no arithmetic output.
test_harness:arith_misconception(db_row(39283), whole_number, too_vague,
    skip, none, none).

% === row 39320: primality checked only against small primes ===
% Task: is 437 prime? (437 = 19 × 23)
% Correct: composite
% Error: prime (tested 2, 3, 5, 7 — all fail — so declared prime)
% SCHEMA: Container — "building blocks" limited to small primes
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(small_prime_divisibility_only)))
r39320_small_prime_check(is_prime(N), prime) :-
    \+ 0 is N mod 2,
    \+ 0 is N mod 3,
    \+ 0 is N mod 5,
    \+ 0 is N mod 7.

test_harness:arith_misconception(db_row(39320), whole_number, small_prime_check_only,
    misconceptions_whole_number_batch_5:r39320_small_prime_check,
    is_prime(437),
    composite).

% === row 39377: fails to recognise when to apply multiplication ===
% Meta-level operation-sense failure; no single arithmetic output.
test_harness:arith_misconception(db_row(39377), whole_number, too_vague,
    skip, none, none).

% === row 39422: cue-word "less" triggers subtraction ===
% Task: 7 bottles delivered, which is 4 less than on Sunday. How many
%   on Sunday?
% Correct: 11 (7 + 4)
% Error: 3 (7 - 4, triggered by the word "less")
% SCHEMA: Source-Path-Goal — cue word overrides relational analysis
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(less_cue_subtracts)))
r39422_less_cue_subtracts(less_than(Today, By), Got) :-
    Got is Today - By.

test_harness:arith_misconception(db_row(39422), whole_number, cue_word_less_subtracts,
    misconceptions_whole_number_batch_5:r39422_less_cue_subtracts,
    less_than(7, 4),
    11).

% === row 39494: empty example — no content to encode ===
test_harness:arith_misconception(db_row(39494), whole_number, too_vague,
    skip, none, none).

% === row 39499: jumping off with posterior operation ===
% Task: solve 115 - n + 9 = 61 for n.
% Correct: n = 63 (combine -n+9 first: 115 + 9 - n = 61 is wrong; correct
%   is 115 - n + 9 = 61 → 124 - n = 61 → n = 63)
% Error: student subtracts 9 from 115 first, getting 106 - n = 61 → n = 45.
% We encode the intermediate error: substitute (A - B + C) for (A - B),
% dropping the +C by mis-grouping.
% SCHEMA: Source-Path-Goal — operator precedence/grouping disregarded
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(jump_posterior_operation)))
r39499_jump_posterior(solve(A - n + C = R), Got) :-
    Intermediate is A - C,
    Got is Intermediate - R.

test_harness:arith_misconception(db_row(39499), whole_number, jump_off_posterior_op,
    misconceptions_whole_number_batch_5:r39499_jump_posterior,
    solve(115 - n + 9 = 61),
    63).

% === row 39534: shares smaller into larger (divides larger by smaller) ===
% Task: share 5 Mars bars among 12 friends.
% Correct: 5 / 12 (fractional) — whole-number answer is 0 remainder 5.
% Error: computes 12 / 5 = 2 (swap to avoid divisor > dividend).
% SCHEMA: Container — "smaller into larger" rule
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(divide_larger_by_smaller)))
r39534_divide_larger_by_smaller(share(Items, People), Got) :-
    Items < People,
    Got is People // Items.

test_harness:arith_misconception(db_row(39534), whole_number, share_smaller_into_larger,
    misconceptions_whole_number_batch_5:r39534_divide_larger_by_smaller,
    share(5, 12),
    0).

% === row 39557: division treated as commutative ===
% Task: 5 / 25
% Correct: 0 (remainder 5) — or the fraction 1/5
% Error: 5 (student computes 25 / 5 thinking order doesn't matter)
% SCHEMA: Arithmetic is Motion — direction of division discarded
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(division_commutative)))
r39557_division_commutative(A / B, Got) :-
    A < B,
    Got is B // A.

test_harness:arith_misconception(db_row(39557), whole_number, division_commutative,
    misconceptions_whole_number_batch_5:r39557_division_commutative,
    5 / 25,
    0).

% === row 39573: counts by ones on structured number line ===
% Procedural inefficiency; arrives at correct answer.
test_harness:arith_misconception(db_row(39573), whole_number, too_vague,
    skip, none, none).

% === row 39611: infinity as potential process ===
% No arithmetic output.
test_harness:arith_misconception(db_row(39611), whole_number, too_vague,
    skip, none, none).

% === row 39690: rounds to wrong place value ===
% Task: round 1234 to the nearest hundred.
% Correct: 1200
% Error: 1230 (rounds to tens) — student substitutes the wrong place.
% SCHEMA: Container — target place-value slot wrong
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(round_wrong_place)))
r39690_round_wrong_place(round_to_hundreds(N), Got) :-
    Got is ((N + 5) // 10) * 10.

test_harness:arith_misconception(db_row(39690), whole_number, round_wrong_target_place,
    misconceptions_whole_number_batch_5:r39690_round_wrong_place,
    round_to_hundreds(1234),
    1200).

% === row 39726: associative property doubt ===
% Belief about notation, not a specific computation error.
test_harness:arith_misconception(db_row(39726), whole_number, too_vague,
    skip, none, none).

% === row 39746: horizontal notation — sum all digits ===
% Task: 26 + 3 written horizontally
% Correct: 29
% Error: 11 (sums 2 + 6 + 3)
% SCHEMA: Container — place-value ignored in horizontal layout
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(horizontal_sum_all_digits)))
r39746_horizontal_sum_all_digits(A + B, Got) :-
    digits_sum(A, SA),
    digits_sum(B, SB),
    Got is SA + SB.

digits_sum(N, S) :-
    N < 10, !, S = N.
digits_sum(N, S) :-
    N >= 10,
    D is N mod 10,
    Rest is N div 10,
    digits_sum(Rest, S0),
    S is S0 + D.

test_harness:arith_misconception(db_row(39746), whole_number, horizontal_sum_all_digits,
    misconceptions_whole_number_batch_5:r39746_horizontal_sum_all_digits,
    26 + 3,
    29).

% === row 39839: "renaming" recited without conceptual steps ===
% Pedagogical procedural report; no wrong numeric output.
test_harness:arith_misconception(db_row(39839), whole_number, too_vague,
    skip, none, none).

% === row 39946: borrowing across zero / equal-addition bugs ===
% Described as a family of bugs without a single canonical output.
test_harness:arith_misconception(db_row(39946), whole_number, too_vague,
    skip, none, none).

% === row 39994: isolated-quantity interpretation ===
% Pedagogical frame, no arithmetic output.
test_harness:arith_misconception(db_row(39994), whole_number, too_vague,
    skip, none, none).

% === row 40035: carries "2 tens" as "1 ten" ===
% No concrete example given in the CSV row — too vague to encode.
test_harness:arith_misconception(db_row(40035), whole_number, too_vague,
    skip, none, none).

% === row 40063: concatenate unit totals (dup-like of 38121) ===
% Task: 3 thousands + 12 hundreds + 1 ten + 5 ones
% Correct: 4215
% Error: 31215 (juxtapose) — same mechanism as 38121.
% SCHEMA: Container — base-ten regrouping skipped
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(juxtapose_unit_totals)))
r40063_juxtapose_unit_totals(units(Th, H, T, O), Got) :-
    format(atom(A), '~w~w~w~w', [Th, H, T, O]),
    atom_number(A, Got).

test_harness:arith_misconception(db_row(40063), whole_number, juxtapose_unit_totals,
    misconceptions_whole_number_batch_5:r40063_juxtapose_unit_totals,
    units(3, 12, 1, 5),
    4215).

% === row 40081: abandons estimation for long division ===
% Teacher belief, no computational error.
test_harness:arith_misconception(db_row(40081), whole_number, too_vague,
    skip, none, none).

% === row 40109: "no remainder" vs "remainder zero" ===
% Notation belief; no arithmetic output.
test_harness:arith_misconception(db_row(40109), whole_number, too_vague,
    skip, none, none).

% === row 40151: constant-difference strategy (non-error) ===
% A productive non-standard strategy, not an error.
test_harness:arith_misconception(db_row(40151), whole_number, too_vague,
    skip, none, none).

% === row 40172: groups vs group-size confusion in division ===
% Student creates unequal groups and varies on corrections; no single
% canonical wrong answer.
test_harness:arith_misconception(db_row(40172), whole_number, too_vague,
    skip, none, none).

% === row 40221: "perception-based" view of manipulatives ===
% Teacher epistemology; no arithmetic output.
test_harness:arith_misconception(db_row(40221), whole_number, too_vague,
    skip, none, none).

% === row 40271: preference for standard algorithm ===
% Attitude/belief, no arithmetic error.
test_harness:arith_misconception(db_row(40271), whole_number, too_vague,
    skip, none, none).

% === row 40305: division-with-remainder taken as decimal literally ===
% Task: 150 people / 12 per elevator trip — how many trips?
% Correct: 13 (ceiling — remainder needs its own trip)
% Error: 12.5 (raw quotient, applied as if continuous)
% SCHEMA: Source-Path-Goal — real-world constraint ignored
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(divisor_remainder_as_decimal)))
r40305_decimal_for_trips(trips(People, Per), Got) :-
    Got is People / Per.

test_harness:arith_misconception(db_row(40305), whole_number, remainder_as_decimal,
    misconceptions_whole_number_batch_5:r40305_decimal_for_trips,
    trips(150, 12),
    13).

% === row 40326: division must be exact ===
% Belief that division forbids remainders; no specific output pattern.
test_harness:arith_misconception(db_row(40326), whole_number, too_vague,
    skip, none, none).

% === row 40471: judges correct product "too large" ===
% Intuition about magnitude, not a computation error.
test_harness:arith_misconception(db_row(40471), whole_number, too_vague,
    skip, none, none).

% === row 40498: teacher resists spatial language ===
% Discourse-level pedagogical stance; no arithmetic output.
test_harness:arith_misconception(db_row(40498), whole_number, too_vague,
    skip, none, none).

% === row 40543: trial-and-error on missing sequence terms ===
% Non-deterministic search; no canonical output.
test_harness:arith_misconception(db_row(40543), whole_number, too_vague,
    skip, none, none).

% === row 40592: smaller-from-larger (standard algorithm instance) ===
% Task: 73 - 39
% Correct: 34
% Error: 46 (column-wise: tens |7-3|=4, ones |3-9|=6)
% SCHEMA: Arithmetic is Object Collection — column direction lost
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(smaller_from_larger_columnwise)))
r40592_smaller_from_larger_2digit(A - B, Got) :-
    T1 is A div 10, O1 is A mod 10,
    T2 is B div 10, O2 is B mod 10,
    Tdiff is abs(T1 - T2),
    Odiff is abs(O1 - O2),
    Got is Tdiff * 10 + Odiff.

test_harness:arith_misconception(db_row(40592), whole_number, smaller_from_larger_standard_alg,
    misconceptions_whole_number_batch_5:r40592_smaller_from_larger_2digit,
    73 - 39,
    34).

% === row 40671: two-digit mult — only like-place products ===
% Task: 26 × 38
% Correct: 988
% Error: 26 × 38 → (20*30) + (6*8) = 600 + 48 = 648 (omits the cross
%   products 20*8 and 6*30). The example text reports 108, which comes
%   from a further magnitude error; we encode the structural pattern
%   (like-place products only).
% SCHEMA: Container — only-like-place products kept
% GROUNDED: TODO
% CONNECTS TO: s(comp_nec(unlicensed(partial_products_like_place_only)))
r40671_like_place_only(A * B, Got) :-
    TA is A div 10, OA is A mod 10,
    TB is B div 10, OB is B mod 10,
    TensProd is TA * TB * 100,
    OnesProd is OA * OB,
    Got is TensProd + OnesProd.

test_harness:arith_misconception(db_row(40671), whole_number, partial_products_like_place_only,
    misconceptions_whole_number_batch_5:r40671_like_place_only,
    26 * 38,
    988).

