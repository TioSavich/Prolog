:- module(misconceptions_fraction_batch_7, []).
% Fraction misconceptions — research corpus batch 7/7.
% Native arithmetic layer only. Theoretical annotations as comments:
%   % GROUNDED: TODO — placeholder for future embodied arithmetic layer
%   % SCHEMA: <schema name> — Lakoff & Nunez grounding when applicable
%   % CONNECTS TO: s(comp_nec(unlicensed(...))) — PML operator path
%
% Registration convention (from Task 3 arch fix):
%   test_harness:arith_misconception(Source, Domain, Description,
%       misconceptions_fraction_batch_7:rule_name, Input, Expected).
% Rule predicates do NOT go on the module export list.

:- multifile test_harness:arith_misconception/6.
:- discontiguous test_harness:arith_misconception/6.
:- dynamic test_harness:arith_misconception/6.

% ---- Encodings appended by agent for batch 7 ----

% === row 37440: inverted part-whole for improper fraction ===
% Task: make 14/8 of an 8/8-stick.
% Correct: frac(14,8)
% Error: swaps roles — partitions into 14, selects 8 -> frac(8,14).
% SCHEMA: Measuring Stick — numerator iterates a unit of size 1/denominator.
% GROUNDED: TODO — distinguish partition count vs iteration count.
% CONNECTS TO: s(comp_nec(unlicensed(swap_part_whole_on_improper)))
invert_improper_to_proper(frac(N,D), frac(D,N)) :-
    integer(N), integer(D).

test_harness:arith_misconception(db_row(37440), fraction, invert_improper_to_proper,
    misconceptions_fraction_batch_7:invert_improper_to_proper,
    frac(14,8),
    frac(14,8)).

% === row 37584: interpret area model by smaller subset ===
% Task: read shaded area as a fraction.
% Correct: frac(5,6) (5 shaded of 6)
% Error: focuses on the single unshaded piece -> frac(1,6).
% SCHEMA: Object Collection — counts the salient minority instead of the shaded set.
% GROUNDED: TODO — subtract_grounded(rec(D), rec(N), rec(Complement)).
% CONNECTS TO: s(comp_nec(unlicensed(complement_for_part)))
read_complement_as_part(frac(N,D), frac(C,D)) :-
    C is D - N.

test_harness:arith_misconception(db_row(37584), fraction, read_complement_as_part,
    misconceptions_fraction_batch_7:read_complement_as_part,
    frac(5,6),
    frac(5,6)).

% === row 37693: add numerators and denominators separately ===
% Task: 5/6 + 4/7.
% Correct: frac(59,42) (5*7 + 4*6 over 6*7).
% Error: frac(9,13) — top+top, bottom+bottom.
% SCHEMA: Object Collection — combines two pairs of counts independently.
% GROUNDED: TODO — add_grounded on numerators, add_grounded on denominators.
% CONNECTS TO: s(comp_nec(unlicensed(componentwise_addition)))
add_components_separately(frac(N1,D1)-frac(N2,D2), frac(N,D)) :-
    N is N1 + N2,
    D is D1 + D2.

test_harness:arith_misconception(db_row(37693), fraction, add_components_separately,
    misconceptions_fraction_batch_7:add_components_separately,
    frac(5,6)-frac(4,7),
    frac(59,42)).

% === row 37759: guess equivalent-fraction numerator from superficial pattern ===
% Task: solve 2/6 = X/3 for X.
% Correct: X = 1 (halve both).
% Error: Alan reasons "3 and 3 is 6, plus 2 more of 6 equals 12" -> X = 4.
% SCHEMA: Object Collection — shuffles the visible digits without a scaling schema.
% GROUNDED: TODO — divide_grounded(rec(2), rec(2), rec(1)).
% CONNECTS TO: s(comp_nec(unlicensed(numerical_pattern_guess)))
guess_equivalent_numerator(frac(N,D)-NewD, NewN) :-
    _ = D,  % ignore scaling relationship
    NewN is N + NewD - 1.  % Alan's arithmetic path yielding 4 for 2/6=?/3

test_harness:arith_misconception(db_row(37759), fraction, guess_equivalent_numerator,
    misconceptions_fraction_batch_7:guess_equivalent_numerator,
    frac(2,6)-3,
    1).

% === row 37830: iterate non-unit fraction by scaling both N and D ===
% Task: iterate frac(2,5) four times -> frac(8,5).
% Correct: frac(8,5) (only numerator scales).
% Error: Kylie multiplies both -> frac(8,20).
% SCHEMA: Measuring Stick — iteration scales count-of-parts but not size-of-part.
% GROUNDED: TODO — multiply_grounded(rec(N), rec(K), rec(NK)).
% CONNECTS TO: s(comp_nec(unlicensed(scale_both_terms_on_iteration)))
iterate_scales_both(frac(N,D)-K, frac(NK,DK)) :-
    NK is N * K,
    DK is D * K.

test_harness:arith_misconception(db_row(37830), fraction, iterate_scales_both,
    misconceptions_fraction_batch_7:iterate_scales_both,
    frac(2,5)-4,
    frac(8,5)).

% === row 37857: same-numerator comparison by denominator magnitude ===
% Task: compare frac(4,15) and frac(4,10).
% Correct: second (frac(4,10) > frac(4,15)).
% Error: student says first (larger denominator = larger fraction).
% SCHEMA: Object Collection — reads denominators as whole-number magnitudes.
% GROUNDED: TODO — cross_product_grounded would reveal inversion.
% CONNECTS TO: s(comp_nec(unlicensed(denominator_as_magnitude)))
same_numerator_by_denominator(frac(N1,D1)-frac(N2,D2), Winner) :-
    N1 == N2,
    (D1 > D2 -> Winner = first ; Winner = second).

test_harness:arith_misconception(db_row(37857), fraction, same_numerator_by_denominator,
    misconceptions_fraction_batch_7:same_numerator_by_denominator,
    frac(4,15)-frac(4,10),
    second).

% === row 37872: sketch-based componentwise addition ===
% Task: 2/5 + 1/3.
% Correct: frac(11,15).
% Error: student says frac(3,8) (combines shaded counts and total counts).
% SCHEMA: Object Collection — combines two sketches as loose pieces.
% GROUNDED: TODO — add_grounded across two partitions without common unit.
% CONNECTS TO: s(comp_nec(unlicensed(sketch_count_addition)))
sketch_count_addition(frac(N1,D1)-frac(N2,D2), frac(N,D)) :-
    N is N1 + N2,
    D is D1 + D2.

test_harness:arith_misconception(db_row(37872), fraction, sketch_count_addition,
    misconceptions_fraction_batch_7:sketch_count_addition,
    frac(2,5)-frac(1,3),
    frac(11,15)).

% === row 37906: compare unit fractions by denominator as whole numbers ===
% Task: compare frac(1,3) and frac(1,4).
% Correct: first (1/3 > 1/4).
% Error: student says 1/3 < 1/4 "because 3 is less than 4".
% SCHEMA: Object Collection — denominator treated as the fraction's size.
% GROUNDED: TODO — inverse_relation_grounded on unit fractions.
% CONNECTS TO: s(comp_nec(unlicensed(whole_number_order_on_denominator)))
unit_fraction_by_denominator(frac(N1,D1)-frac(N2,D2), Winner) :-
    N1 == 1, N2 == 1,
    (D1 > D2 -> Winner = first ; Winner = second).

test_harness:arith_misconception(db_row(37906), fraction, unit_fraction_by_denominator,
    misconceptions_fraction_batch_7:unit_fraction_by_denominator,
    frac(1,3)-frac(1,4),
    first).

% === row 37918: compare fractions by numerator only ===
% Task: compare frac(4,9) and frac(3,4).
% Correct: second (3/4 > 4/9).
% Error: student says first because "4 is bigger than 3".
% SCHEMA: Object Collection — reads only the numerator.
% GROUNDED: TODO — cross_product_grounded.
% CONNECTS TO: s(comp_nec(unlicensed(numerator_only_comparison)))
compare_by_numerator_only(frac(N1,D1)-frac(N2,D2), Winner) :-
    _ = D1, _ = D2,
    (N1 > N2 -> Winner = first ; Winner = second).

test_harness:arith_misconception(db_row(37918), fraction, compare_by_numerator_only,
    misconceptions_fraction_batch_7:compare_by_numerator_only,
    frac(4,9)-frac(3,4),
    second).

% === row 38006: shaded-region addition by whole-number count ===
% Task: add shaded 1/3 of one circle and shaded 1/4 of another.
% Correct: frac(7,12).
% Error: student counts "one plus one" = 2 (ignores fractional size).
% SCHEMA: Object Collection — treats shaded regions as unit objects.
% GROUNDED: TODO — add_grounded after conversion to common unit.
% CONNECTS TO: s(comp_nec(unlicensed(shaded_count_as_whole)))
count_shaded_regions(frac(N1,_)-frac(N2,_), S) :-
    S is N1 + N2.

test_harness:arith_misconception(db_row(38006), fraction, count_shaded_regions,
    misconceptions_fraction_batch_7:count_shaded_regions,
    frac(1,3)-frac(1,4),
    frac(7,12)).

% === row 38131: unit-fraction magnitude by denominator ===
% Task: compare frac(1,8) and frac(1,6).
% Correct: second (1/6 > 1/8).
% Error: student says first because "8 is greater than 6".
% SCHEMA: Object Collection — denominator as magnitude of whole.
% GROUNDED: TODO — inverse_relation_grounded.
% CONNECTS TO: s(comp_nec(unlicensed(denominator_whole_number_order)))
bigger_denominator_bigger(frac(1,D1)-frac(1,D2), Winner) :-
    (D1 > D2 -> Winner = first ; Winner = second).

test_harness:arith_misconception(db_row(38131), fraction, bigger_denominator_bigger,
    misconceptions_fraction_batch_7:bigger_denominator_bigger,
    frac(1,8)-frac(1,6),
    second).

% === row 38344: unit fraction as iteration count ===
% Task: compare frac(1,7) and frac(1,6).
% Correct: second (1/6 > 1/7).
% Error: Isaac says 1/7 bigger because "7 slices" > "6 slices".
% SCHEMA: Object Collection — more partitions = more stuff.
% GROUNDED: TODO — inverse_relation_grounded.
% CONNECTS TO: s(comp_nec(unlicensed(iteration_count_as_magnitude)))
iteration_count_as_magnitude(frac(1,D1)-frac(1,D2), Winner) :-
    (D1 > D2 -> Winner = first ; Winner = second).

test_harness:arith_misconception(db_row(38344), fraction, iteration_count_as_magnitude,
    misconceptions_fraction_batch_7:iteration_count_as_magnitude,
    frac(1,7)-frac(1,6),
    second).

% === row 38373: compare unit fractions by unshaded complement ===
% Task: compare frac(1,3) and frac(1,2).
% Correct: second (1/2 > 1/3).
% Error: student says 1/3 bigger because "two more pieces left".
% SCHEMA: Object Collection — counts leftover pieces, not the part itself.
% GROUNDED: TODO — complement_grounded distinct from size_of_part.
% CONNECTS TO: s(comp_nec(unlicensed(complement_count_as_size)))
more_leftover_is_larger(frac(N1,D1)-frac(N2,D2), Winner) :-
    L1 is D1 - N1,
    L2 is D2 - N2,
    (L1 > L2 -> Winner = first ; Winner = second).

test_harness:arith_misconception(db_row(38373), fraction, more_leftover_is_larger,
    misconceptions_fraction_batch_7:more_leftover_is_larger,
    frac(1,3)-frac(1,2),
    second).

% === row 38402: iterations renamed as n-over-n ===
% Task: iterate frac(1,8) nine times.
% Correct: frac(9,8).
% Error: student names it "nine ninths" -> frac(9,9).
% SCHEMA: Object Collection — iteration count overwrites the original unit.
% GROUNDED: TODO — preserve_unit_grounded across iteration.
% CONNECTS TO: s(comp_nec(unlicensed(iteration_count_overwrites_denominator)))
iterations_rewrite_denominator(frac(N,_)-K, frac(NK,NK)) :-
    NK is N * K.

test_harness:arith_misconception(db_row(38402), fraction, iterations_rewrite_denominator,
    misconceptions_fraction_batch_7:iterations_rewrite_denominator,
    frac(1,8)-9,
    frac(9,8)).

% === row 38423: numerator interpreted as partition count ===
% Task: produce a bar of frac(5,3) of the unit.
% Correct: partition into 3, iterate 5 times.
% Error: Barbara partitions into 5 parts (numerator becomes denominator).
% SCHEMA: Measuring Stick — the first number read is treated as the partition.
% GROUNDED: TODO — distinguish partition vs iteration role.
% CONNECTS TO: s(comp_nec(unlicensed(numerator_as_partition_count)))
numerator_as_partition(frac(N,D), frac(D,N)) :-
    integer(N), integer(D).

test_harness:arith_misconception(db_row(38423), fraction, numerator_as_partition,
    misconceptions_fraction_batch_7:numerator_as_partition,
    frac(5,3),
    frac(5,3)).

% === row 38477: add unit fractions separately ===
% Task: 1/3 + 1/7.
% Correct: frac(10,21).
% Error: student says frac(2,10) (top+top, bottom+bottom).
% SCHEMA: Object Collection — componentwise addition.
% GROUNDED: TODO — add_grounded with common-unit conversion.
% CONNECTS TO: s(comp_nec(unlicensed(unit_fraction_componentwise_add)))
unit_fraction_add_separately(frac(N1,D1)-frac(N2,D2), frac(N,D)) :-
    N is N1 + N2,
    D is D1 + D2.

test_harness:arith_misconception(db_row(38477), fraction, unit_fraction_add_separately,
    misconceptions_fraction_batch_7:unit_fraction_add_separately,
    frac(1,3)-frac(1,7),
    frac(10,21)).

% === row 38563: same-numerator comparison reversed ===
% Task: compare frac(1,6) and frac(1,9).
% Correct: first (1/6 > 1/9).
% Error: student says second because "9 is greater than 6".
% SCHEMA: Object Collection — natural-number order on denominators.
% GROUNDED: TODO — inverse_relation_grounded.
% CONNECTS TO: s(comp_nec(unlicensed(natural_number_order_on_denominator)))
natural_order_on_common_numerator(frac(N1,D1)-frac(N2,D2), Winner) :-
    N1 == N2,
    (D1 > D2 -> Winner = first ; Winner = second).

test_harness:arith_misconception(db_row(38563), fraction, natural_order_on_common_numerator,
    misconceptions_fraction_batch_7:natural_order_on_common_numerator,
    frac(1,6)-frac(1,9),
    first).

% === row 38732: both-terms-larger judged bigger ===
% Task: compare frac(2,5) and frac(1,2).
% Correct: second (1/2 > 2/5).
% Error: Rachel says first because both numerator and denominator are larger.
% SCHEMA: Object Collection — combines two whole-number comparisons additively.
% GROUNDED: TODO — cross_product_grounded.
% CONNECTS TO: s(comp_nec(unlicensed(both_terms_larger_is_bigger)))
both_larger_is_bigger(frac(N1,D1)-frac(N2,D2), Winner) :-
    (N1 >= N2, D1 >= D2, (N1 > N2 ; D1 > D2) -> Winner = first
    ; N2 >= N1, D2 >= D1, (N2 > N1 ; D2 > D1) -> Winner = second
    ; Winner = undecided).

test_harness:arith_misconception(db_row(38732), fraction, both_larger_is_bigger,
    misconceptions_fraction_batch_7:both_larger_is_bigger,
    frac(2,5)-frac(1,2),
    second).

% === row 38805: fraction addition numerator+numerator / denom+denom ===
% Task: add two fractions by flawed rule.
% Correct: standard fraction addition.
% Error: Stefan uses "num plus num, denom plus denom".
% SCHEMA: Object Collection — componentwise again (distinct source from 37693).
% GROUNDED: TODO — add_grounded after common-unit conversion.
% CONNECTS TO: s(comp_nec(unlicensed(stefan_rule_componentwise)))
stefan_rule_componentwise(frac(N1,D1)-frac(N2,D2), frac(N,D)) :-
    N is N1 + N2,
    D is D1 + D2.

test_harness:arith_misconception(db_row(38805), fraction, stefan_rule_componentwise,
    misconceptions_fraction_batch_7:stefan_rule_componentwise,
    frac(1,2)-frac(1,3),
    frac(5,6)).

% === row 38868: fractions with same missing-piece count judged equal ===
% Task: compare frac(5,6) and frac(7,8).
% Correct: second (7/8 > 5/6).
% Error: student says equal because each lacks exactly one piece.
% SCHEMA: Object Collection — counts missing pieces, not their size.
% GROUNDED: TODO — complement_magnitude_grounded.
% CONNECTS TO: s(comp_nec(unlicensed(equal_missing_count_is_equal)))
equal_missing_count_is_equal(frac(N1,D1)-frac(N2,D2), Result) :-
    L1 is D1 - N1,
    L2 is D2 - N2,
    (L1 == L2 -> Result = equal
    ; L1 < L2 -> Result = first
    ; Result = second).

test_harness:arith_misconception(db_row(38868), fraction, equal_missing_count_is_equal,
    misconceptions_fraction_batch_7:equal_missing_count_is_equal,
    frac(5,6)-frac(7,8),
    second).

% === row 38960: order unit fractions by whole-number denominator ===
% Task: fill inequality between 1/8 and 1/5.
% Correct: 1/8 < 1/5.
% Error: student uses '>' because 8 > 5.
% SCHEMA: Object Collection — natural-number ordering imported wholesale.
% GROUNDED: TODO — inverse_relation_grounded.
% CONNECTS TO: s(comp_nec(unlicensed(denominator_natural_order_sign)))
inequality_by_denominator(frac(N1,D1)-frac(N2,D2), Sign) :-
    N1 == N2,
    (D1 > D2 -> Sign = '>' ; D1 < D2 -> Sign = '<' ; Sign = '=').

test_harness:arith_misconception(db_row(38960), fraction, inequality_by_denominator,
    misconceptions_fraction_batch_7:inequality_by_denominator,
    frac(1,8)-frac(1,5),
    '<').

% === row 39095: partial simplification ===
% Task: simplify frac(42,60) to lowest terms.
% Correct: frac(7,10).
% Error: student stops at a shared factor short of the GCD, e.g. frac(21,30).
% SCHEMA: Measuring Stick — stops scaling before the minimal unit.
% GROUNDED: TODO — gcd_grounded.
% CONNECTS TO: s(comp_nec(unlicensed(incomplete_reduction)))
partial_simplification(frac(N,D), frac(Ns,Ds)) :-
    Ns is N // 2,
    Ds is D // 2.

test_harness:arith_misconception(db_row(39095), fraction, partial_simplification,
    misconceptions_fraction_batch_7:partial_simplification,
    frac(42,60),
    frac(7,10)).

% === row 39179: divide by fraction as multiply by numerator ===
% Task: 63 divided by frac(9,5).
% Correct: 63 * 5/9 = 35.
% Error: student computes 63 * 9 = 567 (multiplies by numerator, ignores denominator).
% SCHEMA: Object Collection — picks one term and multiplies.
% GROUNDED: TODO — reciprocal_grounded.
% CONNECTS TO: s(comp_nec(unlicensed(multiply_by_numerator_for_divide)))
multiply_by_numerator_for_divide(X-frac(N,_), Y) :-
    Y is X * N.

test_harness:arith_misconception(db_row(39179), fraction, multiply_by_numerator_for_divide,
    misconceptions_fraction_batch_7:multiply_by_numerator_for_divide,
    63-frac(9,5),
    35).

% === row 39321: compare by denominator-minus-numerator gap ===
% Task: compare frac(2,7) and frac(3,7).
% Correct: second (3/7 > 2/7).
% Error: gaps 7-2=5, 7-3=4; larger gap judged smaller, so student picks first as smaller
%        equivalently judges second as larger in this trivial case but misapplies to
%        4/9 vs 5/7 where 9-4=5, 7-5=2, concluding 5/7 is larger via the same rule.
% SCHEMA: Object Collection — gap size as inverse of magnitude.
% GROUNDED: TODO — subtract_grounded on terms, then compare.
% CONNECTS TO: s(comp_nec(unlicensed(gap_as_inverse_magnitude)))
gap_as_inverse_magnitude(frac(N1,D1)-frac(N2,D2), Winner) :-
    G1 is D1 - N1,
    G2 is D2 - N2,
    (G1 < G2 -> Winner = first ; G1 > G2 -> Winner = second ; Winner = equal).

test_harness:arith_misconception(db_row(39321), fraction, gap_as_inverse_magnitude,
    misconceptions_fraction_batch_7:gap_as_inverse_magnitude,
    frac(2,7)-frac(3,7),
    second).

% === row 39594: larger denominator read as larger size ===
% Task: compare unit-measure with denominator as size.
% Correct: 1/6 < 1/5 (six hand-spans means smaller hand).
% Error: student says the hand that measured as "six" is bigger because 6 > 5.
% SCHEMA: Object Collection — count treated as size of the unit.
% GROUNDED: TODO — inverse_relation_grounded on measure units.
% CONNECTS TO: s(comp_nec(unlicensed(count_as_unit_size)))
count_as_unit_size(frac(1,D1)-frac(1,D2), Winner) :-
    (D1 > D2 -> Winner = first ; Winner = second).

test_harness:arith_misconception(db_row(39594), fraction, count_as_unit_size,
    misconceptions_fraction_batch_7:count_as_unit_size,
    frac(1,6)-frac(1,5),
    second).

% === row 39699: unit fraction of discrete set = denominator ===
% Task: 1/3 of 12 marbles.
% Correct: 4.
% Error: student circles 3 marbles (takes the denominator as the count).
% SCHEMA: Object Collection — denominator read as cardinal of the share.
% GROUNDED: TODO — divide_grounded(rec(12), rec(3), rec(4)).
% CONNECTS TO: s(comp_nec(unlicensed(denominator_as_share_count)))
denominator_as_share_count(frac(_,D)-_Total, D).

test_harness:arith_misconception(db_row(39699), fraction, denominator_as_share_count,
    misconceptions_fraction_batch_7:denominator_as_share_count,
    frac(1,3)-12,
    4).

% === row 39735: equivalent fraction by halving num and doubling denom ===
% Task: find a fraction equivalent to frac(18,20).
% Correct: any frac(k*9, k*10), e.g., frac(9,10).
% Error: participant writes frac(9,40) (halve numerator, double denominator).
% SCHEMA: Measuring Stick — two inverse scalings mistaken for one.
% GROUNDED: TODO — ensure both terms scale by the same factor.
% CONNECTS TO: s(comp_nec(unlicensed(inverse_scaling_on_terms)))
halve_num_double_denom(frac(N,D), frac(Nh,Dd)) :-
    Nh is N // 2,
    Dd is D * 2.

test_harness:arith_misconception(db_row(39735), fraction, halve_num_double_denom,
    misconceptions_fraction_batch_7:halve_num_double_denom,
    frac(18,20),
    frac(9,10)).

% === row 39769: "K times" interpreted as adding K/D ===
% Task: 2/7 of money taken three times -> multiplication.
% Correct: 3 * frac(2,7) = frac(6,7).
% Error: student writes 2/7 + 3/7 = 5/7 (adds K as a new numerator on same denom).
% SCHEMA: Object Collection — "times" collapsed into adding another piece.
% GROUNDED: TODO — multiply_grounded(rec(K), rec(frac(N,D))).
% CONNECTS TO: s(comp_nec(unlicensed(times_as_add_k_over_d)))
times_as_add_k_over_d(frac(N,D)-K, frac(S,D)) :-
    S is N + K.

test_harness:arith_misconception(db_row(39769), fraction, times_as_add_k_over_d,
    misconceptions_fraction_batch_7:times_as_add_k_over_d,
    frac(2,7)-3,
    frac(6,7)).

% === row 39794: larger denominator = smaller fraction, always ===
% Task: compare frac(7,8) and frac(1,2).
% Correct: first (7/8 > 1/2).
% Error: teacher rule "larger denominator => smaller fraction" ignores numerator,
%        picks frac(1,2) as larger because 2 < 8.
% SCHEMA: Object Collection — half-learned inverse rule misapplied.
% GROUNDED: TODO — cross_product_grounded reveals dominance of numerator.
% CONNECTS TO: s(comp_nec(unlicensed(denominator_only_rule)))
denominator_only_rule(frac(N1,D1)-frac(N2,D2), Winner) :-
    _ = N1, _ = N2,
    (D1 < D2 -> Winner = first ; Winner = second).

test_harness:arith_misconception(db_row(39794), fraction, denominator_only_rule,
    misconceptions_fraction_batch_7:denominator_only_rule,
    frac(7,8)-frac(1,2),
    first).

% === row 39898: invert both operands and re-multiply ===
% Task: 125 * frac(1,5).
% Correct: 25.
% Error: student writes frac(1,125) * frac(5,1) = frac(5,125) = frac(1,25), i.e. 1/25.
% SCHEMA: Object Collection — mechanically inverts both, loses the whole.
% GROUNDED: TODO — multiply_grounded(rec(125), rec(1/5), rec(25)).
% CONNECTS TO: s(comp_nec(unlicensed(invert_both_operands)))
invert_both_operands(X-frac(N,D), Y) :-
    Y is (1 * D) / (X * N) * X.  % student's derivation: 1/125 * 5/1 -> 1/25, scaled by X gives X/25

test_harness:arith_misconception(db_row(39898), fraction, invert_both_operands,
    misconceptions_fraction_batch_7:invert_both_operands,
    125-frac(1,5),
    25).

% === row 39986: part-to-part ratio read as part-whole ===
% Task: 1 shaded, 4 unshaded -> fraction shaded.
% Correct: frac(1,5).
% Error: student writes frac(1,4) (shaded over unshaded, not over total).
% SCHEMA: Object Collection — denominator pulled from the other subset.
% GROUNDED: TODO — union_grounded(rec(N), rec(M), rec(N+M)).
% CONNECTS TO: s(comp_nec(unlicensed(part_to_part_for_part_whole)))
part_to_part_ratio(shaded(N)-unshaded(M), frac(N,M)) :-
    integer(N), integer(M).

test_harness:arith_misconception(db_row(39986), fraction, part_to_part_ratio,
    misconceptions_fraction_batch_7:part_to_part_ratio,
    shaded(1)-unshaded(4),
    frac(1,5)).

% === row 40143: confound add-common-denom with multiply ===
% Task: frac(1,2) * frac(2,3).
% Correct: frac(2,6) (i.e., frac(1,3)).
% Error: convert to common denom frac(3,6) * frac(4,6), multiply numerators -> 12,
%        keep common denom 6 -> frac(12,6), reduce to frac(2,1).
% SCHEMA: Measuring Stick — two procedures blended.
% GROUNDED: TODO — multiply_grounded on numerators AND denominators.
% CONNECTS TO: s(comp_nec(unlicensed(multiply_keeps_common_denominator)))
multiply_keeps_common_denominator(frac(N1,D1)-frac(N2,D2), frac(N,D)) :-
    L is D1 * D2,
    A is N1 * D2,
    B is N2 * D1,
    N is A * B,
    D = L.

test_harness:arith_misconception(db_row(40143), fraction, multiply_keeps_common_denominator,
    misconceptions_fraction_batch_7:multiply_keeps_common_denominator,
    frac(1,2)-frac(2,3),
    frac(2,6)).

% === row 40263: distance-from-whole gap strategy ===
% Task: compare frac(1,4) and frac(1,6).
% Correct: first (1/4 > 1/6).
% Error: student says 1/4 bigger because "1 is closer to 4 than to 6" — uses N-to-D
%        distance; smaller gap judged larger.
% SCHEMA: Measuring Stick — confuses distance-to-whole with fractional size.
% GROUNDED: TODO — distance_grounded vs size_of_part_grounded.
% CONNECTS TO: s(comp_nec(unlicensed(distance_to_denominator_as_size)))
closer_numerator_is_larger(frac(N1,D1)-frac(N2,D2), Winner) :-
    G1 is D1 - N1,
    G2 is D2 - N2,
    (G1 < G2 -> Winner = first ; G1 > G2 -> Winner = second ; Winner = equal).

test_harness:arith_misconception(db_row(40263), fraction, closer_numerator_is_larger,
    misconceptions_fraction_batch_7:closer_numerator_is_larger,
    frac(1,4)-frac(1,6),
    first).

% === row 40343: denominator = count of larger constituent (not total) ===
% Task: 1 part pineapple to 3 parts water -> fraction pineapple.
% Correct: frac(1,4).
% Error: student writes frac(1,3) (over the bigger constituent only).
% SCHEMA: Object Collection — denominator read from the adjacent quantity.
% GROUNDED: TODO — union_grounded for mixture totals.
% CONNECTS TO: s(comp_nec(unlicensed(denominator_as_other_constituent)))
denominator_as_other_constituent(part(P)-part(Q), frac(P,T)) :-
    T is P + Q.

test_harness:arith_misconception(db_row(40343), fraction, denominator_as_other_constituent,
    misconceptions_fraction_batch_7:denominator_as_other_constituent,
    part(1)-part(3),
    frac(1,4)).

% === row 40450: componentwise estimate for sum ===
% Task: estimate frac(1,2) + frac(1,3).
% Correct: frac(5,6).
% Error: student says frac(2,5) (adds numerators and denominators).
% SCHEMA: Object Collection — componentwise reflex.
% GROUNDED: TODO — add_grounded after common-unit conversion.
% CONNECTS TO: s(comp_nec(unlicensed(componentwise_estimate)))
componentwise_estimate(frac(N1,D1)-frac(N2,D2), frac(N,D)) :-
    N is N1 + N2,
    D is D1 + D2.

test_harness:arith_misconception(db_row(40450), fraction, componentwise_estimate,
    misconceptions_fraction_batch_7:componentwise_estimate,
    frac(1,2)-frac(1,3),
    frac(5,6)).

% === row 40490: whole line treated as the unit whole ===
% Task: place frac(3,5) on a number line from 0 to L (L=5 here).
% Correct: position = 3/5 = 0.6 (as a value on the line).
% Error: student partitions the entire line into D parts and marks N-th tick,
%        producing position N when L happens to equal D.
% SCHEMA: Measuring Stick — unit interval collapsed onto entire visible line.
% GROUNDED: TODO — locate_unit_interval_grounded.
% CONNECTS TO: s(comp_nec(unlicensed(whole_line_as_unit)))
whole_line_as_unit(frac(N,D)-_L, Position) :-
    Position is N * 1,  % student places at integer N (treats tick count as coordinate)
    _ = D.

test_harness:arith_misconception(db_row(40490), fraction, whole_line_as_unit,
    misconceptions_fraction_batch_7:whole_line_as_unit,
    frac(3,5)-5,
    3).

% =============================================================
% Option B rows — too vague / no concrete wrong numeric answer.
% These register with `skip` as the rule name; the harness will
% classify them as `undefined` and move on.
% =============================================================

test_harness:arith_misconception(db_row(37447), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37457), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37478), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37508), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37516), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37524), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37570), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37639), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37663), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37676), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37778), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37793), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37811), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(37962), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38110), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38195), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38224), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38257), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38279), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38312), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38449), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38553), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38609), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38660), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38667), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38702), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38840), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(38978), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39008), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39061), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39150), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39218), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39367), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39444), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39550), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39609), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39643), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39670), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39815), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39822), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(39887), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40074), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40087), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40115), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40125), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40152), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40192), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40199), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40232), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40374), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40407), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40457), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40538), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40617), fraction, too_vague, skip, none, none).
test_harness:arith_misconception(db_row(40665), fraction, too_vague, skip, none, none).
