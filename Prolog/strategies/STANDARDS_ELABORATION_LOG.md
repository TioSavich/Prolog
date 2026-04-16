# Standards Elaboration — Ralph Loop Progress Log

## Iteration 1: K.NS.1 — Count to 100 by ones and tens

**Standard:** Indiana K.NS.1 / CCSS K.CC.A.1 + K.CC.A.2

**Learning Components (from LearningCommons KG v1.7.0):**
- Count to 10 by ones
- Count to 100 by ones
- Count to 100 by tens

**VPV Mapping:**
- V: number words one through one hundred; decade words
- P: forward successor iteration; decade-skip iteration; counting-on from arbitrary start
- V': "count", "count by", "count on from", "the next number is", "skip count"

**Module:** `prolog/standards/standard_k_ns_1.pl`
**Tests:** `prolog/tests/standards/test_standard_k_ns_1.pl` — 8/8 pass

**Predicates:**
- `count_by_ones/3` — successor iteration from From to To with trace
- `count_by_tens/3` — decade skip counting with trace
- `count_on_from/4` — count forward from arbitrary start by N steps

**IM Grounding:** Paywalled. Grounded from public IM curriculum descriptions:
Grade K Unit 1 "Math in Our World" — counting collections, one-to-one
correspondence, organizing objects, comparing groups.

**Reflection:**
1. `count_on_from` is the primitive that `sar_add_counting_on.pl` deploys.
   The connection exists but is not yet formally registered.
2. No crisis resolved — counting is the base case before crises exist.
3. Gap to K.NS.2: naming. K.NS.1 produces tally sequences but does not
   name them. The V→V' elaboration step IS the naming practice.
4. Carry detection absent. The automaton cannot distinguish 9→10 from 8→9.
   Place-value structure does not emerge from flat successor iteration —
   it requires either the DPDA mechanism (counting2.pl) or reflection on
   stored traces. This is a finding.

**Builds Toward:** 1.NBT.A.1 (count to 120 from any number)

---

## Iteration 2: K.NS.2 — Write whole numbers 0-20, number words 0-10

**Standard:** Indiana K.NS.2 / CCSS K.CC.A.3
**Module:** `prolog/standards/standard_k_ns_2.pl` — 10/10 tests

Implements design/04 number-word layer as curriculum module. The teacher
teaches names incrementally via `learn_numeral/2`. Names are normative
assignments — the system cannot derive that `recollection([tally,tally])`
is called "two"; it must be taught.

---

## Iteration 3: K.NS.3 — One-to-one correspondence and cardinality

**Standard:** Indiana K.NS.3 / CCSS K.CC.B.4 + K.CC.B.5
**Module:** `prolog/standards/standard_k_ns_3.pl` — 8/8 tests

Cardinality principle: last number said = count. Connects counting (K.NS.1)
to naming (K.NS.2) through the "how many?" question. `count_out` inverts
the direction: name → objects.

**Reflection:** Order independence (same count regardless of arrangement)
is verified structurally but the philosophical point — that the learner
discovers this invariance through surprise — is not captured.

---

## Iteration 4: K.NS.4 — Subitizing

**Standard:** Indiana K.NS.4 (no direct CCSS equivalent)
**Module:** `prolog/standards/standard_k_ns_4.pl` — 8/8 tests

Pattern recognition without counting. Perceptual subitizing (1-4) is O(1)
lookup; conceptual subitizing (5-10) decomposes into sub-groups. Cost gap
between subitizing O(1) and counting O(n) is verified in tests.

**Reflection:** Subitizing is the first "efficiency strategy" — not
crisis-driven but perceptual. The building blocks it provides (recognized
sub-groups) support later conceptual subitizing and addition strategies.

---

## Iteration 5: K.NS.5-6 — Comparing groups and numerals

**Standard:** Indiana K.NS.5 + K.NS.6 / CCSS K.CC.C.6 + K.CC.C.7
**Module:** `prolog/standards/standard_k_ns_5_6.pl` — 9/9 tests

Two comparison strategies: matching (pair objects, check leftovers) and
counting (count both, compare recollections). Both strategies produce
identical results — verified in tests.

---

## Iteration 6: K.NS.7 — Place value (ten as a group of ten ones)

**Standard:** Indiana K.NS.7 / CCSS K.NBT.A.1
**Module:** `prolog/standards/standard_k_ns_7.pl` — 10/10 tests

First place-value standard. Decompose/compose teen numbers as "one ten
and N ones." Connects conceptually to DPDA carry mechanism (counting2.pl)
but the connection is not yet wired — requires reflection mechanism.

**Reflection:** `decompose_teen(20)` returns 1 ten and 10 ones, which
is technically correct for this standard's scope but reveals the
limitation: full place value (2 tens, 0 ones) requires the Grade 1
standard 1.NS.2.

---

## Iteration 7: K.CA.1-3 — Addition, subtraction, decomposition within 10

**Standard:** Indiana K.CA.1-3 / CCSS K.OA.A.1-4
**Module:** `prolog/standards/standard_k_ca_1_3.pl` — 10/10 tests

First operations module. Counting-all addition (combine groups, count
total), take-away subtraction, decomposition into pairs, complement to
make 10. These are the practices that the existing strategy automata
(sar_add_counting_on, etc.) formalize at a more advanced level.

**Reflection:** The counting-all strategy here costs O(A+B). The
transition to counting-on (K.NS.1's count_on_from deployed for
addition) costs O(B). This efficiency gap IS the crisis that drives
strategy elaboration from K to Grade 1.

---

## Summary: Kindergarten Complete

| Standard | Module | Tests | Key Practice |
|----------|--------|-------|-------------|
| K.NS.1   | standard_k_ns_1.pl | 8/8 | Counting (successor iteration) |
| K.NS.2   | standard_k_ns_2.pl | 10/10 | Naming (normative assignment) |
| K.NS.3   | standard_k_ns_3.pl | 8/8 | Cardinality (last count = total) |
| K.NS.4   | standard_k_ns_4.pl | 8/8 | Subitizing (pattern recognition) |
| K.NS.5-6 | standard_k_ns_5_6.pl | 9/9 | Comparison (matching, counting) |
| K.NS.7   | standard_k_ns_7.pl | 10/10 | Place value (ten = group of 10) |
| K.CA.1-3 | standard_k_ca_1_3.pl | 10/10 | Operations (add, subtract, decompose) |
| **Total** | **7 modules** | **63/63** | |

---

## Iteration 8: 1.NS.1 — Count to 120 by ones, fives, tens

**Standard:** Indiana 1.NS.1 / CCSS 1.NBT.A.1
**Module:** `prolog/standards/standard_1_ns_1.pl` — 10/10 tests

New practices: count by fives (skip counting), count backward by ones
(predecessor iteration), count backward by tens. Extended numeral naming
to 120 with compositional English word generation.

**Reflection:** Backward counting is genuinely distinct from forward
counting — the discovery that they are inverses is non-trivial and
belongs to the reflection mechanism, not to the counting engine itself.

---

## Iteration 9: 1.NS.2 — Two-digit place value

**Standard:** Indiana 1.NS.2 / CCSS 1.NBT.B.2
**Module:** `prolog/standards/standard_1_ns_2.pl` — 10/10 tests

Generalizes K.NS.7 from one ten-group (teens) to 0-9 tens (full
two-digit decomposition). `decompose_two_digit/3` and `compose_two_digit/3`
are the foundation for all place-value addition strategies.

---

## Iteration 10: 1.CA.1 — Addition/subtraction strategies within 20

**Standard:** Indiana 1.CA.1 / CCSS 1.OA.C.6
**Module:** `prolog/standards/standard_1_ca_1.pl` — 10/10 tests

Five named strategies explicitly connecting to existing automata:
- `add_counting_on` → sar_add_counting_on.pl
- `add_making_ten` → related to sar_add_rmb.pl
- `sub_decompose_to_ten` → sar_sub_decomposition.pl
- `fact_family` → sar_sub_cobo_missing_addend.pl
- `add_doubles_near` → doubles/near-doubles

All strategies agree on 8+6=14, verified in tests.

---

## Iteration 11: 1.CA.3 — Addition within 100 (place value)

**Standard:** Indiana 1.CA.3 / CCSS 1.NBT.C.4
**Module:** `prolog/standards/standard_1_ca_3.pl` — 8/8 tests

Place-value addition with regrouping. Decomposes addends into tens/ones,
adds each place, regroups when ones ≥ 10. Solves the crisis problem
(38+55=93) that motivates the transition from counting-on to place-value
methods.

**Reflection:** The efficiency gap is now concrete:
- counting-on(38, 55): 55 successor operations
- place-value(38, 55): 2 decompositions + 2 additions + 1 regroup
The crisis is real and measurable.

---

## Summary: Grade 1 Complete

| Standard | Module | Tests | Key Practice |
|----------|--------|-------|-------------|
| 1.NS.1   | standard_1_ns_1.pl | 10/10 | Skip counting, backward counting |
| 1.NS.2   | standard_1_ns_2.pl | 10/10 | Two-digit place value |
| 1.CA.1   | standard_1_ca_1.pl | 10/10 | Named addition strategies |
| 1.CA.3   | standard_1_ca_3.pl | 8/8 | Place-value addition within 100 |
| **Total** | **4 modules** | **38/38** | |

**Cumulative: 11 modules, 101 tests, all passing.**

---

## Iterations 12-16: Grade 2

### 2.NS.1 — Count by 1s, 2s, 5s, 10s, 100s to 1000
**Module:** `standard_2_ns_1.pl` — 2/2
Skip counting by 2s and 100s extends 1.NS.1.

### 2.NS.2/4 — Three-digit place value
**Module:** `standard_2_ns_2_4.pl` — 7/7
Hundred as "group of ten tens" — recursive place-value composition.
Decompose/compose three-digit numbers, expanded form.

### 2.NS.3 — Odd/even
**Module:** `standard_2_ns_3.pl` — 3/3
First exhaustive classification of numbers. Determined by grounded
pairing (remove tallies two at a time).

### 2.NS.5 — Compare three-digit numbers
**Module:** `standard_2_ns_5.pl` — 4/4
Place-by-place comparison (hundreds first, then tens, then ones).

### 2.CA.2 — Add/subtract within 1000 (THE automata connection)
**Module:** `standard_2_ca_2.pl` — 12/12

This is the standard where the existing strategy automata live:
- `add_three_digit` — place-value addition with cascading regrouping
- `sub_three_digit` — place-value subtraction with cascade borrowing
- `add_cobo_style` → maps to `sar_add_cobo.pl`
- `sub_decompose_style` → maps to `sar_sub_decomposition.pl`

**Reflection:** Cascade borrowing (500-123) revealed a real algorithmic
challenge: when ones needs to borrow but tens is 0, the system must
borrow from hundreds first. This is exactly the situation that trips
up children and motivates explicit place-value instruction. The bug
in the initial implementation was the same error children make.

---

## Summary: Grade 2 Complete

| Standard | Module | Tests | Key Practice |
|----------|--------|-------|-------------|
| 2.NS.1   | standard_2_ns_1.pl | 2/2 | Skip counting by 2s, 100s |
| 2.NS.2/4 | standard_2_ns_2_4.pl | 7/7 | Three-digit place value |
| 2.NS.3   | standard_2_ns_3.pl | 3/3 | Odd/even classification |
| 2.NS.5   | standard_2_ns_5.pl | 4/4 | Three-digit comparison |
| 2.CA.2   | standard_2_ca_2.pl | 12/12 | Add/sub within 1000 |
| **Total** | **5 modules** | **28/28** | |

**Cumulative: 16 modules, 129 tests, all passing.**

---

## Iterations 17-20: Grade 3

### 3.CA.3-4 — Multiplication and division models
**Module:** `standard_3_ca_3_4.pl` — 8/8
Equal groups, arrays, repeated addition (multiplication). Partition,
repeated subtraction, mult/div families (division). Connects to
`smr_mult_c2c`, `smr_mult_cbo`, `smr_div_dealing_by_ones`, `smr_div_cbo`.

### 3.CA.5 — Multiply/divide within 100 (strategies)
**Module:** `standard_3_ca_5.pl` — 5/5
Skip counting, derived facts (7×8 = 7×7+7), distributive property
(7×6 = 7×5+7×1), division by inverse. All strategies agree.

### 3.NS.2 — Unit fractions (THE FRACTION CRISIS BOUNDARY)
**Module:** `standard_3_ns_2.pl` — 6/6
Fractions as partitioning records: `fraction(Numerator, Denominator)`.
Unit fractions via `make_unit_fraction`, non-unit via `iterate_unit_fraction`.
Denominators {2,3,4,6,8}.

**Reflection — The Crisis:**
The fraction module works but the representation reveals the crisis
documented in FRACTION_CRISIS_ASSESSMENT.md:
1. Variable base — denominator is a parameter, not a constant
2. The FSM synthesis engine cannot generate fraction strategies from
   whole-number primitives because partitioning is not successor iteration
3. The `fraction/2` term structure is fundamentally different from
   `recollection/1` — fractions are not tally sequences

The module models fraction concepts correctly but does NOT integrate
with the ORR cycle. A learner encountering fractions for the first time
would enter crisis, and the synthesis engine would fail. That failure
is the point.

### 3.NS.5 — Compare fractions
**Module:** `standard_3_ns_5.pl` — 5/5
Same-denominator comparison (direct). Same-numerator comparison
(inverted — larger denominator = smaller fraction). Different
numerator AND denominator: incomparable at Grade 3.

---

## Summary: Grade 3 Complete

| Standard | Module | Tests | Key Practice |
|----------|--------|-------|-------------|
| 3.CA.3-4 | standard_3_ca_3_4.pl | 8/8 | Multiplication/division models |
| 3.CA.5   | standard_3_ca_5.pl | 5/5 | Mult/div strategies within 100 |
| 3.NS.2   | standard_3_ns_2.pl | 6/6 | Unit/non-unit fractions |
| 3.NS.5   | standard_3_ns_5.pl | 5/5 | Fraction comparison |
| **Total** | **4 modules** | **24/24** | |

**Cumulative: 20 modules, 153 tests, all passing.**

---

## Next: Grade 4 — Fraction operations (the deep crisis)
