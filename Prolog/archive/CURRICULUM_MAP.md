# N101 Curriculum Map: Crisis Triggers and Strategy Prerequisites

## What this document is

A mapping of the N101 elementary math curriculum sequence (as taught by Tio Savich)
to the existing strategy automata, with crisis triggers, prerequisites, and an
honest classification of what can be synthesized from primitives versus what requires
teaching. Based on analysis of all strategy automata, March 2026.

## The one genuinely new concept

Beyond successor/predecessor, the only genuinely new primitive the learner must
acquire is **decompose_base10** — the paradox of the one and the many. Ten ones
*are* a ten. This insight is the gateway to every strategy that follows.

Everything marked "make bases" below is an application of this one concept in a
different context: RMB makes bases by transferring, Chunking makes bases by
targeting landmarks, Rounding makes bases by approximating, DR makes bases by
splitting the multiplicand, Decomposition makes bases by regrouping columns.

## "Easy" is defined by inference cost

The meta-interpreter already meters inferences. A strategy is "easy" if it costs
few inferences. The system can discover that 4×10 costs 4 steps while 4×9 costs 36,
and therefore splitting 4×9 into 4×10 - 4×1 is worth doing. No explicit knowledge
of the distributive property is needed — just cost awareness.

---

## Grounding status of existing automata

Before any synthesis work, the automata themselves need cleaning. Many use built-in
Prolog arithmetic (`is/2`, `mod`, `//`) where grounded primitives should be used.

| Status | Count | Files |
|--------|-------|-------|
| Grounded | 4 | sar_add_cobo, sar_sub_decomposition, jason, fraction_semantics |
| Hybrid | 6 | smr_mult_cbo, smr_mult_c2c, smr_div_idp, smr_div_ucr, smr_div_dealing_by_ones, smr_mult_commutative_reasoning |
| Lazy | 16 | Everything else (including counting2, counting_on_back, all chunking variants, rmb, rounding, dr, sliding) |

`sar_add_cobo.pl` proves the grounded pattern works. The lazy files need rewriting
to use `add_grounded/3`, `subtract_grounded/3`, `decompose_base10/3`, etc.

---

## Addition (SAR — Strategic Additive Reasoning)

### 1. Counting All [GIVEN]
- **File:** object_level.pl (primordial)
- **What it does:** enumerate(A) + enumerate(B) + recursive_add
- **Cost:** ~A + B + A inferences (enumerate both, then add)
- **Breaks on:** add(8, 5) with budget 15

### 2. Counting On [MISSING — needs strategy wrapper]
- **Mechanism exists:** counting_on_back.pl (DPDA processes tick events)
- **What it would do:** Start at A, iterate successor B times
- **Cost:** B inferences
- **Crisis trigger:** add(8, 5), budget 15 → Counting All costs ~21, exhausts
- **Prerequisite:** successor (already have it)
- **Classification:** **SYNTHESIZABLE** — iterate successor is a composition of what the learner already has
- **Breaks on:** add(38, 55) with budget 20 → 55 successor calls exceeds budget

### 3. Counting Back [MISSING — needs strategy wrapper]
- **Mechanism exists:** counting_on_back.pl (DPDA processes tock events)
- **What it would do:** Start at A, iterate predecessor B times
- **Cost:** B inferences
- **Classification:** **SYNTHESIZABLE** — mirror of Counting On
- **Note:** Not needed for addition; emerges in subtraction (Step 9)

### 4. RMB — Rearranging to Make Bases
- **File:** sar_add_rmb.pl (LAZY — uses is/2, mod, //, max, min)
- **What it does:** 8+5 → move 2 from 5 to 8 → 10+3 = 13
- **Cost:** ~distance_to_base × 2 + 3
- **Crisis trigger:** add(17, 25), budget 15 → Counting On costs 25, exhausts
- **Prerequisite:** **decompose_base10** (the one new concept)
- **Classification:** **TEACHABLE** — requires the concept of a base; oracle must teach decompose_base10
- **What's taught:** "You can rearrange to make a 10. Move 2 from 5 to 8."

### 5. COBO — Count On by Bases and Ones
- **File:** sar_add_cobo.pl (GROUNDED)
- **What it does:** 38+55 → count on 5 tens (48,58,68,78,88) then 5 ones (89,...,93)
- **Cost:** (B÷10) + (B mod 10) inferences
- **Crisis trigger:** add(38, 55), budget 20 → RMB can't cleanly rearrange 55
- **Prerequisite:** decompose_base10 + Counting On
- **Classification:** **SYNTHESIZABLE** — compose decompose + chunked iteration

### 6. Chunking — Chunk to Make Bases
- **File:** sar_add_chunking.pl (LAZY — uses is/2, mod, //)
- **What it does:** Add bases in one step, then chunk ones to hit 10s landmarks
- **Cost:** 1 (base chunk) + strategic ones cost
- **Crisis trigger:** add(27, 36), budget 15 → COBO costs 9, chunking costs less
- **Prerequisite:** COBO + landmark targeting
- **Classification:** **SYNTHESIZABLE** — compose decompose + bulk base addition + strategic ones

### 7. Rounding and Adjusting
- **File:** sar_add_rounding.pl (LAZY — uses is/2, mod, //)
- **What it does:** Round 38→40, add 40+27 via COBO, subtract 2 to adjust
- **Cost:** round + COBO + adjust
- **Crisis trigger:** add(38, 27), budget 12 → Rounding is more efficient than COBO
- **Prerequisite:** COBO + compensation concept
- **Classification:** **SYNTHESIZABLE** — compose round + COBO + count back
- **Sub-strategy:** Embeds COBO internally

### 8. ABAO — Adding Bases and Adding Ones [MISSING]
- **Not implemented**
- **What it would do:** Decompose both numbers, add bases together, add ones together
- **Crisis trigger:** add(52, 38), tight budget → column addition is fastest
- **Prerequisite:** decompose_base10 + column reasoning
- **Classification:** **SYNTHESIZABLE**

---

## Subtraction (SAR)

### 9. CBBO — Count Back By Ones (Take Away)
- **File:** sar_sub_cbbo_take_away.pl (LAZY — uses is/2, //, mod)
- **What it does:** 13-5 → count back by tens then ones: 13, 3, (no tens) → 12,11,10,9,8
- **Cost:** (S÷10) + (S mod 10) inferences
- **Crisis trigger:** First subtraction problem → unknown_operation crisis
- **Prerequisite:** Counting Back (Step 3) + decompose_base10
- **Classification:** **SYNTHESIZABLE** — compose counting back + base decomposition

### 10. COBO Missing Addend — Count On from Subtrahend
- **File:** sar_sub_cobo_missing_addend.pl (LAZY — uses is/2, =<)
- **What it does:** 13-8 → count up from 8 to 13: 9,10,11,12,13 → distance is 5
- **Cost:** (M-S)÷10 + (M-S) mod 10 inferences
- **Crisis trigger:** subtract(42, 17), budget 15
- **Prerequisite:** Counting On + reframing subtraction as "missing addend"
- **Classification:** **TEACHABLE** — the "think of it as addition" reframe must be taught

### 11-13. Chunking A / B / C
- **Files:** sar_sub_chunking_a/b/c.pl (ALL LAZY)
- **Chunking A:** Backward to known part — decompose S by place value, subtract chunks
- **Chunking B:** Backward by known part — count back by unit chunks
- **Chunking C:** Forward from known part — count up by chunks (variant of missing addend)
- **Note:** Chunking A uses `log` and `^` — the most egregious lazy shortcut
- **Classification:** **SYNTHESIZABLE** — pedagogical variants of CBBO/COBO MA

### 14. Rounding (Subtraction)
- **File:** sar_sub_rounding.pl (LAZY)
- **What it does:** Round both M and S down, subtract, adjust
- **Prerequisite:** Rounding (from addition) + sign-aware compensation
- **Classification:** **SYNTHESIZABLE**

### 15. Sliding — Constant Difference
- **File:** sar_sub_sliding.pl (LAZY)
- **What it does:** 22-19 → slide both by 1 → 23-20 = 3
- **Prerequisite:** The invariant (M+K)-(S+K) = M-S
- **Classification:** **TEACHABLE** — the constant-difference insight must be taught
- **Note:** This is "make bases" applied to the problem structure itself

### 16. Decomposition — Column Subtraction with Borrowing
- **File:** sar_sub_decomposition.pl (GROUNDED)
- **What it does:** Decompose both into tens/ones, subtract columns, borrow if needed
- **Prerequisite:** Place-value column reasoning + regrouping (1 ten = 10 ones)
- **Classification:** **TEACHABLE** — borrowing/regrouping must be taught

---

## Multiplication (SMR — Strategic Multiplicative Reasoning)

### 17. C2C — Coordinating Two Counts
- **File:** smr_mult_c2c.pl (HYBRID)
- **What it does:** 3×4 → count 3 groups of 4: 4, 8, 12
- **Cost:** N × S inferences
- **Crisis trigger:** First multiplication problem → unknown_operation crisis
- **Prerequisite:** Counting, grouping concept
- **Classification:** **SYNTHESIZABLE** — nested iteration

### 18. Strategic Counting [MISSING]
- **Not implemented**
- **What it would do:** Use addition strategies to support C2C with larger numbers
- **Example:** 15×23: instead of 345 individual counts, use COBO to add 23 fifteen times
- **Crisis trigger:** multiply(15, 23), budget 25 → C2C exhausts
- **Classification:** **SYNTHESIZABLE** — compose C2C + addition strategies

### 19. CBO — Conversion to Bases and Ones
- **File:** smr_mult_cbo.pl (HYBRID — one `=\=` comparison)
- **What it does:** Create N groups of S, redistribute into groups of 10
- **Uses grounded ops:** add_grounded, subtract_grounded, successor, comparisons
- **Classification:** **SYNTHESIZABLE** — grouping/regrouping from grounded primitives

### 20. Commutative Reasoning
- **File:** smr_mult_commutative_reasoning.pl (HYBRID)
- **What it does:** Recognize A×B = B×A, pick cheaper direction
- **Classification:** **SYNTHESIZABLE** — meta-strategic cost comparison
- **Note:** First explicitly meta-cognitive strategy (choosing HOW to compute)

### 21. DR — Distributive Reasoning
- **File:** smr_mult_dr.pl (LAZY — uses is/2, mod, //)
- **What it does:** 4×9 = 4×(10-1) = 40-4 = 36
- **Crisis trigger:** multiply(6, 9), budget 18 → C2C costs 54, exhausts
- **Prerequisite:** decompose_base10 + cost awareness
- **Classification:** **DISCOVERABLE VIA COST** — system tries 4×10 (cheap!) and 4×1 (cheap!),
  discovers the decomposition is worth doing. No explicit distributive property needed.
- **Key insight:** "Find a nearby easy problem and adjust" — same pattern as Rounding

---

## Division (SMR)

### 22. CBO — Measurement Division
- **File:** smr_div_cbo.pl (LAZY)
- **What it does:** "How many 7s fit in 56?" → organize by base-10 structure
- **Crisis trigger:** First division problem → unknown_operation crisis
- **Prerequisite:** Base-10 structure + grouping from multiplication
- **Classification:** **SYNTHESIZABLE**

### 23. IDP — Inverse of Distributive Property
- **File:** smr_div_idp.pl (HYBRID)
- **What it does:** 56÷7 → "I know 8×7=56, so the answer is 8"
- **Prerequisite:** Learned multiplication facts
- **Classification:** **TEACHABLE** — requires the insight that division inverts multiplication

### 24. Dealing by Ones — Sharing Division
- **File:** smr_div_dealing_by_ones.pl (HYBRID)
- **What it does:** Share 21 items into 3 groups: deal one at a time round-robin
- **Crisis trigger:** "Share 21 cookies among 3 children" (sharing semantics)
- **Prerequisite:** predecessor + round-robin distribution
- **Classification:** **SYNTHESIZABLE** — direct simulation of dealing

### 25. UCR — Using Commutative Reasoning
- **File:** smr_div_ucr.pl (HYBRID)
- **What it does:** Transform sharing→measurement via commutativity: 12÷3 via 3×?=12
- **Prerequisite:** Commutative reasoning from multiplication + missing-factor framing
- **Classification:** **TEACHABLE** — the sharing→measurement transformation must be taught

---

## Summary: what's missing

| Gap | Type | Effort |
|-----|------|--------|
| Counting On strategy wrapper | Missing automaton | Small (~50 lines) |
| Counting Back strategy wrapper | Missing automaton | Small (~50 lines) |
| ABAO (Adding Bases Adding Ones) | Missing automaton | Medium (~100 lines) |
| Strategic Counting (Mult) | Missing automaton | Medium (~100 lines) |
| 16 lazy automata need grounding | Rewrite | Large (systematic) |
| Prerequisite graph in oracle | Missing logic | Medium |
| Developmental state tracker | Missing architecture | Medium |
| Synthesis search loop | Missing architecture | Medium-large |

## Summary: what exists and works

| Component | Status |
|-----------|--------|
| 21 of 25 strategy automata | Implemented (4 fully grounded) |
| Grounded primitives (successor, predecessor, decompose_base10, add/subtract_grounded) | Complete |
| Meta-interpreter with inference metering | Complete |
| Oracle black-box interface | Complete |
| Crisis detection (resource exhaustion, unknown operation) | Complete |
| DPDA counter mechanism (counting_on_back.pl) | Complete (needs wrapper) |
| Strategy cost model (strategy_runtime_cost/3) | Complete |

---

## The synthesis/teaching boundary

15 of 25 strategies are classified as **synthesizable** — the learner could compose
them from primitives it already has. 10 are classified as **teachable** — they require
a conceptual reframe or new insight from the oracle.

The teachable strategies cluster around four types of insight:
1. **decompose_base10** (the one-and-the-many) — gateway to everything
2. **Reframing** (subtraction as missing addend, division as inverse multiplication)
3. **Invariants** (constant difference for sliding, commutativity for UCR)
4. **Prior knowledge** (multiplication facts for IDP)

The honest claim: the system can synthesize Tier 1 strategies from primitives
and verify them by inference cost. For everything else, the oracle teaches a
concept, and the learner acquires the ability to apply it. Making that boundary
visible — "here I composed something new, here I was taught" — is the formal
analogue of where formalization breaks productively.

---

## Relationship to the "on-rails" demonstration

An on-rails demo would walk through this sequence:
1. Give the system add(3,2) with budget 30 → Counting All succeeds
2. Give add(8,5) with budget 15 → Counting All fails → synthesize Counting On
3. Give add(8,5) again → Counting On succeeds (5 steps)
4. Give add(38,55) with budget 20 → Counting On fails (55 steps) → oracle teaches decompose_base10 → synthesize COBO
5. Give add(38,55) again → COBO succeeds (10 steps)
6. Continue through subtraction, multiplication, division...

Each step either synthesizes from current primitives or reveals a conceptual gap
that requires teaching. The demo would make visible: which transitions are genuine
composition, which are oracle-taught, and where the system reaches the limits of
what it can derive on its own.
