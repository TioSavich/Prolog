# Strategy Automata Grounding Status

## Already grounded (6)

These use `grounded_arithmetic` operations (recollection-based) instead of `is/2`:

- `sar_add_counting_on.pl` — foundational addition (iterate successor)
- `sar_sub_counting_back.pl` — foundational subtraction (iterate predecessor)
- `sar_add_cobo.pl` — counting on from bigger (uses grounded comparison + counting)
- `sar_sub_decomposition.pl` — subtraction by decomposition
- `jason.pl` — partitive fractional scheme
- `fraction_semantics.pl` — fraction operations

## Mechanically converted (10) — April 2026

These had `is/2` only for +1, -1, +, -, * operations. Converted to grounded
equivalents via integer_to_recollection round-trip:

- `smr_div_ucr.pl` (2 conversions)
- `smr_div_idp.pl` (2)
- `jason_fsm.pl` (2)
- `smr_mult_commutative_reasoning.pl` (3)
- `smr_mult_cbo.pl` (3)
- `smr_mult_c2c.pl` (6)
- `sar_sub_cobo_missing_addend.pl` (6)
- `sar_sub_chunking_a.pl` (2)
- `sar_sub_chunking_b.pl` (4)
- `counting2.pl` (4)

## Converted with base_decompose_grounded (11) — April 2026

These used `mod`, `//`, `max`, `min`. Converted to use `base_decompose_grounded/4`
(counting PDA stack decomposition) instead of built-in arithmetic:

- `sar_add_rmb.pl` (7 → max/min via comparison, mod via base_decompose)
- `sar_add_rounding.pl` (14 → mod/div via base_decompose, +1/-1 via successor/predecessor)
- `sar_add_chunking.pl` (9 → base decomposition via grounded_utils)
- `sar_sub_rounding.pl` (9 → mirror of addition rounding)
- `sar_sub_sliding.pl` (9 → target calc via base_decompose)
- `sar_sub_cbbo_take_away.pl` (4 → subtrahend decomposition via base_decompose)
- `sar_sub_chunking_c.pl` (8 → base decomposition via grounded_utils)
- `smr_div_cbo.pl` (10 → base conversion division via base_decompose)
- `smr_div_dealing_by_ones.pl` (3 → round-robin mod via base_decompose)
- `smr_mult_dr.pl` (9 → distributive reasoning, heuristic split grounded)
- `counting_on_back.pl` (10 → place-value PDA carry/borrow via successor/predecessor)

## Architectural next step: regenerative partition knowledge base

All 27 automata are now grounded. The `mod`/`//` operations were replaced with
`base_decompose_grounded/4` — which decomposes numbers by counting (repeated
subtraction of the base from the number, counting how many groups fit). This
IS the counting PDA's stack decomposition.

**What this achieved:** zero `is/2` evaluations in any strategy automaton.
Every arithmetic operation routes through recollection-based counting.

**What this did NOT achieve:** the automata still COMPUTE their decompositions
at runtime. A child doing RMB doesn't recompute `28 = 2*10 + 8` every time.
They KNOW it from having counted to 28 before.

### Dependency chain (from LK_RB_Synthesis):
```
Counting → (RMB, COBO) → (Chunking, Rounding, Sliding) → (Subtraction)
         → (C2C) → (Skip Counting) → (Commutative/Distributive, CBO Mult)
         → (Division: Dealing, Accumulation) → (Advanced Division)
```

### Next: partition knowledge base

The counting automaton should assert partition facts as it runs:
- Count to 10 → assert `partition(10, 2, 8)`, `partition(10, 3, 7)`, etc.
- These become experiential knowledge the child REMEMBERS
- Strategic automata query partitions instead of computing decompositions
- The knowledge base should be regenerative and base-flexible (not just base 10)
- Fraction operations (1/7 + 1/5) may require base-7 and base-5 partition tables
