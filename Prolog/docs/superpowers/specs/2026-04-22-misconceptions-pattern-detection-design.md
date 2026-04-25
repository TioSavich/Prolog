# Misconceptions Corpus — Pattern Detection Design

**Date:** 2026-04-22  
**Status:** Approved  
**Location in repo:** `misconceptions/` (new directory)

---

## Goal

Detect structural patterns in 3,532 research-literature misconceptions by encoding each
as a Prolog rule and running it against known correct answers. The output is a classified
corpus: each misconception tagged as `wrong_answer`, `loop_detected`, or `undefined`.

This is empirical pattern detection. It does not depend on the embodied grounded
arithmetic (recollection/Peano representation). It uses Prolog's native arithmetic so
the results are stable, reproducible, and independently verifiable.

The theoretical connection to grounded arithmetic and incompatibility semantics is
preserved as annotated comments in the same files. The two tracks do not contaminate
each other.

---

## The "Lights On / Lights Off" Architecture

Every misconception module is written in two stacked layers within the same file:

```prolog
% NATIVE LAYER — runs always, for pattern detection:
fraction_add_wrong(frac(N1,D1), frac(N2,D2), frac(NS,DS)) :-
    NS is N1 + N2,
    DS is D1 + D2.

% GROUNDED LAYER — commented out until axioms are certified:
% fraction_add_wrong_grounded(frac(N1,D1), frac(N2,D2), frac(NS,DS)) :-
%     add_grounded(rec(N1), rec(N2), NS_rec),
%     add_grounded(rec(D1), rec(D2), DS_rec), ...
% Connects to: s(comp_nec(unlicensed(add_numerators_independently)))
% Schema: container-interior vs boundary confusion (Lakoff/Nunez measuring stick)
```

Agents write native Prolog only. Theoretical annotations are stubs (`% GROUNDED: TODO`)
that the project PI fills in or leaves for a later pass. Switching layers means
uncommenting and substituting — the misconception structure doesn't change.

---

## Scope

### Domains included

| Domain | Prolog vocabulary | Status |
|--------|-----------------|--------|
| Fraction (ordering, add, equiv, models) | `grounded_arithmetic` + butterfly pattern | Ready |
| Decimal / whole-number | Existing 27 automata + `grounded_arithmetic` | Ready |
| Measurement (area, perimeter, elapsed time) | New — container schema vocabulary | New in Module 5 |
| Geometry (quadrilateral classification) | `axioms_geometry.pl` | Ready after Module 0 |

### Populations included (from research corpus)

- `population_type IN ('student', 'PST')`
- `grade_band_as_declared` normalized to K–5, plus PST at all grade levels
- High school / undergraduate / calculus / statistics: excluded from encoding, retained
  in a separate `out_of_frame.csv` export for documentation

### Primary sources

1. **ASKTM data** (`Prolog/Prolog/data/`) — Grade 4–5 coded student responses.
   Prototype source. Already has fine-grained coding and one worked encoding
   (`example_butterfly_misconception.pl`).

2. **research_corpus** (`research_corpus/research.db`) — 3,532 `error_instances` rows
   from 2,151 articles across 10 math-ed journals. Main corpus after prototype is proven.

---

## Modules

### Module 0 — Axiom triage (small, fast, no new code)

**Files touched:** `formalization/axioms_geometry.pl`, `formalization/axioms_number_theory.pl`

Tasks:
- Add inline comments to `axioms_geometry.pl` documenting r1–r6:
  - R1: No sides of X are equal
  - R2: No pair of adjacent sides of X are equal
  - R3: No pair of opposite sides of X are equal
  - R4: Non-parallel sides of X are not congruent
  - R5: No pair of opposite sides of X are parallel
  - R6: No angles of X are right angles
- Write 10 entailment test cases (`formalization/tests/test_geometry_entailment.pl`):
  square entails rectangle, rectangle does not entail square, trapezoid does not entail
  parallelogram, etc.
- Add `is_complete(L)` guard to the M5-Direct rule in `axioms_number_theory.pl` to
  prevent spurious proofs from firing on arbitrary lists.

Nothing else. Geometry axioms are sound once commented; the taxonomy encodes Tio's
manuscript argument correctly.

---

### Module 1 — Misconception infrastructure

**New directory:** `misconceptions/`

**Files:**
- `misconceptions/misconceptions_fraction.pl`
- `misconceptions/misconceptions_measurement.pl`
- `misconceptions/misconceptions_geometry.pl`
- `misconceptions/test_harness.pl`
- `misconceptions/README.md`

**The `misconception/4` convention:**

Every misconception gets a registration fact:

```prolog
misconception(
    Source,       % atom: DB row ID (integer) or ASKTM student code (e.g. '06-03')
    Domain,       % atom: fraction | decimal | whole_number | measurement | geometric
    Description,  % atom: short name, e.g. add_numerator_denominator_separately
    Classification % atom: wrong_answer | loop_detected | undefined | pending
).
```

**`test_harness.pl`** runs each registered misconception rule against known correct
inputs and fills in `Classification`. It uses `call_with_inference_limit/3` for
loop detection (bound: 10,000 inferences). Results are written to
`misconceptions/results.csv` on each run.

---

### Module 2 — ASKTM prototype (fraction)

**Source:** `Prolog/Prolog/data/Grade 4_Fine-Grained Coding_Second Pass/` and
`Grade5 _Fine-Grained Coding_Second Pass/`

**Target:** ~20 misconception variants across 4–5 question types.

Questions covered:
- G4Q1: Order 2/3, 3/4, 3/8 (fraction ordering — butterfly variants already exist)
- G4Q3: Write two fractions equivalent to 3/4 (equivalence misconceptions)
- G4Q7: 3 × 1/4 cup of flour (fraction × whole number)
- G5Q1: 1/3 + 2/3 > or < 1? (fraction addition estimation)
- G5Q7: 3/4 of 2/3 mile (fraction of a fraction — three-level unit coordination)

Encoding follows the butterfly pattern: each B-category student response becomes a named
variant with its student ID, sharing the correct strategy's primitive operations but
with a broken grammar of action.

Native arithmetic only. Grounded layer as `% GROUNDED:` comments.

---

### Module 3 — Research corpus encoding (parallel agents)

**Pre-work (single pass, not parallelized):**

Run these DB normalizations before agent dispatch:
```sql
-- Normalize topic casing
UPDATE error_instances SET mathematical_topic = lower(mathematical_topic);

-- Export in-scope rows
SELECT id, error_description, example, mathematical_domain,
       mathematical_topic, mathematical_subtopic, population
FROM error_instances e
JOIN articles a ON e.article_id = a.id
WHERE e.mathematical_domain IN ('fraction','decimal','whole_number','measurement','geometric')
  AND (a.population_type IN ('student','PST')
       OR a.grade_band_as_declared LIKE '%K%'
       OR a.grade_band_as_declared LIKE '%kindergarten%'
       OR a.grade_band_as_declared LIKE '%first%'
       OR a.grade_band_as_declared LIKE '%second%'
       OR a.grade_band_as_declared LIKE '%third%'
       OR a.grade_band_as_declared LIKE '%fourth%'
       OR a.grade_band_as_declared LIKE '%fifth%'
       OR a.grade_band_as_declared LIKE '%grade 1%'
       OR a.grade_band_as_declared LIKE '%grade 2%'
       OR a.grade_band_as_declared LIKE '%grade 3%'
       OR a.grade_band_as_declared LIKE '%grade 4%'
       OR a.grade_band_as_declared LIKE '%grade 5%'
       OR a.grade_band_as_declared LIKE '%elementary%'
       OR a.grade_band_as_declared LIKE '%primary%');
```

Rows not matching this query go to `misconceptions/out_of_frame.csv` with a brief
reason tag (`domain_out_of_scope | grade_out_of_scope | too_vague`).

**Agent dispatch:**

One agent per batch of ~60 rows (large domains get multiple agents). Each agent receives:
1. Its batch of DB rows (id, description, example, topic, subtopic)
2. The butterfly file as encoding template
3. The `misconception/4` convention
4. The instruction: write native Prolog only; annotate theoretical connections as
   `% GROUNDED: TODO` and `% SCHEMA: TODO`; if a misconception is too vague to
   encode as a rule, register it as `misconception(ID, Domain, too_vague, pending)`
   and move on

**Three outcomes per row (produced by test harness):**
- `wrong_answer(Expected, Got)` — rule runs, produces incorrect result
- `loop_detected` — rule exceeds inference limit; the loop is the incompatibility
- `undefined` — rule fails entirely (no output); likely an ungroundable description

---

### Module 4 — Geometry (quadrilateral classification)

**Source:** research_corpus geometric domain rows + `axioms_geometry.pl`

Misconceptions encoded using `entails_via_incompatibility` directly — no native
arithmetic, no uncertainty about axiom correctness (Module 0 certifies these).

Common archetypes from the corpus:
- "All rectangles are squares" → tests `entails_via_incompatibility(rectangle, square)`,
  which fails — rectangle does not have r2 (no adjacent sides equal), square does
- "A rhombus can't have right angles" → tests whether rhombus excludes r6; it doesn't,
  so rhombus is compatible with right angles (a square is a rhombus)
- "Parallel sides means equal sides" → confuses r1 and r5
- "Squares and rectangles are different shapes" → fails to see entailment direction

Each misconception encodes as a `verify_entailment/3` predicate that calls
`entails_via_incompatibility` and asserts a claim the axiom refutes (or confirms
in the wrong direction). The test harness checks whether the entailment holds or fails
and registers the result as `wrong_answer(claimed_entailment, actual_entailment)`.

---

### Module 5 — Area / perimeter vocabulary (new, minimal)

**Theoretical grounding:** Container schema (Lakoff/Nunez). Interior and Boundary are
incompatible loci for the same shape — you cannot simultaneously count boundary units
and claim you have measured interior coverage.

**New predicates (native layer):**

```prolog
% Area: count of unit squares fitting inside
area_of_rect(W, H, A) :- A is W * H.

% Perimeter: sum of boundary lengths
perimeter_of_rect(W, H, P) :- P is 2 * (W + H).

% The incompatibility:
% GROUNDED: is_incoherent({boundary_count(X, N), interior_coverage(X, N)})
% SCHEMA: In(unit, interior) and On(unit, boundary) are materially incompatible
%         (Container schema, L&N Ch. 2)
```

**Misconception archetypes:**
- Count boundary units for area (boundary where interior expected)
- Add L+W instead of 2(L+W) for perimeter (incomplete boundary traversal)
- PST applies base-10 regrouping to elapsed time (wrong decomposition domain)
- Area formula inverted: P = L × W, A = 2(L+W)

---

## Conventions summary

| Convention | Rule |
|------------|------|
| Native layer | Always executable, uses `is/2`, `=:=`, standard Prolog |
| Grounded layer | Comments only, prefixed `% GROUNDED:` |
| Schema annotation | Comments only, prefixed `% SCHEMA:` |
| Theoretical annotation | Comments only, prefixed `% CONNECTS TO:` |
| Too-vague rows | `misconception(ID, Domain, too_vague, pending)` — no rule written |
| Loop detection bound | 10,000 inferences via `call_with_inference_limit/3` |
| Results file | `misconceptions/results.csv` — regenerated by test harness on each run |

---

## Out of scope

- Euclidean geometric proofs
- Calculus / limits / infinity misconceptions (not K–5)
- Statistics / probability misconceptions
- High school algebra (unless PST population)
- Fixing or extending the grounded arithmetic axioms (separate project)
- Encoding misconceptions from the `axioms_number_theory.pl` Euclid proof machinery

---

## Open questions (not blockers)

1. **Grade band normalization:** The DB has 884 null grade_band rows. These go to
   agents only if `population_type = 'PST'` (PSTs can have any grade level). Otherwise
   treated as `grade_out_of_scope`. A future refinement: use `data/learningcommons/`
   or the LearningCommons MCP to map a declared grade level (e.g. "8th grade") to the
   standard being remediated — an 8th-grade student working on fraction ordering is
   likely remediating a 3rd-grade standard and should be in-scope. Not required for
   the initial pass; flagged here for the next iteration.

2. **Fraction semantics vocabulary:** `fraction_semantics.pl` has `apply_equivalence_rule`
   (grouping and composition). Some fraction misconceptions may be expressible as
   failures of these rules rather than new predicates. Agents should check this first.

3. **Loop detection bound:** 10,000 inferences may be too tight for some legitimate
   counting strategies or too loose to catch subtle loops. Adjust per domain after
   the prototype run.

4. **Back-population to DB:** After encoding, `student_rule` and `incompatible_with`
   fields in `error_instances` can be back-populated from the Prolog registration facts.
   This is a later step, not part of the initial pattern detection.
