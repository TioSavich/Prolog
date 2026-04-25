# Misconceptions Module

Empirical pattern detection for 1,453 math-education misconceptions across five domains, encoded as executable Prolog rules.

## Architecture — lights-on / lights-off

Every misconception file has two layers:

**NATIVE LAYER** (always runs): standard Prolog arithmetic (`is/2`, `=:=`, `mod/2`, comparison operators). This is what the harness executes.

**GROUNDED LAYER** (comments only, for now): where each rule's native-arithmetic step would be replaced with a call into the embodied grounded arithmetic (`grounded_arithmetic`). Comments are prefixed `% GROUNDED: TODO` and `% GROUNDED: <concrete sketch>`.

**SCHEMA** (comments only): Lakoff & Nunez image-schema grounding where it applies. Prefixed `% SCHEMA:`. Currently used for Container (area/perimeter), Measuring Stick (length, time), Arithmetic is Object Collection (fraction addition).

**CONNECTS TO** (comments only): PML operator path marking the unlicensed inferential move. Prefixed `% CONNECTS TO: s(comp_nec(unlicensed(<name>)))`.

The native and grounded layers do not contaminate each other. Switching layers means uncommenting and substituting — the misconception *structure* doesn't change.

## Directory layout

```
misconceptions/
  README.md                          — this file
  test_harness.pl                    — harness module; entry point run_all/0
  results.csv                        — generated on each run (gitignored)

  misconceptions_fraction.pl         — umbrella: ASKTM prototypes (G4Q1/Q3/Q7/G5Q1/Q7) + cascade imports
  misconceptions_fraction_batch_1.pl — research corpus fraction, batch 1 of 7
  misconceptions_fraction_batch_2.pl ... (7 total)

  misconceptions_whole_number.pl     — umbrella + cascade
  misconceptions_whole_number_batch_1.pl ... (5 total)

  misconceptions_decimal.pl          — umbrella + cascade
  misconceptions_decimal_batch_1.pl ... (2 total)

  misconceptions_measurement.pl      — umbrella + Module 5 archetypal vocabulary (5 rules) + cascade
  misconceptions_measurement_batch_1.pl ... (2 total)

  misconceptions_geometry.pl         — umbrella + cascade (note: filename uses 'geometry', batch files use 'geometric' to match corpus domain atom)
  misconceptions_geometric_batch_1.pl ... (2 total)
```

## Registration conventions

**Arithmetic misconceptions** (fraction, whole_number, decimal, measurement):
```prolog
test_harness:arith_misconception(
    Source,         % db_row(Integer) | asktm(Atom) | vocabulary(Atom)
    Domain,         % fraction | whole_number | decimal | measurement
    Description,    % snake_case atom, <=6 words
    RuleName,       % Module:Predicate — e.g. misconceptions_fraction_batch_3:add_num_den_separately
    Input,          % ground term passed to the rule
    Expected        % ground term the CORRECT rule would return
).
```

The harness calls `call(Module:Predicate, Input, Got)` via `call_with_inference_limit/3` (bound 10,000) and classifies the result.

**Entailment misconceptions** (geometric):
```prolog
test_harness:entail_misconception(
    Source,         % db_row(Integer) | asktm(Atom)
    Description,    % snake_case atom
    Shape,          % atom from allowed vocabulary
    Target,         % atom from allowed vocabulary
    Claim           % holds | fails — what the student asserts
).
```
Shape and Target must come from `{square, rectangle, rhombus, parallelogram, trapezoid, kite, quadrilateral}`. The harness consults `entails_via_incompatibility/2` from `formalization/axioms_geometry.pl` and classifies.

## Classifications

- `wrong_answer` — rule runs, produces incorrect result
- `loop_detected` — rule exceeds 10,000 inference limit (the loop *is* the incompatibility)
- `undefined` — rule not defined OR registered as `too_vague` (RuleName = `skip`, a short-circuit)
- `well_formed` — rule happens to produce the correct answer (dedup candidate; the misconception's wrong reasoning coincidentally matches the right result)

## Running

```bash
swipl -l paths.pl -l misconceptions/test_harness.pl -g run_all -t halt
```

Output:
- Summary block to stdout
- `misconceptions/results.csv` (gitignored; regenerated on each run) with one row per registration: `source, domain, description, classification, got`

## Current corpus — 1,453 classifications

**By domain and classification:**

| Domain | wrong_answer | undefined | well_formed | loop_detected | Total |
|--------|-------------:|----------:|------------:|--------------:|------:|
| fraction     | 286 | 344 | 15 | 0 |   645 |
| whole_number | 153 | 228 |  1 | 0 |   382 |
| decimal      | 106 |  79 |  4 | 0 |   189 |
| measurement  |  35 |  72 |  0 | 0 |   107 |
| geometric    |  11 | 119 |  0 | 0 |   130 |
| **Total**    | **591** | **842** | **20** | **0** | **1,453** |

**By source type:**

- ASKTM prototype (Grade 4-5 student responses): 7 rows
- Research corpus (`research_corpus/research.db` rows, in-scope per Task 5 triage): 1,441 rows
- Module 5 archetypal vocabulary (curated schema-level exemplars): 5 rows

Of the 1,453 total registrations, 842 are classified as `undefined` — the majority of these carry the `too_vague` tag in their description, indicating the source row was triaged in-scope but did not reduce to a concrete wrong numeric output in the first pass.

## Sources

### ASKTM data
Grade 4-5 student responses from fine-grained coding, 7 misconception variants across G4Q1, G4Q3, G4Q7, G5Q1, G5Q7 (fraction ordering, equivalence, fraction x whole, fraction addition, fraction-of-fraction). Source code in `misconceptions_fraction.pl` preserving student IDs (06-03, 18-04, 06-20, g4q3_common, etc.) in the `Source` column.

### Research corpus
1,441 in-scope rows from `research_corpus/research.db`, triaged in Task 5 using:
- Domain membership in {fraction, whole_number, decimal, measurement, geometric}
- PST population OR K-5 grade-band keyword match
- Non-empty error description (at least 20 chars)

Out-of-frame rows are exported to `misconceptions/out_of_frame.csv` (gitignored) with `oof_reason` tags.

Encoding performed by parallel agents:
- **Task 6** (fraction): 7 agents, ~91 rows each, ~638 rows total
- **Task 7** (whole_number + decimal): 5 WN agents (~77 rows each) + 2 decimal agents (~95 rows each), ~571 rows total
- **Task 8** (measurement + geometric): 2 measurement agents (~51 rows each) + 2 geometric agents (~70 rows each), ~243 rows total (a subset were comment-only for the geometric-to-measurement-redirect case)

Agents were instructed to lean toward `too_vague` when the research description didn't reduce to a specific wrong numeric output. Of 1,441 corpus rows, the majority registered as `too_vague` — these are candidates for a later pass that opens the source PDF.

### Module 5 archetypal vocabulary
Five curated misconceptions documenting schema-level incompatibilities:
- **Container schema** (L&N Ch. 2): area counted as perimeter, perimeter incomplete traversal, area formula inverted (x2), perimeter formula inverted
- **Measuring Stick schema**: base-10 regrouping applied to elapsed time (clocks are base-60)

These are the schema-level exemplars that the corpus rules invoke. Their `Source` column is `vocabulary(container_schema)` or `vocabulary(measuring_stick_schema)`.

## What this module does NOT do

- **It does not use the embodied grounded arithmetic.** The GROUNDED layer is documented in comments. Switching it on requires the axioms certification described in `axioms_grounded.pl` (out of scope for this pass).
- **It does not generate misconceptions.** It verifies that encoded misconceptions produce the wrong output under native arithmetic. The epistemic interest is the mapping: which schemas ground which wrong transformations.
- **It does not cover calculus, statistics, high-school algebra.** Those domains are out of scope per the Task 5 triage.

## Known limitations and follow-ups

1. **20 well_formed rows.** These are encoded misconceptions whose wrong reasoning happens to produce the correct output for the specific example. They need a more nuanced classifier (trace-based, not output-based) to surface as genuine misconceptions rather than dedup candidates.

2. **842 undefined rows (mostly `too_vague`).** These are misconception descriptions that didn't reduce cleanly to a Prolog rule in the first pass. The next iteration should pull the source PDF for each row (page_refs column in the CSV) and re-attempt encoding with richer context.

3. **Geometric taxonomy limited to 7 shape atoms.** Any misconception involving triangle, circle, 3D shapes, transformations, proofs, etc. is registered as `too_vague`. Extending the axioms would recover a portion of these.

4. **Geometric-to-measurement redirects.** Geometric batch agents identified rows whose errors are measurement-flavored (area/perimeter/angle arithmetic) rather than taxonomic. They are documented as comments in the geometric batch files with their db_row IDs; promoting them to full measurement misconceptions would add actionable rows.

5. **Grade-band triage is keyword-based.** Rows with declared grade "8th grade" whose actual mathematical work is at 3rd-grade level are currently out-of-frame. A future refinement would consult the LearningCommons standards graph to remediate.

## Axiom provenance

The geometric taxonomy rests on `formalization/axioms_geometry.pl`. The six restrictions R1-R6 are documented inline there (Task 1). Ten correctness tests in `formalization/tests/test_geometry_entailment.pl` verify the taxonomy behaves as expected. The M5-Direct guard fix in `formalization/axioms_number_theory.pl` was landed alongside (Task 1).
