# 08 — Pruning and Reorganization

## Principle

The repository has accumulated material from multiple phases of development.
Some of it contradicts the redesign. Some of it is aspirational documentation
that no longer matches what the code does. This document identifies what to
remove, what to reorganize, and what to keep as-is.

**Rule**: Do not delete anything that might be useful as historical reference.
Move to an `archive/` directory rather than deleting.

## Files to archive (move to `archive/`)

### `synthesized_paper.md`
Per CLAUDE.md: "Do not surface synthesized_paper.md in any public-facing
context — it is a ChatGPT draft with no standing." Move to archive.

### Overlapping architecture documents
The following documents overlap significantly and should be consolidated into
the new design documents rather than maintained separately:
- `prolog/ARCHITECTURE.md` — System B architecture overview
- `prolog/DIVASION_ARCHITECTURE.md` — Inside/outside self-observation
- `prolog/CURRICULUM_MAP.md` — Strategy-to-crisis mapping
- `prolog/SYNTHESIS_FEASIBILITY.md` — Implementation status
- `prolog/SYNTHESIS_HONESTY.md` — Honest assessment of synthesis

Keep these in archive for reference. The design/ folder replaces their function.

### Phase-specific test files
Test files for the old architecture's phases should be archived, not deleted.
New tests should be written for the redesigned modules.

## Code to audit and potentially refactor

### Lazy strategy automata (16 files in Prolog/math/)
These files use Prolog's `is/2`, `mod`, `//` instead of grounded primitives.
They are philosophically compromised — they smuggle in arithmetic the system
hasn't earned.

Options:
1. **Ground them** — refactor to use grounded arithmetic throughout. This is
   the most honest approach but labor-intensive.
2. **Document the gap** — keep them but add clear documentation that they use
   native arithmetic and explain why this is philosophically problematic.
3. **Archive them** — move to archive if the redesign makes them obsolete.

Recommendation: Start with option 2 (document the gap), ground them
incrementally as time permits, prioritizing strategies that are central to
the redesign (counting-on, COBO, RMB).

### teacher_server.pl
Will be significantly redesigned per `03_ORACLE_REDESIGN.md`. Current version
should be archived before modification.

### fsm_synthesis_engine.pl
Currently wraps teacher calls. Will be replaced by projective validity tester.
Archive current version.

### crisis_curriculum_primordial.txt and mathematical_curriculum.txt
These curriculum files are tied to the old architecture's crisis model. They
may be useful as reference for designing the new teacher's curriculum levels.
Keep in place but document their status.

## Code to keep as-is

### Grounded arithmetic (grounded_arithmetic.pl)
Sound. Recollection-based operations are the right foundation. May need minor
extensions (e.g., trace-producing versions of existing operations).

### Grounded ENS operations (grounded_ens_operations.pl)
Sound for fractional reasoning. Not immediately needed for the redesign but
should be preserved for Level 5.

### PML operators (pml_operators.pl)
Keep. The s/o/n wrappers and modal operators are the right abstraction.

### Incompatibility semantics (incompatibility_semantics.pl)
Keep. Entailment framework is needed for projective validity testing.

### Pragmatic and semantic axioms
Keep. The dialectical drive axioms are needed for PML integration.
Audit which axioms actually do work in the redesigned system vs. which are
aspirational.

### Automata module (automata.pl)
Keep the Highlander and equality-iterator. Keep the arche-trace mechanism
but do not overclaim its philosophical significance (see naming caution in
`00_PROJECT_OVERVIEW.md`).

### FSM engine (fsm_engine.pl)
Keep. Unified execution for strategy automata is still needed.

### Fully grounded strategy automata
These are the models:
- `sar_add_cobo.pl` — fully grounded
- `sar_sub_decomposition.pl` — fully grounded
- `jason.pl` / `jason_fsm.pl` — fully grounded
- `fraction_semantics.pl` — fully grounded

Keep as exemplars of what grounded strategy modeling looks like.

## New directory structure

```
umedcta-formalization/
├── CLAUDE.md
├── README.md
├── design/                    ← NEW: planning documents (this folder)
│   ├── 00_PROJECT_OVERVIEW.md
│   ├── 01_MEANING_FIELDS.md
│   ├── 02_PROJECTIVE_VALIDITY.md
│   ├── 03_ORACLE_REDESIGN.md
│   ├── 04_NUMBER_WORDS.md
│   ├── 05_COUNTING_TRACES.md
│   ├── 06_REFLECTION.md
│   ├── 07_PML_INTEGRATION.md
│   └── 08_PRUNING.md
├── archive/                   ← NEW: historical material
│   ├── synthesized_paper.md
│   ├── old_architecture_docs/
│   └── old_tests/
├── prolog/
│   ├── grounded_arithmetic.pl  (keep)
│   ├── grounded_utils.pl       (keep, audit)
│   ├── meaning_field.pl        ← NEW
│   ├── projective_validity.pl  ← NEW
│   ├── teacher_server.pl        (redesign)
│   ├── number_words.pl         ← NEW
│   ├── counting_traces.pl      ← NEW
│   ├── reflection.pl           ← NEW
│   ├── meta_interpreter.pl     (keep, integrate)
│   ├── execution_handler.pl    (keep, integrate)
│   ├── fsm_engine.pl           (keep)
│   ├── config.pl               (keep)
│   ├── Prolog/
│   │   ├── pml_operators.pl                (keep)
│   │   ├── incompatibility_semantics.pl    (keep)
│   │   ├── dialectical_engine.pl           (keep)
│   │   ├── pragmatic_axioms.pl             (keep)
│   │   ├── semantic_axioms.pl              (keep)
│   │   ├── automata.pl                     (keep)
│   │   ├── math/                           (keep, audit grounding)
│   │   └── tests/                          (keep, extend)
│   ├── Modal_Logic/            (keep — manuscript connection)
│   └── tests/                  (audit, extend)
```

## Implementation order

1. Number-word layer (`04_NUMBER_WORDS.md`) — simplest, no dependencies
2. Counting traces (`05_COUNTING_TRACES.md`) — extends existing grounded
   arithmetic
3. Meaning fields (`01_MEANING_FIELDS.md`) — needs number-words
4. Teacher module (`03_ORACLE_REDESIGN.md`) — needs meaning fields
5. Reflection mechanism (`06_REFLECTION.md`) — needs traces + meaning fields
6. Projective validity (`02_PROJECTIVE_VALIDITY.md`) — needs all of the above
7. PML integration (`07_PML_INTEGRATION.md`) — cross-cutting, integrate as
   modules stabilize
8. Pruning — do incrementally as redesigned modules replace old functionality
