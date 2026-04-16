---
module: <module-name>
sources:
  - path: <module>/<file>.pl
    sha256: <paste from Stripped_Code/MANIFEST.txt>
  # one entry per .pl file in the module, including tests/ if present
exports:
  # predicate/arity list pulled from :- module(name, [ ... ]) declarations
  # - predicate/arity
operators:
  # operator declarations from :- op(Precedence, Associativity, Name) lines
  # - name/associativity/precedence
research_goal: >
  One sentence. Claims only what the stripped code supports.
  Limitations are explicit ("proposed, not empirically coded").
---

# Fact-sheet: <module-name>

Written from [Stripped_Code/<module>/](../../Stripped_Code/<module>/) without reading any aspirational markdown. Cross-checked against Round 2 after drafting. Voice: SCENE register — hypothetical correspondences, not identities; productive failure is the point of contact.

## What the module defines

Predicates, data shapes, constants. Mechanical inventory. ≤ one page.

## What axioms / inferences are asserted

The mechanical claims the module makes. Not what the docs say they mean — what the code does. ≤ one page.

## What the module does NOT do

The negative space. Hardest to see from inside, most valuable to future readers. Every "we do not implement X" belongs here. ≤ one page.

## Research goal

One sentence. Same as `research_goal` in the front-matter. Limitations explicit.

## Cross-check against Round 2

One paragraph. Reference [docs/phase5/_round2-section-map.md](_round2-section-map.md) to find the relevant section. Note agreements; flag any disagreement by line range and specific claim.
