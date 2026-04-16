# pml/

PML is a 12-operator coding framework for classroom discourse. It tracks epistemic
stance — not what teachers do, but the consciousness from which they do it.

## The 12 operators

Three modes of validity (Subjective, Objective, Normative) crossed with two
polarities (compressive/expansive) and two directions (up/down):

| | Compressive (necessity) | Expansive (possibility) |
|---|---|---|
| **S-mode** (first person) | comp_nec_s | exp_poss_s |
| **O-mode** (second person) | comp_nec_o | exp_poss_o |
| **N-mode** (third person) | comp_nec_n | exp_poss_n |

Person-position markers follow Carspecken's reconstruction: S/O/N modes correspond
to pronoun shifts (first/second/third person) in discourse.

## Contents

- `pml_operators.pl` — operator definitions and mode predicates
- `semantic_axioms.pl` — semantic grounding axioms
- `pragmatic_axioms.pl` — pragmatic axioms connecting practice to inference
- `intersubjective_praxis.pl` — intersubjective inference patterns
- `utils.pl` — utility predicates (select/3, match_antecedents/2)
- `Modal_Logic/` — LaTeX appendices, philosophical dictionary, manuscript overview
  - `AppendixA_Unified_2.tex` — main PML appendix
  - `Dictionary.md` / `Dictionary_voice_edit_enhanced.tex` — philosophical vocabulary
  - `counting.tex`, `Jason.tex` — strategy-specific appendices

## Tests

28 PML core tests pass (in `tests/core_test.pl`).

## Cross-module dependencies

`pragmatic_axioms.pl` imports from `arche-trace/` (automata, incompatibility_semantics).
`intersubjective_praxis.pl` imports from `arche-trace/` (incompatibility_semantics).
