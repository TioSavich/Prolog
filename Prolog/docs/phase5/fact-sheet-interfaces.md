---
module: interfaces
sources: []  # no Prolog sources — this is a markdown-only module
documents:
  - path: interfaces/pml-to-discourse.md
  - path: interfaces/pml-to-learner.md
  - path: interfaces/compression-across-layers.md
  - path: interfaces/arche-trace-to-learner.md
  - path: interfaces/strategies-to-formalization.md
exports: []
operators: []
research_goal: >
  The interfaces/ module is five markdown documents asserting cross-module
  correspondences. After audit against the five stable module fact-sheets,
  the surviving claims are those whose wires are either already in the code
  (pml polarity modulates learner inference cost; strategies and formalization
  share grounded_arithmetic primitives; learner:more_machine_learner calls
  arche-trace:proves/1 for successor; strategies emit s(comp_nec/exp_poss)
  terms) or are honestly flagged as proposed/unformalized in the document
  itself. Every other cross-module claim in these files is a research
  hypothesis, not a coded connection.
---

# Fact-sheet: interfaces

Written from the five interface markdown documents in `interfaces/`, read
against the five stable module fact-sheets (`pml`, `arche-trace`,
`formalization`, `learner`, `strategies`). No Prolog sources in this module.
Voice: SCENE register — hypothetical correspondences, not identities.

## What the module defines

Nothing mechanical. This module has no .pl files, no tests, no exports, no
operators. It is five markdown documents, each asserting a cross-module
correspondence:

- `pml-to-discourse.md` — PML's 12 operators as a proposed coding framework
  for classroom discourse. Person-position → S/O/N mode; categorical vs.
  hedging → compressive/expansive polarity; tempo → up/down direction. The
  document itself states "No transcripts have been coded with PML."
- `pml-to-learner.md` — PML's compressive/expansive polarity modulates the
  learner meta-interpreter's inference cost. Documents the one working wire
  (polarity → context multiplier) and lists several planned but not-yet-coded
  extensions (mode-sensitive cost, meaning fields, decentering, provenance).
- `compression-across-layers.md` — PML polarity and strategy automata's
  temporal compression are "the same structure operating at different levels
  of description." Proposes the fractal-catastrophe reading (each automaton
  a Zeeman disc, spring tension = PML polarity) as generative metaphor.
- `arche-trace-to-learner.md` — Names three augmentation points where the
  learner's formalization goes hollow (erasure, resource exhaustion,
  pathology detection) and documents that the learner does not actually call
  `embodied_prover:proves/4`, `critique:reflect/2`, or
  `dialectical_engine:run_computation/2`.
- `strategies-to-formalization.md` — The 27 strategy automata collectively
  generate arithmetic rich enough to instantiate Robinson Q. Acknowledges
  Gödel numbering is not wired, no Gödel sentence is constructed, and the
  meta-theorem "every theorem of Q is a theorem of the HC" is unproved.

The five documents share a rhetorical shape: **The Claim / What Exists in
Code / Where the Claim Holds / Where the Claim Exceeds What's Formalized /
What Would Strengthen This Interface**. The "Where the Claim Exceeds" and
"What Would Strengthen" sections are where the documents self-audit. They do
self-audit, unevenly.

## What axioms / inferences are asserted

Each of the five documents makes one core cross-module claim. Whether the
claim survives against both relevant module fact-sheets:

**pml-to-discourse.** Claim: PML operators code observable teacher discourse
along three channels. **Does not survive.** The pml fact-sheet states
plainly: "The module does not code any discourse corpus. There is no file in
`pml/` that reads a transcript, tags an utterance with a mode or polarity,
or scores a coder's agreement." No other module hosts a discourse corpus
either. The document concedes this ("No transcripts have been coded with
PML"). The whole document is a research proposal, not a code-report. It is
commentary in the audit sense.

**pml-to-learner.** Claim: PML polarity modulates learner inference cost
through `determine_modal_context` / `get_inference_cost` / tension
multipliers. **Survives for the polarity wire only.** Verified against
source: `learner/meta_interpreter.pl` lines 35–46 define
`is_modal_operator/2` and `get_inference_cost/2` exactly as the document
states; `arche-trace/embodied_prover.pl` lines 46–48 and 58–63 define the
parallel independent copies. The pml fact-sheet confirms the four modal
operators and their polarity split; the learner fact-sheet confirms the
2/1/1 and 2.5/1.0/0.3 multipliers. The document's own "Where the Claim
Exceeds" section accurately flags that mode (S/O/N) has no computational
effect and that meaning fields, decentering, and provenance are not
implemented.

**compression-across-layers.** Claim: PML polarity and strategy temporal
compression are the same structure. **Survives in weakened form.** The
shared wire is real: every one of the 22 math/ automata grep-confirmed emits
`s(comp_nec(...))` or `s(exp_poss(...))` terms; those terms set the
meta-interpreter's modal context; that context multiplies cost. But the
document's own "Where the Claim Exceeds" section admits "'Same Structure' Is
Not Proved" and "The Scalar Multiplier Does Not Distinguish Modes." The
fractal-catastrophe section is explicitly labelled "not yet determined
whether it coheres as formal mathematics or remains a generative metaphor."
The code-backed part of the claim survives; the "same structure" framing is
commentary.

**arche-trace-to-learner.** Claim: Three augmentation points mark where the
learner's formalization goes hollow; the learner uses them. **Mostly does
not survive.** The document itself undermines the strong reading: the
learner does NOT use `proves/4`, does NOT invoke `critique:reflect/2`, does
NOT call `dialectical_engine:run_computation/2`, and has its own
`execution_handler:run_computation/2` instead. The one real wire is
`more_machine_learner:successor(X, Y) :- proves([] => [o(plus(X, 1, Y))])`,
confirmed by the arche-trace fact-sheet. The document's Option B ("Document
the Parallel as Intentional") is the position that matches the code. Most
of the document is commentary on a parallel that does not intersect.

**strategies-to-formalization.** Claim: Strategies and Robinson Q share
grounded-arithmetic primitives; strategies instantiate the operations Q
axiomatises. **Survives at the primitive-sharing level.** The strategies
fact-sheet confirms every math/ automaton calls
`formalization(grounded_arithmetic):integer_to_recollection/2` and
`incur_cost/1`. The formalization fact-sheet confirms Robinson Q axioms
route through `arith_op/4` and `is_recollection/2`. The document's own
"Where the Claim Exceeds" section flags the absent encoding map, missing
Gödel numbering, absent Gödel sentence, and unproved meta-theorem. The
small claim (primitive-sharing) survives; the headline ("Arithmetic
Choreography Raises the Incompleteness Question") is a framing the code
does not support beyond the observation that Q-level axioms exist in the
repo and some strategies produce Q-level operations.

## What the module does NOT do

- **No code.** Cross-module wires in `interfaces/` are not checked by any
  build system or test suite. If a fact-sheet contradicts an interface
  document, nothing in the code will catch it automatically.
- **No shared schema.** Each document has its own claim shape, its own list
  of code references, and its own self-audit section. There is no
  registered cross-reference between documents — the same wire (polarity →
  cost multiplier) appears in three documents with minor variations.
- **No test corpus of transcripts, student traces, or teacher episodes.**
  Every empirical claim in `pml-to-discourse.md` and
  `compression-across-layers.md` rests on data that does not exist in this
  repository.
- **No proof of any cross-module correspondence.** The documents narrate
  correspondences. The fact-sheets, read honestly, confirm shared
  predicates and shared data shapes but do not confirm structural
  identities.
- **No versioning of the wires.** If `more_machine_learner.pl`'s
  `successor/2` stopped calling `proves/1`, nothing in `interfaces/` would
  detect the loss. The documents name the wires; they do not check them.

## Research goal

See front-matter. The `interfaces/` module contains five cross-module claim
documents. After audit against stable module fact-sheets, claims that
survive are those whose wires are either already in the code or whose
empirical grounding is honestly marked as proposed, not coded. The module's
productive role is as a research-agenda archive: it names where the
cross-module work *would* be formalised and where the project is currently
trading on metaphor.

## Cross-check against Round 2

Round 2 has no coverage of `interfaces/`. Round 2 is a mechanical read of
the Prolog sources; `interfaces/` is markdown-only. There is no
cross-reference to make. The absence itself is consistent with the module's
role: Round 2 tracks what the code says, and the code says nothing about
these claims; the interface documents fill the silence with hypothesis.
