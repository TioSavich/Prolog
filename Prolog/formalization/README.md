# formalization/

Robinson Q and the meta-mathematical signal.

This establishes where the project sits relative to Godel's incompleteness theorems.
The claim is not that the system proves incompleteness — it is that children's
arithmetic strategies, taken seriously, generate machinery rich enough to raise the
question.

## Contents

- `robinson_q.pl` — self-contained proof that the Hermeneutic Calculator interprets
  Robinson Arithmetic (Q). 20 tests, zero external dependencies.
- `grounded_arithmetic.pl` — embodied arithmetic: recollection/tally/successor,
  cognitive cost tracking, grounded add/subtract/multiply.
- `grounded_utils.pl` — base decomposition and recomposition utilities.
- `grounded_ens_operations.pl` — ENS operations (partition, disembed, iterate)
  grounded in recollection primitives.

## What is established

- Zero axiomatically grounded via `is_recollection(0, [axiom(zero)])`
- Successor, addition, multiplication derivable through grounded layer
- Robinson axioms Q1-Q7 verified (20/20 tests in robinson_q.pl)
- The Q1-Q7 rules also live in `arche-trace/incompatibility_semantics.pl` (lines
  486-547) as `proves_impl` rules within the sequent calculus

## What remains open

- Godel numbering not wired to syntax encoding
- A Godel sentence for this specific system has not been constructed
- The meta-theorem ("every theorem of Q is a theorem of the HC") is not formally proved
- See `FORMALIZATION_ASSESSMENT.md` at the repo root for the full accounting

## Cross-module dependencies

`grounded_arithmetic` is imported by `strategies/` (all automata use it) and
`learner/` (meta-interpreter uses it). This is shared infrastructure, not a
violation of module boundaries.
