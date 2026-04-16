# arche-trace/

This module marks the precise boundary where formalization goes hollow. Derivations
succeed structurally but proof objects contain no content.

## The erasure mechanism

When sequent variables carry the `arche_trace` attribute, the prover returns
`erasure(...)` instead of `proof(...)`. The derivation succeeds structurally but
the proof object is empty. This happens at exactly the points where human judgment
must take over: identity with Trace, S-O Inversion, double negation elimination,
Oobleck transfer.

This is not a bug. The erasure marks the skeleton boundary — where the formalism
honestly stops being able to say anything.

## The sequent engine and axiom sets

`incompatibility_semantics.pl` is the scene-agnostic sequent calculus engine. It
provides `proves/1` with a priority-ordered rule application:

1. Identity and Explosion (scene-agnostic structural rules)
2. Material axioms (domain-specific, from included axiom sets)
3. Structural rules (forward chaining, arithmetic evaluation)
4. Reduction schemata (negation, conjunction, modal rules)

The engine includes axiom sets from their respective modules via `:- include(...)`:

- `formalization/axioms_geometry.pl` — quadrilateral taxonomy via incompatibility
- `formalization/axioms_robinson.pl` — Robinson Q1-Q7, arithmetic grounding,
  `is_recollection/2`, `normalize/2`, `arith_op/4`
- `formalization/axioms_number_theory.pl` — Euclid's prime proof, primality
- `pml/axioms_eml.pl` — embodied modal logic (dialectical rhythm)
- `learner/axioms_domains.pl` — domain switching, normative crisis, fractions

PML operators (`s/1`, `comp_nec/1`, etc.) live canonically in `pml/pml_operators.pl`
and are re-exported by the engine.

## Contents

- `incompatibility_semantics.pl` — sequent calculus engine + axiom set loader
- `embodied_prover.pl` — alternate prover with `proves/4` (resource-tracked, embodied
  cost model). Still declares module name `incompatibility_semantics` — cannot be
  loaded simultaneously with the engine. Needs its own module name (future cleanup).
- `critique.pl` — sublation/critique mechanism (Being/Nothing/Becoming mediation)
- `dialectical_engine.pl` — dialectical reasoning engine
- `automata.pl` — trace generation and the `contains_trace/1` predicate used by
  the erasure mechanism
- `load.pl` — loader for the full arche-trace + pml stack
- `ARCHE_TRACE_ERASURE.md` — documentation of the four erasure points

## Cross-module dependencies

Imports `formalization/grounded_arithmetic` and `strategies/hermeneutic_calculator`.
Re-exports PML operators from `pml/pml_operators`. Imported BY strategy automata
(for PML operators) and learner (for `proves/1` and domain management).
