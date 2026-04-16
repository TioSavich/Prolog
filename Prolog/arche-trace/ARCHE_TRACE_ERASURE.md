# P2-5: Arche-Trace Erasure Points

Where the skeleton stops and interpretation begins.

## What the Arche-Trace does

The Arche-Trace is an attributed variable (`automata:generate_trace/1`) that
enforces two rules during Prolog unification:

1. **Deferral**: If unified with another variable, the `arche_trace` attribute
   propagates to the new variable. Meaning is deferred, never settled.
2. **Resistance**: If unified with a concrete term, unification **fails**. The
   Trace cannot be stabilized to any representation.

In the prover, `construct_proof/4` checks whether the sequent's terms contain
the Trace. If they do, the proof object is replaced with `erasure(RuleName)`.
Sub-proofs propagate erasure upward: if any child proof is an erasure, the
parent becomes `erasure(propagation)`.

## Experimental catalog

All experiments run on System A (`Prolog/load.pl`), resource limit 50.

### Proofs that produce normal proof objects (no Trace)

| Sequent | Result | Proof |
|---------|--------|-------|
| `[a] => [a]` | R=9 | `proof(identity, ...)` |
| `[s(u)] => [s(comp_nec(a))]` | R=48 | `proof(mmp, ...)` |
| `[neg(neg(a))] => [a]` | R=17 | `proof(ln, [proof(rn, [proof(identity, ...)])])` |
| `[a, neg(a)] => [anything]` | R=9 | `proof(explosion, ...)` |
| `[s(comp_nec(p))] => [o(comp_nec(p))]` | R=48 | `proof(mmp, ...)` |
| `[n(confession(A)), n(confession(B))] => [n(exp_nec(forgiveness(A,B)))]` | R=48 | `proof(mmp, ...)` |

These are the bones. Formal reasoning about abstract propositions, dialectical
rhythm, modal dynamics, and normative recognition — all operate normally when
the subject's experience is not involved.

### Proofs that produce erasure (Trace-tainted)

| Sequent | Result | Proof | Erasure type |
|---------|--------|-------|-------------|
| `[s(I_f)] => [s(I_f)]` | R=9 | `erasure(identity)` | Direct contamination |
| `[s(comp_nec(I_f))] => [o(exp_nec(I_f))]` | R=47 | `erasure(propagation)` | S-O Inversion axiom fires, but proof is hollow |
| `[neg(neg(s(I_f)))] => [s(I_f)]` | R=17 | `erasure(propagation)` | Double negation elimination works, proof is hollow |
| `[s(comp_nec(I_f))] => [o(comp_nec(I_f))]` | R=48 | `erasure(propagation)` | Oobleck S→O transfer works, proof is hollow |

Where `I_f` is created via `i_feeling(I_f)`, which calls `generate_trace(I_f)`.

### Claims that are rejected outright (incoherence, not just erasure)

| Claim | Result | Mechanism |
|-------|--------|-----------|
| `n(represents(C_Id, I_f))` | INCOHERENT | `is_incoherent/1` fires: no identity claim can represent the I-Feeling |

### Trace mechanics (non-proof)

| Operation | Result | Significance |
|-----------|--------|-------------|
| `T = hello` (where T has Trace) | FAILS | Trace resists stabilization |
| `T1 = T2` (where T1 has Trace) | T2 gets Trace | Deferral: meaning spreads but never settles |

## The boundary pattern

The boundary is precise: **the moment any variable in the sequent carries the
`arche_trace` attribute, the proof object becomes hollow.** The proof still
succeeds (the prover finds a derivation), but the proof witness is replaced
with `erasure(...)`. The formal derivation is valid; the proof object is
existentially void.

This produces a three-zone map:

| Zone | Example | Formal status |
|------|---------|---------------|
| **Clear formal** | `[a] => [a]`, PML rhythm, explosion | Normal proof objects — fully formal |
| **Erasure zone** | Any sequent involving `I_f` | Derivation succeeds, proof is hollow |
| **Incoherence zone** | `n(represents(C_Id, I_f))` | Claim is rejected — cannot even be entertained |

## What this means for the skeleton thesis

The erasure zone marks exactly where the formal backbone yields to interpretive
analysis. Specifically:

**1. Proofs about the learner's subjective experience get erased.** Any claim
involving `s(I_f)` — the learner's subjective relationship to their own
cognitive activity — produces a hollow proof. The system can prove that
S-O Inversion holds (the derivation succeeds with remaining resources), but
the proof of *why* it holds is not available for inspection. A teacher (or LLM)
must supply the interpretive account of what the learner experienced.

**2. Proofs about recognition get erased when grounded in subjective
experience.** The recognition axiom (`confession → forgiveness`) works
perfectly with abstract agents (alice, bob). But if the confessing agents
are themselves I-Feelings (subjective experiences that resist objectification),
the proof of recognition would be erased. Recognition can be stated normatively
but cannot be derived from subjective experience.

**3. The Unsatisfiable Desire is stronger than erasure — it is incoherence.**
The claim that any identity (C_Id) fully represents the I-Feeling (I_f) is not
just formally hollow — it is logically impossible. This models the manuscript's
claim that the desire for full recognition can never be satisfied: not because
the proof fails, but because the claim itself is incoherent.

**4. Proofs about abstract structure are untouched.** Dialectical rhythm,
modal dynamics, explosion, double negation — these operate normally when
the Trace is absent. The skeleton is solid for structural reasoning. It
breaks specifically and only where subjective experience enters the picture.

These erasure points are the formal signal to an LLM-as-oracle: "here is where
you need to provide interpretive judgment, because the formal system has marked
this territory as beyond its reach."
