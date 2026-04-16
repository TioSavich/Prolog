---
module: pml
sources:
  - path: pml/axioms_eml.pl
    sha256: 8ae4143013e30ccd39162308d3c318584495e869c415c476b25caf036cd04377
  - path: pml/intersubjective_praxis.pl
    sha256: dd06353e69965a64eb264bd2574b3f5e9ecf7ff5f5e65412f2bce191dd56afed
  - path: pml/pml_operators.pl
    sha256: 9d361ad00fa0f23dc545d987fc01f8378476f8bdbdeb8089b68636ee83a89797
  - path: pml/pragmatic_axioms.pl
    sha256: 0dac1878c39c214acd0a4be108b284d5e9a241f3ef16914e56ea14824f2569b1
  - path: pml/semantic_axioms.pl
    sha256: bc3bfd0a50567c721d7181aa629e6dd0937f58fe157e3f45815da0a5812adacd
  - path: pml/utils.pl
    sha256: 553e879ea558ed8e0a64c64596189afad99387fe7adef309abd74b530a79f2f8
  - path: pml/tests/core_test.pl
    sha256: cb1c7470e454dd57b2ac66cb6c04294d52850f2db306099c1073f50e1fe14f67
  - path: pml/tests/simple_test.pl
    sha256: f3665a9b7b23edeb69ed6c2223926e695c3fc7bb7b54777516463b9894f550d6
exports:
  - s/1
  - o/1
  - n/1
  - comp_nec/1
  - exp_nec/1
  - exp_poss/1
  - comp_poss/1
  - neg/1
  - i_feeling/1
  - identity_claim/1
  - impetus/1
  - select/3
  - match_antecedents/2
operators:
  - comp_nec/fx/500
  - exp_nec/fx/500
  - exp_poss/fx/500
  - comp_poss/fx/500
  - neg/fx/500
  - =>/xfy/1050
research_goal: >
  PML supplies a twelve-operator modal vocabulary (three modes crossed with four
  polarised modalities) whose structural theorems and dialectical transitions
  are exercised by the `tests/` suite against the arche-trace embodied prover;
  the mapping from these operators to observable discourse is proposed in the
  surrounding prose, not empirically coded.
---

# Fact-sheet: pml

Written from [Stripped_Code/pml/](../../Stripped_Code/pml/) without reading the aspirational markdown in `pml/Modal_Logic/`. Cross-checked against Round 2 after drafting. Voice follows SCENE register: hypothetical correspondences, not identities.

## What the module defines

`pml_operators.pl` is the vocabulary file. It declares three mode predicates — `s/1`, `o/1`, `n/1` — as one-argument wrappers, each defined with a single vacuous clause `s(_).`, `o(_).`, `n(_).`. It declares four modal predicates — `comp_nec/1`, `exp_nec/1`, `exp_poss/1`, `comp_poss/1` — and a negation `neg/1`, all as vacuous one-place wrappers. It fixes the operator table: each modal and `neg` as `fx` at precedence 500, and the sequent arrow `=>` as `xfy` at precedence 1050. The module exports those eight symbols.

The three modes crossed with the four polarised modalities yield twelve composite operator positions (e.g. `s(comp_nec _)`, `o(exp_poss _)`, `n(exp_nec _)`). The module does not enumerate the twelve. They appear as applied terms in the other files.

`utils.pl` provides two list helpers the prover needs: `select/3` (split a list into element and remainder) and `match_antecedents/2` (every element of a list is a member of another).

`semantic_axioms.pl` defines `dialectical_transition/2` as eight ground facts mapping a bare stage symbol (`u`, `u_prime`, `a`, `lg`, `t`, `t_b`, `t_n`) to a modal term (`comp_nec(a)`, `exp_poss(lg)`, `comp_nec(neg(u))`, and so on). The file extends the multifile predicate `embodied_prover:material_inference/3` with six clauses: one that unfolds any dialectical transition under mode `s`, four fixed S→O and N→N transfer rules for the four modalities, and (after the 2026-04-16 fix) a hylomorphic shift rule `[o(P)] → n(P)` that lets an objective proof license a normative commitment.

`pragmatic_axioms.pl` defines `i_feeling/1`, `identity_claim/1`, and `impetus/1`. `i_feeling(I_f)` delegates to `arche_trace(automata):generate_trace/1` when its argument is a variable and to `contains_trace/1` when it is bound. `identity_claim(C_Id)` succeeds exactly when `C_Id` does *not* contain a trace. `impetus/1` holds on the single atom `holistic_striving`. The file extends `embodied_prover:material_inference/3` with the S-O Inversion rule (`[s(comp_nec I_f)] → o(exp_nec I_f)` when `i_feeling(I_f)`) and extends `incompatibility_semantics:is_incoherent/1` with the identity-with-feeling paradox: any sequent containing `n(represents(C_Id, I_f))` where `C_Id` is an `identity_claim` and `I_f` is an `i_feeling` is incoherent.

`intersubjective_praxis.pl` defines `oobleck_transition/4` as two facts relating aggressive action to crystallised position and listening action to liquefied position. It extends `embodied_prover:material_inference/3` with an S→O transfer that applies the oobleck mapping under the side-condition `A \= B` (the acting and positioning agents differ), and with a forgiveness rule: two `n(confession(_))` premises from distinct agents yield `n(exp_nec(forgiveness(A, B)))`.

`axioms_eml.pl` asserts eight bare `proves_impl/2` facts — the same stage-to-modality transitions as `semantic_axioms.pl`, but lifted into the incompatibility-semantics sequent form. It adds one recursive `proves_impl` clause that walks chains of `eml_axiom/2` lookups: necessity collapses the premise (`comp_nec` or `exp_nec`) and recurses; possibility discharges against the consequents. `eml_axiom/2` is defined as: a `proves_impl` clause exists and the consequent is one of the four modalities.

The `tests/` suite (`core_test.pl`, `simple_test.pl`) loads via `paths.pl` and `arche_trace(load)`. `core_test.pl` runs five groups: basic infrastructure (modules loaded, operators declared, utils working), automata (`highlander/2`, primes, trace generation), prover basics (identity, explosion, double negation, resource tracking and exhaustion), PML dynamics (seven dialectical-rhythm and Oobleck sequents), and trace mechanism (five I-feeling / identity / erasure checks). `simple_test.pl` covers a subset of the same ground in a flatter format.

## What axioms / inferences are asserted

**Dialectical transitions (Hegelian staging).** Eight stage→modality facts, asserted twice — once as `semantic_axioms:dialectical_transition/2` (for use by `material_inference`) and once as bare `axioms_eml:proves_impl/2` facts (for the EML chain). The stages form two short paths: `{u, u_prime} → comp_nec(a)`; `a → exp_poss(lg)`; `a → comp_poss(t)`; `lg → exp_nec(u_prime)`; `t → comp_nec(neg(u))`; and the two-cycle `t_b ↔ comp_nec(t_n)` (Being–Nothing).

**Inter-modal transfer (the "Oobleck" rules).** Four unconditional transfer rules in `semantic_axioms.pl`: S-compressive-nec → O-compressive-nec; S-expansive-nec → O-expansive-nec; N-compressive-nec → N-expansive-poss; N-expansive-nec → N-compressive-poss. These move a formula from one mode (or polarity) to another without a side-condition.

**O → N hylomorphic shift (added 2026-04-16).** `[o(P)] → n(P)`, unconditional. Objective proof licenses a normative commitment ("what is rational is actual"). Added after the Phase 5 audit flagged that the three modes carried no rule-governed handoffs beyond the S→O Oobleck pair. The reverse direction (N → S internalization) is not yet coded; it depends on trace-grounding conditions the module does not yet formalise.

**S-O Inversion (Axiom 1 / Elusive Subject).** `[s(comp_nec I_f)] → o(exp_nec I_f)` under side-condition `i_feeling(I_f)`. Defined in `pragmatic_axioms.pl`. The `i_feeling` side-condition is the one that routes through the `arche_trace` attribute: the proof descends to the prover, the sequent carries the trace, and the returned proof object is `erasure(_)` rather than `proof(_)`.

**Identity-with-feeling incoherence.** A sequent containing `n(represents(C_Id, I_f))` is flagged as `is_incoherent/1` when `C_Id` is a bare identity claim (no trace) and `I_f` is an `i_feeling` (has trace). This is the "Unsatisfiable Desire" axiom: a finite identity claim cannot represent the trace-bearing self.

**Oobleck between agents (intersubjective).** The S→O inference fires only when the acting agent `A` and the positioned agent `B` are distinct. Aggressive action crystallises the other's position; listening action liquefies it.

**Recognition / forgiveness.** Two `n(confession(_))` premises from distinct agents yield `n(exp_nec(forgiveness(A, B)))`. One rule, one arrow; no machinery for revoking it, no proof of symmetry.

**EML chaining.** `axioms_eml.pl`'s recursive clause treats necessity modalities as premises to be unfolded (reducing the sequent) and possibility modalities as targets to be discharged against the consequent. History tracking (`\+ member(s(P), History)`) prevents immediate re-selection of the same premise.

**What the tests exercise.** `core_test.pl` claims 20+ named tests; the groups it runs cover: module loading and operator declarations, the automata helpers `highlander/2` / `is_prime/1` / `nth_prime/2` / `generate_trace/1`, the prover rules (identity, explosion, left negation with proof ≠ erasure, resource tracking, resource exhaustion as `perturbation(resource_exhaustion)`), the seven PML dynamics sequents including the Bad Infinite cycle, and the five trace tests including the condition that `[s(I_f)] → s(I_f)` returns `erasure(_)` (not a content-bearing proof).

## What the module does NOT do

The module does not define PML as a proof theory beyond the five sequent rules the tests touch (identity, explosion, left-negation / double-negation-elim, resource bound, and dialectical unfolding via `material_inference`). Soundness, completeness, or decidability results for the twelve-operator logic are not formalised. The 28 core tests are structural checks against specific sequents, not meta-theorems.

The module does not code any discourse corpus. There is no file in `pml/` that reads a transcript, tags an utterance with a mode or polarity, or scores a coder's agreement. The operators `s`, `o`, `n` are empty one-place wrappers; no discriminator decides whether a given proposition is subjective, objective, or normative. The same holds for the polarity split (compressive/expansive): nothing in the code tests whether a candidate proposition belongs under `comp_*` or `exp_*`.

The module does not prove that the compression/expansion polarity tracks any observed cognitive or discursive phenomenon. The four `material_inference` transfer rules in `semantic_axioms.pl` fire unconditionally on any matching formula (the two S→O rules preserve polarity; the two N→N rules flip polarity and swap `nec` for `poss`). Whether those transfers correspond to moves a speaker makes in conversation is not tested in code.

The module does not model general multi-agent interaction. The only cross-agent rules are the two in `intersubjective_praxis.pl`: one S→O Oobleck transfer conditioned on `A \= B`, and one forgiveness rule. There is no mechanism for several speakers, for the timing of exchanges, for revocation of a commitment, or for asymmetric recognition.

The module does not ground its dialectical stage symbols (`u`, `u_prime`, `a`, `lg`, `t`, `t_b`, `t_n`) in anything external. They are bare atoms. The mapping from `u` (Hegel's "Universal" or similar) to the symbol `u` is a reading, not a check the code performs.

The module does not implement the `arche_trace` attribute itself. `i_feeling/1` delegates to `arche_trace(automata):generate_trace/1` and `contains_trace/1`, and the `erasure(_)` result is returned by the embodied prover in `arche-trace/`. The pml module supplies the predicates that put the trace-bearing variable into the sequent; the erasure behaviour lives downstream.

The module does not distinguish `proves_impl` (the bare fact store in `axioms_eml.pl`) from the prover-facing `material_inference` rules in `semantic_axioms.pl`. Both files restate the same eight dialectical transitions in different shapes. What uses which is a matter of which module is loaded in which test.

## Research goal

PML supplies a twelve-operator modal vocabulary (three modes crossed with four polarised modalities) whose structural theorems and dialectical transitions are exercised by the `tests/` suite against the arche-trace embodied prover; the mapping from these operators to observable discourse is proposed in the surrounding prose, not empirically coded.

## Cross-check against Round 2

Read lines 110–134 of [Prolog Code Analysis And Simulation ROUND 2 After Strip.md](Prolog%20Code%20Analysis%20And%20Simulation%20ROUND%202%20After%20Strip.md) after drafting. Round 2 and the fact-sheet agree on: the three modes `s/o/n`, the four polarised modalities at precedence 500 `fx`, `dialectical_transition/2` as the eight-fact staging (lines 110–116), the four S-O / N-N transfer rules in `semantic_axioms.pl` framed by Round 2 as "Axiom D: necessity dictates possibility" (line 116), the pragmatic split between trace-bearing `i_feeling` and trace-free `identity_claim` (lines 120–122), the S-O Inversion rule in `pragmatic_axioms.pl` (line 124), and the identity-with-feeling incoherence that fires on `n(represents(C_Id, I_f))` and collapses the state (line 126).

**Mechanical disagreement at line 134.** Round 2 states that the Oobleck S→O inference (and the forgiveness rule) fire "only if the unification engine successfully proves that entity A is identical to entity B". The stripped `intersubjective_praxis.pl` encodes the opposite side-condition: both `material_inference` clauses carry the guard `(A \= B)` — the two agents must be distinct. Round 2's reading inverts the intersubjective constraint. This matters: the text's reading makes the cross-agent rules reduce to reflexive transfers; the code makes them apply only across distinct agents, which is the point of calling the module *intersubjective_praxis*.

Other points are vocabulary rather than mechanics. Round 2 reads the transfer rules as a "physics of modality" (110–117) and names the S-O Inversion the "Elusive Subject" axiom (118–127). Those labels do not appear in the code; they are interpretive overlays on the same clauses.
