# Audit log: interfaces

Written after [docs/phase5/fact-sheet-interfaces.md](fact-sheet-interfaces.md).
Research goal from the fact-sheet is the cut criterion.

## Cut criterion

> The interfaces/ module contains five cross-module claim documents. After
> audit against stable module fact-sheets, claims that survive are those
> whose wires are either already in the code or whose empirical grounding
> is honestly marked as proposed, not coded.

The five relevant fact-sheets (read-only, absolute paths):

- `/tmp/umedcta-audit/pml/docs/phase5/fact-sheet-pml.md`
- `/tmp/umedcta-audit/arche-trace/docs/phase5/fact-sheet-arche-trace.md`
- `/tmp/umedcta-audit/formalization/docs/phase5/fact-sheet-formalization.md`
- `/tmp/umedcta-audit/learner/docs/phase5/fact-sheet-learner.md`
- `/tmp/umedcta-audit/strategies/docs/phase5/fact-sheet-strategies.md`

## Decisions

### interfaces/pml-to-discourse.md

Paired against: pml fact-sheet only (no module in this repo codes classroom
discourse). The document's central claim — PML operators as a coding
framework for teacher talk — is a research proposal, not a code report.

| File | Section / line range | Claim | Verdict | Rationale |
|---|---|---|---|---|
| pml-to-discourse.md | §The Claim (L3–L15) | "PML's 12 operators can be applied as a coding framework for classroom discourse." | commentary | pml fact-sheet: "The module does not code any discourse corpus. There is no file in pml/ that reads a transcript, tags an utterance…" The document itself states "No transcripts have been coded with PML." Research proposal. |
| pml-to-discourse.md | §The 12 Operators (L18–L39) | Four modal operators and three mode predicates exist. | keep | Mechanically correct; pml fact-sheet confirms operators and the three modes as vacuous wrappers. |
| pml-to-discourse.md | §Material Inferences (L41–L62) | 8 dialectical rhythm facts + Oobleck S→O. | keep | Mechanically correct; matches pml fact-sheet's "eight stage→modality facts" and the Oobleck clause. |
| pml-to-discourse.md | §Pragmatic Axioms (L63–L72) | `i_feeling/1` via `generate_trace/1`; `identity_claim/1` must not contain Trace. | keep | pml fact-sheet confirms both predicates and the trace/no-trace asymmetry. |
| pml-to-discourse.md | §Intersubjective Recognition (L74–L84) | Confession rule produces forgiveness; erases when grounded in I-Feeling. | substantiate | Rule exists; the "erases when grounded" claim is implied by the prover behaviour but not directly tested in `intersubjective_praxis.pl`. Follow-up: add a test that a sequent carrying an I-Feeling witness through the forgiveness rule returns `erasure(_)`. |
| pml-to-discourse.md | §Tests (L87–L93) | 28 core tests pass. | keep | Matches pml fact-sheet's test inventory. |
| pml-to-discourse.md | §Channel 1 Person-Position → S/O/N (L99–L108) | Grammatical person maps to mode. | commentary | No code discriminates subjective/objective/normative. pml fact-sheet: "no discriminator decides whether a given proposition is subjective, objective, or normative." |
| pml-to-discourse.md | §Channel 2 Polarity → Categorical/Hedging (L110–L121) | Stance compresses or expands; Boyd & Markarian justification. | commentary | No code scores utterances; proposal only. |
| pml-to-discourse.md | §Channel 3 Tempo → Up/Down (L123–L133) | Prosodic features code up/down direction. | commentary | The "least formalized channel"; document itself admits. No code backs this. |
| pml-to-discourse.md | §Where the Claim Exceeds (L135–L168) | Lists absence of coding manual, empirical validation, inter-rater reliability, N-mode axioms, TalkMoves comparison. | keep | This is the honest self-audit section. Preserves the document's epistemic humility. |
| pml-to-discourse.md | §What Would Strengthen (L171–L193) | Five follow-up research tasks (coding manual, pilot, N-mode axioms, TalkMoves comparison, Hennessy positioning). | keep | Research-agenda register; useful. |
| pml-to-discourse.md | §Literature Positioned (L196–L202) | Reading list. | commentary | Not a code claim. |

Aggregate: mostly commentary with a small spine of code-backed operator
inventory. The document's core claim is explicitly proposed-not-coded, and
the document says so. Cut the whole file to aspirational archive — its role
is research agenda, not interface report.

### interfaces/pml-to-learner.md

Paired against: pml + learner fact-sheets. The document's central claim —
PML polarity modulates inference cost through a single working wire — is
code-backed. Verified by direct read of `learner/meta_interpreter.pl`
(is_modal_operator/2, get_inference_cost/2) and
`arche-trace/embodied_prover.pl` (independent copies of both).

| File | Section / line range | Claim | Verdict | Rationale |
|---|---|---|---|---|
| pml-to-learner.md | §The Claim (L3–L15) | "PML operators modulate the learner's inference cost through a single working wire: the compressive/expansive context multiplier." | keep | Direct verification: meta_interpreter.pl:35–46 maps `comp_nec/comp_poss → compressive`, `exp_nec/exp_poss → expansive`; get_inference_cost 2/1/1. Pml fact-sheet and learner fact-sheet both confirm. |
| pml-to-learner.md | §Modal Context in Meta-Interpreter (L17–L41) | Code excerpt + explanation of context switching and cost multiplication. | keep | Matches the source. Learner fact-sheet confirms "Context multipliers: compressive=2, expansive=1, neutral=1 (inference-cost table)." |
| pml-to-learner.md | §Tension Dynamics Sharper Multipliers (L43–L63) | 2.5 / 1.0 / 0.3; stability second-derivative; 9-entry window. | keep | Learner fact-sheet confirms exactly these constants and thresholds. |
| pml-to-learner.md | §Cost Types Table (L65–L81) | Seven cost entries in config.pl with values. | keep | Matches learner fact-sheet's config.pl inventory. |
| pml-to-learner.md | §The Embodied Prover's Parallel (L84–L104) | Independent parallel copy of the wire in arche-trace. | keep | Arche-trace fact-sheet confirms `determine_modal_context/2` and `get_inference_cost/2` in embodied_prover.pl; explicitly notes these are "re-implemented." |
| pml-to-learner.md | §Modal Trace Output (L107–L117) | `modal_trace/4` term emitted. | substantiate | Mentioned in meta_interpreter.pl's solve clauses (verified) but the "visible through HTTP API" specifics are not checked against server.pl routes in the fact-sheet. Follow-up: spot-check that `/api/events` actually serialises `modal_trace/4`. |
| pml-to-learner.md | §Where the Claim Holds — Cost Multiplier Real Arithmetic (L121–L133) | The wire is not metaphorical; wrapping a goal changes the arithmetic. | keep | Directly supported by both fact-sheets. |
| pml-to-learner.md | §Where the Claim Holds — Tension Instability Context-Sensitive (L135–L149) | Compressive context makes crisis more likely. | keep | Follows from the multiplier values documented in learner fact-sheet. |
| pml-to-learner.md | §Wire Is Polarity-Only (L151–L165) | Mode (S/O/N) has no computational effect. | keep | Honest limitation; pml fact-sheet confirms `s/o/n` are vacuous wrappers. |
| pml-to-learner.md | §No Meaning Fields (L167–L182) | Meaning fields not implemented. | commentary | Proposal for future work; honest. |
| pml-to-learner.md | §Decentering Not Modeled (L184–L194) | Oracle is modally neutral. | keep | Learner fact-sheet: "strategy_appropriate_for/3 picks by a numerical cost heuristic; extract_interpretation/7 is a format/3 template." |
| pml-to-learner.md | §Person-Position Tracking Absent (L196–L208) | Strategy provenance not recorded. | keep | Learner fact-sheet: "No belief revision over strategies…"; no provenance mechanism in the stripped code. |
| pml-to-learner.md | §Context Restoration Limits Modal Flow (L210–L217) | Context resets after sub-goal. | keep | Matches the `solve/6` threading pattern documented in learner fact-sheet. |
| pml-to-learner.md | §What Would Strengthen (L219–L251) | Five follow-up tasks (mode-sensitive cost, provenance tracking, modal persistence, oracle decentering, meaning field prototype). | keep | Research agenda; each item is scoped to specific files the fact-sheets anchor. |

Aggregate: largely code-backed. Keep in place. The document is what this
audit is looking for: a cross-module claim with the wire verified and the
gaps honestly flagged.

### interfaces/compression-across-layers.md

Paired against: pml + strategies + learner fact-sheets. The document argues
that PML polarity and strategy temporal compression are "the same
structure." The weakened claim — strategies emit PML terms; those terms
modulate cost — is code-backed; the strong "same structure" framing is not.

| File | Section / line range | Claim | Verdict | Rationale |
|---|---|---|---|---|
| compression-across-layers.md | §The Claim (L3–L20) | Teacher discourse and student arithmetic "share this polarity." | commentary | Philosophical framing. The strong "same structure" claim is not code-backed; the document itself says as much in the "Where the Claim Exceeds" section. |
| compression-across-layers.md | §Compression in Strategy Automata (L24–L42) | Four addition strategies at descending temporal cost. | keep | Matches strategies fact-sheet's automata list. |
| compression-across-layers.md | §Compression in PML (L44–L62) | Four modal operators encode polarity; dialectical rhythm facts. | keep | Matches pml fact-sheet. |
| compression-across-layers.md | §Strategy Automata Emit PML Operators (L64–L79) | math/ automata emit `s(comp_nec(...))` / `s(exp_poss(...))`. | keep | Verified: grep found `s(comp_nec` / `s(exp_poss` in 22 of 27 math/ files (221 total occurrences). Strategies fact-sheet confirms: "Every non-trivial transition in the math/ automata calls s(comp_nec(Atom)) or s(exp_poss(Atom))." |
| compression-across-layers.md | §The Wire Modal Context Multiplies Cost (L82–L113) | Polarity → context → multiplied cost. | keep | Same wire as pml-to-learner.md §Modal Context; verified. |
| compression-across-layers.md | §Cost Tracking in Strategies (L115–L131) | Seven cost types; ratio IS compression ratio. | substantiate | The cost-type list is correct, but strategies fact-sheet flags: "incur_cost/1 body is `true` — no counter is decremented, no budget is depleted." The ratio claim assumes a counter that is wired. Follow-up: close the incur_cost vacuity so the compression-ratio claim has a measurable denominator. |
| compression-across-layers.md | §Cost Multiplier Works (L135–L142) | 2.5x for compressive tension; "not metaphor — arithmetic in the tension dynamics." | keep | Learner fact-sheet confirms the 2.5/1.0/0.3 tension multipliers. |
| compression-across-layers.md | §Developmental Trajectory Is Real (L144–L151) | CGI-ordered strategies; grounding preserves differential. | commentary | CGI attribution is accurate (per strategies fact-sheet, `math/README.md`). "Developmental trajectory" is a framing claim — the code does not encode a progression graph. |
| compression-across-layers.md | §Bad Infinite Is Compressive Stagnation (L153–L167) | `is_bad_infinite/1` cycle detector. | keep | Arche-trace fact-sheet confirms the predicate at `critique.pl` and notes "is_compressive_node/1 matches on the atom pml_rhythm as the rule head" — the detector works at this structural level. |
| compression-across-layers.md | §"Same Structure" Not Proved (L170–L184) | Formal isomorphism absent. | keep | Honest self-audit. |
| compression-across-layers.md | §Scalar Multiplier Does Not Distinguish Modes (L186–L193) | Polarity is coded; modes aren't. | keep | Same honest limitation as pml-to-learner.md. |
| compression-across-layers.md | §Temporal vs. Discursive Compression (L195–L212) | Brandomian argument for shared inferential-commitment structure. | commentary | Philosophical. Document admits: "this argument is philosophical, not formalized." |
| compression-across-layers.md | §No Empirical Connection (L214–L221) | No data, no catastrophe measurement yet. | keep | Honest. |
| compression-across-layers.md | §What Would Strengthen (L223–L249) | Four follow-up tasks. | keep | Research agenda. |
| compression-across-layers.md | §The Fractal Hypothesis (L251–L261) | Each automaton = Zeeman disc, etc. | commentary | Explicitly labelled "Whether it coheres as formal mathematics or remains a generative metaphor for further formalization is not yet determined." Memory project_fractal_catastrophe_vision.md corroborates that this is a generative metaphor, not a formal claim. |

Aggregate: mostly code-backed for the weak reading; the strong reading is
commentary. The document honestly self-audits. Keep in place — the wire it
describes is live in the code.

### interfaces/arche-trace-to-learner.md

Paired against: arche-trace + learner fact-sheets. Pay special attention to
the acknowledged wire: `learner:more_machine_learner:count_loop/4 →
proves([] => ...)`. Most of this document's content is about the gap
between parallel subsystems that do not directly connect.

| File | Section / line range | Claim | Verdict | Rationale |
|---|---|---|---|---|
| arche-trace-to-learner.md | §The Claim (L3–L20) | Three augmentation points: erasure, resource exhaustion, pathology detection. | commentary | Arche-trace fact-sheet: "The 'four erasure points' enumerated in ARCHE_TRACE_ERASURE.md are four demonstrated sequents in a catalog, not four structural code sites." The three-point framing overclaims structure. The document itself later concedes the points are "not wired into the learner's execution path." |
| arche-trace-to-learner.md | §Learner's Execution Path (L22–L47) | Pseudo-code chain from `run_computation/2` through `solve/6` to perturbation dispatch. | keep | Matches learner fact-sheet's execution-handler dispatch table. |
| arche-trace-to-learner.md | §Scene-Agnostic Sequent Engine (L50–L61) | `proves/1`, include axiom sets. | keep | Matches arche-trace fact-sheet's description of `incompatibility_semantics.pl`. |
| arche-trace-to-learner.md | §Resource-Tracked Prover (L62–L67) | `proves/4`, modal context, `erasure(RuleName)`. | keep | Matches arche-trace fact-sheet's `construct_proof/4` bifurcation. |
| arche-trace-to-learner.md | §Trace Mechanism (L68–L73) | `generate_trace/1`, `contains_trace/1`, `attr_unify_hook/2`. | keep | Matches arche-trace fact-sheet. |
| arche-trace-to-learner.md | §Critique Module (L74–L79) | `reflect/2`, `accommodate/1`, `stress/2`. | keep | Matches arche-trace fact-sheet. |
| arche-trace-to-learner.md | §Dialectical Engine (L80–L83) | `run_computation/2`, `run_fsm/4`. | keep | Matches arche-trace fact-sheet. |
| arche-trace-to-learner.md | §Point 1 Proof Erasure (L87–L117) | `construct_proof/4` excerpt + four-row table of erasure points. | substantiate | `construct_proof/4` code matches source (verified at embodied_prover.pl:98–105 per arche-trace fact-sheet). The four-row table reads as structural catalogue, which arche-trace fact-sheet flags as overclaim: "four demonstrated sequents in a catalog, not four structural code sites." Follow-up: reframe the table as "four demonstrated sequents in the test suite" rather than "four documented erasure points." |
| arche-trace-to-learner.md | §Point 2 Resource Exhaustion (L119–L139) | Two parallel mechanisms (embodied prover's budget vs. learner's tension). | keep | Matches both fact-sheets. |
| arche-trace-to-learner.md | §Point 3 Pathology Detection (L141–L158) | `is_bad_infinite/1` + three accommodate dispatch paths. | keep | Matches arche-trace fact-sheet exactly (including the "accommodate always fails" behaviour). |
| arche-trace-to-learner.md | §Erasure Works in Tests (L160–L177) | Test cases for identity with/without trace. | keep | Matches pml fact-sheet's core_test inventory. |
| arche-trace-to-learner.md | §Crisis Classification Is Operational (L179–L191) | Five crisis types. | keep | Matches learner fact-sheet's taxonomy exactly. |
| arche-trace-to-learner.md | §Domain Axioms Enforce Normative Boundaries (L193–L199) | `axioms_domains.pl` throws normative_crisis. | keep | Matches learner fact-sheet. |
| arche-trace-to-learner.md | §Learner Does Not Use Arche-Trace Prover (L202–L236) | The central gap: learner has parallel machinery; only wire is `successor/2 :- proves([] => [o(plus(X, 1, Y))])`. | keep | **This is the honest payload.** Verified: `more_machine_learner.pl:109 successor(X, Y) :- proves([] => [o(plus(X, 1, Y))]).` The five-row parallel-mechanisms table is accurate per both fact-sheets. |
| arche-trace-to-learner.md | §Module Name Collision (L238–L243) | `embodied_prover.pl` declares as `incompatibility_semantics`. | substantiate | Needs direct verification of the module line in the stripped source. The arche-trace fact-sheet does not flag a name collision; it distinguishes the two modules by file but not by declared module name. Follow-up: check whether the collision is still present in the current stripped code and, if so, document it in the arche-trace fact-sheet's NOT-do section. |
| arche-trace-to-learner.md | §Critique Module Not Called (L245–L250) | `critique.pl:reflect/2` not invoked by learner. | keep | Learner fact-sheet: "reflective_monitor.pl" is the learner's reflection predicate; `critique.pl` is never cited. Gap is real. |
| arche-trace-to-learner.md | §Dialectical Engine Is Unused (L252–L257) | Learner uses its own `run_computation/2`. | keep | Learner fact-sheet exports `execution_handler:run_computation/2`; arche-trace fact-sheet notes `dialectical_engine:run_computation/2` coexists. Gap is real. |
| arche-trace-to-learner.md | §Erasure Has No Pedagogical Consequence (L259–L265) | `erasure(...)` is a marker, not a trigger. | keep | Arche-trace fact-sheet's "No pedagogical claim" and "No phenomenological claim the mechanism could carry" sections support this. |
| arche-trace-to-learner.md | §Option A Wire the Prover (L269–L281) | Proposed integration path. | commentary | Research agenda. |
| arche-trace-to-learner.md | §Option B Document Parallel (L283–L304) | Manuscript distinguishes computing and justifying. | commentary | The position CLAUDE.md explicitly endorses: "Do not merge solve/6 and proves/1." This is the live interpretation of the parallel. |
| arche-trace-to-learner.md | §Option C Route Erasure to Oracle (L306–L311) | Proposed integration path. | commentary | Research agenda. |
| arche-trace-to-learner.md | §Regardless of Option (L313–L320) | Three shared follow-ups (resolve collision, worked example, document commitment). | keep | Concrete follow-ups, scoped. |

Aggregate: mostly code-backed description of parallel subsystems, plus
honest commentary on the gap between them. The document's value is the
accurate parallel-mechanisms table, not the three-point framing. Keep in
place; rename or reframe the "three augmentation points" framing in a
future pass.

### interfaces/strategies-to-formalization.md

Paired against: strategies + formalization fact-sheets. The vacuous
`incur_cost/1` gap is known (both fact-sheets flag it from opposite sides).
The document's small claim (shared primitives) is code-backed; the large
claim (Gödel-adjacent incompleteness) is explicitly flagged as unformalised.

| File | Section / line range | Claim | Verdict | Rationale |
|---|---|---|---|---|
| strategies-to-formalization.md | §The Claim (L3–L16) | Strategies generate arithmetic rich enough to instantiate Robinson Q; strategies "arrive at a system where self-reference is possible." | commentary | Philosophical claim. The strategies fact-sheet confirms primitive-sharing; the formalization fact-sheet confirms Q axioms exist. Neither confirms "self-reference is possible" — formalization fact-sheet: "No Gödel numbering. No Gödel sentence. No meta-theorem." |
| strategies-to-formalization.md | §Robinson Q Axiomatization (L19–L47) | Q1–Q7 table + `arith_op` / `is_recollection` routing. | keep | Matches formalization fact-sheet's Q axiom inventory and the arith_op dispatch. |
| strategies-to-formalization.md | §is_recollection Calls hermeneutic_calculator (L49–L67) | "Robinson Q proofs literally invoke the strategy automata." | keep | Formalization fact-sheet confirms: `is_recollection/2` in `axioms_robinson.pl` calls `hermeneutic_calculator:calculate(Prev, +, 1, ...)`. This is a real cross-module call. |
| strategies-to-formalization.md | §Grounded Arithmetic Primitives (L69–L89) | Table of primitives from grounded_arithmetic.pl and grounded_utils.pl. | keep | Matches formalization fact-sheet's export list. |
| strategies-to-formalization.md | §27 Strategy Automata (L91–L113) | All 27 grounded; FSM interface; history lists. | keep | Matches strategies fact-sheet's automata inventory. |
| strategies-to-formalization.md | §Standards Mapping (L115–L128) | 20 K-3 modules, 153 tests. | keep | Matches strategies fact-sheet exactly. |
| strategies-to-formalization.md | §Hermeneutic Calculator (L130–L140) | `calculate/6` dispatcher. | keep | Matches strategies fact-sheet's `hermeneutic_calculator.pl`. |
| strategies-to-formalization.md | §Shared Primitives (L143–L163) | Strategies and Q use same grounded arithmetic. | keep | Directly supported by both fact-sheets. **This is the core surviving claim.** |
| strategies-to-formalization.md | §Robinson Q Is Instantiated (L165–L178) | 20 tests verify specific instances; route through arith_op → is_recollection → counting histories. | substantiate | Mechanically correct, but formalization fact-sheet flags: "Schematic instances are not universal statements"; the test harness runs 20 `test/2` cases per axiom, not universally quantified proofs. Follow-up: weaken "Robinson Q is instantiated" to "20 schematic instances of Q hold in code." |
| strategies-to-formalization.md | §Strategies Cover Robinson Operations (L180–L193) | Strategies provide successor, addition, multiplication. | keep | Strategies fact-sheet confirms the 5+9+4+4 addition/subtraction/mult/div coverage. |
| strategies-to-formalization.md | §No Explicit Encoding Map (L196–L220) | No function mapping strategy traces to Q derivations. | keep | Honest self-audit. The worked example (steps 1–5) is a research agenda. |
| strategies-to-formalization.md | §Gödel Numbering Not Wired (L222–L228) | Prime utilities exist but not connected to syntax encoding. | keep | Both formalization and arche-trace fact-sheets confirm. |
| strategies-to-formalization.md | §No Gödel Sentence (L230–L234) | Not constructed. | keep | Formalization fact-sheet: "No Gödel sentence." |
| strategies-to-formalization.md | §Meta-Theorem Unproved (L236–L242) | "Every theorem of Q is a theorem of the HC" not proved. | keep | Formalization fact-sheet exact quote: "No meta-theorem." |
| strategies-to-formalization.md | §Schematic vs Universal (L244–L250) | Test output overstates schematic instances. | keep | Formalization fact-sheet: "robinson_q.pl's test output's 'triumphant framing' overstates schematic instances." Same finding, cross-confirmed. |
| strategies-to-formalization.md | §What Would Strengthen (L252–L278) | Five follow-ups (worked example, encoding map, Gödel numbering, meta-theorem for addition, honest framing in tests). | keep | Research agenda, scoped. |

Not caught above but worth noting: **both fact-sheets flag the vacuous
`incur_cost/1`** — `formalization/grounded_arithmetic.pl`'s body is `true`,
so every `incur_cost(...)` call in a math/ automaton is a no-op. The
document does not mention this. The cost-tracking narrative relies on a
counter that is not currently decrementing. Flag: document should
explicitly note that the cost story is narrative-only until `incur_cost/1`
is wired to a counter.

Aggregate: mostly code-backed at the primitive-sharing level; honest about
what is missing at the meta-theorem level. Keep in place. The headline
framing ("Raises the Incompleteness Question") is a stretch, but the body
of the document is an honest accounting.

## Relocations

- `interfaces/pml-to-discourse.md` → `archive/interfaces-aspirational/pml-to-discourse.md`
  — Core claim (PML as discourse coding framework) is explicitly a research
  proposal with no code backing in any module. The pml fact-sheet's
  negative space is definitive: no discourse corpus, no coder, no
  discriminator. The document self-labels as "framework claim, not an
  empirical finding." Belongs with other aspirational material.

The other four documents are retained in `interfaces/` with this audit log
identifying which sections are code-backed (keep), which are honest
research agenda (commentary), and which need concrete follow-up
(substantiate). None of those four documents has a central claim that fails
against the fact-sheets wholesale; each describes at least one verified
cross-module wire.

Commentary banners: none added. Each retained document's own "Where the
Claim Exceeds" / "Where the Claim Holds" structure already partitions
code-claim from commentary at paragraph level. Adding a file-level banner
would duplicate that partition at lower resolution.

## Deletions

None. Everything retained is either code-backed or relocated to archive
with rationale.

## Real cross-module gaps surfaced by the audit

The audit's highest-value output is a short list of specific claims that
are currently being asserted by interface documents but are not backed by
code in any module fact-sheet. Future work needs to close these, or the
documents need to be further weakened:

1. **Vacuous `incur_cost/1`.** Every cost-tracking narrative in
   `compression-across-layers.md` and every ratio claim in
   `strategies-to-formalization.md` rests on a predicate whose body is
   `true`. Until `formalization/grounded_arithmetic.pl:incur_cost/1`
   decrements a real counter, the "compression ratio is real arithmetic"
   and "strategies cost more because they do more temporal work" claims
   are narratively true but mechanically vacuous. Both module fact-sheets
   flag this from opposite sides; neither interface document addresses it.

2. **No elaboration graph between strategies.** `compression-across-layers.md`
   and `strategies-to-formalization.md` both implicitly rely on ordered
   relationships between strategies (counting_on → COBO → RMB → rounding),
   but strategies fact-sheet is emphatic: "No elaboration graph between
   strategies. Strategies are a flat list." The developmental-trajectory
   language in these documents is more than the code can currently support.
   Needs either an elaboration graph in strategies/ or explicit weakening
   of the trajectory claim.

3. **Erasure has no pedagogical consequence.** `arche-trace-to-learner.md`
   names this honestly. But the three interface documents that touch PML
   (discourse, learner, compression) imply that modal erasure is where
   teaching intervention belongs. No code routes an erasure signal to the
   oracle. The philosophical framing is outrunning the wire.

4. **Mode (S/O/N) has no computational effect anywhere.** Three interface
   documents describe S/O/N distinctions as central to the claim, yet the
   pml fact-sheet confirms the three mode predicates are vacuous wrappers
   with no discriminator. The polarity wire is real; the mode wire is not.
   Future work: either implement a mode-sensitive cost or provenance
   mechanism, or weaken every mode-dependent claim in the interface
   documents to "polarity-only."

5. **Module name collision (embodied_prover ↔ incompatibility_semantics).**
   `arche-trace-to-learner.md` flags this. The arche-trace fact-sheet does
   not confirm or deny. Needs direct verification in the current code; if
   still present, belongs in the arche-trace fact-sheet's NOT-do section.

6. **No discourse corpus anywhere.** This is the largest silent gap.
   `pml-to-discourse.md` and (implicitly) `compression-across-layers.md`
   presuppose that PML can be *applied* to talk. No module in this repo
   codes talk. If empirical exposure is a goal, the corpus-acquisition
   step is unscheduled.
