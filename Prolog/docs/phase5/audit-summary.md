# Code-docs coordination audit — summary rollup

Date: 2026-04-16. Plan: [`~/.claude/plans/swirling-kindling-quail.md`](~/.claude/plans/swirling-kindling-quail.md). Scope: six modules (`pml/`, `arche-trace/`, `formalization/`, `learner/`, `strategies/`, `interfaces/`) swept to reconcile the three documentation layers (code comments, markdown, user-facing HTML) with what the stripped Prolog actually does.

Five parallel subagents wrote fact-sheets directly from comment-stripped source, cross-checked each claim against a Gemini Deep Research report that was independently generated from the same stripped source (Round 2), and ruthlessly audited the aspirational markdown layer against the resulting research goals. A cross-module consistency review and a sequential interfaces sweep landed last.

## Six research goals (verbatim)

1. **pml/** — PML supplies a twelve-operator modal vocabulary (three modes crossed with four polarised modalities) whose structural theorems and dialectical transitions are exercised by the `tests/` suite against the arche-trace embodied prover; the mapping from these operators to observable discourse is proposed in the surrounding prose, not empirically coded.
2. **arche-trace/** — The sequent calculus proof object in `embodied_prover.pl` collapses from `proof(...)` to `erasure(...)` at every rule-application site when the sequent contains an arche_trace-attributed variable. The mechanism is singular, not four-part: the bifurcation lives in `construct_proof/4` and propagates by inspection of subproofs and term variables. The "four erasure points" enumerated in `ARCHE_TRACE_ERASURE.md` are four demonstrated sequents in a catalog, not four structural code sites.
3. **formalization/** — Robinson Q's seven axioms (Q1–Q7) and the commutativity of addition and multiplication are asserted as `proves_impl/2` clauses in `axioms_robinson.pl` and `robinson_q.pl`; grounded arithmetic is defined over tally-list recollections in `grounded_arithmetic.pl`, and Q1–Q7 rules in `axioms_robinson.pl` route through `arith_op/4` (the hermeneutic calculator's arithmetic layer). No Gödel numbering is wired, no Gödel sentence is constructed, no meta-theorem linking the HC to Q is proved in code.
4. **learner/** — The ORR cycle is implemented as `solve/6` + `execution_handler:run_computation/2` + `oracle_server:query_oracle/4` + `reorganization_engine`. Crisis classification is mechanical (resource exhaustion, tension instability, unknown operation, normative crisis, incoherence); the learning trajectory is not empirically validated against student behavior. Genuine FSM synthesis is archived; the "synthesize from oracle" path reaches a shim that fails.
5. **strategies/** — 27 FSM automata model children's arithmetic strategies (Carpenter/Fennema CGI taxonomy via Hackenberg, per attribution in `math/README.md` — not carried in the Prolog sources themselves); 20 Indiana K-3 standards modules cover Kindergarten through Grade 3 and dispatch to those automata. No elaboration graph connects the automata to one another. Fractions remain open: `jason_fsm.pl` and `standard_3_ns_2.pl` exist but are not integrated with the ORR cycle.
6. **interfaces/** — The module contains five cross-module claim documents; the surviving claims are those whose wires are in the code or honestly flagged as proposed; the module's productive role is as a research-agenda archive, not an integration layer.

## Cuts by module (24 files total)

| Module | Cuts | Destination |
|---|---|---|
| pml | 15 | [archive/pml-modal-logic/](../../archive/pml-modal-logic/) |
| formalization | 1 | [archive/formalization-stale/](../../archive/formalization-stale/) |
| learner | 4 | [archive/learner-framing/](../../archive/learner-framing/) |
| strategies | 3 | [archive/strategies-aspirational/](../../archive/strategies-aspirational/) |
| interfaces | 1 | [archive/interfaces-aspirational/](../../archive/interfaces-aspirational/) |
| arche-trace | 0 | — (docs confirmed realistic) |

All cuts are git renames (100% similarity). Content is recoverable from history; the relocation is itself the audit's decision-log entry.

## Commentary banners (6 files kept in place with banner)

All in `pml/Modal_Logic/`: `README.md`, `Philosophical_Primer.md`, `Description_Role_of_Formalization.md`, `Overview_of_UMEDCA_Manuscript.md`, `Dictionary.md`, `AppendixA_Unified_2.tex`. Each carries a banner marking the register: commentary, not code-report.

## Findings worth acting on

Six cross-module gaps surfaced during the interfaces sweep. Each is a place where an interface doc's claim cannot be backed because the underlying wire is missing or hollow:

1. **~~`incur_cost/1` is a no-op.~~** *(Closed 2026-04-16, same day.)* Pre-fix: the body was `true` for direct callers; every automaton's `incur_cost(...)` was a name broadcast to nowhere. Post-fix: direct callers accumulate into a module-local counter in `grounded_arithmetic` (`direct_cost_accumulated/1`, `reset_direct_cost_accumulator/0`). Meta-interpreter-routed calls continue to track through the learner's inference budget. Two accumulators, no reconciliation; the local cost table duplicates `learner/config.pl:cognitive_cost/2` and can drift. Accounting is real but approximate.
2. **No strategy elaboration graph.** CGI developmental language in `compression-across-layers.md` and `strategies-to-formalization.md` exceeds what the flat `strategies/math/` list encodes. The AutomatonAnalyzer port from LK_RB_Synthesis remains the obvious remediation.
3. **Erasure has no code-level pedagogical consequence.** `erasure(...)` is a marker produced by the prover; it is never routed to the oracle, to a crisis trigger, or to any intervention. Tio's reading (recorded in memory `project_erasure_reframe.md`) is that this is philosophically correct: erasure lives in discourse, not in the sequent. What the learner should not do is wire erasure to a pedagogical hook — classroom talk is where the construct surfaces ("I don't know why 2+3=5, but I'm sure").
4. **~~Mode wrappers have no computational effect.~~** *(Partly closed 2026-04-16.)* Pre-fix: `s/1`, `o/1`, `n/1` were defined as `s(_).` and used only as pattern-match tags. Post-fix: `semantic_axioms.pl` adds a hylomorphic shift `[o(P)] → n(P)` — objective proof licenses normative commitment. Wrapper bodies remain permissive (type-tags), but modes now have one rule-governed handoff beyond the S→O Oobleck pair. N → S internalization remains open.
5. **Dangling references in learner/.** `reorganize_system/2` calls `more_machine_learner:discover_strategy/3`, which is not defined in the stripped code. `fsm_synthesis_engine.pl` is a shim that always fails. The "learning loop" the ORR cycle describes does not actually close in code.
6. **No discourse corpus.** `pml-to-discourse.md`'s proposal to treat PML as a coding framework for classroom talk presupposes data this repo does not host. The `pml-to-discourse.md` file was cut because the proposal outran the code; if empirical exposure is a goal, corpus acquisition is unscheduled and remains unscheduled.

A seventh finding is cross-cutting between arche-trace and learner: **the only real cross-module invocation in the repo** is `more_machine_learner.pl:count_loop/4 → proves([] => [o(plus(X, 1, Y))])` into `arche-trace/incompatibility_semantics.pl:proves/1`. The learner uses the sequent calculus to derive Peano successors. This is the one wire the fact-sheets unanimously confirm.

## Round 2 cross-check results

The Gemini Round 2 report (stripped-code input only) turned out to be mechanically accurate but vocabulary-separated from the stripped source — Gemini's purple register is endogenous. Three specific Round 2 findings:

- **pml** — Round 2 at L134 inverts the agent-identity constraint for intersubjective `material_inference` rules (says `A = B` required; code requires `A \= B`). Fact-sheet wins; Round 2's reading is flagged.
- **learner** — Round 2 at L191 describes `reflect_and_learn/1` and `critique_and_bootstrap/1` as "placeholder routines"; these predicates are not present in the stripped `more_machine_learner.pl` at all. Silent drift, not honest labeling.
- **strategies** — Round 2 names strategies at the oracle-dispatch level (L163–180) but does not trace any automaton's state machine, does not mention the FSM engine, does not mention the 20 standards modules. The module with the most Prolog has the thinnest Round 2 coverage. Thinness is the finding.

Everywhere else, Round 2's mechanics match the fact-sheets. The original worry that "Deep Research is paraphrasing my docs back at me" was wrong: Deep Research is accurately paraphrasing the code in Gemini's voice, and the purple prose is the model's default, not a confabulation from the documentation.

## What this enables

Stream 1 (cross-repo consolidation): fact-sheets have machine-readable YAML front-matter with SHAs, exports, and operators. A cross-repo indexer can parse them without reading prose. Next step: generate the export summary from the front-matter and compare against LK_RB_Synthesis expectations.

Stream 2 (academic exposure): the PML feasibility paper can now cite [`docs/phase5/fact-sheet-pml.md`](fact-sheet-pml.md) as the code-truth source. The research goal's explicit "proposed, not empirically coded" clause is the honest limitation the paper's §6 "What I don't know" must absorb.

Stream 3 (user-facing HTML via more-zeeman): deferred. Companion reference pages come after all six fact-sheets are stable, which they now are. When built, each page sources from the corresponding fact-sheet and uses the existing `more-zeeman/shared.js` discourse-level toggle.

## What this did NOT do

- Did not rewrite code. Every change is in documentation, `archive/`, or new `docs/phase5/` files.
- Did not build the HTML companion layer.
- Did not import or port from scattered repos.
- Did not attempt to close the six gaps above. The audit makes them visible; closing them is the next phase.

## Artifacts

- 6 fact-sheets (1,391 lines total): [`docs/phase5/fact-sheet-*.md`](.)
- 6 audit logs (498 lines total): [`docs/phase5/audit-*.md`](.)
- 24 relocated files in `archive/<topic>/`
- 3 template/support files: [`docs/phase5/_fact-sheet-template.md`](_fact-sheet-template.md), [`docs/phase5/_audit-template.md`](_audit-template.md), [`docs/phase5/_round2-section-map.md`](_round2-section-map.md)
- Commented-stripped source: [`Stripped_Code/`](../../Stripped_Code/) (gitignored)
- Stripper script: [`scripts/strip_for_audit.py`](../../scripts/strip_for_audit.py)
