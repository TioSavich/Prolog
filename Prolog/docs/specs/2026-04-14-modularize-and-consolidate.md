# Spec: Modularize with Firebreaks, Consolidate Scattered Work

**Date:** 2026-04-14
**Status:** Phase 2 complete with open items (see below)
**Approach:** C → A (module-first extraction with firebreaks, then audience-driven academic products)

---

## Problem

A year of part-fixing without whole-reflecting produced an inconsistent codebase. Core ideas are tangled with vestigial material. Unfinished work is scattered across multiple repositories (LK_RB_Synthesis_Project, September_UMEDCA, UMEDCA_2026). The context window — both Claude's and Tio's — is polluted by documentation that describes problems rather than resolving them. Nothing is share-ready because everything implicates everything else.

The git history confirms the pattern: 56 commits in two weeks, none touching the structural issues (lazy automata, hollow synthesis engine, disconnected modules). Each burst of work responds to a genuine impetus — visualize, polish, formalize Robinson, wire the elaboration chain — but each one drifts from the center.

## Principle

Draw hard boundaries so independent ideas cannot contaminate each other. Each module lives or dies on its own terms. Cross-module connections become explicit interface documents that make philosophical claims, not implicit code tangles. Then each module maps to a specific academic product for a specific audience.

## Core Commitments (what survives)

These were identified and confirmed through collaborative review:

1. **PML as a discourse analysis instrument** — 12 operators, decentering signature, person-position markers (Carspecken). The most shareable, most fundable, most academically positioned piece.
2. **The arche-trace / erasure boundary** — where formal proof goes hollow. A philosophical artifact that belongs in a paper, not tangled into a learner model.
3. **Best Day intervention** — AI pairs students by complementary PML profiles. The fundable application.
4. **ORR cycle visualization** — invitation for non-Prolog audiences. Keep and enact.
5. **Robinson Arithmetic as signal** — math education as meta-mathematical project. Not a proof project. The raising of the incompleteness question is the point.
6. **Strategy automata as choreography** — children's invented arithmetic as written choreography for embodied cognition (from LK_RB_Synthesis). Fractal architecture: Iterative Core + Strategic Shell.
7. **Temporal compression/decompression = PML compression/expansion** — the same polarity structure operates in student arithmetic action and teacher discourse. This is a genuine cross-level claim.

### Repositioned (not core, but not dead)

- **Crisis cycle** → optional module for constructivists. Critical Math is intersubjectivity-first; a monological learner model is a testable module, not a core commitment.
- **"The Negative" / yogic logic** → the original insight driving everything. Lives in the arche-trace philosophical essay when ready. Not committed to any specific form elsewhere until then.

### Confirmed dead

- Synthesis engine (oracle-wrapping, not genuine synthesis)
- Overlapping architecture docs that describe what should exist
- synthesized_paper.md
- Dead pattern detectors (already marked DEPRECATED in code)
- Ralph Loop scoring docs, old plans/specs (mostly flushed in e22c9bd)

---

## Phase 1: Archive — DONE (2026-04-14)

Clear vestigial material. Janitorial, no philosophical decisions.

**Move to `archive/`:**
- `prolog/fsm_synthesis_engine.pl`
- `synthesized_paper.md` (if still present)
- `prolog/SYNTHESIS_HONESTY.md`
- `prolog/SYNTHESIS_FEASIBILITY.md`
- `prolog/ARCHITECTURE.md` (superseded by design/)
- `prolog/CURRICULUM_MAP.md` (superseded by design/)
- Any remaining old plans, specs, phase-specific test files
- DEPRECATED sections of `more_machine_learner.pl` (extract live code, archive the rest)

**Do not archive:** ORR visualization, standards modules, strategy automata, sequent calculus, PML tests, Robinson Q, overview.html.

---

## Phase 1.5: Consolidation Sweep — DONE (2026-04-14)

Before drawing module boundaries, identify what IDEAS and UNFINISHED WORK from other repos belongs here. Not porting code — cataloguing what each module is responsible for, including things that currently live elsewhere.

**Repos to sweep:**
- `LK_RB_Synthesis_Project/` — choreography framing, fractal architecture, AutomatonAnalyzer (87 elaboration relationships), Lakoff/Nunez → Brandom bridge, deontic scorekeeping engine, temporal compression = sublation
- `September_UMEDCA/` — earlier Prolog, strategy automata
- `UMEDCA_2026/` — latest manuscript drafts, Prolog
- `umedcta-portfolio` (github.com/TioSavich/umedcta-portfolio) — communication-focused tools (More Machine, Hermeneutic Calculator, etc.). The communication/enactment split was a mistake; good material here that overlaps with and should inform the modules, especially `pml/` and `learner/` (ORR visualization)
- `Dialectical-Reader` (github.com/TioSavich/Dialectical-Reader) — contains a draft PML reader that will be scrapped, but serves as a "this is a bad version of what I want" reference. Do not erase; use as a negative template when building the real PML coding tool
- `Accessibility_Compliance_Project` (github.com/TioSavich/Accessibility_Compliance_Project) — contains Big Red (IU supercomputer) connection setup. Reference only — relevant when PML analysis needs to run at scale

**Not sweeping:**
- `UMEDCTA-main/` — archival, leave alone

**Output:** One document — `docs/consolidation-map.md` — listing what goes where. Each entry: source repo, file/idea, destination module in this repo, status (idea only / code to port / reference only).

**Goal:** After this phase, LK_RB_Synthesis_Project can be archived as a separate entity. All its living ideas have a home in this repo's module structure.

### NotebookLM Workflow

After each major restructuring phase, regenerate a NotebookLM notebook from the current state of the repo (using `flatten_for_notebooklm.py` at `/Users/tio/Documents/GitHub/flatten_for_notebooklm.py`). This enables part-to-whole reflection: query the whole codebase through Gemini's RAG after pruning, to see what the cleaned version actually says.

Notebook URLs are tracked in `notebooklm-registry.md` (gitignored). Never flush old codebase snapshots — mark them STALE and keep them. Old snapshots preserve reconstruction context across restructuring phases.

---

## Phase 2: Modularize — DONE (2026-04-14/15)

Five module directories, each with tests and a README, plus `paths.pl` at
the root for SWI-Prolog's `file_search_path` aliases.

`incompatibility_semantics.pl` is split into a scene-agnostic sequent engine
(`proves/1`) in `arche-trace/` and five axiom sets included at the right
priority level:

- `formalization/axioms_geometry.pl` — quadrilateral taxonomy
- `formalization/axioms_robinson.pl` — Robinson Q, arithmetic grounding
- `formalization/axioms_number_theory.pl` — Euclid's prime proof
- `pml/axioms_eml.pl` — embodied modal logic (dialectical rhythm)
- `learner/axioms_domains.pl` — domain switching, normative crisis

PML operators live canonically in `pml/pml_operators.pl`, re-exported by the
engine. The embodied prover (`proves/4`) is a separate module in
`arche-trace/embodied_prover.pl`, selectively imported to avoid the
`incoherent/1` namespace clash with the scene-agnostic engine.

All 27 strategy automata are grounded: zero `is/2` in strategy code, every
arithmetic operation routes through recollection-based counting.

**Root-level files:** `overview.html`, `CLAUDE.md`, `SCENE_LEVEL_ARCHITECTURE.md`,
and this spec.

### `pml/`
PML as a discourse analysis instrument.

**Contains:**
- The 12 operators, formally defined
- Person-position markers: S/O/N modes as pronoun shifts (first/second/third person), following Carspecken's reconstruction
- Coding manual: how a human coder applies PML to a transcript
- TalkMoves comparison: the 6 divergence points
- Decentering signature hypothesis
- PML core tests (28 passing)

**Consolidation inputs** (see `docs/consolidation-map.md`):
- MUA framework from LK_RB_Synthesis (`mua.py`) — the Brandom bridge mapping practices to vocabularies. Design pattern for Prolog: vocabulary/practice predicates, LX expansion checking, PP-sufficiency relations.
- Arithmetic-as-ObjectCollection metaphor from LK_RB_Synthesis (`core.py`) — Lakoff/Nunez grounding metaphor as pragmatic projection. Informs how PML connects to embodied arithmetic.
- 2525 formalized inferences from UMEDCA_2026 (`umedca_inferences_core.pl`) — already Prolog. Definitive material inference corpus with Godel numbers and semantic tags.
- Brandomian analysis / MUD examples from LK_RB_Synthesis — reference for coding manual.

**Does NOT contain:** strategy automata, crisis cycle, arche-trace, Robinson, Prolog learner model.

**README says:** "PML is a 12-operator coding framework for classroom discourse. It tracks epistemic stance — not what teachers do, but the consciousness from which they do it."

### `arche-trace/`
Where formal proof goes hollow.

**Contains:**
- Sequent calculus (extracted from `incompatibility_semantics.pl`)
- Arche-trace mechanism (attributed variables resisting unification)
- Erasure boundary documentation
- Stance-Dependent Frames (compression/expansion modulating the incompatibility frame)
- Relevant tests

**Consolidation inputs:**
- Incompatibility engine from LK_RB_Synthesis (`incompatibility_engine.py`) — Python sequent-style prover. Design reference for how deontic scorekeeping maps to sequent rules; actual Prolog implementation already exists here.
- Sublation pattern recognizer from September_UMEDCA (`critique.pl`) — already Prolog. Internalized Being/Nothing/Becoming mediation.

**Does NOT contain:** ORR cycle, strategy automata, HTTP server.

**README says:** "This module marks the precise boundary where formalization goes hollow. Derivations succeed structurally but proof objects contain no content."

### `strategies/`
Children's arithmetic as finite state machines.

**Contains:**
- The 27 strategy automata
- CGI attribution (Carpenter/Fennema via Hackenberg)
- K-3 standards modules (20 modules, 153 tests)
- Choreography framing and fractal architecture (from LK_RB_Synthesis ideas)

**Consolidation inputs:**
- RegisterMachine base class from LK_RB_Synthesis (`strategies.py`) — Minsky machine model with INC/DEC instruction set. The choreography framing in code. Design pattern for formalizing the 20 lazy automata that still use `is/2`.
- AutomatonAnalyzer from LK_RB_Synthesis (`elaboration_analysis.py`) — discovered the 87 algorithmic elaboration relationships via AST pattern detection. Critical for strategy-to-strategy mapping.
- Strategy metadata JSON from LK_RB_Synthesis — structured data for all 27+ strategies with pp_necessities, lx_relations, metaphors. Data, not language-bound.
- Hermeneutic Calculator from portfolio (`SAR_*.html`) — SVG step-through visualizations of children's strategies. Reference for what the automata look like enacted.
- Fractal architecture (Iterative Core + Strategic Shell) — documented in LK_RB_Synthesis but not implemented. Informs module README.

**Does NOT contain:** PML integration, crisis cycle, incompatibility semantics.

**README says:** "These model children's invented arithmetic strategies as executable choreography for embodied cognition. Each strategy is a script for the temporal unfolding of thought." Uses the fractal architecture framing (Iterative Core + Strategic Shell). No documentation about what's broken — just what the strategies are.

### `learner/`
The crisis cycle — a constructivist module.

**Contains:**
- Meta-interpreter (`solve/6`)
- Execution handler
- Oracle/teacher server (teacher creates conditions, not delivers answers)
- ORR visualization / HTTP server
- Config, tension dynamics
- Tests

**Consolidation inputs:**
- Deontic scorekeeper from LK_RB_Synthesis (`deontic_scorekeeper.py`) — manages practices, calculates effective rules, tracks Commitments/Entitlements. Design pattern for normative inference in the crisis cycle.
- LLM choreography from LK_RB_Synthesis (`llm_choreography.py`) — prompt generation for crisis analysis, formatting incompatibilities for LLM context. Informs oracle/teacher redesign.
- Axiom lifecycle from September_UMEDCA (`axiom_lifecycle.pl`) — already Prolog. Material/Formal/Stale/Re-Materialized transitions. Directly relevant to how the learner's knowledge degrades and refreshes.
- More Machine MatrixAutomaton from portfolio (`more-machine.js`) — phase system, bifurcation visualization. Reference for ORR visualization design.
- Embodied practices from LK_RB_Synthesis (`schemas.py`, `object_manipulation.py`) — Image Schemas (Container, Motion). Source domain for metaphorical elaboration. Design reference.

**Does NOT contain:** sequent calculus, PML formal definitions, Robinson.

**README says:** "This models a monological learner who acquires arithmetic strategies through crisis. It is a constructivist module — one possible frame for studying learning. The project's core commitments are intersubjectivity-first; this module exists to be tested and extended by researchers who find the crisis model useful."

### `formalization/`
Robinson Q and the meta-mathematical signal.

**Contains:**
- `robinson_q.pl` (self-contained, 20 tests)
- Grounded arithmetic
- Number theory (Euclid's prime argument)

**Consolidation inputs:**
- Logic terms from LK_RB_Synthesis (`logic_terms.py`) — Python mirror of Prolog term structures. Reference only; the Prolog implementation here is canonical.

**Does NOT contain:** PML, crisis cycle, strategy automata.

**README says:** "This establishes where the project sits relative to Gödel's incompleteness theorems. The claim is not that the system proves incompleteness — it is that children's arithmetic strategies, taken seriously, generate machinery rich enough to raise the question."

---

## Phase 3: Interface Documents — DONE (2026-04-15)

Five documents in `interfaces/` make the cross-module philosophical claims explicit. Each states the claim, grounds it in predicate signatures and file references, marks where the claim exceeds what's formalized, and lists what would strengthen the interface.

- **`pml-to-discourse.md`** — PML operators to observable discourse markers. Person-position → S/O/N; categorical/hedging → ◇/□; felt tempo → ↑/↓. No coding manual, no empirical validation yet.
- **`compression-across-layers.md`** — PML's compression/expansion and strategies' temporal compression/decompression as the same structure at different levels. Wire: the modal cost multiplier. Formal isomorphism: open.
- **`strategies-to-formalization.md`** — Strategy automata to Robinson Q. `is_recollection/2` calls the hermeneutic calculator, so Robinson proofs literally invoke the strategy automata. Explicit trace-to-derivation map: open. Gödel numbering: not wired.
- **`arche-trace-to-learner.md`** — Three augmentation points where the learner model's formalization goes hollow: erasure, resource exhaustion, pathology detection. The learner module does not call `proves/4` or the dialectical engine; the parallel is intentional per `SYSTEM_ASSESSMENT.md` (computing vs. justifying).
- **`pml-to-learner.md`** — PML modulates inference cost through the compressive/expansive multiplier. Mode (S/O/N) does not modulate cost. Meaning fields, decentering, and provenance tracking are hypothetical.

---

## Phase 4: Test Independence — DONE (2026-04-16)

Each module's tests pass in isolation within the current architecture.
Full detail: `docs/PHASE_4_TEST_REPORT.md`.

- `formalization/`: 20/20 Robinson Q
- `strategies/`: 16/16 test files (153+ subtests)
- `learner/`: 10/10 test files
- `arche-trace/`: 7/7 critique + 15/15 dialectical engine
- `pml/`: 28/28 core + 10/10 simple

Firebreaks hold at the module level. Each module's non-test code loads
without pulling in another module's prover machinery, beyond the
documented imports (learner borrows modal operators, `check_norms/1`, and
`proves/1` for successor grounding from arche-trace).

True isolation — tests runnable without `paths.pl` — would require
`multifile`-based axiom distribution instead of the current `include`
approach. Future work, not blocking.

---

## Phase 5: Academic Products

Each module maps to a specific product:

| Module | Product | Venue | Needs |
|--------|---------|-------|-------|
| `pml/` | Feasibility paper | MERJ, ESM, or JMTE | Coding manual, inter-rater pilot, positioning against Hennessy 2020 |
| `arche-trace/` | Philosophical essay | EPT or Studies in Phil & Ed | Yogic logic, Stance-Dependent Frames, performative contradiction |
| `strategies/` + `formalization/` | Dissertation chapter / GitHub appendix | Manuscript companion | Honest status section |
| `learner/` | Interactive demo | Portfolio, conferences | ORR visualization working |
| Best Day (draws on `pml/`) | Grant proposal | NSF DRK-12 or CAREER | PML as measurement, catastrophe theory, intervention design |

---

## Outstanding Tasks (bounded, not preconditions)

- **Port `axiom_lifecycle.pl`** from September_UMEDCA to `learner/`. Material/Formal/Stale/Re-Materialized transitions; stale status and degradation thresholds are directly relevant to the crisis cycle.
- **Design Prolog equivalents for Python patterns** (design decisions, not mechanical translation):
  - RegisterMachine (LK_RB_Synthesis) → how do Minsky machines become Prolog modules in `strategies/`?
  - MUA framework (LK_RB_Synthesis) → vocabulary/practice predicates for `pml/`
  - Deontic scorekeeper (LK_RB_Synthesis) → normative inference in `learner/` — `assertz/retract` or attributed variables?
  - AutomatonAnalyzer elaboration relationships → strategy-to-strategy mapping for `strategies/`
- **Port strategy metadata:** `data/strategy_metadata.json` from LK_RB_Synthesis is structured data (not language-bound). Can move directly into `strategies/`.
- **Archive LK_RB_Synthesis as separate entity:** All living ideas have designated homes in this repo's module structure (see `docs/consolidation-map.md`). Actual archiving waits until the items above are ported and Python design patterns are documented in module READMEs.

The 2525-inference corpus (`umedca_inferences_core.pl`) is not imported. It was generated by an earlier PML prototype reading the manuscript via Gemini API calls; quality is questionable; if ported naively, PML coding collapses into lookup. The corpus stays in `UMEDCA_2026` as an artifact.

---

## What This Design Does NOT Do

- Rebuild the synthesis engine. The archived engine wrapped oracle calls rather than synthesizing from primitives; genuine synthesis is open.
- Resolve the monological/intersubjective tension. The learner is a single-agent model; intersubjectivity-first Critical Math commitments are documented in `learner/README.md` as a research question.
- Commit "The Negative" / yogic logic to any specific form. That material lives in the arche-trace essay when it's written.
- Create new documentation about how broken things are. Fix things; don't narrate the fixes.
