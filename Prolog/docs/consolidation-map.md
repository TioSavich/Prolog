# Consolidation Map

**Date:** 2026-04-14
**Phase:** 1.5 of modularize-and-consolidate spec
**Purpose:** Catalogue what ideas and unfinished work from other repos belongs in
this repo's module structure, so each module knows what it is responsible for —
including things that currently live elsewhere.

**Status key:**
- **code to port** — working code worth moving into this repo
- **idea only** — concept documented but no working code; informs module README/design
- **reference only** — look at but don't move; negative template or infrastructure

---

## Destination: `pml/`

PML as a discourse analysis instrument. 12 operators, person-position markers,
coding manual, decentering signature.

| Source repo | File / idea | Status | Notes |
|---|---|---|---|
| LK_RB_Synthesis | `eple/core/mua.py` — MUA framework (Vocabulary, Practice, AlgorithmicElaboration, PP-Sufficiency, PragmaticProjection) | code to port | Foundational Brandom bridge: maps practices to vocabularies. LX expansion checker. Port the concepts, likely rewrite in Prolog. |
| LK_RB_Synthesis | `eple/domains/arithmetic/core.py` — Arithmetic-as-ObjectCollection metaphor | code to port | Lakoff/Nunez grounding metaphor formalized as pragmatic projection with inference mappings (Combine->Add, Remainder->Subtract). |
| LK_RB_Synthesis | `eple/domains/arithmetic/strategy_as_elaboration.py` — LX elaboration validation | code to port | Tests whether counting-on is an LX elaboration of object-collection practice. Validates the metaphor-to-strategy bridge. |
| LK_RB_Synthesis | `Metaphor_Knowledge_Base.md`, `lakoff_tiny.md`, `lakoff_medium.md` | reference only | 4 grounding metaphors (Object Collection, Object Construction, Measuring Stick, Motion Along Path). Complete Lakoff/Nunez CMT coverage through calculus. |
| LK_RB_Synthesis | `brandomian_analysis.md` — MUD examples with PP-Necessities/Sufficiencies | reference only | Worked examples of Meaning-Use Diagrams. Inform the PML coding manual. |
| UMEDCA_2026 | `umedca_inferences_core.pl` — 2525 formalized inferences | code to port | Complete extraction of material inferences from manuscript with Godel numbers, chunk IDs, semantic tags. 1.5MB. Definitive inference corpus. |
| umedcta-portfolio | `quadrilateral-sub/inferential_strength.html` — Brandom modules on substitution, inferential strength, polarity | reference only | Philosophical scaffolding for PML; strength-metric and incompatibility framework. Interactive but pedagogical, not formal. |
| umedcta-portfolio | `ai-framework/framework.html` — communicative/strategic action comparison | reference only | Philosophical grounding for PML discourse framing. No code to port. |

---

## Destination: `arche-trace/`

Where formal proof goes hollow. Sequent calculus, arche-trace mechanism, erasure
boundary, Stance-Dependent Frames.

| Source repo | File / idea | Status | Notes |
|---|---|---|---|
| LK_RB_Synthesis | `eple/core/incompatibility_engine.py` — sequent-style incompatibility checking | code to port | Python implementation of deontic scorekeeping prover. Evolved from incompatibility_semantics.py. |
| September_UMEDCA | `critique.pl` — sublation pattern recognizer | code to port | Internalized sublation mechanism (Being/Nothing -> Becoming via mediating structures). Pattern-based recognition framework. |

---

## Destination: `strategies/`

Children's arithmetic as finite state machines. 27 strategy automata, CGI
attribution (Carpenter/Fennema via Hackenberg), K-3 standards, choreography
framing, fractal architecture.

| Source repo | File / idea | Status | Notes |
|---|---|---|---|
| LK_RB_Synthesis | `eple/domains/arithmetic/strategies.py` — RegisterMachine base class | code to port | Minsky machine model for cognitive strategies. Concrete implementations: CountingOn, RMB with INC/DEC instruction set. The choreography framing in code. |
| LK_RB_Synthesis | `eple/domains/arithmetic/elaboration_analysis.py` — AutomatonAnalyzer | code to port | Automated AST-based pattern detection from automata. Discovered the 87 algorithmic elaboration relationships. TikZ MUD generation. Critical for strategy-to-strategy mapping. |
| LK_RB_Synthesis | `data/strategy_metadata.json` — 27+ strategy specifications | code to port | Structured data: strategy_id, pp_necessities, pp_sufficiencies, lx_relations, inferences, metaphors. CGI-attribution ready. |
| LK_RB_Synthesis | Project_Overview.md — fractal architecture (Iterative Core + Strategic Shell) | idea only | Two-layer EPLE: MUA Layer (form/metavocabulary) + Content/Prover Layer. Temporal compression/decompression as elaboration. Vision documented, not fully implemented. |
| LK_RB_Synthesis | Temporal compression = sublation claim | idea only | The claim that PML compression/expansion and strategies' temporal compression/decompression are the same structure. Needs formalization. Lives in interface doc `compression-across-layers.md` when built. |
| umedcta-portfolio | `calculator/SAR_*.html`, `counting.html` — Hermeneutic Calculator | code to port | Interactive SVG visualizations of children's arithmetic strategies (Adding Bases and Ones, Chunking, Decomposition). Step-through logic directly relevant to formalizing strategies as state machines. |

---

## Destination: `learner/`

The crisis cycle. Meta-interpreter (solve/6), execution handler, oracle/teacher
server, ORR visualization, config, tension dynamics.

| Source repo | File / idea | Status | Notes |
|---|---|---|---|
| LK_RB_Synthesis | `eple/core/deontic_scorekeeper.py` — deontic scorekeeper | code to port | Manages practices + MURs, calculates effective rules, handles metaphorical projections. Commits/Entitlements tracking. Core normative inference engine. |
| LK_RB_Synthesis | `eple/llm_choreography.py` — LLM integration for crisis analysis | code to port | Prompt generation for cognitive development analysis. Gathers MURs/practices, formats incompatibilities for LLM context. Informs oracle/teacher design. |
| LK_RB_Synthesis | `eple/domains/embodiment/schemas.py`, `object_manipulation.py` — embodied practices | code to port | Image Schemas (Container, Motion), P_ObjectManipulation. Source domain for all metaphorical elaboration. |
| September_UMEDCA | `axiom_lifecycle.pl` — axiom lifecycle (Material -> Formal -> Stale -> Re-Materialized) | code to port | Full lifecycle model for living thought. Includes stale status, pedagogical context forcing high costs, degradation thresholds. Directly relevant to crisis cycle. |
| umedcta-portfolio | `more-zeeman/more-machine.js` — More Machine MatrixAutomaton + phase system | code to port | Progressive-disclosure Zeeman machine. Phase system (5 phases keyed to interaction count), MatrixAutomaton class, bifurcation visualization. Maps to ORR visualization and crisis cycle dynamics. |
| umedcta-portfolio | `five-practices/five_practices.html`, `classroom_web.html` — student thinking networks | reference only | D3-based network visualization and orchestration state tracking. Suggests patterns for ORR cycle visualization but pedagogical, not formal. |

---

## Destination: `formalization/`

Robinson Q and the meta-mathematical signal. Grounded arithmetic, number theory.

| Source repo | File / idea | Status | Notes |
|---|---|---|---|
| LK_RB_Synthesis | `eple/core/logic_terms.py` — Prolog-style logic representations | code to port | Term, Var, Atom, Predicate, Incompatibility classes. Hashing and unification foundation. Python mirror of what `robinson_q.pl` does in Prolog. |
| September_UMEDCA | PHASE3_DIAGNOSIS.md — failed encoding attempts (2525 inferences) | reference only | Documents what went wrong in earlier formalization attempts. Diagnostic value only. |

---

## Reference only (no destination module)

| Source repo | File / idea | Notes |
|---|---|---|
| UMEDCA_2026 | `extraction/technical_content_inventory.md` + chapter outlines (22 .md files) | Structured inventory of all technical figures, notations, songs, transcripts, key definitions. For manuscript revision, not code. |
| UMEDCA_2026 | `notes_from_phil.md` — philosophical feedback (December 2025) | Prelude clarity, representation/unrepresentability distinctions, Derridean vs. Hegelian dialectics. Manuscript guidance. |
| September_UMEDCA | `FICHTEAN_CONTAMINATION_REPORT.md` | Proof the codebase is clean of thesis-antithesis-synthesis. Historical reference. |
| Dialectical-Reader | TypeScript/Node.js Gemini-based document reader | Negative template for PML coding tool. Shows what NOT to build. Do not erase. |
| Accessibility_Compliance_Project | Big Red 200 setup, OCR pipelines, GPU processing scripts | Infrastructure reference for when PML analysis needs HPC scale. Not feature code. |

---

## Post-sweep status

After this map, `LK_RB_Synthesis_Project` can be archived as a separate entity.
Its living ideas have homes:

- Choreography framing, RegisterMachine, elaboration analysis -> `strategies/`
- MUA framework, metaphor knowledge base -> `pml/`
- Deontic scorekeeper, LLM choreography, embodied practices -> `learner/`
- Incompatibility engine -> `arche-trace/`
- Logic terms -> `formalization/`
- Fractal architecture, temporal compression = sublation -> interface docs

`September_UMEDCA` carries one unique piece (axiom lifecycle) and one sublation
pattern recognizer. The rest duplicates what this repo already has.

`UMEDCA_2026` carries the 2525-inference corpus (definitive extraction) and
manuscript-level reference material. No other unique code.

`umedcta-portfolio` remains a separate repo (public-facing tools) but its
Hermeneutic Calculator strategies, More Machine phase system, and quadrilateral
substitution modules overlap with and should inform `strategies/`, `learner/`,
and `pml/` respectively.
