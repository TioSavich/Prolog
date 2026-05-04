# Hermes Geometry Knowledge Base — Overnight Push Design

*2026-05-03. Spec for a hybrid-autonomous overnight ingestion that produces a tiered Prolog knowledge base for K–8 geometry plus the preservice-teacher knowledge layer. Companion to `Hermes_Plan.md`. Drafted before launch; tomorrow morning's wake-up report (`Prolog/Hermes_Geometry_Wakeup.md`) will summarize what landed.*

---

## TL;DR

Build a richer-schema geometry knowledge base in `umedcta-formalization/geometry/`, with a thin bridge file in `Prolog/geometry_bridge.pl` for Hermes recognition-layer consumption. Author tonight in three waves: (1) schema, LK_RB audit, standards mapping; (2) parallel diggers against NotebookLM for Van de Walle, Van Hiele, L&N, N103, plus a misconception harvester reading the existing batch files and `research_corpus`; (3) synthesizer that reconciles concept IDs and writes the morning report. Every record carries a confidence tier (1=anchor, 2=triangulated, 3=single-source-flagged, 4=open-question-no-record). Tomorrow morning is interactive adjudication of Tier 3 + 4.

## Decisions made during brainstorming (2026-05-03)

1. **First consumer (d):** all three — Hermes recognition layer, preservice-teacher methods tool, hand-extending authoring substrate — with the authoring substrate as the immediate target. Schema designed once for all three.
2. **Grade band (c+d):** K–8 deep on content, interleaved with the preservice-teacher *teacher-knowledge* layer (van Hiele level + L&N metaphor + pedagogical bootstrap + misconception cluster) on each topic.
3. **Source choreography (d):** NotebookLM-first via `notebooklm-mcp`, with a Wave-1 audit of `LK_RB_Synthesis_Project/` so we don't re-extract L&N material that already exists in Python. Local PDFs as fallback. Standards via Learning Commons MCP.
4. **Execution model (c):** hybrid — autonomous extraction tonight, interactive Tier 3+4 adjudication tomorrow.
5. **Topology (d):** new `umedcta-formalization/geometry/` module with richer schema; thin `Prolog/geometry_bridge.pl` for Hermes consumption. Existing `misconceptions_geometric_batch_1/2.pl` stays put as seed.
6. **Approach (C):** tiered confidence per record — Tier 1 anchor, Tier 2 triangulated, Tier 3 single-source-flagged, Tier 4 open-question (lives in `OPEN_QUESTIONS.md`, not in `.pl` files).

## Section 1 — Record schema

Geometry's natural axes are *concept × van Hiele level × metaphor source × standard*, plus the same misconception/bootstrap/material-inference layer arithmetic already has. The schema separates them so any axis is queryable.

```prolog
% identity
geom_concept(Id, Name, Topic, GradeBand).

% van Hiele level marker — ships with phrases (the listening-grammar bridge)
van_hiele_marker(ConceptId, Level, MarkerPhrases, Citation).
% Level ∈ 0..4 (visual, analytic, abstract, deductive, rigor)

% Lakoff & Núñez grounding metaphor
metaphor_source(ConceptId, MetaphorName, Mapping, Citation).
% MetaphorName ∈ {container, path, object, motion, measuring_stick,
%                 fictive_motion, blend, ...}

% misconception (BENNY-compatible shape)
geom_misconception(Id, ConceptId, Name, Triggers, Repair, Citation).

% material inference (Brandomian commitment-tracking)
material_inference(ConceptId, Premise, Conclusion, Polarity).
% Polarity ∈ {entitled, incompatible}

% pedagogical bootstrap
bootstrap(Id, ConceptId, Kind, Prompt, Tools, TargetTransition).
% Kind ∈ {question, activity, construction}
% TargetTransition = vH(From,To) or consolidate(L)

% construction primitive (Euclid; stubbed)
construction(Id, Name, Tools, Steps, RelatedPropositions).

% standards anchor
standard_anchor(ConceptId, Framework, Code, Statement).
% Framework ∈ {ccss, in_indiana, im_lesson}

% confidence tier (every record gets one or it doesn't land in a .pl)
tier(RecordRef, Level, SourceList, Notes).
% Level ∈ {1, 2, 3}; Tier 4 → OPEN_QUESTIONS.md

% triangulation log (Tier 2 only)
triangulation(RecordRef, AgreementList).
```

`van_hiele_marker/4` is the listening-grammar bridge — `MarkerPhrases` is what a student at this level actually says, so Hermes can later turn kid-talk about a square into a level-tagged event. `material_inference/4` makes Brandomian commitment-tracking explicit, so Hermes can detect "student treats square as not-a-rectangle" without authoring every downstream misconception.

## Section 2 — Repository topology

```
umedcta-formalization/
├── geometry/
│   ├── README.md                      ← schema doc + how-to-extend
│   ├── schema.pl                      ← predicate declarations + validators
│   ├── concepts/
│   │   ├── shape_recognition.pl       ← K–2 polygon/circle naming
│   │   ├── attributes.pl              ← sides, vertices, angles
│   │   ├── classification.pl          ← polygon hierarchies
│   │   ├── angles.pl
│   │   ├── transformations.pl
│   │   ├── area_perimeter.pl
│   │   ├── volume_surface_area.pl
│   │   ├── similarity_congruence.pl
│   │   ├── coordinate_geometry.pl
│   │   └── pythagoras.pl
│   ├── metaphors/
│   │   ├── lakoff_nunez_inventory.pl
│   │   └── measuring_stick.pl
│   ├── van_hiele/
│   │   ├── levels.pl
│   │   └── transitions.pl
│   ├── bootstrap/
│   │   ├── van_de_walle_activities.pl
│   │   ├── n103_activities.pl
│   │   └── construction_activities.pl
│   ├── standards/
│   │   ├── ccss_geometry.pl
│   │   ├── indiana_geometry.pl
│   │   └── im_lesson_anchors.pl
│   ├── corpus/                        ← raw extracts with citations
│   │   ├── van_de_walle_excerpts.md
│   │   ├── van_hiele_dissertation_excerpts.md
│   │   ├── lakoff_nunez_passages.md
│   │   ├── lakoff_nunez_existing_audit.md
│   │   ├── n103_chapter_extracts.md
│   │   └── euclid_propositions.md
│   ├── tests/
│   │   ├── schema_validation.pl
│   │   └── coverage.pl
│   └── OPEN_QUESTIONS.md              ← Tier 4
└── (existing dirs unchanged)

Prolog/
├── geometry_bridge.pl                 ← thin re-export for Hermes
└── (existing tree unchanged)
```

- **Concepts as primary axis** (not van Hiele level, not metaphor). Tagging modules reference concept IDs.
- **`corpus/`** holds raw extracted passages with citations — source-of-truth for any record that needs to be re-traced.
- **`schema.pl`** carries the validators: every record must have tier + citation + a real concept reference.
- **`geometry_bridge.pl`** is the only file in `Prolog/` — pulls geometry KB into Hermes when needed, doesn't fork authoring.

## Section 3 — Source choreography

**Wave 1 — Foundation (~30 min, mostly synchronous + 2 parallel subagents)**

| Worker | Charter | Output |
|---|---|---|
| Schema author (synchronous) | Writes `schema.pl` with predicate declarations + validators. Seeds `concepts/` files with empty stubs keyed off CCSS K–8.G concept names. Writes `geometry_bridge.pl` skeleton. | `schema.pl`, empty `concepts/*.pl`, `geometry_bridge.pl` |
| LK_RB audit digger (background) | Reads `LK_RB_Synthesis_Project/`, relevant `umedcta-formalization/` extracts, any Python output. Reports what L&N content already exists, where, what's missing. No NotebookLM. | `corpus/lakoff_nunez_existing_audit.md` |
| Standards mapper (background) | Learning Commons MCP queries for K–8.G CCSS + Indiana statements. Maps to seed concept IDs. IM lessons noted as anchors-only. | `standards/ccss_geometry.pl`, `indiana_geometry.pl`, `im_lesson_anchors.pl` |

**Wave 2 — Parallel diggers (5 subagents at once, ~2–4 hours)**

| Worker | Source | Charter |
|---|---|---|
| Van de Walle digger | NotebookLM (Van de Walle chapters) | Van Hiele levels with canonical kid-talk examples; named instructional activities; classification hierarchies. Tier 1 for direct VdW assertions. |
| Van Hiele dissertation digger | NotebookLM (Van Hiele dissertation) | Original level descriptors + transition examples. Triangulates with VdW: agree → Tier 1; disagree → Tier 2 + `triangulation/2`; VH-only → Tier 3. |
| L&N source digger | NotebookLM (L&N text) + Wave-1 audit report | Complete metaphor inventory (container, path, object, motion, measuring-stick, fictive motion, blends). Skips re-extraction where audit shows existing coverage. |
| N103 digger | NotebookLM (N103 chapter Word files) | Chapter-level concepts, textbook vocabulary, named activities. Anchors to concept IDs created by other diggers. |
| Misconception harvester | Local: `misconceptions_geometric_batch_1/2.pl` + `research_corpus/research.db` queries | Geometry/spatial-reasoning misconceptions ported as Tier 1; new corpus harvesting Tier 2/3. Generates `material_inference/4` records alongside. |

**Wave 3 — Synthesis (~20 min, single subagent)**

| Worker | Charter | Output |
|---|---|---|
| Synthesizer | Cross-references all five Wave-2 outputs. Reconciles concept IDs (diggers propose; synthesizer canonicalizes). Promotes Tier 2 records that triangulate cleanly. Writes morning report. | `geometry/README.md`, `geometry/OPEN_QUESTIONS.md`, `Prolog/Hermes_Geometry_Wakeup.md` |

**Choreography rules (the boring discipline that prevents garbage):**

- Every record carries `tier/4` or it doesn't land in a `.pl` — it goes to `OPEN_QUESTIONS.md`.
- Every Tier 1 record has at least one `citation` slot pointing into `corpus/`.
- Diggers can *propose* new concept IDs; only the synthesizer *commits* them. Conflicts surface as Tier 4.
- NotebookLM rate-limit handling: 30s backoff, retry up to 3, then write what's there and flag the gap.
- Every digger writes raw extracts to `corpus/` first, then derives Prolog records from them. Provenance is non-optional.

## Section 4 — Tonight's execution timing

| Time (approx.) | Stage | What |
|---|---|---|
| T+0 | Wave 1 starts | Schema author runs synchronously; LK_RB audit + standards mapper dispatched in background. |
| T+30m | Wave 1 complete | Concept ID seed list canonicalized. |
| T+30m | Wave 2 starts | 5 background subagents dispatched in parallel. |
| T+~3h | Wave 2 complete | Each digger has written its raw extracts to `corpus/` and derived `.pl` records into the appropriate module. |
| T+~3h | Wave 3 starts | Synthesizer reads all Wave-2 outputs, reconciles, writes morning report. |
| T+~3.5h | Done | Wakeup file at `/Users/tio/Documents/GitHub/Prolog/Hermes_Geometry_Wakeup.md`. |

Wave 2 timing is the biggest unknown — depends on NotebookLM rate limits and the size of the L&N text + Van Hiele dissertation in particular. If Wave 2 is still running at T+5h, the synthesizer runs against partial output and flags the gap explicitly in the wake-up file rather than waiting indefinitely.

## Section 5 — Tomorrow's adjudication shape

When you wake up, `Hermes_Geometry_Wakeup.md` will contain, in order:

1. **One-paragraph summary** — what landed, what didn't, the headline numbers (records by tier, concepts covered, standards anchored).
2. **Tier 3 review queue** — every single-source claim, grouped by topic, with the source quote and the proposed fact. Read-and-mark workflow: keep / promote-to-T1 / demote-to-T4 / drop.
3. **Tier 4 open questions** — taste calls the system flagged: concept-ID conflicts, terminology disagreements between Van de Walle and Van Hiele, L&N metaphors that don't cleanly attach to one concept, etc.
4. **Schema or scope holes** — slots the diggers wanted but didn't have. If most diggers asked for the same missing slot, it's likely real.
5. **Coverage gaps** — concepts in CCSS K–8.G that no digger anchored to.
6. **Recommended next session** — what to tackle first based on (3)–(5).

The interactive session reads the wakeup file, walks (2)–(4), then runs the schema-validation tests to confirm the KB is self-consistent before declaring tonight's pass closed.

## Known unknowns / risks

- **NotebookLM rate limits** could throttle Wave 2 enough that some diggers return partial. Mitigation: each digger writes incrementally to `corpus/` so partial work survives.
- **Concept-ID conflicts at Wave 3** are the most likely "morning surprise" — five diggers proposing the same concept under different names. The synthesizer canonicalizes deterministically (alphabetically-first proposed name wins, others become aliases) and surfaces conflicts in Tier 4.
- **Schema slot drift** — if a digger insists on a slot the schema doesn't have, it writes the data to a sidecar file (`<topic>.proposed_extensions.md`) for tomorrow's review rather than silently dropping or hacking the schema.
- **L&N material in `LK_RB_Synthesis_Project/` is in Python, not Prolog** — Wave 1's audit reports what's there; Wave 2's L&N digger does the port-and-extend. Risk: the existing Python work used a different metaphor taxonomy than what we're authoring tonight. The audit will flag this; the synthesizer will reconcile.
- **Euclid is intentionally stubbed** — `bootstrap/construction_activities.pl` will have a small handful of canonical constructions (perpendicular bisector, angle bisector, equilateral triangle on a segment) but no full proof translation. Out-of-scope tonight.
- **IM lesson content not extracted** — only IDs anchored. If you want full IM content, that's a separate session against the local PDFs.

## What this is NOT doing tonight

- Extending Hermes_Plan.md with a §8 (will reference this spec from a future revision).
- Wiring the geometry KB into Hermes recognition layer (that's Phase 1 of Hermes_Plan.md, lesson-grammar compilation).
- Migrating `misconceptions_geometric_batch_1/2.pl` into the new schema (deliberate; preserves seed material as-is for Wave 2 to read).
- Building a CTC-WS context list from the geometry KB (downstream).
- Running the schema-validation tests against generated records (will run during tomorrow's adjudication, not during overnight authoring — diggers should not be blocked by validator regressions overnight).
