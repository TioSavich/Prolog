# Hermes Chatbot Substrate — Mode-Aware Methods Instructor

*2026-05-04. Spec for moving the Codex-integrated chatbot from "kinda working" to "much better methods instructor than Tio can manage" by giving the bot full visibility into the geometry KB through a Prolog query layer, mode-aware card rendering, an explicit UI mode toggle, and IM curriculum extraction. Companion to `Hermes_Plan.md` and the 2026-05-03 geometry overnight design. Tomorrow's wake-up file: `Prolog/Hermes_Chatbot_Wakeup.md`.*

---

## TL;DR

Replace Codex's regex-based geometry context layer with a hybrid Prolog-query-layer + Python card-rendering pipeline that gives the bot complete visibility into the post-resolution-sweep KB (concepts, misconceptions, van Hiele markers, metaphors, bootstraps, developmental arcs, PCK syntheses, standards). Add an explicit four-mode UI toggle (`auto` / `check_answers` / `ask_good_questions` / `lesson_plan`) that biases card selection and system-prompt framing. Extract IM curriculum scope-and-sequence (K–12) and full K–5 teacher guides into `corpus/im_*` markdown plus updated `im_lesson_anchors.pl`. Keep the existing assessing/advancing move grammar, with `lesson_plan` mode bypassing the "ask back rather than answer" pre-flight. Five parallel subagents tonight, ~4-hour wall clock.

## Decisions made during brainstorming (2026-05-04)

1. **UX shape:** (c) substrate + (a) UI toggle — bot is conversationally a methods instructor; UI exposes a four-mode toggle that biases routing.
2. **Capability scope tonight:** check-answers + ask-good-questions + activity-ideas. Student-pairing deferred (different problem shape, requires roster + multi-student state).
3. **Smoke-test target:** all three of (a)/(b)/(c) should land cleanly — substrate work that makes any one work makes all three work.
4. **Architecture:** Approach C — Prolog query layer (`geometry_query.pl`) + thin Python card rendering (`geometry_context.py` rewrite). Schema-aware automatically; clean injection points for future ralph-loop iterations.
5. **Auto mode kept** as a fallback while figuring out what the bot can do.
6. **`lesson_plan` mode bypasses the assessing pre-flight** — it should give answers, not frame everything as a question.
7. **Card budget ~1500 tokens** per prompt, mode determines which cards fill it.
8. **IM extraction full-content** for K–5 lessons (~10 MB markdown; future-proof against parser-missing-fields).
9. **Cards-used debug panel** worth setting up — Tio wants to see the bot think.
10. **Conversion tool:** `pdftotext -layout` default; `docling` fallback when Learning-Goals parser fails on a lesson.

## Section 1 — Prolog query layer (`geometry_query.pl`)

Seven query predicates, each producing structured terms Python can convert into cards.

```prolog
% 1. Concept lookup by tokens with grade-band filter
matching_concepts(+Tokens, +GradeBand, -Concepts).
%   returns concept(Id, Name, Topic, Score)

% 2. Misconception trigger matching against user text
applicable_misconceptions(+UserText, +ConceptIds, -Misconceptions).
%   returns misconception(Id, ConceptId, Name, MatchedTrigger, Repair, Tier)

% 3. Van Hiele markers for a concept (level filterable)
vh_markers_for(+ConceptId, +LevelOpt, -Markers).
%   LevelOpt ∈ 0..4 | any
%   returns marker(Level, Phrases, Citation, Tier)

% 4. Bootstrap activities/questions matched to concept and transition
bootstraps_for(+ConceptId, +TargetTransition, +Kind, -Bootstraps).
%   TargetTransition = vH(From,To) | consolidate(L) | any
%   Kind = question | activity | construction | any
%   returns bs(Id, Kind, Prompt, Tools, Citation, Tier)

% 5. Developmental arc lookup (concept → arc that touches it)
developmental_arc_for(+ConceptOrArcId, -Arc).
%   returns arc(ArcConceptId, FromStance, ToStance, TransitionEvidence)
%         | none

% 6. PCK synthesis for a concept
pck_synthesis_for(+ConceptId, -Synthesis).
%   returns pck(KeyKidThinking, KeyTeacherMoves, DevelopmentalArc, Citation)
%         | none

% 7. Standards-anchored bundle (the lesson-planning hub)
standards_bundle_for(+Framework, +Code, -Bundle).
%   Framework ∈ ccss | in_indiana | im_lesson | any
%   Bundle = bundle(ConceptId, Concept, Misconceptions, VhMarkers,
%                   Bootstraps, Arc, Pck) | not_found
```

**Three load-bearing decisions:**
- `matching_concepts/3` is the hub; most other predicates take its output as input.
- Tier filtering is per-call. Default: tiers 1+2; tier 3 only when explicitly requested.
- `developmental_arc_for/2` is a lookup, not a recommender — Python decides what to do with the arc.

Lives at `/Users/tio/Documents/GitHub/umedcta-formalization/geometry/query.pl`. Loaded by `geometry_bridge.pl` as part of `load_geometry_kb/0`.

## Section 2 — Python card rendering layer

Rewrite of `n101_bot/bridge/geometry_context.py` to call the new Prolog query layer and render structured results as LLM-friendly cards.

```python
def geometry_context(text: str, *, mode: str = "auto",
                     grade_band: list[int] | None = None,
                     standard_code: tuple[str, str] | None = None) -> str:
    """Build the geometry context block for an LLM prompt."""
```

**Mode-specific card selection (~1500-token budget):**

| Mode | Cards (priority order) |
|---|---|
| `check_answers` | misconceptions(3) + vh_markers(2) + dev_arc(1) + concept(1) |
| `ask_good_questions` | bootstraps(3) + vh_markers(2) + concept(1) + dev_arc(1) |
| `lesson_plan` | standards_bundle expanded (concept + misconceptions(2) + bootstraps(3) + vh_markers(2) + dev_arc(1) + pck(1)) |
| `auto` | concept(1) + misconceptions(2) + bootstraps(2) — input-shape-detection fallback |

**Card-renderer functions:** `_render_concept`, `_render_misconception`, `_render_vh_marker`, `_render_bootstrap`, `_render_dev_arc`, `_render_pck`. Each returns LLM-friendly prose with type-tag, citation, and source.

**Prolog-Python interface:** new `geometry_query()` function in `n101_bot/bridge/prolog.py` alongside the existing `detect()` / `entitlement()` / `move_for()`. One round-trip per query; results returned as Python dicts.

## Section 3 — Mode toggle + routing through the bot

Three changes to `n101_bot/bridge/hc_bot.py`:

1. `ask()` takes a `mode` parameter (default `"auto"`).
2. Mode is threaded into `geometry_context()`.
3. Mode-specific framing fragment appended to system prompt:

```python
MODE_FRAMING = {
    "check_answers": "...identify concept, vH level, misconception, repair...",
    "ask_good_questions": "...probing questions, tied to vH-level transition...",
    "lesson_plan": "...standards bundle: concept, prerequisites, misconceptions, activities...",
    "auto": "",  # falls back to existing TEACHER_AUDIENCE_NOTE
}
```

**Assessing pre-flight bypass:**

```python
if move.assessing and mode != "lesson_plan":
    # render Amy-style probing question (existing path)
else:
    # produce direct answer (existing advancing path)
```

`TurnRecord` extended with `mode` and `cards_used` fields for UI display + debugging.

## Section 4 — IM curriculum extraction

`n101_bot/scripts/im_extract.py` with two entry points:

```python
def extract_scope_sequences(grades=ALL):
    """Tier 1: all 12 grades' Course-scope-and-sequence-.pdf →
       corpus/im_scope_and_sequence/<grade>.md + im_lesson_anchors.pl"""

def extract_teacher_guides(grades=[K,1,2,3,4,5]):
    """Tier 2: full per-lesson teacher guide content →
       corpus/im_teacher_guides/<grade>/<unit>/<lesson>.md +
       im_lesson_anchors.pl"""
```

**Conversion strategy:**
- Default: `pdftotext -layout` (~1 s per lesson PDF).
- Fallback: `docling` for any lesson where the Learning-Goals parser fails.
- Spot-check 3–5 lessons across grades early to validate the parser.

**Output paths:**
```
umedcta-formalization/geometry/
├── corpus/
│   ├── im_scope_and_sequence/<grade>.md         ← Tier 1, 12 files
│   └── im_teacher_guides/<grade>/<unit>/<lesson>.md  ← Tier 2, ~960 files K–5
└── standards/
    └── im_lesson_anchors.pl   ← Statement slot upgraded with title+goal
```

**Volumes:**
- Tier 1: 12 PDFs × ~10 KB markdown ≈ 120 KB.
- Tier 2: ~960 PDFs × ~10 KB markdown ≈ 10 MB.
- Wall-clock: ~60–90 minutes total.

## Section 5 — Console UI changes

**Server side** (`hermes_console_server.py`):

```python
@app.post("/ask")
async def ask(request: AskRequest):
    mode = request.mode or "auto"
    record = bot.ask(request.question, mode=mode)
    return {
        "answer": record.final_answer,
        "thinking": record.final_thinking,
        "commitments": [c.as_dict() for c in record.final_commitments],
        "mode": mode,                   # NEW
        "cards_used": record.cards_used, # NEW (debug panel)
        "matched_concepts": record.detected_terms,
    }
```

**UI side** (Hermes Console HTML/JS):
- Mode dropdown in chat header (auto / check answers / ask q's / lesson plan).
- `localStorage` persistence across reloads.
- Per-message mode-tag badge.
- Cards-used debug panel (collapsible, off by default but easily toggled).

**Out of scope tonight:** avatar lights, ZCM cusp display, multi-student talk-rows. Those are Phase 3 of `Hermes_Plan.md`.

## Section 6 — Tests

**Three smoke tests** (`tests/test_geometry_modes_smoke.py`) — end-to-end through `gemma4:26b`:

1. `test_smoke_a_check_answers_tilted_square` — "A first-grader says a tilted square is a diamond" in `check_answers` mode → bot identifies orientation issue + level-0/1 marker + suggests teacher move.
2. `test_smoke_b_lesson_plan_quadrilateral_classification` — "I'm planning a 4th-grade lesson…" in `lesson_plan` mode → standards_bundle fires + ≥3 bootstraps surface + vH-level-2 inclusion-relation mentioned.
3. `test_smoke_c_walk_trapezoid_arc` — "Walk me through exclusive→inclusive trapezoid" in `auto` mode → trapezoid_classification_arc surfaces + N103 Activity 2.13 referenced.

**Per-layer unit tests** (deterministic, no LLM):
- `test_geometry_query.pl` — all 7 predicates against the live KB.
- `test_geometry_context_v2.py` — card rendering + mode selection + budget.
- `test_hc_bot_modes.py` — mode-parameter wiring, `lesson_plan` bypass.
- `test_im_extract.py` — parser correctness on a fixture PDF.
- `test_console_mode_endpoint.py` — POST /ask with mode → mode echoed.

Existing 76 tests must still pass; verified at end of each wave.

## Section 7 — Tonight's execution plan

```
T+0       Wave 1 (sync, ~30 min):
            geometry_query.pl authored + smoke-tested via swipl.

T+30m     Wave 2 (5 parallel subagents, ~3 hours):
            S1  geometry_context.py rewrite + card renderers.
            S2  hc_bot.py mode wiring + framing + lesson_plan bypass.
            S3  Console server /ask param + UI mode dropdown + cards-used.
            S4  IM extraction Tier 1 + Tier 2 K–5.
            S5  Test authoring (3 smoke + 5 unit suites).

T+~3.5h   Wave 3 (sync, ~30 min):
            All tests run. Smoke prompts through live gemma4:26b.
            Hermes_Chatbot_Wakeup.md written.

T+~4h     Done.
```

## Known unknowns / risks

- **gemma4:26b call latency** during smoke tests can stretch the synthesis wave. Mitigation: synthesis writer publishes whatever has run + flags gaps.
- **IM PDF parser brittleness** across grade-level header variations. Mitigation: spot-check + docling fallback.
- **Existing 76 tests passing through the rewrite** — risk of regressions in geometry_context.py. Mitigation: subagent S5 runs the full existing suite at end of work.
- **Concept-ID drift between query layer and existing data** — the post-resolution-sweep KB has 230 concepts; query predicates need to handle the full set including `_arc` developmental concepts.

## What this is NOT doing tonight

- Student pairing (deferred — different problem shape).
- Avatar lights, ZCM cusp display, multi-student talk rows (Hermes_Plan Phase 3).
- Full IM lesson content beyond K–5 (Geometry/Algebra1/Algebra2 high school deferred to Learning Commons MCP eventually).
- Ralph loop setup (Tio explicitly: "we're probably not quite ready for the ralph loop yet").
- Touching the move_grammar's assessing/advancing logic beyond the lesson_plan bypass.
