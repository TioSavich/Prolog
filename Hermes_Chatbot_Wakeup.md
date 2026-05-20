# Hermes Chatbot Substrate — Wake-up Report

*2026-05-04, drafted overnight while Tio slept; addendum at the top from the morning iteration loop. Companion to `Hermes_Plan.md`, `Hermes_Geometry_Wakeup.md`, and the spec at `docs/superpowers/specs/2026-05-04-hermes-chatbot-substrate-design.md`.*

---

## Iteration round 2 — live-console feedback (2026-05-04, mid-day)

After round 1, Tio tested two prompts in the live console and both produced bad output. Both fixed; iteration loop continues.

**Live test before:**

| Prompt | Mode | Old response | Diagnosis |
|---|---|---|---|
| "What does a van hiele level 1 look like?" | auto | "You might ask the student what they think it means to 'know' a shape… this term is not in our current vocabulary…" | (a) `GEOMETRY_VOCAB` gate didn't recognize "hiele/level" → no cards. (b) No `geom_concept` records existed for the levels themselves; vH levels were attached to *other* concepts via `van_hiele_marker/4`. |
| "plan a lesson on quadrilaterals" | LESSON | "You might ask: I want to make sure I'm following your thought process…" | `TEACHER_AUDIENCE_NOTE` ("frame as 'You might ask:'") was hardcoded for all teacher-audience output and overrode `MODE_FRAMING[lesson_plan]`. |

**Round 2 fixes:**

1. **New `TEACHER_LESSON_PLAN_AUDIENCE_NOTE`** in `hc_bot.py` replaces the default "You might ask:" framing when in lesson_plan mode. Tells the model to address the teacher directly with a structured planning brief.
2. **Effective-mode resolution moved earlier in `ask()`** so `MODE_FRAMING`, the audience note, and the assessing-bypass check all use the same resolved mode. Previously the bypass used `effective_mode` but the audience note used the user-passed `mode`, which left "auto" as `mode` even after resolution to `lesson_plan`.
3. **`GEOMETRY_VOCAB` extended** with pedagogical terms (`hiele`, `level`, `misconception`, `lesson`, `transition`, `inclusive`, `exclusive`, etc.) so methods-instructor questions about levels and lessons gate through the geometry layer.
4. **New `concepts/van_hiele_levels.pl`** with 5 first-class `geom_concept` records — one per van Hiele level — each with a dense description that's substantive enough to use as an answer card. Cross-linked via `material_inference` to `square_rectangle_classification` and `quadrilateral_hierarchy` so the marker examples surface alongside.
5. **Definition-style hints added to `LESSON_PLAN_HINTS`** (`what is a`, `what does a`, `what are the`, `tell me about`, `explain`, `look like`, `describe`). Methods-instructor users asking the bot to explain something get content, not a probing question reflected back.

**Live test after:**

| Prompt | Mode | New response |
|---|---|---|
| "What does a van hiele level 1 look like?" | auto → resolves to lesson_plan | Structured explanation: "At Level 1 (Visualization), students see shapes as whole, integrated objects… 'It's a square because it looks like a box'… In your classroom, you will see Level 1 thinking when…" with concrete examples and a list of behavioral markers. Cards: `van_hiele_level_2_abstract`, `supporting_example_treated_as_proof`. |
| "plan a lesson on quadrilaterals" | lesson_plan | Full lesson-planning brief with **Anchor Concept**, **Prerequisites**, **Common Misconceptions to Watch For** (inc. `quadrilateral_is_parallelogram`, `supporting_example_treated_as_proof`), and recommended N103 activities (medial quadrilateral discovery, perpendicular bisector). 6 cards used. |

**Test status after round 2:** 110 deterministic + 3 smoke = **113/113 pass**. Validator clean.

The round-2 fixes follow the same pattern as round-1: most failures are gates (GEOMETRY_VOCAB, audience-note hardcoding, mode-resolution timing) rather than substrate bugs. Each fix opens a wider input range. Recommendation: keep testing with real prompts; each new failure shape will identify another gate to relax or another concept type to add.

---

## Morning iteration addendum (2026-05-04, post-wakeup)

**All three live smoke tests now pass at `gemma4:26b` in 34.51 s wall-clock. 110 deterministic tests still pass. Validator clean.**

The iteration loop closed the linkage gaps the overnight wakeup flagged. Three discrete fixes:

1. **Neighborhood expansion in the KB query layer.** Added `concepts_in_neighborhood/3` to `query.pl` that walks `material_inference` + `developmental_marker` cross-links to find related concepts. Added `concepts/cross_links.pl` with explicit cross-links for the orientation cluster (`tilted_square_as_diamond` → `orientation_invariant_naming`), the trapezoid arc cluster (`trapezoid_classification_arc` → `exclusive_definition` / `inclusive_definition` / `parallelogram_as_trapezoid`), and the square classification arc cluster.

2. **Misconception priority sort in the Python orchestrator.** `geometry_context.py` now expands the *top* matched concept's neighborhood (not all 5), then sorts merged misconceptions by `misc_ids` position so neighborhood-of-top-concept misconceptions outrank misconceptions linked to lower-scored matches. Smoke A's `square_only_axis_aligned` now ranks first instead of fifth. Auto mode also gained an `arc(1)` card slot.

3. **Auto-mode resolution propagated to the bot's lesson-plan bypass.** Promoted `resolve_mode` to a public function in `geometry_context.py`. `hc_bot.py` calls it before the `move.assessing` pre-flight check, so when auto-mode resolves to `lesson_plan` (e.g., on inputs like "Walk me through… in a 5th grade lesson"), the bypass fires and the bot produces content rather than a probing question. Added new auto-mode lesson-plan hints (`walk me through`, `grade lesson`, `transition between`, `exclusive→inclusive`, `how to handle`, `how to teach`).

**Smoke results before → after:**

| Test | Before | After |
|---|---|---|
| Smoke A (check_answers tilted square) | ❌ fail (irrelevant misconception cards surfaced) | ✅ pass (`square_only_axis_aligned` is top card; bot mentions orientation/rotation) |
| Smoke B (lesson_plan quadrilateral classification) | ✅ pass | ✅ pass |
| Smoke C (auto trapezoid arc walk) | ❌ fail (assessing pre-flight fired despite arc cards present) | ✅ pass (auto resolves to lesson_plan; bypass fires; bot mentions exclusive + inclusive) |

**Files touched in this iteration:**

- `umedcta-formalization/geometry/query.pl` — added `concepts_in_neighborhood/3` and `neighbor_concept/2`.
- `umedcta-formalization/geometry/concepts/cross_links.pl` — new file, 8 explicit cross-links across three clusters.
- `n101_bot/src/geometry_runner.pl` — added dispatcher case for `concepts_in_neighborhood`.
- `n101_bot/bridge/prolog.py` — registered `concepts_in_neighborhood` in `GEOMETRY_PREDICATES`.
- `n101_bot/bridge/geometry_context.py` — neighborhood expansion + priority sort, expanded auto-mode hints, public `resolve_mode`, `arc(1)` added to auto plan.
- `n101_bot/bridge/hc_bot.py` — calls `resolve_mode` before assessing-bypass check.

**What this proves about the substrate:** the original wakeup file's diagnosis was correct — the substrate was structurally fine; the gaps were in cross-concept linkage (data) and auto-mode propagation (code). Both were small fixes. The `concepts_in_neighborhood` predicate generalizes — future cross-link work just needs more `material_inference` records in `cross_links.pl` (or wherever); no orchestrator changes needed.

**Performance:** no degradation. Auto-mode turns now spawn one extra swipl call (the neighborhood expansion), adding ~2 s to the pre-LLM round-trip. Acceptable.

**Console at `127.0.0.1:8765`** is the hands-on demo. Try `lesson_plan` mode with: *"I'm planning a 5th grade lesson on classifying quadrilaterals."* Try `auto` mode with: *"Walk me through how to handle exclusive→inclusive trapezoid in a 5th grade lesson."* Try `check_answers` mode with: *"A first-grader says a tilted square is a diamond."* All three should now surface relevant cards in the debug panel.

---

## Original wake-up report (overnight 2026-05-04)


---

## TL;DR

Substrate is real and working. The bot now has full visibility into the post-resolution-sweep KB through a Prolog query layer + Python card renderer, with mode-aware card selection (`auto` / `check_answers` / `ask_good_questions` / `lesson_plan`). The console exposes the mode toggle plus a cards-used debug panel so you can watch the bot think. IM curriculum extraction landed cleanly: 12 grades' scope-and-sequence + K–5 full lesson teacher guides (879 lessons, 0 failures). All 110 deterministic tests pass; one of three live smoke tests passes at `gemma4:26b`, the other two reveal a specific KB-linkage gap rather than substrate failure. The bot is genuinely structurally better than what Codex shipped — but it's a substrate that now needs a tuning pass on cross-concept linkage to fully use that structure.

---

## Final coverage and validation

```
report(1188, 96, 35, 31, 96, 1045, 5, 4, 335, 1106, 100)
       Concepts Mis Met VH  Bs Std PCK DM  T1   T2   T3
```

- **1,188 concepts** (was 230 before tonight, +958 IM lesson stubs from S4)
- **1,045 standards anchors** (was 87 — every IM K–5 lesson now has a `standard_anchor`)
- **96 misconceptions, 35 metaphors, 31 vH markers, 96 bootstraps, 5 PCK, 4 dev-markers** (unchanged from yesterday)
- **Tier 1: 335, Tier 2: 1,106, Tier 3: 100**
- **Validator: clean** — `validate_geom_kb` reports zero errors. Two cosmetic singleton-variable warnings in `lakoff_nunez_inventory.pl` predate this work.

## What landed tonight

| Wave | Subagent | Output |
|---|---|---|
| W1 (sync) | — | `umedcta-formalization/geometry/query.pl` (8 query predicates, all smoke-tested) |
| W2 S1 | Python rewrite | `geometry_query()` in `prolog.py` + rewritten `geometry_context.py` (270→732 lines) + new `geometry_context_with_cards()` sister function |
| W2 S2 | Bot wiring | `hc_bot.py` mode parameter (419→680 lines), `MODE_FRAMING` dict, lesson_plan bypass of assessing pre-flight, `TurnRecord.cards_used` |
| W2 S3 | Console | `/ask` endpoint added (Codex's earlier message claimed it existed; it didn't), mode dropdown + cards-used debug panel + per-message mode badge in both consoles |
| W2 S4 | IM extraction | `n101_bot/scripts/im_extract.py` + Tier 1 (12 grades scope-and-sequence) + Tier 2 (879 K–5 lessons full content) + 980 new `standard_anchor` records |
| W2 S5 | Tests | 6 new test files, 50 new tests, 110 total deterministic pass |
| W3 (sync) | — | sort_by_score bug fix, smoke runs at `gemma4:26b`, this report |

## Test results

| Tier | Count | Status |
|---|---|---|
| Existing 76 + new deterministic | 110 | ✅ all pass |
| New Prolog plunit (S5's `test_geometry_query.pl`) | 17 | ✅ all pass (run via `swipl ... run_tests`) |
| Smoke A — check_answers tilted square | 1 | ❌ fail at `gemma4:26b` (KB-linkage gap, see below) |
| Smoke B — lesson_plan quadrilateral | 1 | ✅ pass at `gemma4:26b` |
| Smoke C — auto mode trapezoid arc walk | 1 | ❌ fail at `gemma4:26b` (KB-linkage gap, see below) |

## The headline finding (read this first)

**The substrate works; the KB cross-linkage is incomplete.**

For smoke A — input `"A first-grader says a tilted square is a diamond"` in `check_answers` mode — the system end-to-end works:
- Tokens correctly extracted: `[first, grader, says, tilted, square, diamond]`
- Top concept correctly ranked: `tilted_square_as_diamond` (score 23, score margin of 8 above runner-up)
- Cards correctly rendered into prompt
- Bot correctly routes through the assessing pre-flight (check_answers keeps it)

But the *misconception cards surfaced are wrong for the input* — `area_measured_with_ruler`, `square_not_rhombus`, `square_not_rectangle`. Why?

```
linked_misconceptions(["tilted_square_as_diamond"], 3) → []
```

The orientation misconceptions (`square_only_axis_aligned`, `diamond_not_recognized_as_square`) are correctly authored, but they're attached to the concept `orientation_invariant_naming` — **not** to `tilted_square_as_diamond`. The two concepts aren't cross-linked in the KB. The flow falls back to misconceptions linked to lower-ranked matched concepts, which are about area / rhombus / rectangle classification — irrelevant to the orientation issue.

The bot, given irrelevant cards, correctly notices they don't apply and falls back to a generic Amy-style probe ("What do you notice about how the student is identifying the shape?"). That's the bot working as designed; the gap is upstream.

**The fix is in the data, not the code.** Three options for tomorrow:

1. **Cross-link via `material_inference`** — author `material_inference(tilted_square_as_diamond, "manifests as", "rejecting tilted forms as squares", entitled)` linking tilted_square_as_diamond to orientation_invariant_naming. Schema-clean, doesn't duplicate. Recommended.
2. **Duplicate the misconceptions** — author `geom_misconception` records for `tilted_square_as_diamond` directly. Quick but creates maintenance debt (two records say the same thing).
3. **Code-side related-concept walk** — extend the Python orchestrator (or add a Prolog query predicate) to traverse `material_inference` and `developmental_marker` relations from the top matched concept and pool misconceptions across the cluster. Most general fix; biggest refactor.

(2) is wrong. (1) and (3) are both reasonable; (1) is faster, (3) is more durable.

Smoke C is the same shape: input mentions "trapezoid" but the matching layer doesn't strongly link to `trapezoid_classification_arc`. The arc surfaces in direct lookups but doesn't get pulled by token-matching since "trapezoid" tokens go to other concepts.

Smoke B passes because lesson-plan mode goes through `standards_bundle_for/3` directly when a CCSS code is detected, bypassing the matching-then-linking flow.

## Tier 3 findings worth your attention

These are second-tier signals that don't block anything but are worth knowing:

- **swipl spawn-per-query latency.** S1's `geometry_query()` spawns swipl-per-call (~2 s). An `auto` turn ≈ 4 spawns ≈ 8 s pre-LLM; `lesson_plan` ≈ 7 spawns ≈ 14 s. With `gemma4:26b` itself slow, the round-trip is 20–60 s per turn. Functional but not snappy. The `geometry_runner.pl` dispatcher is structured so a future long-running REPL loop is a drop-in replacement when this matters.
- **Two consoles in the repo.** S3 wired the mode toggle into both `n101_bot/web/hermes_gemma_console.html` (live, server-connected) and `Critical Math (2)/redesign/hermes/Hermes Console.html` (static design mockup). Decision deferred: do you want to consolidate? The static one's avatar/lights/talk-row design is the better skin; the wired one is the actual implementation.
- **Codex's `/ask` claim was inaccurate.** The earlier conversation said `/ask` was added; the file showed only `/api/chat` etc. S3 added the real `/ask` endpoint; both endpoints are now mode-aware.
- **957 IM lesson stubs are auto-generated `geom_concept` records** with `Topic = developmental` and IDs like `im_grade3_unit7_lesson2`. They satisfy schema validation (every `standard_anchor` references a real `geom_concept`) but lack rich tagging-layer connections. They're standards-anchored and have lesson titles + learning goals in their `Statement` slot, but they're not yet bundled to vH markers, misconceptions, or bootstraps. The synthesizer can canonicalize them onto existing canonical concepts where appropriate.
- **`developmental_arc_for/2` second clause has a `!` after a disjunction.** S5 flagged for review. On inspection it's correct — the cut commits to the first disjunct that succeeds, and the third clause provides the `none` fallback. No fix needed.

## What I'd recommend doing first tomorrow

1. **Apply the cross-link fix** for smoke A (option 1 above) — author maybe 5–10 `material_inference` records cross-linking concept clusters where matching produces orphaned cards. Then re-run smoke A to confirm.
2. **Spot-check the IM extracted lessons.** Pick 3 lessons across grades from `corpus/im_teacher_guides/` and confirm the parser captured what you'd expect. The S4 spot-check showed clean parsing on K-2-1, G1-3-5, G3-7-2, G5-7-3 — but the broader 879 lesson corpus warrants a sample.
3. **Decide on console consolidation.** The static mockup is the right skin; the wired one is the right backend. Picking one (or merging) closes a real source of confusion.
4. **If you want a snappier round-trip,** convert `geometry_runner.pl` to a long-running REPL loop. ~30 minutes of work; brings auto-mode turns from ~8 s pre-LLM to <1 s pre-LLM.
5. **The "much better methods instructor" check.** Open the console at `127.0.0.1:8765`, set mode to `lesson_plan`, type *"I'm planning a 5th grade lesson on classifying quadrilaterals"*. Smoke B passed in that flow at `gemma4:26b`; this is the user-facing demo of "yes, this is much better."

## Files that landed tonight

```
umedcta-formalization/geometry/
├── query.pl                              ← Wave 1 (8 query predicates)
└── corpus/
    ├── im_scope_and_sequence/<grade>.md  ← S4 Tier 1 (12 files)
    └── im_teacher_guides/<grade>/...     ← S4 Tier 2 (879 files)

umedcta-formalization/geometry/standards/
└── im_lesson_anchors.pl                  ← S4 expanded 119 → 4012 lines

n101_bot/
├── bridge/
│   ├── geometry_context.py               ← S1 rewrite (270 → 732 lines)
│   ├── prolog.py                          ← S1 added geometry_query()
│   ├── hc_bot.py                          ← S2 mode wiring (+261 lines)
│   └── hermes_console_server.py           ← S3 /ask endpoint + mode plumbing
├── src/
│   └── geometry_runner.pl                 ← S1 JSON dispatcher (271 lines)
├── scripts/
│   └── im_extract.py                      ← S4 extraction script
├── tests/
│   ├── test_geometry_query.pl             ← S5 (17 plunit tests)
│   ├── test_geometry_context_v2.py        ← S5 (13 tests)
│   ├── test_hc_bot_modes.py               ← S5 (9 tests)
│   ├── test_im_extract.py                 ← S5 (9 tests)
│   ├── test_console_mode_endpoint.py      ← S5 (3 tests)
│   ├── test_geometry_modes_smoke.py       ← S5 (3 smoke tests)
│   └── conftest.py                         ← S5 live marker registration
└── web/
    └── hermes_gemma_console.html          ← S3 (mode dropdown + debug panel)

Critical Math (2)/redesign/hermes/
└── Hermes Console.html                    ← S3 visual parity update

Prolog/
├── docs/superpowers/specs/
│   └── 2026-05-04-hermes-chatbot-substrate-design.md   ← spec
└── Hermes_Chatbot_Wakeup.md                ← this file
```

## What this is NOT

- Not the ralph loop. Tio explicitly said "we're probably not quite ready for the ralph loop yet" — tonight set up the substrate so the loop has somewhere to iterate on.
- Not student pairing. Deferred per design.
- Not multi-student dialogue / avatar / ZCM cusp lights. That's Phase 3 of `Hermes_Plan.md`.
- Not a fully tuned chatbot. The substrate is in; tuning passes (cross-linkage fixes, prompt-engineering iterations on assessing-mode templates, IM-stub canonicalization) come next.

---

*Drafted overnight 2026-05-04 by Claude. The substrate is honest — what works works; what doesn't is documented above. The chatbot at `127.0.0.1:8765` is genuinely better than what Codex shipped: it has structural visibility into the geometry KB, mode-aware routing, real IM curriculum data, and a debug surface for watching the bot think. Smoke B passing at `gemma4:26b` is the smallest-thing-that-works proof. Smoke A and C reveal exactly where the next iteration's leverage lies.*
