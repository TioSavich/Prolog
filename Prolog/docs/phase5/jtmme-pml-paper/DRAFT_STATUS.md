# Draft status — 2026-04-16

Seven section drafts now live in this folder. They have not been assembled into a single paper.md yet; the paper-skeleton's "Next steps" item 3 specified per-section files first, then assembly. Assembly is Tio's call.

## Files

| Section | File | Words | Target | State |
|---|---|---:|---|---|
| §1 Intro | `section-1-intro.md` | 618 | 500–700 | drafted |
| §2 Best Day | `section-2-best-day.md` | 1,053 | 900–1,100 | drafted from `sources/canonical-intervention-description.md` |
| §3 Feeling Body | `section-3-feeling-body.md` | 1,259 | 1,000–1,300 | **scaffolding** — Carspecken notebook trawl not run; `[TRAWL]` placeholders inline |
| §4 S-mode Axioms | `section-4-s-mode-axioms.md` | 1,236 | 1,200–1,500 | drafted; cites `docs/phase5/fact-sheet-pml.md` as receipt |
| §5 Neurosymbolic Sketch | `section-5-neurosymbolic-sketch.md` | 1,042 | 800–1,000 | drafted; 42 words over, trimmable |
| §6 What I Don't Know | `section-6-what-i-dont-know.md` | 938 | 700–900 | drafted; sourced against Phase 5 fact-sheets |
| §7 Closing | `section-7-closing.md` | 476 | 400–500 | drafted |
| **Total** | | **6,622** | | within JTM-ME ≤12,000 for theoretical/philosophical |

## What the drafts do and do not do

**Drafted honestly about the code.** §4 presents the S-mode axioms as dialectical movements in plain English; the Prolog is kept behind the page. §6 reports specific limits sourced against the fact-sheets in `docs/phase5/` (imbalance across S/O/N modes, scene-dependent inter-modal transitions, unclosed learning loop, approximate cost accounting). Two recent fixes — `incur_cost/1` accumulation and the `[o(P)] → n(P)` hylomorphic shift — are reflected where relevant.

**Draft state honors the "blocked on trawl" constraint for §3.** The paper-skeleton names the NotebookLM Carspecken trawl as a prerequisite for §3. The trawl has not been run. The §3 draft carries the argument structure (three validity registers, why subjective validity cannot ground in introspection-as-observation, why the attributed variable refuses unification) with inline `[TRAWL]` markers showing where Carspecken citations need sourcing. Tio treating §3 as scaffolding, not finished prose, is the right posture.

**Reference state for the code.** The paper's claims about the code are anchored to the tag `paper-reference-2026-04-16` (git tag on `main`). The Phase 5 fact-sheets at `docs/phase5/fact-sheet-<module>.md` carry SHA-pinned per-file references; a reviewer can verify any code claim against a specific commit state.

## Open gaps before submission

1. **§3 Carspecken trawl.** `notebooklm-trawl-task.md` in this folder is the ready-to-run spec. It targets the notebook at `a10046c9-73dc-474b-80cd-2f0659b513b9`. Until the trawl runs, §3's Carspecken passages are scaffolding rather than sourced.
2. **Voice pass.** Tio's voice commitments (anti-ocular, anti-schlock, anti-AI-tell) have been observed while drafting, but a sweep through `deschlocker` / `carspecken-voice-editor` / `philosophical-dictionary-checker` before submission is still called for by the paper-skeleton.
3. **Section order and transitions.** Each section ends with a transition paragraph to the next. If the assembled paper shifts section order or adjusts the intro, those transitions may need minor rewrites.
4. **Appendix.** The paper-skeleton permits a short appendix with a glossary of S-mode shorthand and the Prolog operator tables. Not drafted; probably ≤500 words when written.
5. **Trusted-reader pass.** The paper-skeleton lists Erik Jacobson (measurement / TalkMoves angle), David Bowers, Alex Moore as candidates. Pre-submission review would catch things the author and Claude cannot catch from inside the drafting.

## What the drafts do *not* attempt

- Do not reproduce the aspirational claims from `pml/Modal_Logic/` that the Phase 5 audit relocated to `archive/pml-modal-logic/`. The paper draws only on what the fact-sheets support.
- Do not overclaim the neurosymbolic test. §5 is explicit that the test has not been run.
- Do not treat Best Day as a study. §2 names it as praxis, not evidence.
- Do not use N-mode or O-mode axioms as if on a par with S-mode. §4 presents only S-mode; §6 names the imbalance directly.

## Assembly (when Tio is ready)

Concatenating in order with a single `# Title` at the top and removing the per-section `*Target:*` lines produces the paper. The README and `paper-skeleton.md` remain in the folder as provenance for how the draft was scoped.
