# Audit log: learner

Written after [docs/phase5/fact-sheet-learner.md](fact-sheet-learner.md). Research goal from the fact-sheet is the cut criterion.

## Cut criterion

> The ORR cycle (Observe, React, Reorganize) is implemented as `solve/6` + `execution_handler:run_computation/2` + `oracle_server:query_oracle/4` + `reorganization_engine`. Crisis classification is mechanical (resource exhaustion, tension instability, unknown operation, normative crisis, incoherence); the learning trajectory is not empirically validated against student behavior. Genuine FSM synthesis is archived; the "synthesize from oracle" path reaches a shim that fails.

## Decisions

| File | Section / line range | Claim | Verdict | Rationale |
|---|---|---|---|---|
| `SCENE_LEVEL_ARCHITECTURE.md` | whole file (L1–249) | Carspecken's Four Scenes may correspond to codebase features; frames correspondences as hypotheses, not identities; explicit "productive failure is the point of contact" stance | **keep** | Already in SCENE voice. Serves as voice template for the phase 5 rewrite. Makes no load-bearing empirical claim about the learner. |
| `learner/README.md` | whole file (L1–55) | Describes the crisis cycle, the five core predicates, and how to run the server | **keep** | Mechanical; matches the stripped code. Characterizes oracle as "philosophically hollow" and `fsm_synthesis_engine.pl` as "utility shim." Honest. |
| `docs/SYSTEM_ASSESSMENT.md` | whole file (L1–403) | "System A / System B" assessment; claims current integration + proposes five augmentation recommendations (P2-2 through P3+); table of difficulty/impact at L318–325 | **cut** | Mixed doc. Honest sections ("What the skeleton cannot do" L265–285, "Essential limitations" L364–393) coexist with specific implementation claims (P2-2 post-synthesis validation, P2-3 finer crisis classification via IS, P2-4 modal cost unification) that are proposed, not coded. Path label "root `prolog/`" at L22 is stale — the learner lives in `learner/`. Cut-policy prefers whole-file relocation; the fact-sheet's §NOT-does section already captures the honest content. |
| `docs/SYSTEM_ASSESSMENT.md` | L22 | Describes learner as "System B (Crisis Learning, root `prolog/`)" | (cut vehicle) | Path is wrong. `learner/` is the current home. |
| `docs/SYSTEM_ASSESSMENT.md` | L316–325 | Recommendation table with difficulty/impact ratings for five unshipped recommendations | (cut vehicle) | Design intent, not code-backed. Characteristic of a mixed-claim doc. |
| `docs/NEUROSYMBOLIC_FRAMING.md` | whole file (L1–141) | "The umedcta-formalization codebase is neurosymbolic by architecture" (L32); the oracle "marks exactly where the neural component enters" (L39–41) | **cut** | The learner code has no neural component. No LLM call, no embedding, no gradient — the oracle is a lookup table with `format/3` glosses (confirmed in fact-sheet §oracle_server). The doc itself admits this at L46–48 ("In the current implementation, that judgment comes from the oracle's pre-computed strategy table. In future work, it comes from an LLM"). Calling an architecture "neurosymbolic by architecture" when the neural half is explicitly future work is overclaiming. |
| `docs/NEUROSYMBOLIC_FRAMING.md` | L32 | "neurosymbolic by architecture" | (cut vehicle) | Specific overclaim. Code has no neural component. |
| `docs/NEUROSYMBOLIC_FRAMING.md` | L39–41 | Oracle "marks exactly where the neural component enters" | (cut vehicle) | The oracle enters a Prolog clause dispatch, not a neural component. The "entry" is architectural intent, not code. |
| `design/03_ORACLE_REDESIGN.md` | whole file (L1–221) | "Replace the oracle with a teacher — an agent that monitors the student's activity for recognizable patterns" (L1–10); migration path from current oracle (L178–187) | **cut** | Aspirational redesign. The code still has `oracle_server.pl` with `query_oracle/4` as a dispatcher, not a teacher_recognize/teacher_prompt/teacher_validate interface. The doc is transparent about this ("Migration path from current oracle" at L178). Not code-backed. Research goal direction. |
| `design/06_REFLECTION.md` | whole file (L1–221) | Four approaches to reflection, with recommendation (Approach 4, recognition-triggered) (L144–163) | **cut** | Aspirational. The code has `reflective_monitor.pl` which counts failure signatures (per fact-sheet §reflective_monitor), not the four approaches described here. Doc is transparent: "We do not yet have a satisfactory answer" (L13). Research goal direction. |

## Relocations

Files moved to `archive/learner-framing/`:

- `docs/SYSTEM_ASSESSMENT.md` — Mixed doc with stale path references and unshipped recommendations; honest content already folded into fact-sheet §NOT-does.
- `docs/NEUROSYMBOLIC_FRAMING.md` — Claims "neurosymbolic by architecture" for code that has no neural component.
- `design/03_ORACLE_REDESIGN.md` — Proposes teacher_recognize/teacher_prompt/teacher_validate interface; the code still has `oracle_server.pl` as dispatcher.
- `design/06_REFLECTION.md` — Surveys four approaches to reflection; the code has a failure-counting `reflective_monitor.pl`, not any of the four.

## Deletions

None. All cuts are relocations to `archive/learner-framing/` — recoverable from git history and the archive tree.

## Notes

- `SCENE_LEVEL_ARCHITECTURE.md` stays in its current location because it is the voice template for the phase-5 rewrite.
- `learner/README.md` stays because it is mechanical and honest.
- The four cut docs each contain material that, re-written, could serve as research-question documents. That rewrite is outside the scope of this audit. Relocation preserves the material for future use without letting it sit at the top of `docs/` or `design/` as if it described what the code does.
- No kept doc requires a commentary banner: the kept docs (SCENE, README) are already frank about what they claim and don't claim.
