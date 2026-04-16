# PML Theoretical Report — Phase 5 Assessment (Revised 2026-04-16b)

**Module:** `pml/`
**Genre:** theoretical report, not empirical study
**Length target:** 5,000–7,000 words (JTM-ME allows up to 12,000 for Theoretical/Philosophical)
**Working folder:** [`jtmme-pml-paper/`](./jtmme-pml-paper/) — drafting work lives there; this file is the assessment.

**Change from 2026-04-16 draft:**
- Target venue confirmed as JTM-ME (Tio sits on the editorial board; see `.auto-memory/jtmme_journal.md`). Candidate-venue hedging removed.
- Correction: "feeling body" is Carspecken's theoretical concept for the pre-reflective embodied ground of subjective validity claims. It is *not* a single narrated scene. Prior draft treated it as a scene; that was my error and the paper plan has been rebuilt around it.
- Section ordering confirmed: Best Day precedes the axioms. Best Day is praxis, not a research project, and the paper treats it that way.

---

## Target venue

**Journal for Theoretical & Marginal Mathematics Education (JTM-ME)**, Theoretical / Philosophical category. Tio sits on the editorial board; recuse from handling the submission, but submit through the standard process with masked review to keep the editorial relationship clean. ISSN 2996-1645, open access, no fees, APA 7th. Article cap is 12,000 words; the planned paper budgets well under that.

Editorial context that matters for framing: David M. Bowers (EIC, UTK), Christopher H. Dubbs (associate, East Stroudsburg), Alexander S. Moore (founding, Westfield State). Scope explicitly welcomes critical, philosophical, psychoanalytic, experimental, theoretical, post-qualitative, and "marginal" scholarship. This is the rare venue where Carspecken's feeling body, Brandom's *Spirit of Trust*, polarized modal logic, and a Best Day-style intervention can be presented together without translation.

## What the paper actually says

Rough first-person rendering:

> I teach a course in which an intervention called Best Day emerged. The intervention worked. It was praxis, not a study — decades of teaching and sustained theoretical work showing up once, in one class, with twenty-two preservice teachers. Since 2017 I have been working on the topology of intersubjective space, and PML is what the formalization currently looks like. This paper presents the S-mode axioms of PML, drawn from Carspecken's account of the feeling body and carrying Brandom's *Spirit of Trust* commitment that modality is the grammar of recognition rather than something added on top. I do not have principled N-mode or O-mode axioms. I do not know how to test the framework. Here is a sketch of what a test could look like given recent neurosymbolic work. And here is a proper first-class section on what I do not know. I am putting this in front of JTM-ME because I want reviewer feedback before the next seed grant cycle.

Not an empirical study. Not preregistered. Not a framework paper in Hennessy's sense. A report.

## Proposed section structure

Full skeleton with per-section word budgets and content notes lives in [`jtmme-pml-paper/paper-skeleton.md`](./jtmme-pml-paper/paper-skeleton.md). Summary ordering:

1. **Introduction** (~600 words) — announce genre, frame request for feedback.
2. **Best Day as praxis** (~1,000 words) — the intervention, treated as motivating practice. Tight. Based on the canonical description Tio uploaded 2026-04-16 and archived at `jtmme-pml-paper/sources/canonical-intervention-description.md`.
3. **The feeling body** (~1,200 words) — **Carspecken's theoretical concept**, not a narrated scene. The pre-reflective embodied ground on which subjective validity claims stand. The I-feeling mechanism explained in prose as the refusal of the self to be inspected as an object.
4. **S-mode axioms with *Spirit of Trust* on the ground floor** (~1,400 words) — 12-operator architecture once, then the S-mode material inferences from `pml/axioms_eml.pl` in plain English, one by one, each carrying its genealogy from Tio's notebook (this is what the NotebookLM trawl is for).
5. **What a test could look like** (~900 words) — neurosymbolic sketch; TalkMoves as site; all 12 operators (not a narrowed subset); separability, baseline, preregistration discipline; Brandomian metavocabulary framing for the rule structure.
6. **What I don't know** (~800 words) — first-class section. N-mode not principled, O-mode partial, inter-modal transitions scene-dependent, torus topology unproved, phenomenological grounding uncertain, objective validity zero.
7. **Closing — what I would like feedback on** (~450 words) — explicit list of questions for reviewers.

## What exists that can go in straight

- `pml/` module with 28/28 tests establishing internal consistency (cited, not the subject).
- Canonical Best Day description (`jtmme-pml-paper/sources/canonical-intervention-description.md`) — the only approved source for the Best Day section.
- Tio's NotebookLM notebook at https://notebooklm.google.com/notebook/a10046c9-73dc-474b-80cd-2f0659b513b9, holding the long genealogy of every named concept and axiom variable. Trawl task documented at `jtmme-pml-paper/notebooklm-trawl-task.md` for Claude Code to run before drafting.
- Sumner lab / TalkMoves citations (Suresh 2018/2019/2021, Jacobs 2022, Scornavacco; provided by Tio in conversation).
- Brandom *Spirit of Trust* connection (noted across memory files and interface docs).
- Pragmatic-axiom I-feeling mechanism (`pml/pragmatic_axioms.pl`).
- Honest uncertainty language (`.auto-memory/pml_uncertainty.md`, `.auto-memory/formalization_code_relationship.md`).

## What needs to be written from scratch

- Introduction framing the report genre.
- Best Day section written from the canonical description, at the right length, without swallowing the paper.
- Feeling body section presenting Carspecken's concept for a math-ed audience that has not read Carspecken, distinguishing Carspecken's account from Tio's development of it. Depends on the NotebookLM trawl.
- S-mode axiom section with per-axiom plain-English rendering and notebook-sourced philosophical grounding. Depends on the NotebookLM trawl.
- Neurosymbolic proposal section (seed material from the April 16 conversation; needs rewrite as paper prose).
- "What I don't know" as a full section.
- Closing request for feedback.

## What this product does not force

Because the paper is a theoretical report:

- No LLM–Prolog bridge build required (the bridge is proposed, not built).
- No preregistration file required (no empirical study is being run).
- No Subset 2 sandbox required.
- No Rosetta stone delivered (the paper proposes one; delivery is future work).
- No inter-rater coding pilot required. (Tio's 2026-04-16 correction stands: the 12-operator scheme is not something human coders can IRR on, and the paper should not pretend otherwise.)
- No changes to the pml module code.
- No N-mode axiom development required for the paper. The paper names the gap openly.
- No commitment to the current folder location. Tio will move the working folder once the shape stabilizes.

## What the paper does force

Writing only, plus a trawl:

- A NotebookLM trawl of Tio's notebook, producing source files for each named concept and axiom (see `jtmme-pml-paper/notebooklm-trawl-task.md`).
- A readable prose rendering of Carspecken's feeling body concept, adequate for a math-ed audience.
- An honest public statement of the S-mode axioms at the current level of confidence, each anchored to a notebook passage.
- A careful, short Best Day description drawn directly from the canonical Savich (March 2026) source.
- A neurosymbolic proposal written clearly enough that a reviewer with relevant expertise can identify where it breaks.
- A "what I don't know" section at full epistemic resolution.

## Single largest risk

The paper reads as philosophical theater — clever framing, no science, no empirical commitments. Mitigation is the "what I don't know" section. If that section is the most honest part of the paper, the piece works as a report and invites the kind of feedback the grant cycle needs. If it collapses into ritual humility, the paper fails even as theater.

Secondary risk: the Best Day section overruns and the paper turns into a grant preview. Tio flagged this directly — he has a well-developed account of why Best Day worked and is deliberately not putting all of it in this paper, both to avoid flooding the reader with spatialized metaphors that can only function as metaphors and to avoid confusing reviewers into reading Best Day as a research project. Keep §2 tight.

Tertiary risk: the feeling body gets rendered again as a scene rather than as Carspecken's theoretical concept. This was my error in the previous draft; it is easy to slide back into because "the scene of the feeling body" is evocative language. The NotebookLM trawl task (Tier 1, item 5) is explicit on this point to prevent a relapse.

## Honest draft language for the paper's own limitations section

For reference. Something close to this, in the paper's own voice, sits at the close of §6:

> This paper is a report. The axioms for S-mode I present here derive from Carspecken's concept of the feeling body as the pre-reflective embodied ground of subjective validity claims, and are shaped by Brandom's *A Spirit of Trust*, in which modality is the grammar of recognition rather than a layer added to cognition. The S-mode axioms have been implemented in Prolog and pass a suite of internal-consistency tests; that establishes that the axioms are jointly satisfiable, and nothing more. I do not yet have principled axioms for N-mode or O-mode. I do not have a worked account of inter-modal transitions, and my working hypothesis — that these transitions are scene-dependent rather than universal — would, if correct, explain why the system behaves more like musical notation than like a logic. I do not know how to test the framework. The neurosymbolic sketch in §5 is a proposal I am submitting for reviewer response, not a method I have run. The Best Day intervention described in §2 is the practice that motivated this formalization and continues to outrun it. I am writing this report in the hope that the theoretical-mathematics-education community will tell me where the idea breaks.

## Timeline

- NotebookLM trawl (Claude Code, unattended): 1–2 days wall clock depending on notebook size and API throughput.
- Tio review of trawl output: 3–5 days.
- Section-by-section drafting: 3–4 weeks.
- Internal review via `deschlocker`, `carspecken-voice-editor`, `philosophical-dictionary-checker`, final `carspecken-agentic-review` pass: 1 week.
- Shared with 1–2 trusted readers (Erik Jacobson being the natural first): 2 weeks.
- Submission to JTM-ME: within 7–8 weeks of today.
- Expected review turnaround: 3–6 months depending on the masked-review queue.

## Code audit as a drafting precondition

Separate from the NotebookLM concept trawl, the paper's §3 and §4 sit on top of specific Prolog files (`pml/axioms_eml.pl` for the S-mode material inferences, `pml/pragmatic_axioms.pl` plus `arche-trace/` for the I-feeling mechanism). Tio's concern that the code may not do what the documentation claims deserves a first-class response before drafting begins. [`code-audit-plan.md`](./code-audit-plan.md) lays out the plan: strip comments from the Tier 1 and Tier 2 modules, upload the stripped `.txt` files to a fresh NotebookLM notebook that contains no documentation, re-run the Deep Research prompt, and compare against the existing baroque walkthrough. The preliminary finding is that the existing walkthrough is mechanically accurate (not gibberish) but lacquered with the documentation's philosophical vocabulary; a doc-free re-read will confirm or refute this. Any Tier 1 or Tier 2 code/doc disagreement affecting the S-mode axioms or the I-feeling mechanism is a drafting blocker for §3/§4.

## Sequencing relative to other Phase 5 products

This paper is now independent of the infrastructure builds the other Phase 5 products would need. It does not race the grant; it feeds the grant by putting Best Day on the record and surfacing reviewer feedback on the formalism before the grant has to defend it. If the paper is well-received, the Best Day grant gains a citable theoretical anchor. If a reviewer identifies a serious gap in the Carspecken reading or the *Spirit of Trust* positioning, the grant benefits from learning that before the proposal is written, not after.

This is the lowest-cost, highest-feedback Phase 5 move available right now.
