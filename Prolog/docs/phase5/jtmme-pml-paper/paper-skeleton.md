# [Working title — placeholder]

*Something like: "Axioms for Subjective Validity from the Feeling Body: A Report on Polarized Modal Logic, with a Note on the Best Day Intervention"*

**Target:** Journal for Theoretical & Marginal Mathematics Education (JTM-ME)
**Category:** Theoretical / Philosophical (≤12,000 words)
**Author:** Theodore M. Savich

---

## Status

Skeleton only. Drafting happens after the NotebookLM trawl (see `notebooklm-trawl-task.md`) so that each axiom presented can carry its genealogy from Tio's notebook.

Tio sits on the editorial board of JTM-ME. He will recuse from handling his own submission; the editorial relationship is a reason to submit here, not a reason to hide the work elsewhere.

---

## Ordering (confirmed 2026-04-16)

Best Day precedes the axioms. The intervention is the praxis that motivated the formalization, not an empirical test of it. Ordering: 1 Intro → 2 Best Day → 3 Feeling Body → 4 S-mode Axioms → 5 Neurosymbolic Sketch → 6 What I Don't Know → 7 Closing.

---

## §1. Introduction — a report, not a study

**Target word count:** 500–700

Announce the genre in the first paragraph. This is a report from inside a long-running formalization project, submitted to a venue that welcomes marginal work. The reader should know by the second paragraph that (a) the paper is not an empirical study, (b) the paper is not a big-claims framework paper, and (c) the author is asking for reviewer feedback rather than declaring results.

Name what the paper does contain: a description of an instructional intervention (Best Day) that forced a formalization; axioms for one modal-pragmatic region (S-mode) of that formalization; a sketch of what a test might look like given recent neurosymbolic work; and a first-class section on what the author does not yet know.

Introduce the theoretical frame in one paragraph: Carspecken's feeling body as the phenomenological ground for subjective validity claims; Brandom's *A Spirit of Trust* for modality built into recognition rather than added to cognition; Hegel's *in*finite / finite movement as the backdrop against which the Best Day intervention made sense to the author.

---

## §2. Best Day — praxis that preceded the formalization

**Target word count:** 900–1,100

Based on the canonical description in `sources/canonical-intervention-description.md`. This section should be tight enough not to swallow the paper and rich enough that a reader understands what happened. Treat Best Day as *praxis* — decades of teaching experience and sustained theoretical work showing up once, in one class, with 22 preservice teachers.

Key elements to include, in roughly this order:

- The classroom context. M300, Teaching in a Pluralistic Society, at Indiana University. The political constraint (conservative state, adjacent-discipline case of monitored instruction). Why the constraint turned out to be productive.
- The fifth practice problem. The Five Practices' "connecting" is hard because it asks the instructor to hold a combinatorial space of connections in working memory. 22 students → 231 pairwise connections. Unaided cognition fails.
- The process. Students write "Experience of Difference" papers. Tio reads, writes feedback, notices connections he cannot hold. Tio's feedback (not student work) is fed to a NotebookLM populated with Hegel, Brandom, Habermas, Carspecken. AI thematizes *his* interpretive labor. Tio then iterates toward dyadic candidate questions, rebuilds them by hand, prints a network diagram of all 231 possible connections, and hands each student a unique subset of named dyadic prompts.
- Why it worked, in brief. One paragraph. The existential fear of being misrecognized was treated with care. Tio worked the whole semester to build a classroom where students could say "no" to a question. Discussion handouts voiced critiques of their own prompts. Guided meditation with toes and sublation. A song from Tio's dissertation modeling vulnerability. The question form gives students authority to reject the premise. Do not try to explain everything here; a full account is spread across Tio's prior work and the notebook, and this paper is not the place for it.
- What the intervention is not. Not a research study. Not a one-off experiment. Not a mystery. Praxis drawing on decades of teaching and intense theoretical work, performed once with no systematic data collection. It is the motivating practice, not the subject of the paper.
- The transition to the formalization. One paragraph. Best Day works; the author has been trying to formalize the topology of intersubjective space since 2017; PML is what the formalization currently looks like.

**What the section must not do:**
- Overclaim (no "the intervention demonstrates...").
- Reduce to an AI-in-the-classroom story. AI's role was cognitive tooling on the instructor's own interpretive labor. AI did not interact with students.
- Over-explain the theoretical machinery. The point is to let the reader feel what the problem was; the machinery comes later.

---

## §3. The feeling body — Carspecken's account of subjective validity's ground

**Target word count:** 1,000–1,300

**Correction from prior draft:** this is not a narrated scene. "Feeling body" is a concept in Carspecken's horizon analysis. It names the pre-reflective embodied sense one has of one's own inner states — the ground on which subjective validity claims stand, distinct from objective (empirical) and normative (deontic) validity claims. Drawing on the NotebookLM trawl (Tier 1, item 5), present Carspecken's account in his own vocabulary where possible, and then identify where Tio's development departs from or extends Carspecken.

Key moves for the section:

- Distinguish Carspecken's three validity registers (objective, subjective, normative) briefly, citing the horizon analysis material from the notebook.
- Explain why subjective validity cannot be grounded in introspection-as-observation. Introspection-as-observation is picture-thinking applied to the self; it assumes the self can be inspected the way an external object can be. Carspecken's move is to ground subjective validity in the embodied felt sense — the feeling body — which is neither an object observed nor a purely conceptual posit.
- Introduce the I-feeling mechanism. In Tio's PML it is formalized as an attributed variable that refuses unification with any concrete term. The philosophical reason for this refusal is that identifying the I-feeling with a concrete term is precisely the picture-thinking move the account is trying to avoid. Explain this in prose, not in Prolog.
- Flag the author's uncertainty about how the feeling body relates to intersubjectivity. The feeling body is primarily a phenomenological ground for first-person subjective claims. How it couples to the second- and third-person validity registers is where the PML's N-mode and O-mode axioms either succeed or reveal themselves as underdeveloped.

This section does the heaviest conceptual work in the paper. It should close by naming the two questions it opens: (a) what are the dynamics internal to subjective validity claims (answered in §4), and (b) how do subjective moves transfer to objective and normative moves (named but not answered).

---

## §4. Axioms for S-mode, with Spirit of Trust on the ground floor

**Target word count:** 1,200–1,500

Present PML's 12-operator architecture once, in prose: three modes (S, O, N) × compressive/expansive polarity × necessity/possibility. State that this paper focuses on the S-mode axioms.

Then present the S-mode material inferences from `pml/axioms_eml.pl` in plain English, one by one, with each drawing on the NotebookLM genealogy (Tier 3). Roughly:

- Unity compresses into awareness (`s(u) → s(□↓ a)`).
- Awareness opens possibility of release (`s(a) → s(◇↑ lg)`).
- Awareness opens possibility of fixation (`s(a) → s(◇↓ t)`).
- The being/nothing bad-infinite cycle (`s(t_b) ↔ s(□↓ t_n)`).
- Tension compresses into the negation of unity (`s(t) → s(□↓ ¬u)`).
- Release expansively necessitates the return to unity at a higher register (`s(lg) → s(□↑ u′)`).
- Two confessions expansively necessitate forgiveness (intersubjective recognition; from `intersubjective_praxis.pl`).

For each axiom: state it in English, locate it in the notebook, name which Hegelian / Carspeckenian / Brandomian passage it operationalizes, and note the polarity ("this axiom does X under compression; it does Y under expansion").

Brandom's *A Spirit of Trust* move is present in the architecture: modality is not added on top of recognition, it is the grammar of recognition. This is why the axioms look modal rather than declarative. State this once, early in the section, and let the axioms bear it out.

**What the section must not do:**
- Present N-mode or O-mode axioms as if they were on the same footing. They aren't. Say so.
- Transliterate the Prolog literally into English. The paper's reader is not a Prolog reader; they need the dialectical movement, not the syntax.
- Claim that the internal consistency established by the Prolog tests is any kind of empirical validation. It isn't.

---

## §5. What a test could look like, given recent neurosymbolic work

**Target word count:** 800–1,000

The test proposal as a sketch. Reference:

- The Sumner lab / TalkMoves corpus and the publications Tio flagged: Suresh et al. 2018, 2019, 2021; Jacobs et al. 2022; Scornavacco et al.
- The neurosymbolic framing (LARQL, Gary Marcus-adjacent work).
- The proposed architecture: LLM assigns operator profiles to utterances; Prolog carries the formal inference; predictions are tested against an already-labeled corpus nobody has pre-read. Operator assignments run against all 12 operators (not a narrowed subset); non-firing in a given discourse genre is itself data.
- The minimum discipline: separability between LLM perception and Prolog inference (a second party should be able to run the same Prolog on the same operator assignments and reproduce the predictions); a baseline to beat (same-LLM-without-PML); preregistration of the prediction task before opening the test corpus.
- The Brandomian architecture of the test: deontic-normative as pragmatic metavocabulary for alethic modal vocabulary. The N-mode operators make explicit what is implicit in the S/O structure of the discourse. This shapes what the Prolog rules should look like — not flat transition prediction but grammar-of-explicit-making.

The section closes by naming exactly what the author is asking the reader for: feedback on the proposal from anyone who has run this kind of neurosymbolic test. It is a proposal, not a method the author has already executed.

---

## §6. What I don't know

**Target word count:** 700–900 — **first-class section, not a buried paragraph**

Items:

- N-mode axioms are not principled. `pml/semantic_axioms.pl` has only N→N reflexive patterns. N→S (internalization) and N→O (application) are named in prose and have no inference rules.
- O-mode axioms are partial. The oobleck principle captures an S→O transfer but the full O-mode register is underdeveloped.
- Inter-modal transitions appear to be scene-dependent rather than universal. The oobleck metaphor works in one scene; it is not demonstrably a general law. This is why the framework is called a logic but behaves more like musical notation — the transformation rules may be genre-bound.
- The torus topology Tio finds suggestive is not proved. The topological picture is a long-standing heuristic in Tio's work; the Prolog does not establish it as a formal result.
- The phenomenological grounding is uncertain. Tio can describe the feeling body from within Carspecken's account; he cannot guarantee the formal operators track the phenomenology they are meant to track. This is the hermeneutic circle problem recorded in `.auto-memory/pml_uncertainty.md`.
- The objective validity of the framework is, at time of submission, zero. The Prolog tests establish internal consistency and nothing more.
- The framework is complicated enough to illuminate anything. Illumination is therefore cheap. Objective validity is the open question.

Each bullet, in the final draft, expands into a short paragraph with specific reference — a file, a note, a passage — so the reader can verify that the author is not performing humility but reporting real limits.

---

## §7. Closing — what the author would like feedback on

**Target word count:** 400–500

Explicit list:

- Is the Carspecken reading of the feeling body defensible? Where does the paper's presentation diverge from Carspecken's own?
- Do the S-mode axioms look right to readers more at home in modal logic than the author?
- Is the neurosymbolic test proposal plausible, or does it break for reasons the author has not foreseen?
- Is the Best Day intervention at all interesting for this community, and if so, where is its research home?
- What prior work is the author missing?

Close with the report genre named: this is an invitation to a community, not a conclusion.

---

## Appendix / Supplementary — optional

If JTM-ME's author-managed supplementary material policy permits, include:
- A short glossary of the S-mode shorthand terms (u, u_prime, a, lg, t, t_b, t_n).
- The Prolog operator tables for reference, with the understanding that the paper does not expect its reader to read them.

---

## Style commitments

Tio's voice commitments apply throughout:

- Epistemic humility without collapse into self-abasement.
- Anti-ocular language — no "see," "picture," "view," "perspective" (see `.claude/skills/epistemic-code-voice/` for the full inventory).
- Anti-schlock — no pedagogical hand-holding, no explicated jokes, no "of course" moves.
- Anti-AI-tell — specifically watch for negative parallelism ("X is not A but rather B"), triadic lists, and em-dash pivots; use them sparingly where they earn their place.
- Skills to run before submission: `deschlocker`, `carspecken-voice-editor` (on philosophical sections specifically), `philosophical-dictionary-checker`, and a final `carspecken-agentic-review` pass.

---

## Next steps

1. Claude Code runs `notebooklm-trawl-task.md`. Output lands in `sources/`. Expect 4–6 files plus a `TRAWL_SUMMARY.md`.
2. Tio reviews the trawl, flags corrections, decides which divergences between notebook and Prolog are going to become paper content and which are out of scope.
3. Drafting begins, section by section, using this skeleton as an outline. Each section gets its own `.md` file in the folder, then assembled later.
4. First full draft runs through `deschlocker` and `carspecken-voice-editor`.
5. Share with one or two trusted readers before submission. Candidates: Erik Jacobson (measurement angle, TalkMoves entry point), David Bowers or Alex Moore if editorial relationships permit (they would need to recuse from review).
6. Submit to JTM-ME, Theoretical / Philosophical category, masked review to keep the board relationship clean.
