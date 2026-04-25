# The Hermeneutic Calculator, Reframed

Date: 2026-04-16
Status: Ideation, pre-spec
Author context: Tio Savich, with Claude as scribe

## What changed in this session

The first two passes of this brainstorm treated the Prolog system as an ontology. Tio corrected that: it was never meant to be exhaustive. It was meant to **listen**. The CGI origin story is literal. Tio watched the CGI videos, saw children doing things, and wrote the automata to catch what was there. Earlier still, the system's primary method was to encode errors. Content was defined **by what it excludes**. That is Brandom's material incompatibility in its simplest form, sitting in the repo since before anyone named it that.

Three things follow from the reframe:

1. The system's job is not to judge, tutor, or assess. Its first job is to listen well enough that a professor can say "yes, that's what I meant" or "no, that's not it." Discernment is downstream of listening.
2. The authoring primitive is not "here is what this term means." It is "here is what this term does NOT mean." Incompatibility-first. You can ask a professor "what do you say NO to?" and they will answer for an hour. You cannot ask them to author a knowledge base, because they will refuse, correctly, on the grounds that the question is absurd.
3. The Prolog module is not one project. It is a site where several projects converge, each with a different public face and each with its own grant. The philosophical spine does not have to be visible in the product name to remain load-bearing in the code.

## What larql actually is

larql is not constrained-decoding. It decompiles a transformer into a vindex — a graph database whose nodes are the model's own learned features and whose edges are relations the model already uses to answer questions. You can DESCRIBE an entity, see its edges across layers, INSERT new edges, DELETE old ones, and COMPILE the result back into standard safetensors. Patches are ~10 KB per fact. The base vindex is readonly; all edits live in an overlay you can version, share, or revert.

This changes the architecture. We do not need to sit at the decoder and filter tokens. We write facts directly into the model's weights. When a student asks "what is formalism," the weights answer with Amy's definition because that edge now dominates. Alternative definitions fall in probability not because we prevented them but because the model itself now prefers the authored edge. Inferentialism implemented, literally, as a weights-level edit. And because patches compose, a syllabus becomes a stackable set of .vlp files that can be diff'd, code-reviewed, and rolled back.

This also means "vocabulary protection" is a different kind of thing than it sounded like. It is not a wall around what the model is allowed to say. It is an **authored preference** built into the model's own reasoning, detectable in the residual stream (larql's TRACE shows which layers contribute what at each step).

## The modular architecture

Three modules. Each is a deployable product with its own grant. Each rests on the same philosophical base. Each has an internal name (philosophically honest) and an external name (grant-legible).

### Module 1 — Vocabulary Determinacy
- **Internal:** Incompatibility-first course vocabulary; Brandom's material inference as authoring primitive.
- **External:** "Course Vocabulary Protection for Faculty" (IU Luddy AI+X, NSF IUSE Level 1).
- **Year:** now → summer 2026; pilot in N101.
- **Deliverable:** a professor loads their course's notes, answers a structured "what do you say NO to?" interview, and gets a .vlp patch that deterministically shifts the model's answers toward their definitions. Students can chat with it; the professor can audit.
- **Measurable:** pre/post term-drift on a held-out question set; rate of professor-approved answers.

### Module 2 — Math in the Prolog
- **Internal:** Strategy automata as formal student-thinking models; Prolog proof-loops as pedagogical signals; erasure points as interesting.
- **External:** "Strategy Automata for Formative Assessment" (NSF DRK-12 Exploring; IU School of Ed).
- **Year:** 2 (funded by Module 1 or by a DRK-12 Exploring).
- **Deliverable:** given a student's written or spoken solution, classify strategy against the automata; surface the erasure points and the loop states to the teacher as interpretable diagnostics.
- **Measurable:** inter-rater agreement between the automaton's classification and a trained human coder on classroom transcripts.

### Module 3 — Catastrophe Listening
- **Internal:** Cusp-catastrophe model of student cognitive strain. Real-time audio and video from Amy Hackenberg's teaching experiment lab. Two attractors, two folds: the aha fold and the frustration fold. The system probes toward the first while staying far from the second. Carspecken realized as dynamics, not as visualization.
- **External:** "Real-Time Classroom Discourse Analytics for Responsive Teaching" (NSF DRK-12 D&D with Amy as co-PI; possibly an NSF AI Institute thematic if the right one opens).
- **Year:** 3.
- **Deliverable:** an instrumented classroom that gives Amy a real-time "about-to-aha / about-to-frustrate" map for each small group; teacher dashboard surfaces two or three groups at a time.
- **Measurable:** predictive validity of the strain signal against a trained teacher's "who needs me now" judgment.

Modules 1 and 2 feed Module 3. The Prolog proof engine is the source of the strain signal in 3. The authored vocabulary in 1 is the ground truth for the strategy classification in 2. The whole stack remains coherent even if a module is publicly pitched as a stand-alone tool.

## What the philosophy buys us, even when backgrounded

| Module | What reviewer sees | What the code actually does |
|---|---|---|
| 1 | Term-drift reduction tool | Authors material-incompatibility graphs |
| 2 | Formative-assessment classifier | Runs inferential commitment proofs |
| 3 | Discourse analytics | Models the space of reasons dynamically |

In every case, the grant-legible description is a true description. We are not hiding anything. We are simply declining to lead with the philosophical framing in contexts where it would cost us the audience. This is what Tio already does when teaching N101 without saying "Brandom" out loud. Same discipline.

## Tonight's benchmark: a safe vocabulary list

**Scope, honestly cut:**

Tonight's deliverable is the **Prolog half** of Module 1 — the authoring and validation layer. The larql half (writing the patch into a real vindex and running inference against it) will be a follow-up session because it requires building the larql Rust workspace, downloading or extracting a Gemma vindex (~3-6 GB on disk), and at least one dry run on hardware we have not yet verified.

**What "safe" means:**
- Every term in the list is either a positive definition drawn from N101 coursenotes or an incompatibility explicitly stated in the notes.
- Every claim the validator accepts is traceable to a Prolog clause with a file:line reference back to the coursenotes.
- When the validator rejects a claim, it returns the specific incompatibility clause that fired, so Amy can see why.

**Target: 8-12 terms, drawn from days 1-3 of the notes**

Proposed starter set (all extractable from what I've read):

| Term | Positive commitment | Incompatibilities |
|---|---|---|
| quantity | property of an object that is measurable | "a value of a quantity" is not itself a quantity; a number without a property is not a quantity |
| measurement unit | the unit with which the measurement process counts | the object being measured is not its unit |
| measurement process | the subdivide-and-count operation | imagining a value is not a process |
| measure (value) | the number of units found | the measure is not the quantity itself |
| base (ten or other) | a group used recursively to build numbers | a base is not just "ten"; it is not an arbitrary label |
| counting | determining the value of a quantity | reciting number words without reference to a quantity is not counting |
| explanation | description + reasons + (sometimes) consequences | description alone is not an explanation; "because it's easier" is not a reason |
| strategy (SCA, RMB, Chunking) | a named pattern of additive reasoning | two strategies with the same answer are not "the same strategy" |
| base-five "10" | one base and zero loose tallies | the digit 5 cannot be used for the base in base five |
| creative activity (in math) | generating, choosing, revising pathways | drill and rote arithmetic are not creative activity |

That is the target for tonight.

## What I need from Tio before coding

1. **Do you want the Prolog module to live under `strategies/` or under a new directory** (e.g., `vocabularies/n101/`)? I lean toward the second — vocabularies are a distinct kind of artifact from strategy automata, and mixing them will get messy when a second course shows up.
2. **File format preference**: one big Prolog file per course, or one file per term? I lean toward one file per term with an index — it keeps authoring changes atomic and makes a future authoring UI easier.
3. **Should the validator take plain strings** (rough, cheap) or **pre-tokenized / light-parsed claims** (more structure, slower path to first demo)? For tonight I propose plain strings with keyword-level incompatibility triggers. Crude, but it produces a demo.
4. **Should we stub the larql patch emitter tonight** (i.e., write the `.vlp` generator in Prolog but not actually apply a patch), or defer it entirely? I lean defer. The Prolog side is the part that is both authorable and verifiable without a GPU.
5. **Is there a Gemma vindex already on disk somewhere on this machine**, or would we be building one from scratch? If already present, we might be able to do a larql smoke test in the same session. If not, we defer.

## What's explicitly deferred

- The K-12 deployment story. Module 1 is N101 (IU undergrads). K-12 is Module 3 territory.
- The chorus / projector / per-Chromebook LLM designs from the earlier brainstorm. Not dead — staged.
- Fractions. Still unsolved in the repo per `FRACTION_CRISIS_ASSESSMENT.md`. Out of scope for N101 Module 1 because N101 starts with quantity and base systems, not fractions.
- The catastrophe model. Year 3.
- Any grant writing beyond a paragraph. First show working tool, then write proposal.

## Open conceptual question

The professor interview — "what do you say NO to?" — is the load-bearing authoring interaction. It works for Amy because she already thinks this way. It may not work for professors who treat their syllabi as lists of topics rather than systems of commitments. If that is common (I suspect it is), then Module 1 needs a second authoring path: upload coursenotes, let the system *propose* a draft incompatibility set, professor edits. That is harder to build and more politically useful. Worth thinking about before spec.

---

**Next step in this session:** Tio answers questions 1-5 above (can be a short list). Then I write a spec for the tonight prototype and we code.
