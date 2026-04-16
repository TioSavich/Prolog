# NotebookLM Trawl Task — PML Genealogy for JTM-ME Paper

**For:** Claude Code, using the NotebookLM MCP
**Target notebook:** https://notebooklm.google.com/notebook/a10046c9-73dc-474b-80cd-2f0659b513b9
**Output location:** `docs/phase5/jtmme-pml-paper/sources/`
**Purpose:** Build an annotated source file for the JTM-ME paper by tracing PML axioms and key concepts back through Tio's notebook. Tio has been working on the topology of intersubjective space since 2017; the axioms did not fall out of the sky, they crystallized from years of reading and writing. The paper needs that genealogy available as the axioms are presented.

---

## What to trawl for

### Tier 1 — named concepts Tio called out explicitly

For each, pull every passage from the NotebookLM notebook that discusses the concept, summarize its role in Tio's development, and save the raw quoted passages with their source citations (where NotebookLM surfaces them):

1. **"space of reasons"** — Sellarsian / Brandomian concept; the inferential web Tio is trying to topologize. Look for how Tio treats it as a *topological* object rather than a flat field.
2. **"I-feeling"** — the PML mechanism where the attributed variable refuses unification. Trace how this relates to Carspecken's feeling body, to identity claims, and to subjective validity.
3. **"somatic sublation"** — the Hegelian move worked through the body rather than purely conceptually. How does this relate to the guided meditation with "toes and sublation" Tio used in M300?
4. **"oobleck principle"** (also "oobleck dynamic" / "oobleck transfer") — the S-to-O transfer where subjective compression becomes objective under certain pressures. The name is a metaphor; the question is *what the metaphor is formalizing*.
5. **"feeling body"** — Carspecken's concept. This is *not a narrative scene* in Carspecken; it is Carspecken's theoretical account of how subjective validity claims are grounded in pre-reflective embodied experience. Pull Tio's notes on this specifically, in Carspecken's own language where possible, and distinguish Carspecken's concept from Tio's development of it.
6. **"topology of intersubjective space"** / **"torus topology"** — the geometric picture of the space of reasons Tio has been developing since 2017. Where does the torus come in, what does it do, and where does Tio express doubt about its strict applicability (versus its heuristic value)?

### Tier 2 — PML axiom shorthand

The Prolog in `pml/axioms_eml.pl`, `pml/semantic_axioms.pl`, and `pml/intersubjective_praxis.pl` uses short variable names (`u`, `u_prime`, `a`, `lg`, `t`, `t_b`, `t_n`, and others). For the paper to present the axioms to a human reader, each shorthand needs to be traceable to a concept in the notebook. For each shorthand variable:

- Enumerate by reading the three axiom files. List every shorthand term, its current one-line gloss in the Prolog, and the axiom(s) it appears in.
- For each term, query the notebook for the concept it stands in for (e.g., `u` for "unity," `a` for "awareness," `lg` for "letting go / release").
- Pull representative passages and a short natural-language definition.
- Flag any term whose notebook-meaning doesn't match the Prolog gloss — this is a place the paper will need to disambiguate or correct.

### Tier 3 — EML material inferences

The eight material inferences in `axioms_eml.pl` (unity compresses into awareness, awareness opens possibility of release, being-nothing bad-infinite cycle, and so on) each encode a dialectical movement. For each inference:

- Write it in plain English as a dialectical movement.
- Find the notebook passage where that movement is worked through phenomenologically or philosophically.
- Note which Hegelian / Carspeckenian / Brandomian source Tio draws on at that point.

### Tier 4 — the intersubjective recognition axiom

`pml/intersubjective_praxis.pl` encodes "two confessions → expansive necessity of forgiveness" — drawn from Brandom's *A Spirit of Trust* reading of Hegel's magnanimity/forgiveness structure. Pull Tio's notebook passages on this specifically, including any discussion of how this axiom relates to the Best Day intervention (where the hypothetical dyadic connection is itself a confession-structured invitation).

---

## How to structure the output

For each item in Tiers 1–4, create a file under `docs/phase5/jtmme-pml-paper/sources/` with the following structure:

```markdown
# [concept name or axiom]

## Role in PML
[one-paragraph gloss of how this concept functions in the current Prolog]

## Genealogy from Tio's notebook
[chronologically-ordered pulled passages with source citations, if NotebookLM surfaces them]

## Carspecken / Brandom / Hegel / Habermas source anchors
[quoted passages from the primary theoretical sources in the notebook]

## Open questions for the paper
[places where the notebook genealogy and the current Prolog diverge, or where Tio expressed uncertainty]
```

File naming: `tier1-space-of-reasons.md`, `tier1-i-feeling.md`, `tier2-axiom-shorthand.md` (one combined file for all the shorthand), `tier3-material-inferences.md`, `tier4-recognition-axiom.md`.

---

## What NOT to do

- Do not synthesize beyond what the notebook contains. If a passage is missing, say so explicitly — that's a signal to Tio about where the theoretical grounding is thin.
- Do not paraphrase away technical distinctions. If Carspecken's "feeling body" and Tio's use of "I-feeling" are close but not identical, preserve both languages.
- Do not attempt to write paper sections from the trawl. The trawl is raw material. Drafting happens after Tio reviews the sources.
- Do not include anything from student work, student names, or Best Day intervention artifacts that involve identifiable students. Tio has a separate anonymized canonical description (see `sources/canonical-intervention-description.md`) that is the only approved source on Best Day for this paper.

---

## Sequencing

1. Start with Tier 1 — the named concepts. These determine whether the rest of the trawl is usable.
2. Tier 2 (shorthand enumeration) can run in parallel with Tier 1 if you have the Prolog module open.
3. Tiers 3 and 4 depend on Tier 1 being complete, since the material inferences and recognition axiom build on the named concepts.
4. After the trawl, write a one-page `TRAWL_SUMMARY.md` in this folder that flags: (a) which concepts have strong notebook grounding, (b) which have thin grounding, (c) which have divergences between notebook and Prolog. This is the deliverable Tio will read first.

---

## Note on the feeling body — avoid the error in my earlier read

In the prior draft of `pml-feasibility-paper.md` (now corrected), I treated "the scene of the feeling body" as a single narrative scene Tio had written axioms about. Tio corrected this: the *feeling body* is Carspecken's theoretical account of how subjective validity claims are grounded in pre-reflective embodied experience. The PML S-mode axioms are drawn from Carspecken's theoretical framework, not from narrating one particular scene. The trawl needs to surface Carspecken's theoretical apparatus here, not a scene. Expect the notebook to carry Carspecken in dense and possibly fragmentary form — pull the fragments rather than synthesizing them into a tidy summary.
