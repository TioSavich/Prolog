# Mathematics Education Literature Corpus

A self-contained research package: **2,151 articles** across 12 mathematics education journals, with **3,532 extracted misconception entries** coded for orientation, antecedent, intervention, repair, vocabulary, and page references.

All paths in this package are **relative** to this folder. Copy or upload the entire `research_corpus/` directory anywhere — no external references.

---

## Contents

```
research_corpus/
├── research.db               # SQLite, canonical source of truth
├── research.db.schema.sql    # full schema
├── references.bib            # unified BibTeX, one entry per article
├── pdfs/<JOURNAL>/<BibKey>.pdf    # 2,151 renamed PDFs organized by journal
├── extraction_results/       # raw per-source JSON extractions from NotebookLM
├── exports/
│   ├── articles.csv          # one row per article
│   ├── misconceptions.csv    # one row per misconception, denormalized
│   └── catalog.md            # human-readable catalog grouped by journal
├── scripts/
│   ├── load_extractions.py   # load extraction JSONs into DB (idempotent)
│   └── export_catalog.py     # regenerate the CSVs and catalog.md
└── README.md                 # this file
```

**Total size:** ~1.8 GB (mostly PDFs).

---

## Journal coverage

| Journal | Articles |
|---------|---------:|
| JRME — Journal for Research in Mathematics Education | 288 |
| ZDM — Zentralblatt für Didaktik der Mathematik | 288 |
| JMB — Journal of Mathematical Behavior | 252 |
| ESM — Educational Studies in Mathematics | 470 |
| IJMEST — International J. Math. Education in Sci. & Tech. | 302 |
| IJSME — International J. Science & Mathematics Education | 120 |
| MERJ — Mathematics Education Research Journal | 154 |
| JMTE — Journal of Mathematics Teacher Education | 149 |
| RME — Research in Mathematics Education | 77 |
| MTL — Mathematical Thinking and Learning | 51 |

**1,117 papers** (52%) substantively engage with errors / misconceptions / perturbations as subject matter.

---

## Filename convention

`pdfs/<JOURNAL>/<JOURNAL>_<FirstAuthorSurname>_<Year>_<FirstTitleWord>.pdf`

Examples:
- `pdfs/JMB/JMB_Hackenberg_2007_Units.pdf`
- `pdfs/JRME/JRME_Izsak_2008_Teaching.pdf`
- `pdfs/MTL/MTL_Steffe_2004_On.pdf`

The same slug is the BibTeX citation key in `references.bib`.

---

## Schema at a glance

### `articles` — one row per article

Identity & location:
- `id`, `bibtex_key`, `doi`, `local_pdf_path` (relative), `notebooklm_source_id`

Bibliographic:
- `authors`, `year`, `title`, `journal`

Study context:
- `country`, `language`, `curriculum_context`
- `population_type` — `student | PST | in_service | mixed | other`
- `grade_band_as_declared`, `sample_size`
- `study_design` — `clinical_interview | teaching_experiment | design_experiment | case_study | survey | ...`
- `theoretical_framework`

Evidence flags (for later PDF digging):
- `has_figures`, `has_student_work_figures`, `has_transcripts`
- `figure_page_refs`, `transcript_page_refs`

Article-level analysis:
- `incompatibility_foregrounded` — true if the article substantively engages error/perturbation/friction as its subject matter
- `orientation`, `extraction_notes`

### `error_instances` — one row per misconception

Links to article by `article_id`. Each misconception records:
- `error_description`, `example`
- `mathematical_domain` — `whole_number | fraction | decimal | percent | rational | integer | proportional | algebraic | geometric | measurement | probability | statistics | calculus | combinatorial | other`
- `mathematical_topic`, `mathematical_subtopic`
- `vocabulary_used_raw` — the exact language the article uses (preserved verbatim)
- `orientation` — `deficit | productive | mixed | sociocultural | uncoded`
- `locus_of_attribution` — `student | teacher | task | curriculum | culture | multiple | unclear`
- `antecedent` — what the student did/knew just before the error surfaced
- `intervention` — pedagogical design (manipulatives, task sequence, microworld, etc.)
- `repair` — in-the-moment teacher/researcher move
- `salience` — `primary | secondary | passing`
- `page_refs`

Other tables (`vocabulary_terms`, `negation_types`, `traditions`, `gems`, `citations`, `decentering_instances`) are ready for downstream theoretical coding and currently mostly empty.

---

## Query recipes

### Find misconceptions about middle-school PSTs' geometric knowledge
```sql
SELECT a.bibtex_key, a.authors, a.year, a.title,
       e.error_description, e.vocabulary_used_raw, e.page_refs,
       a.local_pdf_path
FROM error_instances e
JOIN articles a ON e.article_id = a.id
WHERE a.population_type = 'PST'
  AND a.grade_band_as_declared LIKE '%middle%'
  AND e.mathematical_domain = 'geometric';
```

### Papers with transcripts of Korean students working on fractions
```sql
SELECT bibtex_key, authors, year, title, transcript_page_refs, local_pdf_path
FROM articles
WHERE country LIKE '%Korea%'
  AND has_transcripts = 1
  AND EXISTS (SELECT 1 FROM error_instances e
              WHERE e.article_id = articles.id
                AND e.mathematical_domain = 'fraction');
```

### Orientations by domain
```sql
SELECT e.mathematical_domain, e.orientation, COUNT(*) AS n
FROM error_instances e
GROUP BY 1, 2 ORDER BY 1, n DESC;
```

### Articles using "perturbation" vocabulary
```sql
SELECT a.bibtex_key, a.authors, a.year, e.error_description
FROM error_instances e
JOIN articles a ON e.article_id = a.id
WHERE e.vocabulary_used_raw LIKE '%perturbation%';
```

### Pull PDF paths + page refs for agent-driven deep analysis
```sql
SELECT bibtex_key, local_pdf_path,
       (SELECT GROUP_CONCAT(page_refs, '; ') FROM error_instances e WHERE e.article_id = a.id) AS relevant_pages
FROM articles a
WHERE bibtex_key IN ('JMB_Hackenberg_2007_Units', 'JMB_Steffe_2002_New');
```

---

## Reproducing the pipeline

### Re-load extractions into DB
```bash
python3 scripts/load_extractions.py
```
Idempotent by `notebooklm_source_id`; re-running refreshes article metadata and replaces misconception rows for that article.

### Regenerate exports
```bash
RESEARCH_DB=research.db EXPORT_DIR=exports python3 scripts/export_catalog.py
```

---

## Attribution

Extraction performed using Anthropic's Claude via NotebookLM MCP server against 12 MathEd journal corpora uploaded to NotebookLM. The `vocabulary_used_raw` field preserves each article's language verbatim, per the project's theoretical interest in how vocabularies carry epistemological commitments.

Each `extraction_results/<JOURNAL>/<source_id>.json` file is the raw per-source output from a structured query with strict JSON schema. These files are the provenance trail — re-running `load_extractions.py` regenerates the DB from them.

---

## Known caveats

1. **Orientation drift.** NotebookLM occasionally codes a misconception as `deficit` when the surrounding article uses deficit language descriptively even though the theoretical frame is productive. Spot-check where orientation matters most.
2. **Author parsing** uses heuristics for surname extraction. Works for ~99% of cases.
3. **Transcript & figure flags** are derived from NotebookLM's read of the text. Use `page_refs` as the canonical pointer and verify by opening the PDF.
4. **Downstream theoretical coding is NOT done.** `vocabulary_terms`, `negation_types`, `traditions`, `gems`, `citations`, `decentering_instances` tables exist but are empty — requires reading source texts (Brandom, Carspecken, Habermas) against the corpus.
