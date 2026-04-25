#!/usr/bin/env python3
"""Export research.db to flat CSVs and a human-readable markdown catalog.

Produces:
  exports/articles.csv       — one row per article, all metadata
  exports/misconceptions.csv — one row per misconception, denormalized with article info
  exports/catalog.md         — readable catalog grouped by journal, with misconceptions inline

Run: python3 export_catalog.py
"""
import csv
import os
import sqlite3
from pathlib import Path

# Allow overriding DB path + export dir via env vars for corpus use
DEFAULT_ROOT = Path(__file__).parent
DB_PATH = Path(os.environ.get("RESEARCH_DB", DEFAULT_ROOT / "research.db"))
EXPORT_DIR = Path(os.environ.get("EXPORT_DIR", DEFAULT_ROOT / "exports"))
EXPORT_DIR.mkdir(exist_ok=True, parents=True)


def main():
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    c = conn.cursor()

    # --- articles.csv ---
    c.execute("""
        SELECT id, bibtex_key, authors, year, title, journal,
               doi, country, language, curriculum_context,
               population_type, grade_band_as_declared, sample_size,
               study_design, theoretical_framework,
               has_figures, has_student_work_figures, has_transcripts,
               figure_page_refs, transcript_page_refs,
               incompatibility_foregrounded, orientation, extraction_notes,
               notebooklm_source_id, notebooklm_notebook_id
        FROM articles
        WHERE notebooklm_source_id IS NOT NULL
        ORDER BY year, authors
    """)
    articles = [dict(r) for r in c.fetchall()]

    with open(EXPORT_DIR / "articles.csv", "w", newline="") as fh:
        writer = csv.DictWriter(fh, fieldnames=list(articles[0].keys()))
        writer.writeheader()
        writer.writerows(articles)
    print(f"articles.csv: {len(articles)} rows")

    # --- misconceptions.csv (denormalized) ---
    c.execute("""
        SELECT e.id AS misconception_id, e.article_id,
               a.authors, a.year, a.title, a.journal, a.country, a.population_type, a.grade_band_as_declared,
               e.error_description, e.example,
               e.mathematical_domain, e.mathematical_topic, e.mathematical_subtopic,
               e.orientation, e.locus_of_attribution, e.salience,
               e.antecedent, e.intervention, e.repair,
               e.vocabulary_used_raw, e.page_refs
        FROM error_instances e
        JOIN articles a ON e.article_id = a.id
        WHERE a.notebooklm_source_id IS NOT NULL
        ORDER BY a.year, a.authors, e.id
    """)
    misc = [dict(r) for r in c.fetchall()]

    with open(EXPORT_DIR / "misconceptions.csv", "w", newline="") as fh:
        writer = csv.DictWriter(fh, fieldnames=list(misc[0].keys()))
        writer.writeheader()
        writer.writerows(misc)
    print(f"misconceptions.csv: {len(misc)} rows")

    # --- catalog.md ---
    nb_name = {
        "756fcf49-2a4b-4aeb-ab1c-0271120fa71f": "JRME",
        "3a6f6f0e-fb60-48a1-87fb-8d8266787904": "ZDM",
        "eb2f671d-b169-492b-823a-18bba108cd0e": "JMB",
        "a2acbde0-237d-4e83-9fbe-2dbaf778119d": "ESM (A-K)",
        "f95f32e6-2950-498a-a7a7-c563f9d808b7": "ESM (L-Z)",
        "1dbd32d5-0913-431b-bc4d-b617af0b99e5": "MERJ",
        "5cc5a6f6-1641-45c7-a74b-2235177f418f": "IJMEST (A-L)",
        "0dd73888-fe91-4514-b3df-595f86971db5": "IJMEST (M-Z)",
        "993c37ef-6d70-4939-9346-7fcd9ba924ad": "JMTE",
        "de8d7a68-f489-4417-a1e7-40a713aed946": "IJSME",
        "bc1c03cc-9a1a-4691-bcc0-78ead81efd07": "RME",
        "9f753496-36a7-427d-ab76-eaf90b2a7492": "MTL",
    }

    c.execute("""
        SELECT id, authors, year, title, country, population_type, grade_band_as_declared,
               sample_size, study_design, theoretical_framework,
               has_figures, has_student_work_figures, has_transcripts,
               incompatibility_foregrounded, extraction_notes,
               notebooklm_notebook_id
        FROM articles
        WHERE notebooklm_source_id IS NOT NULL
        ORDER BY notebooklm_notebook_id, year, authors
    """)
    rows = c.fetchall()

    c.execute("""
        SELECT article_id, error_description, example, mathematical_domain, mathematical_topic,
               orientation, locus_of_attribution, salience,
               antecedent, intervention, repair, vocabulary_used_raw, page_refs
        FROM error_instances
        ORDER BY article_id, id
    """)
    misc_by_article = {}
    for m in c.fetchall():
        misc_by_article.setdefault(m["article_id"], []).append(m)

    total = len(rows)
    with_misc = sum(1 for r in rows if misc_by_article.get(r["id"]))
    total_misc = sum(len(v) for v in misc_by_article.values())

    lines = []
    lines.append("# Master Catalog: Student Errors, Misconceptions, and Perturbations in Mathematics Education\n")
    lines.append(f"**Corpus:** {total} articles across 12 MathEd journals, extracted from NotebookLM via structured per-article queries.\n")
    lines.append(f"**Papers substantively engaging errors/misconceptions/perturbations:** {with_misc} ({with_misc*100//total}%)\n")
    lines.append(f"**Total misconception entries:** {total_misc}\n")
    lines.append("\n**Orientation codes:** D = Deficit, P = Productive/Generative, M = Mixed/Transitional, SC = Sociocultural/Co-constructed, U = Uncoded\n")
    lines.append("**Salience codes:** [P] = primary focus of article, [S] = secondary thread, [·] = passing mention\n")
    lines.append("---\n")

    current_nb = None
    for r in rows:
        nb = nb_name.get(r["notebooklm_notebook_id"], "Unknown")
        if nb != current_nb:
            lines.append(f"\n## {nb}\n")
            current_nb = nb

        authors = (r["authors"] or "").strip()
        title = (r["title"] or "").strip()
        year = r["year"]
        header = f"### {authors} ({year}) — {title}"
        lines.append(header)

        bits = []
        if r["country"]: bits.append(f"**Country:** {r['country']}")
        if r["population_type"]: bits.append(f"**Population:** {r['population_type']}")
        if r["grade_band_as_declared"]: bits.append(f"**Grade:** {r['grade_band_as_declared']}")
        if r["sample_size"]: bits.append(f"**N:** {r['sample_size']}")
        if r["study_design"]: bits.append(f"**Design:** {r['study_design']}")
        if r["theoretical_framework"]: bits.append(f"**Framework:** {r['theoretical_framework']}")
        if r["has_figures"]: bits.append("figures")
        if r["has_student_work_figures"]: bits.append("student-work figures")
        if r["has_transcripts"]: bits.append("transcripts")
        if bits:
            lines.append(" · ".join(bits))

        if r["extraction_notes"]:
            lines.append(f"\n> {r['extraction_notes']}")

        ms = misc_by_article.get(r["id"], [])
        if not ms:
            lines.append("\n*No misconceptions extracted.*\n")
            continue

        for m in ms:
            orient = {"deficit": "D", "productive": "P", "mixed": "M",
                      "sociocultural": "SC", "uncoded": "U"}.get(m["orientation"] or "", "?")
            sal = {"primary": "[P]", "secondary": "[S]", "passing": "[·]"}.get(m["salience"] or "", "")
            head = f"- **{sal} [{orient}]**"
            if m["mathematical_domain"]:
                head += f" *{m['mathematical_domain']}"
                if m["mathematical_topic"]:
                    head += f" · {m['mathematical_topic']}"
                head += "*"
            lines.append(head)
            lines.append(f"  - **Misconception:** {m['error_description']}")
            if m["example"]:
                lines.append(f"  - *Example:* {m['example']}")
            if m["vocabulary_used_raw"]:
                lines.append(f"  - *Vocabulary:* {m['vocabulary_used_raw']}")
            if m["locus_of_attribution"]:
                lines.append(f"  - *Locus:* {m['locus_of_attribution']}")
            if m["antecedent"]:
                lines.append(f"  - *Antecedent:* {m['antecedent']}")
            if m["intervention"]:
                lines.append(f"  - *Intervention:* {m['intervention']}")
            if m["repair"]:
                lines.append(f"  - *Repair:* {m['repair']}")
            if m["page_refs"]:
                lines.append(f"  - *Pages:* {m['page_refs']}")
        lines.append("")

    with open(EXPORT_DIR / "catalog.md", "w") as fh:
        fh.write("\n".join(lines))
    print(f"catalog.md: {len(lines)} lines, {with_misc} papers with misconceptions, {total_misc} total misconception entries")

    conn.close()


if __name__ == "__main__":
    main()
