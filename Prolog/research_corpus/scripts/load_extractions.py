#!/usr/bin/env python3
"""Load extracted JSONs into research.db.

Idempotent: re-running updates existing articles and replaces their misconception rows.
Run from project root: python3 load_extractions.py
"""
import json
import sqlite3
import sys
from pathlib import Path
from datetime import datetime

PROJECT_ROOT = Path(__file__).parent
EXTRACTION_DIR = PROJECT_ROOT / "extraction_results"
DB_PATH = PROJECT_ROOT / "research.db"

NOTEBOOK_IDS = {
    "JRME":        "756fcf49-2a4b-4aeb-ab1c-0271120fa71f",
    "ZDM":         "3a6f6f0e-fb60-48a1-87fb-8d8266787904",
    "JMB":         "eb2f671d-b169-492b-823a-18bba108cd0e",
    "ESM_A-K":     "a2acbde0-237d-4e83-9fbe-2dbaf778119d",
    "ESM_L-Z":     "f95f32e6-2950-498a-a7a7-c563f9d808b7",
    "MERJ":        "1dbd32d5-0913-431b-bc4d-b617af0b99e5",
    "IJMEST_A-L":  "5cc5a6f6-1641-45c7-a74b-2235177f418f",
    "IJMEST_M-Z":  "0dd73888-fe91-4514-b3df-595f86971db5",
    "JMTE":        "993c37ef-6d70-4939-9346-7fcd9ba924ad",
    "IJSME":       "de8d7a68-f489-4417-a1e7-40a713aed946",
    "RME":         "bc1c03cc-9a1a-4691-bcc0-78ead81efd07",
    "MTL":         "9f753496-36a7-427d-ab76-eaf90b2a7492",
}

# Pilot files (filename → source_id, notebook_dir) mapping
PILOT_MAP = {
    "01_olive_steffe_2002.json":        ("934a9ce8-0a36-4774-8c5b-3866b55657d3", "JMB"),
    "02_steffe_2002.json":              ("ba9a3662-5a66-4296-af80-f7dab76c3e88", "JMB"),
    "03_steffe_2004.json":              ("d0f12518-ffad-4c1f-a002-641865fa5ce0", "MTL"),
    "04_hackenberg_2007.json":          ("1bc5c26c-e9d9-4f9f-957d-5ddb4e18088a", "JMB"),
    "05_hackenberg_tillema_2009.json":  ("6ce78aad-0291-4859-8661-e55f23f26cba", "JMB"),
    "06_izsak_tillema_tunc-pekkan_2008.json": ("b8a00ff0-0bd1-4434-918a-3b46170da31c", "JRME"),
    "07_hackenberg_2010.json":          ("5ea8cccd-bea4-40ac-9c2c-ae473f4c4148", "JRME"),
    "08_tillema_2013.json":             ("6db71adf-d350-499b-86a7-bad5120143eb", "JMB"),
    "09_izsak_jacobson_2017.json":      ("97d56a0d-4663-4efd-baea-ad4eb7d495f3", "JRME"),
    "10_jacobson_lobato_orrill_2018.json": ("c65855c6-1613-4843-b4e6-9f835aa98fb4", "IJSME"),
}


def normalize_bool(v):
    """Handle JSON booleans and the occasional string 'true'/'false' from NotebookLM drift."""
    if v is None:
        return None
    if isinstance(v, bool):
        return 1 if v else 0
    if isinstance(v, str):
        s = v.strip().lower()
        if s in ("true", "yes"):
            return 1
        if s in ("false", "no"):
            return 0
        if s in ("not stated", "n/a", "unknown", ""):
            return None
    return None


def normalize_str(v):
    """Drop sentinel 'not stated' strings that leaked through."""
    if v is None:
        return None
    if isinstance(v, str):
        s = v.strip()
        if s.lower() in ("not stated", "n/a", "unknown", "null", ""):
            return None
        return s
    return str(v)


def normalize_int(v):
    if v is None:
        return None
    if isinstance(v, int):
        return v
    if isinstance(v, str):
        try:
            return int(v.strip())
        except (ValueError, AttributeError):
            return None
    return None


def normalize_enum(v, allowed):
    """Map to allowed set or None. Case-insensitive, strip."""
    if v is None:
        return None
    s = str(v).strip().lower().replace("-", "_").replace(" ", "_")
    for a in allowed:
        if s == a.lower():
            return a
    return None


def parse_article_record(data, source_id, notebook_id, source_file):
    """Extract (article_dict, misconceptions_list) from an extraction JSON."""
    article = data.get("article", {}) or {}
    misconceptions = data.get("misconceptions", []) or []

    a = {
        "title":                normalize_str(article.get("title_as_given")) or f"[untitled:{source_id[:8]}]",
        "authors":              normalize_str(article.get("authors_as_given")) or "UNKNOWN",
        "year":                 normalize_int(article.get("year")),
        "journal":              normalize_str(article.get("journal")),
        "doi":                  normalize_str(article.get("doi")),
        "notebooklm_source_id": source_id,
        "notebooklm_notebook_id": notebook_id,
        "country":              normalize_str(article.get("country")),
        "language":             normalize_str(article.get("language")),
        "curriculum_context":   normalize_str(article.get("curriculum_context")),
        "population_type":      normalize_enum(article.get("population_type"),
                                ["student","PST","in_service","mixed","other"]),
        "grade_band_as_declared": normalize_str(article.get("grade_band_as_declared")),
        "sample_size":          normalize_str(article.get("sample_size")),
        "study_design":         normalize_enum(article.get("study_design"),
                                ["clinical_interview","teaching_experiment","design_experiment",
                                 "case_study","survey","video_analysis","textbook_analysis",
                                 "mixed","other","not_applicable"]),
        "theoretical_framework": normalize_str(article.get("theoretical_framework")),
        "has_figures":           normalize_bool(article.get("has_figures")),
        "has_student_work_figures": normalize_bool(article.get("has_student_work_figures")),
        "has_transcripts":       normalize_bool(article.get("has_transcripts")),
        "figure_page_refs":      normalize_str(article.get("figure_page_refs")),
        "transcript_page_refs":  normalize_str(article.get("transcript_page_refs")),
        "incompatibility_foregrounded": normalize_bool(article.get("incompatibility_foregrounded")),
        "extraction_notes":     normalize_str(article.get("notes")),
        "extraction_date":      datetime.now().isoformat(),
        "extraction_source":    f"notebooklm_per_article_query ({source_file})",
    }

    ms = []
    for m in misconceptions:
        if not isinstance(m, dict):
            continue
        ms.append({
            "error_description":    normalize_str(m.get("misconception_description")) or "",
            "example":              normalize_str(m.get("example")),
            "mathematical_domain":  normalize_str(m.get("mathematical_domain")),
            "mathematical_topic":   normalize_str(m.get("mathematical_topic")),
            "mathematical_subtopic": normalize_str(m.get("mathematical_subtopic")),
            "vocabulary_used_raw":  normalize_str(m.get("vocabulary_used_raw")),
            "orientation":          normalize_enum(m.get("orientation"),
                                    ["deficit","productive","mixed","sociocultural","uncoded"]),
            "locus_of_attribution": normalize_str(m.get("locus_of_attribution")),
            "antecedent":           normalize_str(m.get("antecedent")),
            "intervention":         normalize_str(m.get("intervention")),
            "repair":               normalize_str(m.get("repair")),
            "salience":             normalize_enum(m.get("salience"),
                                    ["primary","secondary","passing"]),
            "page_refs":            normalize_str(m.get("page_refs")),
        })
    return a, ms


def upsert_article(conn, a):
    """UPSERT by notebooklm_source_id; return article.id."""
    c = conn.cursor()
    c.execute("SELECT id FROM articles WHERE notebooklm_source_id = ?",
              (a["notebooklm_source_id"],))
    row = c.fetchone()
    if row:
        article_id = row[0]
        c.execute("""
            UPDATE articles SET
                title=?, authors=?, year=?, journal=?, doi=?,
                notebooklm_notebook_id=?,
                country=?, language=?, curriculum_context=?,
                population_type=?, grade_band_as_declared=?, sample_size=?,
                study_design=?, theoretical_framework=?,
                has_figures=?, has_student_work_figures=?, has_transcripts=?,
                figure_page_refs=?, transcript_page_refs=?,
                incompatibility_foregrounded=?,
                extraction_notes=?, extraction_date=?, extraction_source=?,
                updated_at=CURRENT_TIMESTAMP
            WHERE id=?
        """, (a["title"], a["authors"], a["year"], a["journal"], a["doi"],
              a["notebooklm_notebook_id"],
              a["country"], a["language"], a["curriculum_context"],
              a["population_type"], a["grade_band_as_declared"], a["sample_size"],
              a["study_design"], a["theoretical_framework"],
              a["has_figures"], a["has_student_work_figures"], a["has_transcripts"],
              a["figure_page_refs"], a["transcript_page_refs"],
              a["incompatibility_foregrounded"],
              a["extraction_notes"], a["extraction_date"], a["extraction_source"],
              article_id))
    else:
        c.execute("""
            INSERT INTO articles
            (title, authors, year, journal, doi,
             notebooklm_source_id, notebooklm_notebook_id,
             country, language, curriculum_context,
             population_type, grade_band_as_declared, sample_size,
             study_design, theoretical_framework,
             has_figures, has_student_work_figures, has_transcripts,
             figure_page_refs, transcript_page_refs,
             incompatibility_foregrounded,
             extraction_notes, extraction_date, extraction_source)
            VALUES (?,?,?,?,?, ?,?, ?,?,?, ?,?,?, ?,?, ?,?,?, ?,?, ?, ?,?,?)
        """, (a["title"], a["authors"], a["year"], a["journal"], a["doi"],
              a["notebooklm_source_id"], a["notebooklm_notebook_id"],
              a["country"], a["language"], a["curriculum_context"],
              a["population_type"], a["grade_band_as_declared"], a["sample_size"],
              a["study_design"], a["theoretical_framework"],
              a["has_figures"], a["has_student_work_figures"], a["has_transcripts"],
              a["figure_page_refs"], a["transcript_page_refs"],
              a["incompatibility_foregrounded"],
              a["extraction_notes"], a["extraction_date"], a["extraction_source"]))
        article_id = c.lastrowid
    return article_id


def replace_misconceptions(conn, article_id, ms):
    c = conn.cursor()
    # Drop only rows that came from extraction (leave manually-coded alone via extraction_source marker)
    c.execute("DELETE FROM error_instances WHERE article_id=?", (article_id,))
    for m in ms:
        c.execute("""
            INSERT INTO error_instances
            (article_id, error_description, example,
             mathematical_domain, mathematical_topic, mathematical_subtopic,
             orientation, notes,
             antecedent, intervention, repair, salience,
             locus_of_attribution, vocabulary_used_raw, page_refs)
            VALUES (?,?,?, ?,?,?, ?,?, ?,?,?,?, ?,?,?)
        """, (article_id, m["error_description"], m["example"],
              m["mathematical_domain"], m["mathematical_topic"], m["mathematical_subtopic"],
              m["orientation"], None,
              m["antecedent"], m["intervention"], m["repair"], m["salience"],
              m["locus_of_attribution"], m["vocabulary_used_raw"], m["page_refs"]))


def main():
    conn = sqlite3.connect(DB_PATH)
    conn.execute("PRAGMA foreign_keys = ON")

    n_articles = 0
    n_misc = 0
    n_parse_errors = 0
    n_empty = 0

    # Pilot files (flat in extraction_results/)
    for fname, (source_id, notebook_short) in PILOT_MAP.items():
        fpath = EXTRACTION_DIR / fname
        if not fpath.exists():
            print(f"SKIP (missing): {fname}", file=sys.stderr)
            continue
        try:
            data = json.loads(fpath.read_text())
        except json.JSONDecodeError as e:
            print(f"PARSE ERROR in {fname}: {e}", file=sys.stderr)
            n_parse_errors += 1
            continue
        notebook_id = NOTEBOOK_IDS[notebook_short]
        a, ms = parse_article_record(data, source_id, notebook_id, fname)
        article_id = upsert_article(conn, a)
        replace_misconceptions(conn, article_id, ms)
        n_articles += 1
        n_misc += len(ms)
        if not ms:
            n_empty += 1

    # Notebook subdirs
    for notebook_short, notebook_id in NOTEBOOK_IDS.items():
        subdir = EXTRACTION_DIR / notebook_short
        if not subdir.exists():
            continue
        for fpath in sorted(subdir.glob("*.json")):
            source_id = fpath.stem  # filename is <source_id>.json
            try:
                data = json.loads(fpath.read_text())
            except json.JSONDecodeError as e:
                print(f"PARSE ERROR in {subdir.name}/{fpath.name}: {e}", file=sys.stderr)
                n_parse_errors += 1
                continue
            a, ms = parse_article_record(data, source_id, notebook_id,
                                         f"{subdir.name}/{fpath.name}")
            article_id = upsert_article(conn, a)
            replace_misconceptions(conn, article_id, ms)
            n_articles += 1
            n_misc += len(ms)
            if not ms:
                n_empty += 1

    conn.commit()
    conn.close()

    print(f"Articles upserted:    {n_articles}")
    print(f"Misconception rows:   {n_misc}")
    print(f"Empty-misconception:  {n_empty}")
    print(f"Parse errors (skipped): {n_parse_errors}")


if __name__ == "__main__":
    main()
