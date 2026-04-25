#!/usr/bin/env python3
"""
Normalize topic casing and export in-scope rows to CSV files.

In-scope: fraction, decimal, whole_number, measurement, geometric domains
          AND (PST population OR K-5 grade band keywords)

Out-of-scope rows → misconceptions/out_of_frame.csv with reason tag.
In-scope rows     → misconceptions/corpus_inscope.csv (one per domain).

Run from repo root:
  python3 research_corpus/scripts/triage_export.py
"""

import sqlite3
import csv
import os
from collections import Counter

DB = "research_corpus/research.db"
OUT_INSCOPE = "misconceptions/corpus_inscope.csv"
OUT_OOF = "misconceptions/out_of_frame.csv"

IN_SCOPE_DOMAINS = {'fraction', 'decimal', 'whole_number', 'measurement', 'geometric'}

GRADE_KEYWORDS = [
    'elementary', 'primary', 'kindergarten',
    'first', 'second', 'third', 'fourth', 'fifth',
    'grade 1', 'grade 2', 'grade 3', 'grade 4', 'grade 5',
    'k-5', 'k-3', 'k-2', 'grade k',
    'grades 1', 'grades 2', 'grades 3', 'grades 4', 'grades 5',
]

def is_inscope_grade(grade_band, pop_type):
    if pop_type in ('student', 'PST'):
        return True
    if not grade_band:
        return False
    gb = grade_band.lower()
    return any(kw in gb for kw in GRADE_KEYWORDS)

def oof_reason(domain, grade_band, pop_type, error_desc):
    if domain not in IN_SCOPE_DOMAINS:
        return 'domain_out_of_scope'
    if not is_inscope_grade(grade_band, pop_type):
        return 'grade_out_of_scope'
    if not error_desc or len(error_desc.strip()) < 20:
        return 'too_vague'
    return None

def main():
    os.makedirs("misconceptions", exist_ok=True)
    conn = sqlite3.connect(DB)
    conn.row_factory = sqlite3.Row
    cur = conn.cursor()

    # Normalize topic casing (idempotent).
    cur.execute(
        "UPDATE error_instances SET mathematical_topic = lower(mathematical_topic) "
        "WHERE mathematical_topic != lower(mathematical_topic)"
    )
    conn.commit()
    print(f"Normalized {cur.rowcount} topic rows")

    # Fetch all rows with article metadata.
    cur.execute("""
        SELECT e.id, e.error_description, e.example, e.mathematical_domain,
               e.mathematical_topic, e.mathematical_subtopic,
               e.orientation, e.salience, e.page_refs,
               a.population_type, a.grade_band_as_declared,
               a.bibtex_key, a.authors, a.year, a.journal,
               a.local_pdf_path
        FROM error_instances e
        JOIN articles a ON e.article_id = a.id
    """)
    rows = cur.fetchall()
    conn.close()

    inscope_fields = ['id', 'error_description', 'example', 'mathematical_domain',
                      'mathematical_topic', 'mathematical_subtopic',
                      'population_type', 'grade_band_as_declared',
                      'bibtex_key', 'authors', 'year', 'journal',
                      'orientation', 'salience', 'page_refs', 'local_pdf_path']
    oof_fields = inscope_fields + ['oof_reason']

    inscope_rows = []
    oof_rows = []

    for r in rows:
        reason = oof_reason(r['mathematical_domain'], r['grade_band_as_declared'],
                            r['population_type'], r['error_description'])
        if reason:
            row = dict(r) | {'oof_reason': reason}
            oof_rows.append(row)
        else:
            inscope_rows.append(dict(r))

    with open(OUT_INSCOPE, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=inscope_fields, extrasaction='ignore')
        w.writeheader()
        w.writerows(inscope_rows)

    with open(OUT_OOF, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=oof_fields, extrasaction='ignore')
        w.writeheader()
        w.writerows(oof_rows)

    # Summary by domain
    domain_counts = Counter(r['mathematical_domain'] for r in inscope_rows)
    print(f"\nIn-scope: {len(inscope_rows)} rows → {OUT_INSCOPE}")
    for domain, n in sorted(domain_counts.items(), key=lambda x: -x[1]):
        print(f"  {domain}: {n}")
    print(f"\nOut-of-frame: {len(oof_rows)} rows → {OUT_OOF}")

    # Breakdown of out-of-frame reasons for visibility
    oof_counts = Counter(r['oof_reason'] for r in oof_rows)
    for reason, n in sorted(oof_counts.items(), key=lambda x: -x[1]):
        print(f"  {reason}: {n}")

if __name__ == '__main__':
    main()
