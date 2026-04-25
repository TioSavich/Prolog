#!/usr/bin/env python3
"""
Split whole_number and decimal rows of misconceptions/corpus_inscope.csv
into batches. Round-robin distribution so each batch gets representative coverage.

Output:
  misconceptions/whole_number_batch_1.csv ... whole_number_batch_5.csv  (~76-77 rows each)
  misconceptions/decimal_batch_1.csv ... decimal_batch_2.csv            (~94-95 rows each)

Run from repo root:
  python3 research_corpus/scripts/split_wn_decimal_batches.py
"""

import csv
from pathlib import Path

INPUT = Path("misconceptions/corpus_inscope.csv")

SPLITS = [
    ('whole_number', 5),
    ('decimal', 2),
]

def main():
    with INPUT.open(newline='') as f:
        reader = csv.DictReader(f)
        fieldnames = reader.fieldnames
        rows_by_domain = {}
        for r in reader:
            dom = r['mathematical_domain']
            rows_by_domain.setdefault(dom, []).append(r)

    for domain, n_batches in SPLITS:
        rows = rows_by_domain.get(domain, [])
        print(f"\n{domain}: {len(rows)} rows -> {n_batches} batches")
        batches = [[] for _ in range(n_batches)]
        for i, row in enumerate(rows):
            batches[i % n_batches].append(row)
        for i, batch in enumerate(batches, start=1):
            out = Path(f"misconceptions/{domain}_batch_{i}.csv")
            with out.open('w', newline='') as f:
                w = csv.DictWriter(f, fieldnames=fieldnames)
                w.writeheader()
                w.writerows(batch)
            print(f"  batch {i}: {len(batch)} rows -> {out}")

if __name__ == '__main__':
    main()
