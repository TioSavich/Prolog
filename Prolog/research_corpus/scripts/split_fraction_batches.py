#!/usr/bin/env python3
"""
Split the fraction-domain rows of misconceptions/corpus_inscope.csv into 7 equal-size batches.

Output:
  misconceptions/fraction_batch_1.csv
  misconceptions/fraction_batch_2.csv
  ...
  misconceptions/fraction_batch_7.csv

Each batch has the same header as corpus_inscope.csv; rows are distributed
round-robin so each batch has representative coverage of topics (instead of
all batch 1 rows coming from the earliest articles in the corpus).

Run from repo root:
  python3 research_corpus/scripts/split_fraction_batches.py
"""

import csv
from pathlib import Path

INPUT = Path("misconceptions/corpus_inscope.csv")
N_BATCHES = 7

def main():
    with INPUT.open(newline='') as f:
        reader = csv.DictReader(f)
        fieldnames = reader.fieldnames
        rows = [r for r in reader if r['mathematical_domain'] == 'fraction']

    print(f"Total fraction rows: {len(rows)}")
    batches = [[] for _ in range(N_BATCHES)]
    # Round-robin distribution so each batch is topically representative
    for i, row in enumerate(rows):
        batches[i % N_BATCHES].append(row)

    for i, batch in enumerate(batches, start=1):
        out = Path(f"misconceptions/fraction_batch_{i}.csv")
        with out.open('w', newline='') as f:
            w = csv.DictWriter(f, fieldnames=fieldnames)
            w.writeheader()
            w.writerows(batch)
        print(f"  batch {i}: {len(batch)} rows -> {out}")

if __name__ == '__main__':
    main()
