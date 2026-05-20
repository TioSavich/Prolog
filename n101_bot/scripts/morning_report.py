"""Compact morning report: summarize the most recent overnight run.

Usage:
    .venv/bin/python scripts/morning_report.py

Prints:
- totals: passed vs failed
- per-question pass rate if multiple iterations
- every violation seen, grouped by term and rule
- slowest/fastest response, token stats
- sample of the cleanest and the most-violating answers
"""
from __future__ import annotations

import csv
import glob
import json
import statistics
import sys
from collections import Counter, defaultdict
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
LOGS = ROOT / "logs"


def latest_run() -> tuple[Path, Path] | None:
    csvs = sorted(LOGS.glob("results_*.csv"))
    if not csvs:
        return None
    csv_path = csvs[-1]
    stamp = csv_path.stem.replace("results_", "")
    runs_dir = LOGS / f"runs_{stamp}"
    return csv_path, runs_dir


def main() -> int:
    latest = latest_run()
    if not latest:
        print("no overnight run found under logs/")
        return 1
    csv_path, runs_dir = latest
    print(f"run:   {csv_path.name}")
    print(f"runs:  {runs_dir.name}/")
    print()

    with open(csv_path, newline="") as f:
        rows = list(csv.DictReader(f))
    if not rows:
        print("csv is empty")
        return 1

    total = len(rows)
    passed = sum(1 for r in rows if r["passed"] == "true" and r["violation_count"] == "0")
    failed = total - passed

    print(f"total:  {total}")
    print(f"passed: {passed}")
    print(f"failed: {failed}")
    print()

    # per-question pass rate (useful when ITERATIONS > 1)
    q_buckets: dict[str, list[bool]] = defaultdict(list)
    for r in rows:
        key = f"q{r['qindex']}"
        ok = r["passed"] == "true" and r["violation_count"] == "0"
        q_buckets[key].append(ok)

    if max(len(v) for v in q_buckets.values()) > 1:
        print("per-question pass rate:")
        for q in sorted(q_buckets, key=lambda x: int(x[1:])):
            hits = q_buckets[q]
            rate = sum(hits) / len(hits)
            print(f"  {q:>5}  {sum(hits)}/{len(hits)}  ({rate:.0%})")
        print()

    # Collect all violations across all runs
    violation_counter: Counter = Counter()
    violation_examples: dict[tuple[str, str], dict] = {}
    durations: list[float] = []
    tokens: list[int] = []

    for json_file in sorted(runs_dir.glob("*.json")):
        try:
            d = json.loads(json_file.read_text())
        except json.JSONDecodeError:
            continue
        durations.append(float(d.get("duration_ms", 0)))
        tokens.append(int(d.get("eval_tokens", 0)))
        for v in d.get("violations", []):
            key = (v["term"], v["rule"])
            violation_counter[key] += 1
            if key not in violation_examples:
                violation_examples[key] = {
                    "question": d["question"],
                    "answer": d["answer"][:400],
                    "trigger": v["trigger"],
                }

    if violation_counter:
        print("violations seen (term :: rule  x count):")
        for (term, rule), count in violation_counter.most_common():
            print(f"  {term} :: {rule}  x {count}")
            ex = violation_examples[(term, rule)]
            print(f"    trigger: {ex['trigger']!r}")
            print(f"    in Q: {ex['question']}")
            print(f"    excerpt: {ex['answer'][:200]}...")
            print()
    else:
        print("no violations across entire run.")
        print()

    if durations:
        print("duration stats (ms):")
        print(f"  min:    {min(durations):.0f}")
        print(f"  median: {statistics.median(durations):.0f}")
        print(f"  max:    {max(durations):.0f}")
        print(f"  total:  {sum(durations)/1000:.1f} s")
        print()

    if tokens:
        print(f"tokens: total {sum(tokens)}, median/call {statistics.median(tokens):.0f}")

    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
