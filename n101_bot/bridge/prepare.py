"""Regenerate vocabulary artifacts by shelling out to SWI-Prolog.

Writes to logs/vocabulary.json, logs/vocabulary.lql, and logs/system_prompt.txt.
Prolog is the single source of truth. Python only reads these cached artifacts.
"""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

from .runtime_env import resolve_swipl

ROOT = Path(__file__).resolve().parent.parent
LOGS = ROOT / "logs"
SRC = ROOT / "src" / "vocabulary.pl"


def regenerate() -> None:
    LOGS.mkdir(exist_ok=True)
    json_path = LOGS / "vocabulary.json"
    lql_path = LOGS / "vocabulary.lql"
    prompt_path = LOGS / "system_prompt.txt"

    goal = (
        f"use_module('{SRC}'), "
        f"export_json('{json_path}'), "
        f"emit_lql('{lql_path}'), "
        f"system_prompt(P), "
        f"open('{prompt_path}', write, S), write(S, P), close(S), "
        f"halt."
    )
    proc = subprocess.run(
        [resolve_swipl(root=ROOT), "-q", "-g", goal, "-g", "halt(1)"],
        capture_output=True,
        text=True,
        cwd=ROOT,
    )
    if proc.returncode != 0:
        print(proc.stdout, file=sys.stderr)
        print(proc.stderr, file=sys.stderr)
        raise SystemExit(f"swipl regenerate failed (rc={proc.returncode})")

    for p in (json_path, lql_path, prompt_path):
        if not p.exists() or p.stat().st_size == 0:
            raise SystemExit(f"artifact missing or empty: {p}")

    print(f"regenerated: {json_path.name}, {lql_path.name}, {prompt_path.name}")


if __name__ == "__main__":
    regenerate()
