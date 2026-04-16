#!/usr/bin/env python3
"""Strip comments from Prolog sources for the outside-reader code audit.

See docs/phase5/code-audit-plan.md for context and intent. The stripped
files are written to a configurable staging directory outside the repo
working tree so no artifact risks being committed.

Usage:
    python scripts/strip_for_audit.py \
        --repo-root /Users/tio/Documents/GitHub/umedcta-formalization \
        --staging ~/sessions/brave-modest-bardeen/audit-staging/stripped \
        --module pml:pml/*.pl
"""
from __future__ import annotations

import argparse
import hashlib
import sys
from pathlib import Path


def strip_prolog(source: str) -> str:
    """Return `source` with Prolog comments removed via a quote-aware
    state machine.

    - `% ...\\n` line comments (full-line and trailing) are removed.
    - `/* ... */` block comments (including multi-line) are removed.
    - Single-quoted atoms `'...'` and double-quoted strings `"..."`
      pass through unchanged, including doubled-quote and backslash
      escapes.
    - Character-code literals `0'X` pass through even when X is `%`,
      `'`, `"`, or a backslash escape like `0'\\n`.
    - Trailing whitespace is stripped; runs of blank lines are
      collapsed to a single blank line.
    """
    out: list[str] = []
    i = 0
    n = len(source)
    state = "NORMAL"

    while i < n:
        c = source[i]
        nxt = source[i + 1] if i + 1 < n else ""

        if state == "NORMAL":
            if c == "0" and nxt == "'":
                out.append(c)
                out.append(nxt)
                i += 2
                if i < n and source[i] == "\\":
                    out.append(source[i])
                    i += 1
                    if i < n:
                        out.append(source[i])
                        i += 1
                elif i < n:
                    out.append(source[i])
                    i += 1
                continue
            if c == "%":
                state = "LCOMMENT"
                i += 1
                continue
            if c == "/" and nxt == "*":
                state = "BCOMMENT"
                i += 2
                continue
            if c == "'":
                state = "SQUOTE"
                out.append(c)
                i += 1
                continue
            if c == '"':
                state = "DQUOTE"
                out.append(c)
                i += 1
                continue
            out.append(c)
            i += 1
            continue

        if state == "SQUOTE":
            if c == "\\" and i + 1 < n:
                out.append(c)
                out.append(source[i + 1])
                i += 2
                continue
            if c == "'" and nxt == "'":
                out.append(c)
                out.append(nxt)
                i += 2
                continue
            if c == "'":
                out.append(c)
                state = "NORMAL"
                i += 1
                continue
            out.append(c)
            i += 1
            continue

        if state == "DQUOTE":
            if c == "\\" and i + 1 < n:
                out.append(c)
                out.append(source[i + 1])
                i += 2
                continue
            if c == '"' and nxt == '"':
                out.append(c)
                out.append(nxt)
                i += 2
                continue
            if c == '"':
                out.append(c)
                state = "NORMAL"
                i += 1
                continue
            out.append(c)
            i += 1
            continue

        if state == "LCOMMENT":
            if c == "\n":
                out.append("\n")
                state = "NORMAL"
            i += 1
            continue

        if state == "BCOMMENT":
            if c == "*" and nxt == "/":
                state = "NORMAL"
                i += 2
                continue
            i += 1
            continue

    return _collapse_blank_lines(_strip_trailing_ws("".join(out)))


def _strip_trailing_ws(s: str) -> str:
    return "\n".join(line.rstrip() for line in s.split("\n"))


def _collapse_blank_lines(s: str) -> str:
    lines = s.split("\n")
    out: list[str] = []
    blank_run = 0
    for line in lines:
        if line.strip() == "":
            blank_run += 1
            if blank_run <= 1:
                out.append("")
        else:
            blank_run = 0
            out.append(line)
    while out and out[0] == "":
        out.pop(0)
    while len(out) >= 2 and out[-1] == "" and out[-2] == "":
        out.pop()
    body = "\n".join(out)
    if body and not body.endswith("\n"):
        body += "\n"
    return body


def _sha256(path: Path) -> str:
    h = hashlib.sha256()
    h.update(path.read_bytes())
    return h.hexdigest()


def _write_stripped(
    source: Path, staging_root: Path, module_name: str, repo_root: Path
) -> dict:
    rel = source.relative_to(repo_root)
    out_path = staging_root / module_name / (source.name + ".txt")
    out_path.parent.mkdir(parents=True, exist_ok=True)
    original = source.read_text(encoding="utf-8")
    stripped = strip_prolog(original)
    sha = _sha256(source)
    header = (
        "% ============================================================\n"
        f"% Source: {rel}\n"
        f"% SHA256 (of original): {sha}\n"
        "% Stripped by scripts/strip_for_audit.py — comments removed.\n"
        "% ============================================================\n\n"
    )
    out_path.write_text(header + stripped, encoding="utf-8")
    return {
        "source": str(rel),
        "output": str(out_path.relative_to(staging_root)),
        "sha256": sha,
        "bytes_before": len(original.encode("utf-8")),
        "bytes_after": len(stripped.encode("utf-8")),
    }


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--repo-root", required=True, type=Path)
    ap.add_argument("--staging", required=True, type=Path)
    ap.add_argument(
        "--module",
        action="append",
        required=True,
        metavar="NAME:GLOB",
        help="module tier spec, e.g. pml:pml/*.pl (repeatable)",
    )
    args = ap.parse_args(argv)

    repo_root = args.repo_root.resolve()
    staging = args.staging.expanduser().resolve()
    staging.mkdir(parents=True, exist_ok=True)

    try:
        staging.relative_to(repo_root)
    except ValueError:
        pass
    else:
        print(
            f"note: staging is inside repo root ({staging.relative_to(repo_root)}); "
            "ensure the path is gitignored so artifacts don't risk being committed.",
            file=sys.stderr,
        )

    manifest_rows: list[dict] = []
    for spec in args.module:
        if ":" not in spec:
            print(f"bad --module spec: {spec}", file=sys.stderr)
            return 2
        name, pattern = spec.split(":", 1)
        matched = sorted(repo_root.glob(pattern))
        if not matched:
            print(f"no files matched: {pattern}", file=sys.stderr)
            continue
        for src in matched:
            if src.suffix != ".pl":
                continue
            row = _write_stripped(src, staging, name, repo_root)
            row["module"] = name
            manifest_rows.append(row)
            print(
                f"  {name:<8} {row['source']:<40} "
                f"{row['bytes_before']:>6} → {row['bytes_after']:>6} bytes"
            )

    manifest_path = staging / "MANIFEST.txt"
    with manifest_path.open("w", encoding="utf-8") as f:
        f.write("# MANIFEST — stripped Prolog sources for code audit\n")
        f.write("# See docs/phase5/code-audit-plan.md in the umedcta-formalization repo.\n")
        f.write(f"# Repo root: {repo_root}\n")
        f.write(f"# Staging:   {staging}\n\n")
        f.write(
            f"{'module':<10} {'bytes_before':>12} {'bytes_after':>12}  "
            f"{'source':<42}  sha256\n"
        )
        f.write("-" * 120 + "\n")
        for row in manifest_rows:
            f.write(
                f"{row['module']:<10} {row['bytes_before']:>12} {row['bytes_after']:>12}  "
                f"{row['source']:<42}  {row['sha256']}\n"
            )

    print(f"\nmanifest: {manifest_path}")
    print(f"files:    {len(manifest_rows)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
