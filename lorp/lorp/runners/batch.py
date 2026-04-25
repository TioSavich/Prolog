"""Batch runner.

Usage:
    python -m lorp.runners.batch --corpus path/to/dir [--glob '*.txt']

Walks the directory (non-recursively by default; pass --recursive to
recurse), processes each matching file via the same pipeline as
runners.single, and prints a final pass/reject tally.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from .single import process_passage


def _main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Run immanent-reader over a corpus directory.")
    parser.add_argument("--corpus", required=True, type=Path, help="Directory of passage files.")
    parser.add_argument("--glob", default="*.txt", help="Filename glob (default: *.txt).")
    parser.add_argument("--recursive", action="store_true", help="Recurse into subdirectories.")
    parser.add_argument("--config", default=None, type=Path, help="Path to config.yaml.")
    parser.add_argument(
        "--limit", default=None, type=int,
        help="Process at most N files (useful for smoke-testing a large corpus).",
    )
    args = parser.parse_args(argv)

    if not args.corpus.is_dir():
        print(f"corpus path is not a directory: {args.corpus}", file=sys.stderr)
        return 2

    pattern = args.glob
    files = sorted(args.corpus.rglob(pattern) if args.recursive else args.corpus.glob(pattern))
    if args.limit is not None:
        files = files[: args.limit]

    if not files:
        print(f"no files matched '{pattern}' under {args.corpus}", file=sys.stderr)
        return 2

    passes = 0
    rejects = 0
    for path in files:
        rc = process_passage(passage_path=path.resolve(), config_path=args.config)
        if rc == 0:
            passes += 1
        else:
            rejects += 1

    print(f"\n=== batch summary ===\n  passed:   {passes}\n  rejected: {rejects}")
    return 0 if rejects == 0 else 1


if __name__ == "__main__":
    sys.exit(_main(sys.argv[1:]))
