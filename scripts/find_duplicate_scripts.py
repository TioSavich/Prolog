#!/usr/bin/env python3
"""Scan the repository for duplicate .py/.pl scripts and optionally delete redundancies."""

from __future__ import annotations

import argparse
import hashlib
import os
from pathlib import Path
from typing import Dict, List


def compute_hash(path: Path, chunk_size: int = 8192) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(chunk_size), b""):
            digest.update(chunk)
    return digest.hexdigest()


def find_duplicates(root: Path, extensions: List[str]) -> Dict[str, List[Path]]:
    duplicates: Dict[str, List[Path]] = {}
    for path in root.rglob("*"):
        if not path.is_file():
            continue
        if path.suffix.lower() not in extensions:
            continue
        file_hash = compute_hash(path)
        duplicates.setdefault(file_hash, []).append(path)
    return {h: paths for h, paths in duplicates.items() if len(paths) > 1}


def delete_redundant(paths: List[Path], keep: Path) -> None:
    for path in paths:
        if path == keep:
            continue
        path.unlink()


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root",
        type=Path,
        default=Path("."),
        help="Repository root to scan (default: current directory).",
    )
    parser.add_argument(
        "--extensions",
        nargs="*",
        default=[".py", ".pl"],
        help="File extensions to consider (default: .py .pl).",
    )
    parser.add_argument(
        "--delete",
        action="store_true",
        help="Delete redundant duplicates, keeping the first path alphabetically.",
    )
    args = parser.parse_args()

    root = args.root.resolve()
    duplicates = find_duplicates(root, [ext.lower() for ext in args.extensions])

    if not duplicates:
        print("No duplicate scripts detected.")
        return

    for digest, paths in duplicates.items():
        print(f"Duplicate group (sha256={digest}):")
        for path in sorted(paths):
            print(f"  - {path.relative_to(root)}")
        if args.delete:
            keep_path = sorted(paths)[0]
            delete_redundant(paths, keep_path)
            print(f"    -> kept {keep_path.relative_to(root)}, deleted {len(paths) - 1} file(s)")


if __name__ == "__main__":
    main()
