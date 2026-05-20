#!/usr/bin/env python3
"""Synchronize tracked Hermes package files to a flash-drive package root."""
from __future__ import annotations

import argparse
import fnmatch
import json
import shutil
import subprocess
from pathlib import Path
from typing import Iterable


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_MANIFEST = Path(__file__).with_name("flash_sync_manifest.json")


def load_manifest(path: Path = DEFAULT_MANIFEST) -> dict:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if payload.get("version") != 1:
        raise ValueError(f"unsupported flash sync manifest version: {payload.get('version')}")
    for key in (
        "include_roots",
        "exclude_patterns",
        "include_overrides",
        "required_files",
        "ensure_dirs",
        "never_create_dirs",
    ):
        if not isinstance(payload.get(key), list):
            raise ValueError(f"manifest key must be a list: {key}")
    return payload


def plan_sync_files(source_root: Path | str, manifest: dict) -> list[str]:
    source = Path(source_root)
    candidates = _candidate_files(source, manifest["include_roots"])
    planned: set[str] = set()

    for rel_path in candidates:
        if _included(rel_path, manifest):
            planned.add(rel_path)

    for rel_path in manifest["required_files"]:
        _validate_relative_path(rel_path)
        if not (source / rel_path).is_file():
            raise FileNotFoundError(f"required flash sync source is missing: {rel_path}")
        planned.add(rel_path)

    return sorted(planned)


def sync_flash_package(
    source_root: Path | str,
    dest_root: Path | str,
    manifest: dict,
    *,
    dry_run: bool = False,
) -> dict[str, list[str]]:
    source = Path(source_root).resolve()
    dest = Path(dest_root).resolve()
    if source == dest:
        raise ValueError("source and destination roots must differ")
    if _is_relative_to(dest, source):
        raise ValueError("destination must not live inside source root")

    files = plan_sync_files(source, manifest)
    ensured_dirs = _safe_dirs(manifest["ensure_dirs"])
    never_create = set(_safe_dirs(manifest["never_create_dirs"]))
    overlap = sorted(set(ensured_dirs).intersection(never_create))
    if overlap:
        raise ValueError(f"ensure_dirs must not include never_create_dirs: {overlap}")

    if not dry_run:
        for rel_dir in ensured_dirs:
            (dest / rel_dir).mkdir(parents=True, exist_ok=True)
        for rel_path in files:
            src = source / rel_path
            target = dest / rel_path
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src, target)

    return {
        "copied": files,
        "ensured_dirs": ensured_dirs,
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=ROOT)
    parser.add_argument("--dest", type=Path, required=True)
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)

    manifest = load_manifest(args.manifest)
    result = sync_flash_package(
        args.source,
        args.dest,
        manifest,
        dry_run=args.dry_run,
    )
    action = "would copy" if args.dry_run else "copied"
    print(f"{action} {len(result['copied'])} files")
    print(f"ensured {len(result['ensured_dirs'])} writable directories")
    return 0


def _candidate_files(source: Path, include_roots: Iterable[str]) -> set[str]:
    git_files = _git_ls_files(source)
    if git_files is not None:
        return {
            path
            for path in git_files
            if any(_path_under_root(path, include_root) for include_root in include_roots)
        }

    candidates: set[str] = set()
    for include_root in include_roots:
        _validate_relative_path(include_root)
        root = source / include_root
        if root.is_file():
            candidates.add(include_root)
        elif root.is_dir():
            for path in root.rglob("*"):
                if path.is_file():
                    candidates.add(path.relative_to(source).as_posix())
    return candidates


def _git_ls_files(source: Path) -> set[str] | None:
    root_result = subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        cwd=source,
        check=False,
        text=True,
        capture_output=True,
    )
    if root_result.returncode != 0:
        return None
    if Path(root_result.stdout.strip()).resolve() != source.resolve():
        return None

    result = subprocess.run(
        ["git", "ls-files"],
        cwd=source,
        check=False,
        text=True,
        capture_output=True,
    )
    if result.returncode != 0:
        return None
    return {line.strip() for line in result.stdout.splitlines() if line.strip()}


def _included(rel_path: str, manifest: dict) -> bool:
    _validate_relative_path(rel_path)
    if rel_path in manifest["include_overrides"]:
        return True
    return not any(fnmatch.fnmatch(rel_path, pattern) for pattern in manifest["exclude_patterns"])


def _path_under_root(rel_path: str, include_root: str) -> bool:
    if rel_path == include_root:
        return True
    return rel_path.startswith(f"{include_root.rstrip('/')}/")


def _safe_dirs(paths: Iterable[str]) -> list[str]:
    return [_validate_relative_path(path) for path in paths]


def _validate_relative_path(path: str) -> str:
    rel = Path(path)
    if rel.is_absolute() or ".." in rel.parts:
        raise ValueError(f"path must be relative and stay inside package root: {path}")
    return rel.as_posix()


def _is_relative_to(path: Path, parent: Path) -> bool:
    try:
        path.relative_to(parent)
    except ValueError:
        return False
    return True


if __name__ == "__main__":
    raise SystemExit(main())
