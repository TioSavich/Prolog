#!/usr/bin/env python3
"""Check the Hermes source/asset boundary before staging n101_bot."""
from __future__ import annotations

import fnmatch
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MANIFEST = ROOT / "n101_bot" / "PACKAGING_MANIFEST.md"

REQUIRED_TRACKABLE = [
    "n101_bot/PACKAGING_MANIFEST.md",
    "n101_bot/README.md",
    "n101_bot/requirements.txt",
    "n101_bot/bridge/hc_bot.py",
    "n101_bot/bridge/hermes_console_server.py",
    "n101_bot/bridge/runtime_env.py",
    "n101_bot/bridge/path_contract.py",
    "n101_bot/bridge/event_importer.py",
    "n101_bot/bridge/reallms_smoke.py",
    "n101_bot/src/vocabulary.pl",
    "n101_bot/src/hermes_worker.pl",
    "n101_bot/vocabularies/n101/quantity.pl",
    "n101_bot/web/hermes_gemma_console.html",
    "n101_bot/runtime/README.md",
    "n101_bot/reallms/USAGE.md",
    "n101_bot/scripts/console.sh",
    "n101_bot/scripts/reallms_smoke.sh",
    "n101_bot/tests/test_runtime_env.py",
    "n101_bot/tests/test_path_contract.py",
    "n101_bot/tests/test_event_importer.py",
    "n101_bot/tests/test_runtime_scripts.py",
    "n101_bot/tests/test_reallms_smoke.py",
    "n101_bot/tests/test_reallms_smoke_script.py",
    "packaging/flash_sync_manifest.json",
    "packaging/sync_flash_package.py",
    "packaging/tests/test_flash_sync_package.py",
]

REQUIRED_IGNORED = [
    "n101_bot/.venv/bin/python",
    "n101_bot/models/gemma-3-4b-it/model-00001-of-00002.safetensors",
    "n101_bot/llama32-1b.vindex/down_weights.bin",
    "n101_bot/logs/results_20260416_210650.csv",
    "n101_bot/.pytest_cache/CACHEDIR.TAG",
    "n101_bot/bridge/__pycache__/hc_bot.cpython-312.pyc",
    "n101_bot/.DS_Store",
    "data/inputs/raw_transcript.txt",
    "runtime/cache/CACHEDIR.TAG",
    "N101_Student_Packet.pdf",
]

FORBIDDEN_TRACK_PATTERNS = [
    "n101_bot/.venv/**",
    "n101_bot/models/**",
    "n101_bot/llama32-1b.vindex/**",
    "n101_bot/logs/**",
    "n101_bot/**/__pycache__/**",
    "n101_bot/.pytest_cache/**",
    "n101_bot/**/*.pyc",
    "n101_bot/.DS_Store",
    "n101_bot/**/.DS_Store",
    "data/**",
    "runtime/**",
    "N101_Student_Packet.pdf",
    "N101coursenotes_f24.*",
]


def inside_git_worktree(root: Path = ROOT) -> bool:
    result = subprocess.run(
        ["git", "rev-parse", "--is-inside-work-tree"],
        cwd=root,
        check=False,
        text=True,
        capture_output=True,
    )
    return result.returncode == 0 and result.stdout.strip() == "true"


def forbidden_by_local_policy(path: str) -> bool:
    return any(fnmatch.fnmatch(path, pattern) for pattern in FORBIDDEN_TRACK_PATTERNS)


def git_check_ignore(path: str, *, root: Path = ROOT) -> bool:
    if not inside_git_worktree(root):
        return forbidden_by_local_policy(path)
    result = subprocess.run(
        ["git", "check-ignore", "-q", path],
        cwd=root,
        check=False,
    )
    if result.returncode == 0:
        return True
    if result.returncode == 1:
        return False
    raise RuntimeError(f"git check-ignore failed for {path}")


def manifest_text() -> str:
    if not MANIFEST.exists():
        raise AssertionError(f"missing manifest: {MANIFEST.relative_to(ROOT)}")
    return MANIFEST.read_text(encoding="utf-8")


def tracked_files(*, root: Path = ROOT) -> list[str]:
    if not inside_git_worktree(root):
        return []
    result = subprocess.run(
        ["git", "ls-files"],
        cwd=root,
        check=True,
        text=True,
        capture_output=True,
    )
    return [line.strip() for line in result.stdout.splitlines() if line.strip()]


def main() -> int:
    errors: list[str] = []
    text = ""
    try:
        text = manifest_text()
    except AssertionError as exc:
        errors.append(str(exc))

    for path in REQUIRED_TRACKABLE:
        if not (ROOT / path).exists():
            errors.append(f"required source is missing: {path}")
        if git_check_ignore(path):
            errors.append(f"required source is still ignored: {path}")
        if text and path not in text:
            errors.append(f"manifest does not list required source: {path}")

    for path in REQUIRED_IGNORED:
        if not git_check_ignore(path):
            errors.append(f"unsafe/local asset is not ignored: {path}")
        if text and path in text:
            errors.append(f"manifest explicitly lists unsafe/local asset: {path}")

    for tracked in tracked_files():
        for pattern in FORBIDDEN_TRACK_PATTERNS:
            if fnmatch.fnmatch(tracked, pattern):
                errors.append(f"forbidden asset is tracked: {tracked}")

    if errors:
        for error in errors:
            print(f"ERROR: {error}", file=sys.stderr)
        return 1

    print("Hermes packaging manifest and ignore rules look safe.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
