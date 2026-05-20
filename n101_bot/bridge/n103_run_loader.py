"""Load research-safe N103 run outputs without raw student work."""
from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any


FORBIDDEN_KEYS = {
    "raw_text",
    "text",
    "body",
    "message",
    "student",
    "student_id",
    "student_name",
    "pseudonym_map",
    "real_name",
    "email",
    "netid",
    "sis_id",
    "author_student_id",
    "author_raw_name",
    "source_id",
    "evidence",
    "raw_input",
}


def load_safe_runs(root: Path | str) -> dict[str, Any]:
    source = Path(root)
    runs = []
    for path in sorted(source.rglob("pairings_safe.json")):
        payload = json.loads(path.read_text(encoding="utf-8"))
        safe_pairings = payload.get("research_safe_pairings", [])
        _assert_safe_metadata(safe_pairings, path=str(path))
        coverage = payload.get("coverage", {})
        _assert_safe_metadata(coverage, path=str(path))
        needs_check = payload.get("needs_instructor_check", [])
        _assert_safe_metadata(needs_check, path=str(path))
        run_dir = path.parent
        runs.append(
            {
                "run_id": run_dir.name,
                "path": str(run_dir),
                "pairing_count": len(safe_pairings) if isinstance(safe_pairings, list) else 0,
                "research_safe_pairings": safe_pairings if isinstance(safe_pairings, list) else [],
                "coverage": coverage if isinstance(coverage, dict) else {},
                "needs_instructor_check_count": len(needs_check) if isinstance(needs_check, list) else 0,
            }
        )
    return {
        "source": str(source),
        "run_count": len(runs),
        "runs": runs,
    }


def _assert_safe_metadata(value: Any, *, path: str, pointer: str = "$") -> None:
    if isinstance(value, dict):
        for key, child in value.items():
            key_text = str(key)
            if key_text in FORBIDDEN_KEYS:
                raise ValueError(f"unsafe field {key_text!r} at {path}:{pointer}.{key_text}")
            _assert_safe_metadata(child, path=path, pointer=f"{pointer}.{key_text}")
        return
    if isinstance(value, list):
        for index, child in enumerate(value):
            _assert_safe_metadata(child, path=path, pointer=f"{pointer}[{index}]")
        return
    if isinstance(value, (str, int, float, bool)) or value is None:
        return
    raise ValueError(f"unsupported value at {path}:{pointer}: {type(value).__name__}")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Summarize N103 pairings_safe.json outputs without raw student work."
    )
    parser.add_argument("root", help="Directory containing N103 run output folders")
    args = parser.parse_args(argv)
    print(json.dumps(load_safe_runs(args.root), indent=2, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
