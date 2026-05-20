"""Manifest-driven Hermes run boundary.

This module is the first real-input gate. It reads a manifest and canonical
metadata events from `data/inputs`, writes canonical snapshots to
`data/derived`, and writes run results to `data/outputs`. It does not parse raw
transcripts or call REALLMS.
"""
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Any, Callable, Mapping

from .event_importer import assert_pair_graph_safe
from .path_contract import resolve_path_contract
from .persistent_prolog import PersistentPrologWorker


ROOT = Path(__file__).resolve().parent.parent
RUN_ID_PATTERN = re.compile(r"^[A-Za-z0-9_.-]+$")


def load_manifest(manifest_path: Path | str) -> dict[str, Any]:
    payload = json.loads(Path(manifest_path).read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError("run manifest must be a JSON object")
    return payload


def run_manifest(
    manifest_path: Path | str,
    *,
    app_root: Path | str = ROOT,
    env: Mapping[str, str] | None = None,
    worker_factory: Callable[[], Any] = PersistentPrologWorker,
) -> dict[str, Any]:
    app_root_path = Path(app_root)
    contract = resolve_path_contract(app_root_path, env=env)
    safe_manifest_path = _resolve_manifest_path(manifest_path, contract.input_root)
    manifest = load_manifest(safe_manifest_path)
    run_id = _run_id(manifest)
    events_path = _resolve_input_path(manifest.get("events_file"), contract.input_root)
    events = _load_events(events_path)

    worker = worker_factory()
    try:
        scores = worker.request("batch_event_score", events=events)
        pairs = worker.request("pair_score", events=events)
        graph = worker.request("pair_graph", events=events)
    finally:
        close = getattr(worker, "close", None)
        if callable(close):
            close()

    run_dir = Path("runs") / run_id
    derived_events_path = contract.derived_root / run_dir / "canonical_events.json"
    result_path = contract.output_root / run_dir / "result.json"
    summary_path = contract.output_root / run_dir / "summary.json"

    result = {
        "privacy": "canonical_metadata_only_no_student_work",
        "run_id": run_id,
        "description": manifest.get("description", ""),
        "manifest_path": str(safe_manifest_path),
        "events_path": str(events_path),
        "event_count": len(events),
        "events": events,
        "score_count": len(scores),
        "scores": scores,
        "pair_count": len(pairs),
        "pairs": pairs,
        "graph": graph,
        "graph_edge_count": len(graph.get("edges", [])) if isinstance(graph, dict) else 0,
    }
    assert_pair_graph_safe(result)

    _write_json(derived_events_path, events)
    _write_json(result_path, result)
    summary = _summary(result, derived_events_path, result_path, summary_path)
    _write_json(summary_path, summary)
    return summary


def _run_id(manifest: dict[str, Any]) -> str:
    run_id = str(manifest.get("run_id") or "").strip()
    if not run_id:
        raise ValueError("run manifest requires run_id")
    if not RUN_ID_PATTERN.fullmatch(run_id):
        raise ValueError("run_id may contain only letters, numbers, dots, dashes, and underscores")
    return run_id


def _resolve_input_path(value: Any, input_root: Path) -> Path:
    if not isinstance(value, str) or not value.strip():
        raise ValueError("run manifest requires events_file")
    candidate = Path(value)
    if not candidate.is_absolute():
        candidate = input_root / candidate
    resolved = candidate.resolve()
    if not _is_relative_to(resolved, input_root):
        raise ValueError("events_file must live under input root")
    if not resolved.exists():
        raise FileNotFoundError(f"events_file does not exist: {resolved}")
    return resolved


def _resolve_manifest_path(value: Path | str, input_root: Path) -> Path:
    candidate = Path(value)
    if not candidate.is_absolute():
        candidate = input_root / candidate
    resolved = candidate.resolve()
    if not _is_relative_to(resolved, input_root):
        raise ValueError("manifest must live under input root")
    if not resolved.exists():
        raise FileNotFoundError(f"manifest does not exist: {resolved}")
    return resolved


def _load_events(path: Path) -> list[dict[str, Any]]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, list):
        raise ValueError("events_file must contain a JSON list")
    if not all(isinstance(event, dict) for event in payload):
        raise ValueError("event rows must be JSON objects")
    assert_pair_graph_safe({"events": payload})
    return payload


def _summary(
    result: dict[str, Any],
    derived_events_path: Path,
    result_path: Path,
    summary_path: Path,
) -> dict[str, Any]:
    return {
        "privacy": result["privacy"],
        "run_id": result["run_id"],
        "event_count": result["event_count"],
        "score_count": result["score_count"],
        "pair_count": result["pair_count"],
        "graph_edge_count": result["graph_edge_count"],
        "derived_events_path": str(derived_events_path),
        "result_path": str(result_path),
        "summary_path": str(summary_path),
    }


def _write_json(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(payload, ensure_ascii=False, sort_keys=True, indent=2) + "\n",
        encoding="utf-8",
    )


def _is_relative_to(path: Path, parent: Path) -> bool:
    try:
        path.resolve().relative_to(parent.resolve())
    except ValueError:
        return False
    return True


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Run Hermes from a data/inputs manifest.")
    parser.add_argument("--manifest", required=True, help="manifest path under data/inputs")
    parser.add_argument("--pretty", action="store_true", help="pretty-print summary JSON")
    args = parser.parse_args(argv)

    try:
        summary = run_manifest(args.manifest)
    except Exception as exc:  # pragma: no cover - CLI error reporting
        print(f"manifest run failed: {exc}", file=sys.stderr)
        return 2

    print(json.dumps(summary, ensure_ascii=False, sort_keys=True, indent=2 if args.pretty else None))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
