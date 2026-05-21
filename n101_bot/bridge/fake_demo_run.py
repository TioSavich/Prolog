"""Explicit synthetic Hermes run for source and flash-package smoke tests.

The demo fixture is already canonical metadata. It is not a transcript parser
and does not contain classroom text, source identifiers, paths, or actor IDs.
REALLMS revoicing is opt-in so normal verification never calls the network.
"""
from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path
from typing import Any, Callable

from .event_importer import assert_pair_graph_safe
from .persistent_prolog import PersistentPrologWorker
from .reallms_revoicer import RealLMSRevoicer


ROOT = Path(__file__).resolve().parent.parent
DEFAULT_EVENTS = ROOT / "samples" / "fake_demo_events.json"


def load_demo_events(path: Path | str = DEFAULT_EVENTS) -> list[dict[str, Any]]:
    events_path = Path(path)
    payload = json.loads(events_path.read_text(encoding="utf-8"))
    if not isinstance(payload, list):
        raise ValueError("fake demo events must be a JSON list")
    if not all(isinstance(event, dict) for event in payload):
        raise ValueError("fake demo event rows must be JSON objects")
    assert_pair_graph_safe({"events": payload})
    return payload


def run_fake_demo(
    events_path: Path | str = DEFAULT_EVENTS,
    *,
    worker_factory: Callable[[], Any] = PersistentPrologWorker,
    revoice: bool = False,
    revoicer_factory: Callable[..., Any] = RealLMSRevoicer,
    model: str | None = None,
) -> dict[str, Any]:
    events = load_demo_events(events_path)
    worker = worker_factory()
    try:
        scores = worker.request("batch_event_score", events=events)
        pairs = worker.request("pair_score", events=events)
        graph = worker.request("pair_graph", events=events)
    finally:
        close = getattr(worker, "close", None)
        if callable(close):
            close()

    result: dict[str, Any] = {
        "privacy": "synthetic_metadata_only_no_student_work",
        "event_count": len(events),
        "events": events,
        "score_count": len(scores),
        "scores": scores,
        "pair_count": len(pairs),
        "pairs": pairs,
        "graph": graph,
    }
    assert_pair_graph_safe(result)

    if revoice:
        result["revoice"] = run_pair_revoice(
            pairs=pairs,
            graph=graph,
            revoicer_factory=revoicer_factory,
            model=model,
        )
        assert_pair_graph_safe(result)

    return result


def run_pair_revoice(
    *,
    pairs: list[dict[str, Any]],
    graph: dict[str, Any],
    revoicer_factory: Callable[..., Any] = RealLMSRevoicer,
    model: str | None = None,
) -> dict[str, Any]:
    return _run_revoice(
        pairs=pairs,
        graph=graph,
        revoicer_factory=revoicer_factory,
        model=model,
    )


def _run_revoice(
    *,
    pairs: list[dict[str, Any]],
    graph: dict[str, Any],
    revoicer_factory: Callable[..., Any],
    model: str | None,
) -> dict[str, Any]:
    if os.environ.get("HERMES_FORCE_OFFLINE") == "1":
        raise RuntimeError("REALLMS revoice requested but HERMES_FORCE_OFFLINE=1")
    pair = _first_pair_with_question(pairs)
    question_move = pair["question_moves"][0]
    pair_context = _safe_pair_context(pair, graph)
    revoicer = revoicer_factory(model=model) if model else revoicer_factory()
    result = revoicer.revoice(question_move=question_move, pair_context=pair_context)
    as_dict = getattr(result, "as_dict", None)
    payload = as_dict() if callable(as_dict) else result
    if not isinstance(payload, dict):
        raise TypeError("revoicer returned a non-dict result")
    safe_payload = _safe_revoice_payload(payload)
    assert_pair_graph_safe(safe_payload)
    return safe_payload


def _safe_revoice_payload(payload: dict[str, Any]) -> dict[str, Any]:
    safe = {
        "provider": payload.get("provider"),
        "model": payload.get("model"),
        "content": payload.get("content"),
        "blocked": bool(payload.get("blocked", False)),
    }
    filter_result = payload.get("filter_result")
    if isinstance(filter_result, dict):
        safe["filter_result"] = {
            "blocked": bool(filter_result.get("blocked", False)),
            "hits": filter_result.get("hits", []),
        }
    return safe


def _first_pair_with_question(pairs: list[dict[str, Any]]) -> dict[str, Any]:
    for pair in pairs:
        moves = pair.get("question_moves")
        if isinstance(moves, list) and moves:
            return pair
    raise ValueError("fake demo produced no pair with question moves")


def _safe_pair_context(pair: dict[str, Any], graph: dict[str, Any]) -> dict[str, Any]:
    edge = _edge_for_pair(pair.get("pair_id"), graph)
    context = {
        "pair_id": pair.get("pair_id"),
        "event_a": pair.get("event_a"),
        "event_b": pair.get("event_b"),
        "pseudonym_a": pair.get("pseudonym_a"),
        "pseudonym_b": pair.get("pseudonym_b"),
        "roles": pair.get("roles", []),
        "score": pair.get("score"),
        "reasons": pair.get("reasons", []),
        "graph_edge": edge,
        "demo_policy": "synthetic metadata only; do not infer or quote student work",
    }
    assert_pair_graph_safe(context)
    return context


def _edge_for_pair(pair_id: Any, graph: dict[str, Any]) -> dict[str, Any] | None:
    for edge in graph.get("edges", []):
        if isinstance(edge, dict) and edge.get("id") == pair_id:
            return edge
    return None


def write_demo_output(payload: dict[str, Any], output: str | Path) -> Path:
    output_path = _resolve_output_path(output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(
        json.dumps(payload, ensure_ascii=False, sort_keys=True, indent=2) + "\n",
        encoding="utf-8",
    )
    return output_path


def _resolve_output_path(output: str | Path) -> Path:
    candidate = Path(output)
    if not candidate.is_absolute():
        output_root = Path(os.environ.get("HERMES_OUTPUT_ROOT") or ROOT.parent / "data" / "outputs")
        candidate = output_root / candidate
    resolved = candidate.resolve()
    root = ROOT.resolve()
    if resolved == root or root in resolved.parents:
        raise ValueError("fake demo output must not live inside app code root")
    return resolved


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Run a synthetic Hermes event demo.")
    parser.add_argument("--events", default=str(DEFAULT_EVENTS), help="metadata-only JSON event fixture")
    parser.add_argument("--revoice", action="store_true", help="opt in to one REALLMS revoicing call")
    parser.add_argument("--model", default=None, help="optional REALLMS model override")
    parser.add_argument("--output", default=None, help="write JSON to HERMES_OUTPUT_ROOT-relative path")
    parser.add_argument("--pretty", action="store_true", help="pretty-print JSON output")
    args = parser.parse_args(argv)

    try:
        result = run_fake_demo(args.events, revoice=args.revoice, model=args.model)
    except Exception as exc:  # pragma: no cover - CLI error reporting
        print(f"fake demo failed: {exc}", file=sys.stderr)
        return 2

    indent = 2 if args.pretty else None
    rendered = json.dumps(result, ensure_ascii=False, sort_keys=True, indent=indent)
    if args.output:
        output_path = write_demo_output(result, args.output)
        print(json.dumps({"ok": True, "output": str(output_path)}, sort_keys=True))
    else:
        print(rendered)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
