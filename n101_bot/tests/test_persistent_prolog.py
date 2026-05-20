from __future__ import annotations

import json
from pathlib import Path

import pytest

from bridge.persistent_prolog import PersistentPrologWorker, resolve_swipl


GOLDEN_EVENTS = Path("/Users/tio/Desktop/Best_Day/PML/golden_runtime_events.jsonl")
UMEDCTA_ROOT = Path("/Users/tio/Documents/GitHub/umedcta-formalization")


def _golden_events() -> list[dict]:
    return [
        json.loads(line)
        for line in GOLDEN_EVENTS.read_text(encoding="utf-8").splitlines()
        if line.strip()
    ]


def test_resolve_swipl_prefers_explicit_argument():
    assert resolve_swipl("custom-swipl") == "custom-swipl"


def test_resolve_swipl_prefers_environment_override(monkeypatch):
    monkeypatch.setenv("HERMES_SWIPL", "/Volumes/Hermes/bin/swipl")
    assert resolve_swipl(None) == "/Volumes/Hermes/bin/swipl"


def test_resolve_swipl_prefers_bundled_runtime(tmp_path, monkeypatch):
    fake_root = tmp_path / "n101_bot"
    bundled = fake_root / "runtime" / "swi-prolog" / "bin" / "swipl"
    bundled.parent.mkdir(parents=True)
    bundled.write_text("#!/bin/sh\n", encoding="utf-8")
    monkeypatch.delenv("HERMES_SWIPL", raising=False)

    assert resolve_swipl(None, root=fake_root) == str(bundled)


def test_resolve_swipl_falls_back_to_system_name(monkeypatch, tmp_path):
    monkeypatch.delenv("HERMES_SWIPL", raising=False)
    assert resolve_swipl(None, root=tmp_path) == "swipl"


def test_worker_defaults_to_runtime_umedcta_root(monkeypatch, tmp_path):
    expected = tmp_path / "Hermes" / "umedcta-formalization"
    app_root = tmp_path / "Hermes" / "n101_bot"
    expected.mkdir(parents=True)
    monkeypatch.delenv("UMEDCTA_ROOT", raising=False)
    monkeypatch.setattr("bridge.persistent_prolog.ROOT", app_root)

    worker = PersistentPrologWorker()

    assert worker.umedcta_root == expected


def test_worker_honors_launcher_umedcta_root(monkeypatch, tmp_path):
    expected = tmp_path / "portable-formalization"
    monkeypatch.setenv("UMEDCTA_ROOT", str(expected))

    worker = PersistentPrologWorker()

    assert worker.umedcta_root == expected


@pytest.fixture
def worker():
    proc = PersistentPrologWorker(umedcta_root=UMEDCTA_ROOT)
    try:
        yield proc
    finally:
        proc.close()


def test_worker_health(worker):
    result = worker.request("health")
    assert result["worker"] == "hermes_swi"
    assert "event_scoring" in result["loaded"]
    assert "geometry" in result["loaded"]


def test_unknown_operation_returns_structured_error(worker):
    response = worker.raw_request({"id": "bad_1", "op": "not_a_real_op"})
    assert response["id"] == "bad_1"
    assert response["ok"] is False
    assert response["error"]["type"] == "unknown_op"


def test_event_score_limit_node(worker):
    ev_0007 = next(event for event in _golden_events() if event["event_id"] == "ev_0007")
    score = worker.request("event_score", event=ev_0007)
    assert score["event_id"] == "ev_0007"
    reconstructive = score["reconstructive_findings"]
    assert reconstructive["action"] == "quarantine"
    assert reconstructive["limit_nodes"]


def test_batch_event_score_scores_all_golden_events(worker):
    scores = worker.request("batch_event_score", events=_golden_events())
    assert len(scores) == 10
    assert [score["event_id"] for score in scores] == [f"ev_{i:04d}" for i in range(1, 11)]


def test_pair_score_returns_research_safe_pair_candidates(worker):
    pairs = worker.request("pair_score", events=_golden_events())
    assert pairs
    geometry_pair = next(pair for pair in pairs if pair["pair_id"] == "pair_ev_0003_ev_0008")
    assert geometry_pair["event_a"] == "ev_0003"
    assert geometry_pair["event_b"] == "ev_0008"
    assert geometry_pair["pseudonym_a"] == "T"
    assert geometry_pair["pseudonym_b"] == "E"
    assert "shared_domain(geometry)" in geometry_pair["reasons"]
    assert "repair_affordance" in geometry_pair["reasons"]
    assert all(pair["event_a"] != "ev_0007" and pair["event_b"] != "ev_0007" for pair in pairs)

    serialized = json.dumps(pairs)
    for forbidden in [
        "raw_text",
        "actor_id",
        "source_id",
        "path",
        "tilted square",
        "not a math person",
        "bigger denominator",
    ]:
        assert forbidden not in serialized

    first_move = geometry_pair["question_moves"][0]
    assert "question_id" in first_move
    assert "move_type" in first_move
    assert "validity_register" in first_move
    assert first_move["prompt_score"] > 0
    assert first_move["score_reasons"]
    assert "text" not in first_move


def test_pair_graph_returns_research_safe_visualization_payload(worker):
    graph = worker.request("pair_graph", events=_golden_events())
    assert graph["nodes"]
    assert graph["edges"]

    node = next(node for node in graph["nodes"] if node["id"] == "ev_0003")
    assert node == {"id": "ev_0003", "label": "T", "role": "teacher"}

    edge = next(edge for edge in graph["edges"] if edge["id"] == "pair_ev_0003_ev_0008")
    assert edge["source"] == "ev_0003"
    assert edge["target"] == "ev_0008"
    assert edge["weight"] > 0
    assert edge["question_count"] > 0
    assert "shared_domain(geometry)" in edge["reasons"]

    serialized = json.dumps(graph)
    for forbidden in [
        "raw_text",
        "actor_id",
        "source_id",
        "path",
        "tilted square",
        "not a math person",
        "bigger denominator",
    ]:
        assert forbidden not in serialized


def test_geometry_matching_concepts(worker):
    concepts = worker.request(
        "geometry",
        predicate="matching_concepts",
        args=[["tilted", "square", "diamond"], [1, 2, 3, 4, 5]],
    )
    assert concepts
    assert all(item["kind"] == "concept" for item in concepts)
    assert any(item["id"] == "tilted_square_as_diamond" for item in concepts)


def test_geometry_rejects_unknown_predicate(worker):
    response = worker.raw_request(
        {
            "id": "bad_geometry",
            "op": "geometry",
            "predicate": "abolish",
            "args": [],
        }
    )
    assert response["id"] == "bad_geometry"
    assert response["ok"] is False
    assert response["error"]["type"] == "unknown_geometry_predicate"
