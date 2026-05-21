from __future__ import annotations

import importlib.util
import json
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
SAMPLE = ROOT / "samples" / "fake_demo_events.json"
SCRIPT = ROOT / "scripts" / "fake_demo_run.sh"


FORBIDDEN = {
    "raw_text",
    "text",
    "actor_id",
    "student",
    "student_id",
    "student_name",
    "source_id",
    "path",
    "evidence",
}


def _module():
    assert importlib.util.find_spec("bridge.fake_demo_run") is not None
    from bridge import fake_demo_run

    return fake_demo_run


def _assert_no_forbidden_keys(value):
    if isinstance(value, dict):
        for key, child in value.items():
            assert key not in FORBIDDEN
            _assert_no_forbidden_keys(child)
    elif isinstance(value, list):
        for child in value:
            _assert_no_forbidden_keys(child)


def test_fake_demo_events_are_metadata_only():
    events = json.loads(SAMPLE.read_text(encoding="utf-8"))

    assert len(events) >= 2
    assert all(event["event_id"].startswith("ev_demo_") for event in events)
    assert all(event["actor"]["pseudonym"].startswith("S") for event in events)
    assert all(event["source"]["metadata"]["domain"] == "geometry" for event in events)
    _assert_no_forbidden_keys(events)


def test_run_fake_demo_uses_worker_with_metadata_only_events():
    fake_demo_run = _module()
    calls = []

    class FakeWorker:
        def request(self, op, **payload):
            calls.append({"op": op, "payload": payload})
            if op == "batch_event_score":
                return [{"event_id": event["event_id"]} for event in payload["events"]]
            if op == "pair_score":
                return [
                    {
                        "pair_id": "pair_ev_demo_0001_ev_demo_0002",
                        "event_a": "ev_demo_0001",
                        "event_b": "ev_demo_0002",
                        "score": 9,
                        "reasons": ["shared_domain(geometry)", "repair_affordance"],
                        "question_moves": [
                            {
                                "question_id": "q_demo_0002",
                                "move_type": "AQST",
                                "validity_register": "objective_truth",
                                "prompt_score": 4,
                                "score_reasons": ["repair_affordance"],
                            }
                        ],
                    }
                ]
            if op == "pair_graph":
                return {
                    "nodes": [
                        {"id": "ev_demo_0001", "label": "S01", "role": "teacher"},
                        {"id": "ev_demo_0002", "label": "S02", "role": "student"},
                    ],
                    "edges": [
                        {
                            "id": "pair_ev_demo_0001_ev_demo_0002",
                            "source": "ev_demo_0001",
                            "target": "ev_demo_0002",
                            "weight": 9,
                            "question_count": 1,
                            "reasons": ["repair_affordance"],
                        }
                    ],
                }
            raise AssertionError(op)

        def close(self):
            calls.append({"op": "close"})

    result = fake_demo_run.run_fake_demo(worker_factory=FakeWorker)

    assert result["privacy"] == "synthetic_metadata_only_no_student_work"
    assert result["event_count"] == len(result["events"]) >= 2
    assert result["score_count"] == len(result["scores"])
    assert result["pair_count"] == 1
    assert [call["op"] for call in calls] == [
        "batch_event_score",
        "pair_score",
        "pair_graph",
        "close",
    ]
    _assert_no_forbidden_keys(result)
    _assert_no_forbidden_keys(calls[0]["payload"]["events"])


def test_run_fake_demo_can_revoice_one_synthetic_question_with_fake_revoicer():
    fake_demo_run = _module()
    captured = {}

    class FakeWorker:
        def request(self, op, **payload):
            if op == "batch_event_score":
                return [{"event_id": event["event_id"]} for event in payload["events"]]
            if op == "pair_score":
                return [
                    {
                        "pair_id": "pair_ev_demo_0001_ev_demo_0002",
                        "event_a": "ev_demo_0001",
                        "event_b": "ev_demo_0002",
                        "score": 9,
                        "reasons": ["shared_domain(geometry)", "repair_affordance"],
                        "question_moves": [
                            {
                                "question_id": "q_demo_0002",
                                "move_type": "AQST",
                                "validity_register": "objective_truth",
                                "prompt_score": 4,
                                "score_reasons": ["repair_affordance"],
                            }
                        ],
                    }
                ]
            if op == "pair_graph":
                return {
                    "nodes": [
                        {"id": "ev_demo_0001", "label": "S01", "role": "teacher"},
                        {"id": "ev_demo_0002", "label": "S02", "role": "student"},
                    ],
                    "edges": [
                        {
                            "id": "pair_ev_demo_0001_ev_demo_0002",
                            "source": "ev_demo_0001",
                            "target": "ev_demo_0002",
                            "weight": 9,
                            "question_count": 1,
                            "reasons": ["repair_affordance"],
                        }
                    ],
                }
            raise AssertionError(op)

        def close(self):
            pass

    class FakeRevoiceResult:
        def as_dict(self):
            return {
                "provider": "reallms",
                "model": "fake",
                "content": "What definition could both of these claims answer to?",
                "blocked": False,
                "filter_result": {
                    "text": "What definition could both of these claims answer to?",
                    "original": "What definition could both of these claims answer to?",
                    "blocked": False,
                    "hits": [],
                },
            }

    class FakeRevoicer:
        def revoice(self, *, question_move, pair_context):
            captured["question_move"] = question_move
            captured["pair_context"] = pair_context
            return FakeRevoiceResult()

    result = fake_demo_run.run_fake_demo(
        worker_factory=FakeWorker,
        revoice=True,
        revoicer_factory=FakeRevoicer,
    )

    assert result["revoice"]["provider"] == "reallms"
    assert result["revoice"]["content"].startswith("What definition")
    assert result["revoice"]["filter_result"] == {"blocked": False, "hits": []}
    _assert_no_forbidden_keys(captured)
    _assert_no_forbidden_keys(result)
    assert captured["question_move"]["question_id"] == "q_demo_0002"
    assert captured["pair_context"]["pair_id"] == "pair_ev_demo_0001_ev_demo_0002"


def test_write_demo_output_uses_runtime_output_root(tmp_path, monkeypatch):
    fake_demo_run = _module()
    output_root = tmp_path / "outputs"
    monkeypatch.setenv("HERMES_OUTPUT_ROOT", str(output_root))
    payload = {"privacy": "synthetic_metadata_only_no_student_work", "pair_count": 1}

    output_path = fake_demo_run.write_demo_output(payload, "runs/fake_demo.json")

    assert output_path == output_root / "runs" / "fake_demo.json"
    assert json.loads(output_path.read_text(encoding="utf-8")) == payload


def test_write_demo_output_rejects_code_tree_paths(tmp_path):
    fake_demo_run = _module()

    try:
        fake_demo_run.write_demo_output({}, ROOT / "fake_demo_output.json")
    except ValueError as exc:
        assert "must not live inside app code root" in str(exc)
    else:
        raise AssertionError("expected code-root output path to be rejected")


def test_fake_demo_script_uses_portable_python_and_module_entrypoint():
    script = SCRIPT.read_text(encoding="utf-8")

    assert "HERMES_PYTHON" in script
    assert 'PYTHON_BIN="$HERMES_PYTHON"' in script
    assert 'PYTHON_BIN="$ROOT/.venv/bin/python"' in script
    assert 'PYTHON_BIN="python3"' in script
    assert 'PACKAGE_ROOT="$(cd "$ROOT/.." && pwd)"' in script
    assert 'FLASH_ROOT="$(cd "$PACKAGE_ROOT/.." && pwd)"' in script
    assert 'for env_file in "$PACKAGE_ROOT/.env" "$FLASH_ROOT/.env" "$ROOT/.env"; do' in script
    assert 'export PYTHONPATH="$ROOT:$ROOT/vendor${PYTHONPATH:+:$PYTHONPATH}"' in script
    assert '"$PYTHON_BIN" -m bridge.fake_demo_run "$@"' in script
