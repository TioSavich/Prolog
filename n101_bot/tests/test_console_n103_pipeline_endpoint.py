from __future__ import annotations

import json

from bridge import hermes_console_server
from bridge.hermes_console_server import HermesHandler


class FakeHandler:
    _handle_n103_pipeline = HermesHandler._handle_n103_pipeline

    def __init__(self):
        self.responses = []

    def _send_json(self, payload: dict, *, status: int = 200) -> None:
        self.responses.append({"payload": payload, "status": status})


def test_n103_pipeline_endpoint_returns_prolog_pairs_without_raw_text(monkeypatch):
    captured = {}

    def fake_pipeline(events):
        captured["events"] = events
        return {
            "privacy": "pseudonymized_metadata_only_no_student_work",
            "event_count": len(events),
            "events": [{"event_id": "ev_0001", "actor": {"pseudonym": "S01"}}],
            "pairs": [{"pair_id": "pair_ev_0001_ev_0002"}],
            "graph": {"nodes": [], "edges": []},
            "scores": [],
        }

    monkeypatch.setattr(hermes_console_server, "run_prolog_pair_pipeline", fake_pipeline)
    handler = FakeHandler()

    handler._handle_n103_pipeline(
        {
            "transcript": "Alice: A square is not a rectangle.\nBob: A square can be a rectangle."
        }
    )

    response = handler.responses[-1]
    assert response["status"] == 200
    serialized = json.dumps(response["payload"])
    assert "Alice" not in serialized
    assert "square is not a rectangle" not in serialized
    assert response["payload"]["pairs"] == [{"pair_id": "pair_ev_0001_ev_0002"}]
    assert [event.student for event in captured["events"]] == ["Alice", "Bob"]


def test_n103_pipeline_endpoint_requires_input():
    handler = FakeHandler()

    handler._handle_n103_pipeline({})

    response = handler.responses[-1]
    assert response["status"] == 400
    assert response["payload"]["error_type"] == "n103_pipeline_input"
