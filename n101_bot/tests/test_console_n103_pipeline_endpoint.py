from __future__ import annotations

import json

from bridge import hermes_console_server
from bridge.hermes_console_server import HermesHandler, _looks_like_discussion_transcript


class FakeHandler:
    _handle_n103_pipeline = HermesHandler._handle_n103_pipeline
    _handle_chat = HermesHandler._handle_chat

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


def test_discussion_transcript_detection_requires_speaker_lines():
    assert _looks_like_discussion_transcript(
        "Alice: A square is not a rectangle.\nBob: I think it can be."
    )
    assert _looks_like_discussion_transcript(
        "Student 1: Rectangles have to be long.\nStudent 2: Squares have four right angles."
    )
    assert not _looks_like_discussion_transcript(
        "How should I ask students about squares and rectangles?"
    )


def test_chat_endpoint_rejects_transcript_like_student_work():
    handler = FakeHandler()

    handler._handle_chat(
        {
            "message": (
                "Alice: A square is not a rectangle because rectangles are long.\n"
                "Bob: It can be a rectangle if the definition is inclusive."
            )
        }
    )

    response = handler.responses[-1]
    assert response["status"] == 400
    assert response["payload"]["error_type"] == "chat_transcript_safety"
    assert response["payload"]["route"] == "n103_pipeline"
