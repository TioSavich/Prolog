from __future__ import annotations

from bridge.hermes_console_server import HermesHandler


FORBIDDEN_KEYS = {
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


class FakeHandler:
    def __init__(self):
        self.responses = []

    def _send_json(self, payload: dict, *, status: int = 200) -> None:
        self.responses.append({"payload": payload, "status": status})


def _walk(value):
    if isinstance(value, dict):
        for key, child in value.items():
            yield str(key), child
            yield from _walk(child)
    elif isinstance(value, list):
        for child in value:
            yield from _walk(child)


def test_n103_workflows_endpoint_returns_safe_course_prompt_metadata():
    assert hasattr(HermesHandler, "_handle_n103_workflows")
    FakeHandler._handle_n103_workflows = HermesHandler._handle_n103_workflows
    handler = FakeHandler()

    handler._handle_n103_workflows({})

    response = handler.responses[-1]
    assert response["status"] == 200
    payload = response["payload"]
    assert payload["course"] == "N103"
    assert payload["privacy"] == "metadata_only_no_student_work"
    assert payload["event_schema"]["required"] == ["event_id", "actor", "source", "symbolic", "pml"]
    assert len(payload["workflows"]) >= 2
    assert {
        "unit",
        "title",
        "cluster",
        "prompt_focus",
        "pairing_use",
        "question_moves",
        "sample_event_count",
    }.issubset(payload["workflows"][0])
    assert "inclusive" in payload["workflows"][0]["prompt_focus"].lower()

    for key, _ in _walk(payload):
        assert key not in FORBIDDEN_KEYS
