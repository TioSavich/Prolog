from __future__ import annotations

import json
from types import SimpleNamespace

from bridge import reallms_smoke


class FakeClient:
    def __init__(self):
        self.calls = []

    def chat(self, system_prompt: str, user_content: str):
        self.calls.append({"system_prompt": system_prompt, "user_content": user_content})
        return SimpleNamespace(
            content="Synthetic response only.",
            raw_content="Synthetic response only.",
            model="fake-model",
            total_duration_ms=12.0,
            blocked=False,
        )


def test_smoke_check_uses_synthetic_payload_only():
    client = FakeClient()

    result = reallms_smoke.smoke_check(client_factory=lambda: client)

    serialized_calls = json.dumps(client.calls)
    assert result["ok"] is True
    assert result["model"] == "fake-model"
    assert result["content_chars"] == len("Synthetic response only.")
    assert "student" not in serialized_calls.lower()
    assert "transcript" not in serialized_calls.lower()
    assert "raw_text" not in serialized_calls.lower()


def test_main_refuses_force_offline(monkeypatch, capsys):
    monkeypatch.setenv("HERMES_FORCE_OFFLINE", "1")

    status = reallms_smoke.main([])

    captured = capsys.readouterr()
    assert status == 2
    assert "HERMES_FORCE_OFFLINE" in captured.err


def test_main_refuses_missing_api_key(monkeypatch, capsys):
    monkeypatch.delenv("HERMES_FORCE_OFFLINE", raising=False)
    monkeypatch.delenv("REALLMS_API_KEY", raising=False)

    status = reallms_smoke.main([])

    captured = capsys.readouterr()
    assert status == 2
    assert "REALLMS_API_KEY is not configured" in captured.err
