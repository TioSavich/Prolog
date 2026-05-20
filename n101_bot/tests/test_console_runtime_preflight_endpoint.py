from __future__ import annotations

from bridge import hermes_console_server
from bridge.hermes_console_server import HermesHandler


class FakeHandler:
    _handle_runtime_preflight = HermesHandler._handle_runtime_preflight

    def __init__(self):
        self.responses = []

    def _send_json(self, payload: dict, *, status: int = 200) -> None:
        self.responses.append({"payload": payload, "status": status})


def test_runtime_preflight_endpoint_reports_portable_runtime_status(monkeypatch):
    calls = []

    def fake_preflight(root):
        calls.append(root)
        return {
            "portable_ready": False,
            "swipl_source": "system",
            "swipl_path": "swipl",
            "local_runtime_dirs": True,
        }

    monkeypatch.setattr(hermes_console_server, "runtime_preflight", fake_preflight)
    handler = FakeHandler()

    handler._handle_runtime_preflight()

    assert calls == [hermes_console_server.ROOT]
    assert handler.responses == [
        {
            "status": 200,
            "payload": {
                "runtime": {
                    "portable_ready": False,
                    "swipl_source": "system",
                    "swipl_path": "swipl",
                    "local_runtime_dirs": True,
                }
            },
        }
    ]
