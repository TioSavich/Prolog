"""Tests for the /ask endpoint mode wiring on the Hermes console server.

The server is plain http.server.ThreadingHTTPServer (no FastAPI), so we
spin up a real instance on a free port and POST against it. Skipped when
Ollama is unreachable since /ask delegates to the bot.

Spec: docs/superpowers/specs/2026-05-04-hermes-chatbot-substrate-design.md §5
"""
from __future__ import annotations

import json
import socket
import threading
import time
from http.server import ThreadingHTTPServer

import pytest
import requests

from bridge.hermes_console_server import HermesHandler
from bridge.ollama_client import ping


pytestmark = pytest.mark.skipif(
    not ping(),
    reason="Ollama daemon not reachable; /ask requires the bot",
)


def _free_port() -> int:
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind(("127.0.0.1", 0))
    port = s.getsockname()[1]
    s.close()
    return port


@pytest.fixture(scope="module")
def server_url():
    port = _free_port()
    server = ThreadingHTTPServer(("127.0.0.1", port), HermesHandler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    # Give the server a moment to start accepting connections.
    time.sleep(0.2)
    yield f"http://127.0.0.1:{port}"
    server.shutdown()
    server.server_close()
    thread.join(timeout=2.0)


def test_ask_endpoint_accepts_mode(server_url):
    response = requests.post(
        f"{server_url}/ask",
        json={"question": "What is a square?", "mode": "check_answers"},
        timeout=240.0,
    )
    assert response.status_code == 200
    data = response.json()
    # The server may serialize either the full record or a stripped payload —
    # both shapes should echo the mode and surface cards_used.
    if "record" in data:
        record = data["record"]
        assert record.get("mode") == "check_answers"
        assert "cards_used" in record
    else:
        assert data.get("mode") == "check_answers"
        assert "cards_used" in data


def test_ask_endpoint_default_mode_is_auto(server_url):
    response = requests.post(
        f"{server_url}/ask",
        json={"question": "What is a square?"},
        timeout=240.0,
    )
    assert response.status_code == 200
    data = response.json()
    record = data.get("record") or data
    assert record.get("mode") == "auto"


def test_ask_endpoint_lesson_plan_mode(server_url):
    response = requests.post(
        f"{server_url}/ask",
        json={
            "question": "Plan a 4th grade lesson on quadrilateral classification.",
            "mode": "lesson_plan",
        },
        timeout=240.0,
    )
    assert response.status_code == 200
    data = response.json()
    record = data.get("record") or data
    assert record.get("mode") == "lesson_plan"
