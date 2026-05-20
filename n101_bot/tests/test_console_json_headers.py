from __future__ import annotations

import io

from bridge.hermes_console_server import HermesHandler


class FakeJsonHandler:
    _send_json = HermesHandler._send_json
    _send_cors_headers = HermesHandler._send_cors_headers

    def __init__(self):
        self.status = None
        self.headers = []
        self.wfile = io.BytesIO()

    def send_response(self, status):
        self.status = status

    def send_header(self, key, value):
        self.headers.append((key, value))

    def end_headers(self):
        pass


def test_json_responses_are_marked_no_store():
    handler = FakeJsonHandler()

    handler._send_json({"ok": True})

    headers = dict(handler.headers)
    assert handler.status == 200
    assert headers["Content-Type"] == "application/json"
    assert headers["Cache-Control"] == "no-store, max-age=0"
    assert headers["Pragma"] == "no-cache"
    assert headers["Expires"] == "0"
    assert headers["Access-Control-Allow-Origin"] == "*"
    assert headers["Access-Control-Allow-Headers"] == "Content-Type"
    assert headers["Access-Control-Allow-Methods"] == "GET, POST, OPTIONS"
    assert handler.wfile.getvalue() == b'{"ok": true}'


def test_handler_supports_browser_cors_preflight():
    assert hasattr(HermesHandler, "do_OPTIONS")
