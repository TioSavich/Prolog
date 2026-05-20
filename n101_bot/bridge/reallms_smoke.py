"""Opt-in synthetic REALLMS smoke check for Hermes."""
from __future__ import annotations

import json
import os
import sys
from collections.abc import Callable
from typing import Any

from .reallms_revoicer import RealLMSChatClient, RealLMSError, reallms_api_key_configured


SYNTHETIC_SYSTEM_PROMPT = (
    "You are checking an API connection. Reply with one short sentence. "
    "Do not request, quote, or infer private data."
)
SYNTHETIC_USER_CONTENT = "Synthetic Hermes connectivity check. Return a brief confirmation."


def smoke_check(
    *,
    client_factory: Callable[[], Any] = RealLMSChatClient,
) -> dict[str, object]:
    client = client_factory()
    result = client.chat(SYNTHETIC_SYSTEM_PROMPT, SYNTHETIC_USER_CONTENT)
    return {
        "ok": True,
        "model": getattr(result, "model", "unknown"),
        "duration_ms": getattr(result, "total_duration_ms", None),
        "content_chars": len(getattr(result, "content", "") or ""),
        "blocked": bool(getattr(result, "blocked", False)),
        "payload": "synthetic_connectivity_check_only",
    }


def main(argv: list[str] | None = None) -> int:
    _ = argv
    if os.environ.get("HERMES_FORCE_OFFLINE") == "1":
        print("Refusing REALLMS smoke check because HERMES_FORCE_OFFLINE=1.", file=sys.stderr)
        return 2
    if not reallms_api_key_configured():
        print("REALLMS_API_KEY is not configured; smoke check was not run.", file=sys.stderr)
        return 2
    try:
        result = smoke_check()
    except RealLMSError as exc:
        print(str(exc), file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
