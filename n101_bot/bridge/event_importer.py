"""Runtime event import boundary for Hermes.

Raw classroom text may enter here in memory, but downstream pair scoring and
graphing must receive metadata-only canonical events.
"""
from __future__ import annotations

import json
from typing import Any

from .hermes_n103 import HermesEvent


PAIR_GRAPH_FORBIDDEN_KEYS = {
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


def assert_pair_graph_safe(value: Any, *, path: str = "$") -> None:
    if isinstance(value, dict):
        for key, child in value.items():
            key_text = str(key)
            if key_text in PAIR_GRAPH_FORBIDDEN_KEYS:
                raise ValueError(f"unsafe pair_graph field at {path}.{key_text}")
            assert_pair_graph_safe(child, path=f"{path}.{key_text}")
        return
    if isinstance(value, list):
        for index, child in enumerate(value):
            assert_pair_graph_safe(child, path=f"{path}[{index}]")
        return


def events_from_payload(raw: object) -> list[HermesEvent]:
    if isinstance(raw, str):
        text = raw.strip()
        if not text:
            return []
        try:
            parsed = json.loads(text)
        except json.JSONDecodeError:
            return _events_from_transcript(raw)
        return events_from_payload(parsed)
    if isinstance(raw, dict):
        for key in ("events", "posts", "messages"):
            if isinstance(raw.get(key), list):
                return events_from_payload(raw[key])
        raw = [raw]
    if not isinstance(raw, list):
        raise ValueError("events must be a JSON list, object, or transcript text")

    events: list[HermesEvent] = []
    for idx, item in enumerate(raw, start=1):
        if not isinstance(item, dict):
            raise ValueError("event rows must be objects")
        event = HermesEvent(
            student=str(item.get("student") or item.get("speaker") or item.get("name") or "Unknown"),
            text=str(item.get("text") or item.get("body") or item.get("message") or ""),
            source=str(item.get("source") or "local"),
            timestamp=str(item.get("timestamp") or item.get("time") or ""),
            event_id=str(item.get("id") or item.get("event_id") or idx),
        )
        if event.text.strip():
            events.append(event)
    return events


def _events_from_transcript(text: str) -> list[HermesEvent]:
    events: list[HermesEvent] = []
    for idx, line in enumerate(text.splitlines(), start=1):
        if ":" not in line:
            continue
        speaker, body = line.split(":", 1)
        event = HermesEvent(
            student=speaker.strip() or "Unknown",
            text=body.strip(),
            source="transcript",
            event_id=str(idx),
        )
        if event.text:
            events.append(event)
    return events
