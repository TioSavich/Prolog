from __future__ import annotations

import json

from bridge.hermes_n103 import HermesEvent
from bridge.n103_prolog_pipeline import canonical_events_from_hermes_events, run_prolog_pair_pipeline


def test_canonical_events_strip_names_and_student_work():
    events = [
        HermesEvent(
            student="Alice Realname",
            text="A square is not a rectangle because rectangles have to be long.",
            event_id="raw-1",
        ),
        HermesEvent(
            student="Bob Realname",
            text="A square can also be a rectangle if the definition is inclusive.",
            event_id="raw-2",
        ),
    ]

    canonical = canonical_events_from_hermes_events(events)

    assert [event["event_id"] for event in canonical] == ["ev_0001", "ev_0002"]
    assert [event["actor"]["pseudonym"] for event in canonical] == ["S01", "S02"]
    serialized = json.dumps(canonical)
    for forbidden in [
        "Alice",
        "Bob",
        "Realname",
        "square is not a rectangle",
        "rectangles have to be long",
        "text",
        "raw_text",
        "student_id",
    ]:
        assert forbidden not in serialized
    assert canonical[0]["symbolic"]["incompatibilities"]
    assert canonical[0]["question_candidates"]
    assert canonical[0]["symbolic"]["material_inferences"]


def test_run_prolog_pair_pipeline_uses_worker_with_safe_canonical_events():
    calls = []

    class FakeWorker:
        def request(self, op, **payload):
            calls.append({"op": op, "payload": payload})
            if op == "batch_event_score":
                return [{"event_id": event["event_id"]} for event in payload["events"]]
            if op == "pair_score":
                return [{"pair_id": "pair_ev_0001_ev_0002", "score": 8}]
            if op == "pair_graph":
                return {"nodes": [], "edges": []}
            raise AssertionError(op)

        def close(self):
            calls.append({"op": "close"})

    packet = run_prolog_pair_pipeline(
        [
            HermesEvent(student="Alice", text="A square is not a rectangle.", event_id="a"),
            HermesEvent(student="Bob", text="A square can also be a rectangle.", event_id="b"),
        ],
        worker_factory=FakeWorker,
    )

    assert packet["event_count"] == 2
    assert packet["pairs"] == [{"pair_id": "pair_ev_0001_ev_0002", "score": 8}]
    assert [call["op"] for call in calls] == [
        "batch_event_score",
        "pair_score",
        "pair_graph",
        "close",
    ]
    serialized_worker_payload = json.dumps(calls[0]["payload"]["events"])
    assert "Alice" not in serialized_worker_payload
    assert "square is not a rectangle" not in serialized_worker_payload
