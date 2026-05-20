from __future__ import annotations

from bridge import hermes_console_server
from bridge.hermes_console_server import HermesHandler


CANONICAL_EVENTS = [
    {
        "event_id": "ev_0003",
        "actor": {"role": "teacher", "pseudonym": "T"},
        "source": {"source_type": "teacher_note", "metadata": {"domain": "geometry"}},
    },
    {
        "event_id": "ev_0008",
        "actor": {"role": "student", "pseudonym": "E"},
        "source": {"source_type": "student_writing", "metadata": {"domain": "geometry"}},
    },
]


SAFE_PAIRS = [
    {
        "pair_id": "pair_ev_0003_ev_0008",
        "event_a": "ev_0003",
        "event_b": "ev_0008",
        "pseudonym_a": "T",
        "pseudonym_b": "E",
        "roles": ["teacher", "student"],
        "score": 8,
        "reasons": ["shared_domain(geometry)", "repair_affordance"],
        "question_moves": [
            {
                "question_id": "q_0003",
                "move_type": "FMST",
                "validity_register": "objective_truth",
                "target_commitment": "student_names_by_orientation",
                "prompt_score": 7,
                "score_reasons": ["constraint(targets_live_commitment)"],
            }
        ],
    }
]


SAFE_GRAPH = {
    "nodes": [
        {"id": "ev_0003", "label": "T", "role": "teacher"},
        {"id": "ev_0008", "label": "E", "role": "student"},
    ],
    "edges": [
        {
            "id": "pair_ev_0003_ev_0008",
            "source": "ev_0003",
            "target": "ev_0008",
            "weight": 8,
            "reasons": ["shared_domain(geometry)", "repair_affordance"],
            "question_count": 1,
            "move_types": ["FMST"],
        }
    ],
}


class FakeWorker:
    calls = []

    def __init__(self):
        pass

    def request(self, op, **payload):
        self.calls.append({"op": op, "payload": payload})
        if op == "pair_score":
            return SAFE_PAIRS
        if op == "pair_graph":
            return SAFE_GRAPH
        raise AssertionError(f"unexpected worker op {op}")

    def close(self):
        pass


class FakeHandler:
    _handle_pair_graph = HermesHandler._handle_pair_graph
    _handle_pair = HermesHandler._handle_pair

    def __init__(self):
        self.responses = []

    def _send_json(self, payload: dict, *, status: int = 200) -> None:
        self.responses.append({"payload": payload, "status": status})


def test_pair_graph_endpoint_returns_worker_safe_payload(monkeypatch):
    FakeWorker.calls = []
    monkeypatch.setattr(hermes_console_server, "PersistentPrologWorker", FakeWorker)
    handler = FakeHandler()

    handler._handle_pair_graph({"events": CANONICAL_EVENTS})

    response = handler.responses[-1]
    assert response["status"] == 200
    payload = response["payload"]
    assert payload == {"pairs": SAFE_PAIRS, "graph": SAFE_GRAPH}
    assert FakeWorker.calls == [
        {"op": "pair_score", "payload": {"events": CANONICAL_EVENTS}},
        {"op": "pair_graph", "payload": {"events": CANONICAL_EVENTS}},
    ]


def test_pair_graph_endpoint_rejects_missing_events(monkeypatch):
    monkeypatch.setattr(hermes_console_server, "PersistentPrologWorker", FakeWorker)
    handler = FakeHandler()

    handler._handle_pair_graph({"events": {"not": "a list"}})

    response = handler.responses[-1]
    assert response["status"] == 400
    assert response["payload"]["error"] == "events list is required"


def test_pair_graph_endpoint_rejects_raw_student_work_before_worker(monkeypatch):
    FakeWorker.calls = []
    monkeypatch.setattr(hermes_console_server, "PersistentPrologWorker", FakeWorker)
    handler = FakeHandler()

    handler._handle_pair_graph(
        {
            "events": [
                {
                    **CANONICAL_EVENTS[0],
                    "raw_text": "A first-grader says a tilted square is a diamond.",
                }
            ]
        }
    )

    response = handler.responses[-1]
    assert response["status"] == 400
    assert response["payload"]["error_type"] == "pair_graph_safety"
    assert "raw_text" in response["payload"]["error"]
    assert FakeWorker.calls == []


def test_legacy_pair_endpoint_refuses_raw_text_echo_path(monkeypatch):
    FakeWorker.calls = []
    monkeypatch.setattr(hermes_console_server, "PersistentPrologWorker", FakeWorker)
    handler = FakeHandler()

    handler._handle_pair({"events": []})

    response = handler.responses[-1]
    assert response["status"] == 400
    assert response["payload"]["error_type"] == "pair_safety"
    assert "metadata-only" in response["payload"]["error"]
    assert FakeWorker.calls == []
