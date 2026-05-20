from __future__ import annotations

from bridge import hermes_console_server
from bridge.hermes_console_server import HermesHandler
from bridge.reallms_revoicer import RevoiceSafetyError


SAFE_QUESTION_MOVE = {
    "question_id": "q_0003",
    "move_type": "FMST",
    "validity_register": "objective_truth",
    "target_commitment": "student_names_by_orientation",
    "constraints_satisfied": ["targets_live_commitment", "opens_validity_claim"],
    "prompt_score": 7,
    "score_reasons": ["constraint(targets_live_commitment)"],
}


SAFE_PAIR_CONTEXT = {
    "pair_id": "pair_ev_0003_ev_0008",
    "event_a": "ev_0003",
    "event_b": "ev_0008",
    "pseudonym_a": "T",
    "pseudonym_b": "E",
    "roles": ["teacher", "student"],
    "score": 8,
    "reasons": ["shared_domain(geometry)", "repair_affordance"],
}


class FakeRevoiceResult:
    content = "Ask what properties stay the same, without deciding for them."
    blocked = False

    def as_dict(self):
        return {
            "content": self.content,
            "provider": "reallms",
            "model": "fake-reallms",
            "blocked": self.blocked,
            "filter_result": {"blocked": False, "hits": []},
        }


class FakeRevoicer:
    calls = []

    def __init__(self, **kwargs):
        self.kwargs = kwargs

    def revoice(self, *, question_move, pair_context):
        self.calls.append({"question_move": question_move, "pair_context": pair_context})
        if "raw_text" in question_move:
            raise RevoiceSafetyError("unsafe revoicing field at $.raw_text")
        return FakeRevoiceResult()


class FakeHandler:
    _handle_revoice = HermesHandler._handle_revoice

    def __init__(self):
        self.responses = []

    def _send_json(self, payload: dict, *, status: int = 200) -> None:
        self.responses.append({"payload": payload, "status": status})


def _install_fake_revoicer(monkeypatch):
    FakeRevoicer.calls = []
    monkeypatch.setattr(hermes_console_server, "RealLMSRevoicer", FakeRevoicer)


def test_revoice_endpoint_accepts_safe_metadata(monkeypatch):
    _install_fake_revoicer(monkeypatch)
    handler = FakeHandler()
    handler._handle_revoice(
        {
            "question_move": SAFE_QUESTION_MOVE,
            "pair_context": SAFE_PAIR_CONTEXT,
            "model": "fake-reallms",
        }
    )

    response = handler.responses[-1]
    assert response["status"] == 200
    data = response["payload"]
    assert data["revoice"]["provider"] == "reallms"
    assert data["revoice"]["model"] == "fake-reallms"
    assert data["revoice"]["content"] == FakeRevoiceResult.content
    assert "raw_content" not in data["revoice"]
    assert FakeRevoicer.calls == [
        {"question_move": SAFE_QUESTION_MOVE, "pair_context": SAFE_PAIR_CONTEXT}
    ]


def test_revoice_endpoint_rejects_raw_student_work(monkeypatch):
    _install_fake_revoicer(monkeypatch)
    handler = FakeHandler()

    try:
        handler._handle_revoice(
            {
                "question_move": {**SAFE_QUESTION_MOVE, "raw_text": "A tilted square is a diamond."},
                "pair_context": SAFE_PAIR_CONTEXT,
            }
        )
    except RevoiceSafetyError as exc:
        error = str(exc)
    else:
        raise AssertionError("expected RevoiceSafetyError")

    assert "raw_text" in error
