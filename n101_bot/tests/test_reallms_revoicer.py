from __future__ import annotations

import json

import pytest
import requests

from bridge.reallms_revoicer import (
    DEFAULT_REALLMS_BASE_URL,
    DEFAULT_REALLMS_MODEL,
    RealLMSError,
    RealLMSChatClient,
    RealLMSRevoicer,
    RevoiceSafetyError,
    build_revoicing_payload,
    reallms_api_key_configured,
    resolve_chat_completions_url,
    resolve_reallms_model,
)


SAFE_QUESTION_MOVE = {
    "question_id": "q_0003",
    "move_type": "FMST",
    "validity_register": "objective_truth",
    "target_commitment": "student_names_by_orientation",
    "constraints_satisfied": ["targets_live_commitment", "opens_validity_claim"],
    "prompt_score": 7,
    "score_reasons": [
        "constraint(targets_live_commitment)",
        "constraint(opens_validity_claim)",
        "validity_register(objective_truth)",
    ],
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


class FakeResponse:
    status_code = 200
    text = "{}"

    def __init__(self, payload: dict):
        self._payload = payload

    def json(self) -> dict:
        return self._payload


class BadJsonResponse:
    status_code = 200
    text = "not-json"

    def json(self) -> dict:
        raise ValueError("not json")


class ErrorResponse:
    status_code = 401

    def __init__(self, text: str):
        self.text = text

    def json(self) -> dict:
        return {}


def test_build_revoicing_payload_uses_safe_metadata_only():
    payload = build_revoicing_payload(
        question_move=SAFE_QUESTION_MOVE,
        pair_context=SAFE_PAIR_CONTEXT,
    )
    serialized = json.dumps(payload)
    assert "raw_text" not in serialized
    assert "actor_id" not in serialized
    assert "source_id" not in serialized
    assert "tilted square" not in serialized
    assert "question_move" in serialized
    assert "pair_context" in serialized


def test_build_revoicing_payload_rejects_forbidden_nested_fields():
    unsafe = {**SAFE_QUESTION_MOVE, "raw_text": "A tilted square is a diamond."}
    with pytest.raises(RevoiceSafetyError):
        build_revoicing_payload(question_move=unsafe, pair_context=SAFE_PAIR_CONTEXT)


def test_revoicer_posts_openai_compatible_chat_payload_and_filters_output():
    captured = {}

    def fake_post(url, *, headers, json, timeout):
        captured["url"] = url
        captured["headers"] = headers
        captured["json"] = json
        captured["timeout"] = timeout
        return FakeResponse(
            {
                "model": "reallms-test",
                "choices": [
                    {
                        "message": {
                            "content": "Ask what properties stay the same, without deciding for them."
                        }
                    }
                ],
            }
        )

    revoicer = RealLMSRevoicer(
        base_url="https://reallms.example.test/v1",
        api_key="test-key",
        model="reallms-test",
        http_post=fake_post,
    )

    result = revoicer.revoice(
        question_move=SAFE_QUESTION_MOVE,
        pair_context=SAFE_PAIR_CONTEXT,
    )

    assert captured["url"] == "https://reallms.example.test/v1/chat/completions"
    assert captured["headers"]["Authorization"] == "Bearer test-key"
    assert captured["json"]["model"] == "reallms-test"
    serialized_request = json.dumps(captured["json"])
    assert "raw_text" not in serialized_request
    assert "actor_id" not in serialized_request
    assert "source_id" not in serialized_request
    assert result.content == "Ask what properties stay the same, without deciding for them."
    assert result.blocked is False
    assert result.as_dict()["provider"] == "reallms"
    assert "raw_content" not in result.as_dict()


def test_revoicer_blocks_denylisted_model_output():
    def fake_post(url, *, headers, json, timeout):
        return FakeResponse(
            {
                "choices": [
                    {
                        "message": {
                            "content": "This is shit."
                        }
                    }
                ],
            }
        )

    revoicer = RealLMSRevoicer(
        base_url="https://reallms.example.test/v1",
        api_key="test-key",
        model="reallms-test",
        http_post=fake_post,
    )

    result = revoicer.revoice(
        question_move=SAFE_QUESTION_MOVE,
        pair_context=SAFE_PAIR_CONTEXT,
    )

    assert result.blocked is True
    assert "shit" not in result.content


def test_reallms_defaults_match_n103_run_pipeline():
    assert DEFAULT_REALLMS_BASE_URL == "https://reallms.rescloud.iu.edu/direct/v1"
    assert DEFAULT_REALLMS_MODEL == "gemma-4-31B-it"


def test_reallms_chat_url_accepts_base_or_full_endpoint():
    assert (
        resolve_chat_completions_url("https://reallms.example.test/direct")
        == "https://reallms.example.test/direct/v1/chat/completions"
    )
    assert (
        resolve_chat_completions_url("https://reallms.example.test/direct/v1")
        == "https://reallms.example.test/direct/v1/chat/completions"
    )
    assert (
        resolve_chat_completions_url("https://reallms.example.test/direct/v1/chat/completions")
        == "https://reallms.example.test/direct/v1/chat/completions"
    )


def test_blank_reallms_url_and_model_fall_back_to_defaults(monkeypatch):
    monkeypatch.delenv("REALLMS_BASE_URL", raising=False)
    monkeypatch.delenv("REALLMS_MODEL", raising=False)

    assert resolve_chat_completions_url("") == (
        "https://reallms.rescloud.iu.edu/direct/v1/chat/completions"
    )
    assert resolve_reallms_model("") == DEFAULT_REALLMS_MODEL


def test_reallms_api_key_configured_rejects_placeholders(monkeypatch):
    monkeypatch.delenv("REALLMS_API_KEY", raising=False)
    assert reallms_api_key_configured() is False
    monkeypatch.setenv("REALLMS_API_KEY", "YOUR_KEY_HERE")
    assert reallms_api_key_configured() is False
    monkeypatch.setenv("REALLMS_API_KEY", "sk-real")
    assert reallms_api_key_configured() is True


def test_revoicer_requires_configured_api_key_before_post(monkeypatch):
    monkeypatch.delenv("REALLMS_API_KEY", raising=False)
    calls = []

    def fake_post(url, *, headers, json, timeout):
        calls.append(url)
        return FakeResponse({"choices": [{"message": {"content": "unused"}}]})

    revoicer = RealLMSRevoicer(
        base_url="https://reallms.example.test/v1",
        api_key=None,
        model="reallms-test",
        http_post=fake_post,
    )

    with pytest.raises(RealLMSError, match="REALLMS_API_KEY is not configured"):
        revoicer.revoice(
            question_move=SAFE_QUESTION_MOVE,
            pair_context=SAFE_PAIR_CONTEXT,
        )

    assert calls == []


def test_reallms_chat_client_wraps_request_exception_as_reallms_error():
    def fake_post(url, *, headers, json, timeout):
        raise requests.exceptions.Timeout("network timed out")

    client = RealLMSChatClient(
        base_url="https://reallms.example.test/direct/v1",
        api_key="test-key",
        model="gemma-4-31B-it",
        http_post=fake_post,
    )

    with pytest.raises(RealLMSError, match="reallms request failed: network timed out"):
        client.chat("system prompt", "user content")


def test_reallms_chat_client_wraps_invalid_json_as_reallms_error():
    def fake_post(url, *, headers, json, timeout):
        return BadJsonResponse()

    client = RealLMSChatClient(
        base_url="https://reallms.example.test/direct/v1",
        api_key="test-key",
        model="gemma-4-31B-it",
        http_post=fake_post,
    )

    with pytest.raises(RealLMSError, match="reallms returned invalid JSON"):
        client.chat("system prompt", "user content")


def test_reallms_error_redacts_key_material_before_raise():
    def fake_post(url, *, headers, json, timeout):
        return ErrorResponse(
            "Authentication Error. Received API Key: test-key-123. "
            "Key Hash (Token): abcdef123456. Bearer sk-secret-token"
        )

    client = RealLMSChatClient(
        base_url="https://reallms.example.test/direct/v1",
        api_key="test-key",
        model="gemma-4-31B-it",
        http_post=fake_post,
    )

    with pytest.raises(RealLMSError) as excinfo:
        client.chat("system prompt", "user content")

    message = str(excinfo.value)
    assert "test-key-123" not in message
    assert "abcdef123456" not in message
    assert "sk-secret-token" not in message
    assert "[redacted]" in message


def test_reallms_chat_client_posts_openai_compatible_payload_and_filters_output():
    captured = {}

    def fake_post(url, *, headers, json, timeout):
        captured["url"] = url
        captured["headers"] = headers
        captured["json"] = json
        captured["timeout"] = timeout
        return FakeResponse(
            {
                "model": "gemma-4-31B-it",
                "choices": [
                    {
                        "message": {
                            "content": "Consider asking what property decides the classification."
                        }
                    }
                ],
            }
        )

    client = RealLMSChatClient(
        base_url="https://reallms.example.test/direct/v1",
        api_key="test-key",
        model="gemma-4-31B-it",
        http_post=fake_post,
    )

    result = client.chat("system prompt", "user content", temperature=0.3)

    assert captured["url"] == "https://reallms.example.test/direct/v1/chat/completions"
    assert captured["headers"]["Authorization"] == "Bearer test-key"
    assert captured["json"]["model"] == "gemma-4-31B-it"
    assert captured["json"]["temperature"] == 0.3
    assert result.model == "gemma-4-31B-it"
    assert result.content == "Consider asking what property decides the classification."
    assert result.eval_count == 0
