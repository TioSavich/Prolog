from __future__ import annotations

import pytest

from bridge import hc_bot
from bridge.denylist import filter_output
from bridge.hc_bot import HermeneuticBot


def test_bot_returns_reallms_config_record_when_api_key_missing(monkeypatch):
    def fail_chat(*args, **kwargs):
        raise AssertionError("unconfigured REALLMS path should not call a renderer")

    monkeypatch.delenv("REALLMS_API_KEY", raising=False)
    monkeypatch.delenv("HERMES_RENDERER", raising=False)
    monkeypatch.setattr(hc_bot, "RealLMSChatClient", fail_chat)
    bot = HermeneuticBot()

    record = bot.ask("hi")

    assert record.model == "reallms-unconfigured"
    assert "REALLMS_API_KEY" in record.final_answer
    assert record.detected_terms == []
    assert record.eval_tokens == 0
    assert record.duration_ms == 0
    assert record.mode == "auto"


def test_bot_uses_reallms_renderer_when_api_key_is_configured(monkeypatch):
    monkeypatch.setenv("REALLMS_API_KEY", "sk-real")
    monkeypatch.delenv("HERMES_RENDERER", raising=False)
    calls = []

    class FakeRealLMSChatClient:
        def __init__(self, *, model):
            calls.append({"model": model})

        def chat(self, system_prompt, user_message, *, temperature):
            calls.append(
                {
                    "system_prompt": system_prompt,
                    "user_message": user_message,
                    "temperature": temperature,
                }
            )
            return hc_bot.ChatResult(
                content="Ask what property decides the classification.",
                raw_content="Ask what property decides the classification.",
                model="gemma-4-31B-it",
                total_duration_ms=123,
                eval_count=0,
                filter_result=filter_output("ok"),
            )

    monkeypatch.setattr(hc_bot, "RealLMSChatClient", FakeRealLMSChatClient)
    bot = HermeneuticBot()

    record = bot.ask("What is a quantity?", mode="check_answers")

    assert record.model == "gemma-4-31B-it"
    assert "quantity" in record.detected_terms
    assert record.final_answer == "Ask what property decides the classification."
    assert record.mode == "check_answers"
    assert calls[0] == {"model": "gemma-4-31B-it"}


def test_bot_sanitizes_reallms_auth_failure_details(monkeypatch):
    monkeypatch.setenv("REALLMS_API_KEY", "sk-real")
    monkeypatch.delenv("HERMES_RENDERER", raising=False)

    class FailingRealLMSChatClient:
        def __init__(self, *, model):
            pass

        def chat(self, system_prompt, user_message, *, temperature):
            raise hc_bot.RealLMSError(
                "reallms returned 401: Authentication Error, Invalid proxy server token "
                "passed. Received API Key = sk-...ABCD, Key Hash (Token) "
                "=34045e69ff7de3a98f5e32ac07f3943be8700dbceaf4067d5c4a2d699e9530dc"
            )

    monkeypatch.setattr(hc_bot, "RealLMSChatClient", FailingRealLMSChatClient)
    bot = HermeneuticBot()

    record = bot.ask("What is a quantity?")

    assert "rejected" in record.final_answer
    assert "REALLMS_API_KEY" in record.final_answer
    assert "sk-" not in record.final_thinking
    assert "Key Hash" not in record.final_thinking
