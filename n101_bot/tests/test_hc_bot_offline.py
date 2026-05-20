from __future__ import annotations

import pytest

from bridge import hc_bot
from bridge.hc_bot import HermeneuticBot


def test_bot_returns_offline_prolog_record_when_ollama_is_unreachable(monkeypatch):
    def fail_chat(*args, **kwargs):
        raise AssertionError("offline path should not call Ollama chat")

    monkeypatch.setattr(hc_bot, "ping", lambda: False)
    monkeypatch.setattr(hc_bot, "chat", fail_chat)
    bot = HermeneuticBot()

    record = bot.ask("hi")

    assert record.model == "offline-prolog"
    assert record.final_answer.startswith("Offline Prolog mode:")
    assert record.detected_terms == []
    assert record.eval_tokens == 0
    assert record.duration_ms == 0
    assert record.mode == "auto"


def test_bot_falls_back_to_offline_record_when_model_call_fails(monkeypatch):
    monkeypatch.setattr(hc_bot, "ping", lambda: True)

    def fail_chat(*args, **kwargs):
        raise hc_bot.OllamaError("model gemma:2b not found")

    monkeypatch.setattr(hc_bot, "chat", fail_chat)
    bot = HermeneuticBot()

    record = bot.ask("What is a quantity?", mode="check_answers")

    assert record.model == "offline-prolog"
    assert "quantity" in record.detected_terms
    assert "quantity" in record.final_answer
    assert "model gemma:2b not found" in record.final_thinking
    assert record.mode == "check_answers"
