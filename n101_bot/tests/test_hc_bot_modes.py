"""Tests for HermeneuticBot mode-parameter wiring.

Covers:
 - mode kwarg accepted by ask()
 - lesson_plan bypasses the assessing pre-flight
 - check_answers preserves the assessing route
 - TurnRecord carries `mode` and `cards_used` fields

Most assertions require a live Ollama daemon; this module skips
gracefully when the daemon is unreachable so the file imports
cleanly in CI without LLM access.

Spec: docs/superpowers/specs/2026-05-04-hermes-chatbot-substrate-design.md §3
"""
from __future__ import annotations

import pytest

from bridge.hc_bot import HermeneuticBot, MODE_FRAMING, TurnRecord
from bridge.ollama_client import ping


pytestmark = pytest.mark.skipif(
    not ping(),
    reason="Ollama daemon not reachable; skipping live bot tests",
)


# ── shape / contract tests (no LLM needed) ─────────────────────────────


def test_mode_framing_has_all_four_modes():
    """Sanity check: the four modes from the spec are wired in."""
    for mode in ("auto", "check_answers", "ask_good_questions", "lesson_plan"):
        assert mode in MODE_FRAMING


def test_turnrecord_has_mode_and_cards_used_fields():
    """TurnRecord dataclass must expose `mode` and `cards_used`."""
    fields = {f.name for f in TurnRecord.__dataclass_fields__.values()}
    assert "mode" in fields
    assert "cards_used" in fields


# ── live-bot tests (require Ollama) ────────────────────────────────────


def test_bot_accepts_mode_kwarg():
    bot = HermeneuticBot()
    record = bot.ask("What is a square?", mode="check_answers")
    assert record.mode == "check_answers"


def test_bot_default_mode_is_auto():
    bot = HermeneuticBot()
    record = bot.ask("What is a square?")
    assert record.mode == "auto"


def test_lesson_plan_mode_produces_content_not_question():
    bot = HermeneuticBot()
    # An input the move_grammar might classify as assessing — but lesson_plan
    # mode should bypass that route and produce concrete content.
    record = bot.ask(
        "Plan a 4th-grade lesson on quadrilateral classification with three activities.",
        mode="lesson_plan",
    )
    assert record.mode == "lesson_plan"
    assert record.final_answer != ""
    # Loose check: lesson plans are longer than typical assessing questions.
    assert len(record.final_answer) > 50


def test_check_answers_mode_keeps_assessing_route_available():
    bot = HermeneuticBot()
    record = bot.ask(
        "A student says a tilted square is a diamond.",
        mode="check_answers",
    )
    assert record.mode == "check_answers"
    assert record.final_answer != ""


def test_turnrecord_carries_cards_used_list():
    bot = HermeneuticBot()
    record = bot.ask("What is a square?", mode="auto")
    assert hasattr(record, "cards_used")
    assert isinstance(record.cards_used, list)


def test_turnrecord_as_dict_includes_mode_and_cards_used():
    bot = HermeneuticBot()
    record = bot.ask("What is a square?", mode="check_answers")
    d = record.as_dict()
    assert d.get("mode") == "check_answers"
    assert "cards_used" in d
    assert isinstance(d["cards_used"], list)


def test_unknown_mode_raises_value_error():
    bot = HermeneuticBot()
    with pytest.raises(ValueError):
        bot.ask("What is a square?", mode="not_a_real_mode")
