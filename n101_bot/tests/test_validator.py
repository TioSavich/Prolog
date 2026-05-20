"""Unit tests for bridge.validator — the Python mirror of the Prolog matcher."""
from __future__ import annotations

import json
from pathlib import Path

import pytest

from bridge.validator import Violation, load_vocabulary, strip_thinking, validate


ROOT = Path(__file__).resolve().parent.parent
VOCAB_PATH = ROOT / "logs" / "vocabulary.json"


@pytest.fixture(scope="module")
def vocabulary():
    assert VOCAB_PATH.exists(), (
        f"{VOCAB_PATH} missing — run .venv/bin/python -m bridge.prepare first"
    )
    return load_vocabulary(VOCAB_PATH)


def test_vocabulary_shape(vocabulary):
    assert len(vocabulary) == 10
    names = {t["name"] for t in vocabulary}
    expected = {
        "quantity",
        "measurement_unit",
        "measurement_process",
        "measure",
        "counting",
        "base",
        "base_five",
        "explanation",
        "strategy",
        "creative_activity",
    }
    assert names == expected


def test_every_term_has_positive_defs_and_incompats(vocabulary):
    for term in vocabulary:
        assert term["positive_defs"], f"{term['name']} has no positive defs"
        assert term["incompatibilities"], f"{term['name']} has no incompatibilities"
        for ic in term["incompatibilities"]:
            assert ic["rule"]
            assert ic["triggers"], f"{term['name']} incompat {ic['rule']} has no triggers"
            assert ic["correction"]


def test_every_term_has_source_citation(vocabulary):
    for term in vocabulary:
        assert term["sources"], f"{term['name']} has no sources"
        for src in term["sources"]:
            assert src["file"]
            assert src["lines"]


def test_validator_catches_because_its_easier(vocabulary):
    text = "We add the ones first because it is easier to do it that way."
    hits = validate(text, vocabulary)
    assert any(h.term == "explanation" for h in hits)


def test_validator_catches_same_strategy_claim(vocabulary):
    text = "Both students got 43 so they used the same strategy."
    hits = validate(text, vocabulary)
    assert any(h.term == "strategy" for h in hits)


def test_validator_catches_base_five_five_digit(vocabulary):
    text = "In base five, the digit 5 is one base."
    hits = validate(text, vocabulary)
    assert any(h.term == "base_five" for h in hits)


def test_validator_clean_on_faithful_answer(vocabulary):
    # An answer that uses Amy's distinctions correctly should NOT trigger.
    text = (
        "A quantity is a measurable property of an object. The value 20 centimeters "
        "is the measure of a book's height; the quantity is the height itself."
    )
    hits = validate(text, vocabulary)
    # This answer is faithful; specifically it says 'the value ... is the measure'
    # which is the correct Amy-move. It shouldn't trigger quantity-incompatibility.
    for h in hits:
        if h.term == "quantity" and "value" in h.trigger.lower():
            pytest.fail(f"false positive on faithful answer: {h}")


def test_validator_case_insensitive(vocabulary):
    lower = "BECAUSE IT IS EASIER"
    hits = validate(lower, vocabulary)
    assert any(h.term == "explanation" for h in hits)


def test_strip_thinking_round_trip():
    raw = "<think>let me think about quantities</think>\n\nA quantity is X."
    answer, thinking = strip_thinking(raw)
    assert answer == "A quantity is X."
    assert thinking == "let me think about quantities"


def test_strip_thinking_no_tags():
    raw = "A quantity is a measurable property."
    answer, thinking = strip_thinking(raw)
    assert answer == raw
    assert thinking == ""


def test_no_trigger_overlaps_break_matching(vocabulary):
    """Every trigger must be findable in its own text."""
    for term in vocabulary:
        for ic in term["incompatibilities"]:
            for trigger in ic["triggers"]:
                hits = validate(trigger, vocabulary)
                assert any(
                    h.trigger.lower() == trigger.lower() for h in hits
                ), f"trigger {trigger!r} for {term['name']}.{ic['rule']} didn't match itself"
