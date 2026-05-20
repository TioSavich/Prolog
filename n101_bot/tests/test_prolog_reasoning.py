"""Tests for bridge/prolog.py — the material-inference layer.

Exercises the Python wrapper, which shells to swipl, so these are slower
than the pure-Python validator tests. Grouped separately for that reason.
"""
from bridge.prolog import detect, commitments, reason, focused_system_prompt


# ── detect_terms ──

def test_detect_finds_quantity_in_question():
    assert "quantity" in detect("What is a quantity?")


def test_detect_finds_via_alias():
    assert "quantity" in detect("Are these quantities measurable?")


def test_detect_returns_empty_for_unrelated():
    assert detect("What is the weather today?") == []


def test_detect_finds_multiple_terms():
    hits = set(detect("A measure is a number that a measurement process finds for a quantity."))
    assert {"quantity", "measure", "measurement_process"}.issubset(hits)


# ── commitments (assertions that fire incompatibility triggers) ──

def test_commitment_fires_on_declarative_trigger():
    cs = commitments("20 hours per week is a quantity")
    assert len(cs) == 1
    assert cs[0].term == "quantity"
    assert "value is not itself the quantity" in cs[0].rule.lower()
    assert "value" in cs[0].correction.lower()


def test_commitment_is_empty_on_question_form():
    # A question is not an assertion — no commitment undertaken.
    assert commitments("Is 20 hours per week a quantity?") == []


def test_commitment_fires_on_because_its_easier():
    cs = commitments("They used making ten because it is easier.")
    assert any("easier" in c.rule.lower() or "easier" in c.trigger.lower() for c in cs)


def test_multiple_commitments_from_multiple_triggers():
    cs = commitments("A number is a quantity and any property is a quantity.")
    assert len(cs) >= 2


# ── reasoning report shape ──

def test_reason_report_contains_consequences():
    # exact trigger phrasing from quantity.pl's "bare number" incompat
    report = reason("Jasmine says a number is a quantity.")
    assert "quantity" in report.detected
    assert len(report.commitments) >= 1
    assert len(report.consequences) == len(report.commitments)
    for c in report.consequences:
        assert c["status"] == "blocked"
        assert c["correction"]


def test_reason_empty_for_benign_assertion():
    report = reason("A quantity is a measurable property.")
    # no incompatibility triggers fire on an Amy-faithful assertion
    assert report.commitments == []


# ── focused system prompt ──

def test_focused_prompt_includes_only_named_terms():
    prompt = focused_system_prompt(["quantity"])
    assert "TERM: quantity" in prompt
    assert "TERM: base_five" not in prompt
    assert "TERM: counting" not in prompt


def test_empty_terms_falls_back_to_full_prompt():
    # When no terms detected, the bot still needs vocabulary context
    full = focused_system_prompt([])
    assert "TERM: quantity" in full
    assert "TERM: base_five" in full  # full vocab present


def test_focused_prompt_scales_with_term_count():
    one = focused_system_prompt(["quantity"])
    two = focused_system_prompt(["quantity", "measure"])
    assert len(two) > len(one)
    assert "TERM: measure" in two
    assert "TERM: measure" not in one
