"""Tests for the Prolog move grammar surfaced via bridge.prolog.move_for."""
from bridge.prolog import move_for


# ── classifier branches ──

def test_bare_arithmetic_is_fmst():
    m = move_for("5 + 8 =")
    assert m.kind == "arithmetic_computation"
    assert m.move_tag == "FMST"
    assert m.assessing is True
    assert m.class_info["strategy_stated"] is False
    assert m.class_info["operands"] == [5, 8]


def test_arithmetic_with_making_ten_is_lst():
    m = move_for("5 + 8 = 13 because I made a ten")
    assert m.kind == "arithmetic_computation"
    assert m.move_tag == "LST"
    assert m.assessing is False
    assert m.class_info["strategy"] == "making_ten"


def test_arithmetic_with_counting_on_is_lst():
    m = move_for("5 + 8 = 13, I counted on from 8")
    assert m.move_tag == "LST"
    assert m.class_info["strategy"] == "counting_on"


def test_vocabulary_question_is_aqst():
    m = move_for("What is a quantity?")
    assert m.kind == "vocabulary_question"
    assert m.move_tag == "AQST"
    assert m.assessing is False  # advancing, not assessing
    assert m.class_info["term"].strip() == "quantity"


def test_strategy_report_is_fmst():
    # teacher reporting a student's work — probe what they notice
    m = move_for("Jasmine says a number is a quantity")
    assert m.kind == "strategy_report"
    assert m.move_tag == "FMST"
    assert m.assessing is True


def test_unclear_prompt_defaults_to_fmst():
    m = move_for("Hello")
    assert m.kind == "unclear"
    assert m.move_tag == "FMST"


# ── template rendering ──

def test_assessing_template_has_slot_fills():
    m = move_for("5 + 8 =")
    rendered = m.rendered_template()
    # '{{operands}}' placeholder should have been filled from classifier output
    assert "{{" not in rendered
    assert "5 and 8" in rendered


def test_advancing_template_names_strategy():
    m = move_for("5 + 8 = 13 because I made a ten")
    rendered = m.rendered_template()
    assert "making_ten" in rendered
    assert "{{" not in rendered


def test_assessing_templates_forbid_answer():
    # Every FMST template should tell the LLM not to hand over the answer
    bare = move_for("5 + 8 =").rendered_template().lower()
    assert "no answer" in bare or "don't" in bare or "do not" in bare or "without" in bare
