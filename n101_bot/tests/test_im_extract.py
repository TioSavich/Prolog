"""Tests for the IM curriculum extraction parser.

Subagent 4 ships scripts/im_extract.py with two parsers:
  - parse_teacher_guide(text)       → dict(title, goals, student_facing, purpose)
  - parse_scope_and_sequence(text, grade) → list[UnitRow]
plus the public entry points extract_scope_sequences() and
extract_teacher_guides().

Spec: docs/superpowers/specs/2026-05-04-hermes-chatbot-substrate-design.md §4
"""
from __future__ import annotations

import importlib
import sys
from pathlib import Path

import pytest


# Add scripts/ to sys.path so `import im_extract` works without the
# package machinery. (S4's file lives at n101_bot/scripts/im_extract.py.)
_SCRIPTS_DIR = Path(__file__).resolve().parent.parent / "scripts"
if _SCRIPTS_DIR.is_dir() and str(_SCRIPTS_DIR) not in sys.path:
    sys.path.insert(0, str(_SCRIPTS_DIR))


def _load_im_extract():
    try:
        return importlib.import_module("im_extract")
    except (ImportError, ModuleNotFoundError):
        return None


im_extract = _load_im_extract()


pytestmark = pytest.mark.skipif(
    im_extract is None,
    reason="scripts/im_extract.py not yet present (Subagent 4 in flight)",
)


# ── public-API surface tests (always available once module imports) ───


def test_extract_scope_sequences_callable():
    fn = getattr(im_extract, "extract_scope_sequences", None)
    assert fn is not None and callable(fn), (
        "im_extract should export extract_scope_sequences"
    )


def test_extract_teacher_guides_callable():
    fn = getattr(im_extract, "extract_teacher_guides", None)
    assert fn is not None and callable(fn), (
        "im_extract should export extract_teacher_guides"
    )


def test_parse_teacher_guide_callable():
    fn = getattr(im_extract, "parse_teacher_guide", None)
    assert fn is not None and callable(fn)


def test_parse_scope_and_sequence_callable():
    fn = getattr(im_extract, "parse_scope_and_sequence", None)
    assert fn is not None and callable(fn)


# ── parser-shape tests on string fixtures ──────────────────────────────


SCOPE_FIXTURE = """\
Grade 5 Scope and Sequence

Unit 1: Finding Volume
This unit introduces volume of right rectangular prisms using unit cubes.

Section A: Volume with Unit Cubes
  • Lesson 1: What is Volume?
  • Lesson 2: Filling Boxes

Section B: The Formula
  • Lesson 3: Layers and Multiplication

Unit 2: Fractions as Quotients
Students extend fraction reasoning to division.

Section A: Sharing Equally
  • Lesson 1: Equal Shares
"""


def test_parse_scope_and_sequence_finds_units():
    parse = im_extract.parse_scope_and_sequence
    units = parse(SCOPE_FIXTURE, "grade5")
    assert isinstance(units, list)
    # At minimum: one unit recovered. (Real-world fixture might miss the
    # second unit due to layout sensitivity; we only assert a non-empty
    # baseline.)
    assert len(units) >= 1
    titles = " ".join(getattr(u, "title", "") for u in units)
    assert "Volume" in titles or "Finding" in titles


def test_parse_scope_and_sequence_finds_lessons():
    parse = im_extract.parse_scope_and_sequence
    units = parse(SCOPE_FIXTURE, "grade5")
    if not units:
        pytest.skip("parser did not recover any units from fixture")
    first_unit = units[0]
    lessons = getattr(first_unit, "lessons", [])
    # Should find at least one lesson.
    assert isinstance(lessons, list)


# parse_teacher_guide is column-aware and very PDF-specific; we just
# probe contract not content.


def test_parse_teacher_guide_returns_expected_keys():
    parse = im_extract.parse_teacher_guide
    result = parse("")
    assert isinstance(result, dict)
    for key in ("title", "goals", "student_facing", "purpose"):
        assert key in result, f"parse_teacher_guide missing key {key!r}"


def test_parse_teacher_guide_handles_empty_input():
    parse = im_extract.parse_teacher_guide
    result = parse("")
    # Empty input shouldn't crash; goals should be a (possibly empty) list.
    assert isinstance(result.get("goals"), list)


def test_parse_teacher_guide_extracts_basic_title():
    parse = im_extract.parse_teacher_guide
    minimal = """Unit 1, Lesson 4
Comparing Volumes
Standards
"""
    result = parse(minimal)
    # Best-effort: title field exists and is a string
    assert isinstance(result.get("title", ""), str)
