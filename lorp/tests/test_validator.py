"""Unit tests for the validator. No model dependency."""

from __future__ import annotations

import pytest

from lorp.pipeline.validator import validate_immanence, validate_syntax


GOOD_PROLOG = """
:- module(list_t, []).
both_quoted_and_unquoted("vp_ch1_sign_sign", "sign").
translates_in_source("Derrida", "expression", "Ausdruck").
"""

BAD_SYNTAX = """
:- module(list_t, [])
both_quoted_and_unquoted("vp_ch1_sign_sign", "sign")
"""  # missing period after module/2 and after the fact

UNSAFE_FIXED_POINT = """
:- module(list_t, []).
fixed_point("trace", trace_function).
"""

UNSAFE_SELF_NAMES = """
:- module(list_t, []).
self_names("sign").
"""

SAFE_COMMENT_WITH_UNSAFE_WORD = """
:- module(list_t, []).
% fixed_point/2 is NOT allowed. This line should be stripped before
% the immanence check, so the predicate below should still pass.
both_quoted_and_unquoted("p", "x").
"""


def test_syntax_good():
    v = validate_syntax(GOOD_PROLOG)
    assert v.ok, v.error_text


def test_syntax_bad():
    v = validate_syntax(BAD_SYNTAX)
    assert not v.ok
    assert v.error_text


def test_immanence_catches_fixed_point():
    v = validate_immanence(UNSAFE_FIXED_POINT)
    assert not v.ok
    assert any("fixed_point" in pat for pat, _ in v.violations)


def test_immanence_catches_self_names():
    v = validate_immanence(UNSAFE_SELF_NAMES)
    assert not v.ok
    assert any("self_names" in pat for pat, _ in v.violations)


def test_immanence_ignores_comments():
    v = validate_immanence(SAFE_COMMENT_WITH_UNSAFE_WORD)
    assert v.ok, v.violations


def test_immanence_good():
    v = validate_immanence(GOOD_PROLOG)
    assert v.ok, v.violations


@pytest.mark.parametrize("pred", [
    "is_trace_function",
    "is_supplement_function",
    "gaifman_structure",
])
def test_immanence_catches_parameterized_unsafe(pred: str):
    src = f":- module(list_t, []).\n{pred}(x).\n"
    v = validate_immanence(src)
    assert not v.ok, f"expected {pred} to be flagged"
