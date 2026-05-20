"""Sanity check: Python validator's findings should mirror Prolog's."""
from __future__ import annotations

import subprocess
from pathlib import Path

import pytest

from bridge.validator import load_vocabulary, validate


ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "src" / "vocabulary.pl"
VOCAB = ROOT / "logs" / "vocabulary.json"


def prolog_validate(text: str) -> int:
    """Call swipl and return the hit count."""
    escaped = text.replace("'", "\\'")
    goal = (
        f"use_module('{SRC}'), "
        f"validate_response('{escaped}', _V, N), "
        f"format('~w', [N]), halt."
    )
    proc = subprocess.run(
        ["swipl", "-q", "-g", goal, "-g", "halt(1)"],
        capture_output=True,
        text=True,
        cwd=ROOT,
    )
    if proc.returncode != 0:
        pytest.fail(f"swipl failed: {proc.stderr}")
    return int(proc.stdout.strip())


@pytest.fixture(scope="module")
def vocabulary():
    assert VOCAB.exists(), "run bridge.prepare first"
    return load_vocabulary(VOCAB)


@pytest.mark.parametrize(
    "text",
    [
        "A quantity is a measurable property of an object.",
        "Because it is easier, we add ones first.",
        "Both students got 43 so they used the same strategy.",
        "In base five the digit 5 represents the base.",
    ],
)
def test_python_and_prolog_agree(text, vocabulary):
    py_hits = len(validate(text, vocabulary))
    pl_hits = prolog_validate(text)
    assert py_hits == pl_hits, (
        f"mismatch on {text!r}: python={py_hits}, prolog={pl_hits}"
    )
