"""Tests for the LQL emitter output — shape, edge count, parseability."""
from __future__ import annotations

from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parent.parent
LQL_PATH = ROOT / "logs" / "vocabulary.lql"
JSON_PATH = ROOT / "logs" / "vocabulary.json"


@pytest.fixture(scope="module")
def lql_text():
    assert LQL_PATH.exists(), f"{LQL_PATH} missing — run bridge.prepare"
    return LQL_PATH.read_text()


def test_lql_starts_with_patch_block(lql_text):
    assert 'BEGIN PATCH "n101-vocabulary.vlp";' in lql_text
    assert lql_text.strip().endswith("SAVE PATCH;")


def test_lql_has_has_definition_edges(lql_text):
    # Every term should have at least one has_definition edge.
    count = lql_text.count('"has_definition"')
    assert count >= 10, f"expected >=10 has_definition edges, got {count}"


def test_lql_has_incompatible_with_edges(lql_text):
    count = lql_text.count('"incompatible_with"')
    # At least one per term; most have multiple.
    assert count >= 10


def test_lql_references_every_term(lql_text):
    import json
    vocab = json.loads(JSON_PATH.read_text())
    for term in vocab:
        name = term["name"]
        # The name appears as an entity in at least one INSERT.
        assert f'"{name}"' in lql_text, f"term {name} not in LQL output"


def test_lql_confidence_annotations(lql_text):
    # Every INSERT should carry a CONFIDENCE value.
    inserts = [ln for ln in lql_text.splitlines() if ln.strip().startswith("INSERT")]
    assert inserts, "no INSERT statements found"
    for line in inserts:
        assert "CONFIDENCE" in line, f"INSERT without CONFIDENCE: {line}"
