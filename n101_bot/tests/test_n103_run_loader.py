from __future__ import annotations

import json

import pytest

from bridge.n103_run_loader import load_safe_runs


def test_load_safe_runs_reads_pairings_safe_json_without_pseudonym_map(tmp_path):
    run_dir = tmp_path / "2026-05-20_prompt"
    run_dir.mkdir()
    (run_dir / "pairings_safe.json").write_text(
        json.dumps(
            {
                "pseudonym_map": {"real-student-id": "P01"},
                "research_safe_pairings": [
                    {
                        "pair_id": "pair_1",
                        "students": ["P01", "P02"],
                        "reason": "contrasting hierarchy reasoning",
                        "prompt": "Ask about the definition boundary.",
                    }
                ],
                "coverage": {"paired": 2, "unpaired": 0},
                "needs_instructor_check": [{"pseudonym": "P03", "reason": "missing response"}],
            }
        ),
        encoding="utf-8",
    )

    packet = load_safe_runs(tmp_path)

    assert packet["source"] == str(tmp_path)
    assert packet["run_count"] == 1
    run = packet["runs"][0]
    assert run["run_id"] == "2026-05-20_prompt"
    assert run["pairing_count"] == 1
    assert run["coverage"] == {"paired": 2, "unpaired": 0}
    assert run["needs_instructor_check_count"] == 1
    assert "pseudonym_map" not in json.dumps(run)
    assert run["research_safe_pairings"][0]["students"] == ["P01", "P02"]


def test_load_safe_runs_rejects_raw_student_work_keys(tmp_path):
    run_dir = tmp_path / "unsafe"
    run_dir.mkdir()
    (run_dir / "pairings_safe.json").write_text(
        json.dumps(
            {
                "research_safe_pairings": [
                    {"pair_id": "bad", "raw_text": "student wrote this"}
                ],
            }
        ),
        encoding="utf-8",
    )

    with pytest.raises(ValueError, match="raw_text"):
        load_safe_runs(tmp_path)


def test_load_safe_runs_rejects_identifier_maps_inside_safe_outputs(tmp_path):
    run_dir = tmp_path / "unsafe"
    run_dir.mkdir()
    (run_dir / "pairings_safe.json").write_text(
        json.dumps(
            {
                "research_safe_pairings": [
                    {"pair_id": "bad", "pseudonym_map": {"real-student-id": "P01"}}
                ],
            }
        ),
        encoding="utf-8",
    )

    with pytest.raises(ValueError, match="pseudonym_map"):
        load_safe_runs(tmp_path)


def test_load_safe_runs_returns_empty_packet_when_no_safe_outputs(tmp_path):
    assert load_safe_runs(tmp_path) == {
        "source": str(tmp_path),
        "run_count": 0,
        "runs": [],
    }
