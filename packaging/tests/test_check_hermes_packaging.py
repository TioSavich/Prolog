from __future__ import annotations

import importlib.util
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CHECKER_PATH = ROOT / "packaging" / "check_hermes_packaging.py"


def load_checker():
    spec = importlib.util.spec_from_file_location("check_hermes_packaging", CHECKER_PATH)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_non_git_package_mode_does_not_shell_out_to_git(tmp_path):
    checker = load_checker()

    assert checker.git_check_ignore("n101_bot/logs/results.csv", root=tmp_path) is True
    assert checker.git_check_ignore("n101_bot/README.md", root=tmp_path) is False
    assert checker.tracked_files(root=tmp_path) == []
