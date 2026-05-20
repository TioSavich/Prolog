from __future__ import annotations

import importlib.util
import tempfile
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


def test_non_git_package_mode_does_not_shell_out_to_git():
    checker = load_checker()

    with tempfile.TemporaryDirectory(dir="/private/tmp") as temp_dir:
        root = Path(temp_dir)

        assert checker.inside_git_worktree(root) is False
        assert checker.git_check_ignore("n101_bot/logs/results.csv", root=root) is True
        assert checker.git_check_ignore("n101_bot/.DS_Store", root=root) is True
        assert checker.git_check_ignore("data/inputs/raw_transcript.txt", root=root) is True
        assert checker.git_check_ignore("runtime/cache/CACHEDIR.TAG", root=root) is True
        assert checker.git_check_ignore("n101_bot/README.md", root=root) is False
        assert checker.tracked_files(root=root) == []
