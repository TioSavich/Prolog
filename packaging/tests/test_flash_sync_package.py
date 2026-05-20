from __future__ import annotations

import json
import importlib.util
import stat
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SYNC_PATH = ROOT / "packaging" / "sync_flash_package.py"


def load_sync_module():
    spec = importlib.util.spec_from_file_location("sync_flash_package", SYNC_PATH)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_flash_sync_manifest_keeps_inputs_and_runtime_payloads_out():
    sync = load_sync_module()
    manifest = sync.load_manifest(sync.DEFAULT_MANIFEST)

    assert "data/inputs" in manifest["never_create_dirs"]
    assert "data/derived" in manifest["ensure_dirs"]
    assert "data/outputs" in manifest["ensure_dirs"]
    assert "runtime/tmp" in manifest["ensure_dirs"]
    assert "n101_bot/bridge/path_contract.py" in manifest["required_files"]
    assert "n101_bot/bridge/fake_demo_run.py" in manifest["required_files"]
    assert "n101_bot/scripts/console.sh" in manifest["required_files"]
    assert "n101_bot/scripts/ralph_verify.sh" in manifest["required_files"]
    assert "n101_bot/scripts/fake_demo_run.sh" in manifest["required_files"]
    assert "n101_bot/samples/fake_demo_events.json" in manifest["required_files"]
    assert "n101_bot/tests/test_fake_demo_run.py" in manifest["required_files"]


def test_plan_sync_files_excludes_untracked_like_data_and_cache_payloads(tmp_path):
    sync = load_sync_module()
    source = tmp_path / "source"
    source.mkdir()
    _write(source / "n101_bot" / "bridge" / "path_contract.py", "contract")
    _write(source / "n101_bot" / "runtime" / "README.md", "runtime docs")
    _write(source / "n101_bot" / "runtime" / "tmp" / "cache.txt", "cache")
    _write(source / "data" / "inputs" / "raw.txt", "student work")
    _write(source / "runtime" / "cache" / "CACHEDIR.TAG", "cache")
    _write(source / ".DS_Store", "metadata")

    manifest = {
        "include_roots": ["n101_bot"],
        "exclude_patterns": [
            "data/**",
            "runtime/**",
            "n101_bot/runtime/**",
            "n101_bot/**/.DS_Store",
            ".DS_Store",
        ],
        "include_overrides": ["n101_bot/runtime/README.md"],
        "required_files": ["n101_bot/bridge/path_contract.py"],
        "ensure_dirs": [],
        "never_create_dirs": ["data/inputs"],
    }

    planned = sync.plan_sync_files(source, manifest)

    assert "n101_bot/bridge/path_contract.py" in planned
    assert "n101_bot/runtime/README.md" in planned
    assert "n101_bot/runtime/tmp/cache.txt" not in planned
    assert "data/inputs/raw.txt" not in planned
    assert "runtime/cache/CACHEDIR.TAG" not in planned
    assert ".DS_Store" not in planned


def test_sync_flash_package_copies_files_preserves_mode_and_creates_only_safe_dirs(tmp_path):
    sync = load_sync_module()
    source = tmp_path / "source"
    dest = tmp_path / "dest"
    source.mkdir()
    executable = source / "n101_bot" / "scripts" / "console.sh"
    _write(source / "n101_bot" / "bridge" / "path_contract.py", "contract")
    _write(executable, "#!/usr/bin/env bash\n")
    executable.chmod(executable.stat().st_mode | stat.S_IXUSR)

    manifest = {
        "include_roots": ["n101_bot"],
        "exclude_patterns": [],
        "include_overrides": [],
        "required_files": [
            "n101_bot/bridge/path_contract.py",
            "n101_bot/scripts/console.sh",
        ],
        "ensure_dirs": ["data/derived", "data/outputs", "runtime/tmp"],
        "never_create_dirs": ["data/inputs"],
    }

    result = sync.sync_flash_package(source, dest, manifest)

    assert result["copied"] == [
        "n101_bot/bridge/path_contract.py",
        "n101_bot/scripts/console.sh",
    ]
    assert (dest / "n101_bot" / "bridge" / "path_contract.py").read_text() == "contract"
    assert (dest / "n101_bot" / "scripts" / "console.sh").stat().st_mode & stat.S_IXUSR
    assert (dest / "data" / "derived").is_dir()
    assert (dest / "data" / "outputs").is_dir()
    assert (dest / "runtime" / "tmp").is_dir()
    assert not (dest / "data" / "inputs").exists()


def test_manifest_json_is_valid():
    sync = load_sync_module()
    payload = json.loads(sync.DEFAULT_MANIFEST.read_text(encoding="utf-8"))

    assert payload["version"] == 1
    assert isinstance(payload["include_roots"], list)
    assert isinstance(payload["exclude_patterns"], list)


def _write(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")
