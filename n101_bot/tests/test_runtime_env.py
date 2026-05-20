from __future__ import annotations

from bridge.runtime_env import build_runtime_env, runtime_preflight


def test_runtime_env_keeps_temp_and_cache_inside_app(tmp_path):
    env = build_runtime_env(tmp_path, base_env={})

    assert env["HERMES_APP_HOME"] == str(tmp_path)
    assert env["HERMES_TMPDIR"] == str(tmp_path / "runtime" / "tmp")
    assert env["TMPDIR"] == str(tmp_path / "runtime" / "tmp")
    assert env["XDG_CACHE_HOME"] == str(tmp_path / "runtime" / "cache")
    assert env["PYTHONPYCACHEPREFIX"] == str(tmp_path / "runtime" / "pycache")
    assert (tmp_path / "runtime" / "tmp").is_dir()
    assert (tmp_path / "runtime" / "cache").is_dir()
    assert (tmp_path / "runtime" / "pycache").is_dir()


def test_runtime_env_preserves_existing_model_and_reallms_config(tmp_path):
    env = build_runtime_env(
        tmp_path,
        base_env={
            "HERMES_MODEL": "custom-model",
            "REALLMS_BASE_URL": "https://example.test/v1",
            "REALLMS_API_KEY": "secret",
        },
    )

    assert env["HERMES_MODEL"] == "custom-model"
    assert env["REALLMS_BASE_URL"] == "https://example.test/v1"
    assert env["REALLMS_API_KEY"] == "secret"


def test_runtime_env_sets_bundled_swipl_when_present(tmp_path):
    bundled = tmp_path / "runtime" / "swi-prolog" / "bin" / "swipl"
    bundled.parent.mkdir(parents=True)
    bundled.write_text("#!/bin/sh\n", encoding="utf-8")

    env = build_runtime_env(tmp_path, base_env={})

    assert env["HERMES_SWIPL"] == str(bundled)


def test_runtime_env_respects_existing_swipl_override(tmp_path):
    env = build_runtime_env(tmp_path, base_env={"HERMES_SWIPL": "/opt/swi/bin/swipl"})

    assert env["HERMES_SWIPL"] == "/opt/swi/bin/swipl"


def test_runtime_preflight_reports_missing_bundled_swipl_and_local_dirs(tmp_path):
    report = runtime_preflight(tmp_path, base_env={})

    assert report["portable_ready"] is False
    assert report["swipl_source"] == "system"
    assert report["swipl_path"] == "swipl"
    assert report["bundled_swipl"]["path"] == str(
        tmp_path / "runtime" / "swi-prolog" / "bin" / "swipl"
    )
    assert report["bundled_swipl"]["exists"] is False
    assert report["local_runtime_dirs"] is True
    assert report["tmp_dir"] == str(tmp_path / "runtime" / "tmp")
    assert report["cache_dir"] == str(tmp_path / "runtime" / "cache")
    assert report["pycache_dir"] == str(tmp_path / "runtime" / "pycache")


def test_runtime_preflight_reports_bundled_swipl_ready(tmp_path):
    bundled = tmp_path / "runtime" / "swi-prolog" / "bin" / "swipl"
    bundled.parent.mkdir(parents=True)
    bundled.write_text("#!/bin/sh\n", encoding="utf-8")

    report = runtime_preflight(tmp_path, base_env={})

    assert report["portable_ready"] is True
    assert report["swipl_source"] == "bundled"
    assert report["swipl_path"] == str(bundled)
    assert report["bundled_swipl"]["exists"] is True


def test_runtime_preflight_treats_env_pointing_to_bundled_swipl_as_bundled(tmp_path):
    bundled = tmp_path / "runtime" / "swi-prolog" / "bin" / "swipl"
    bundled.parent.mkdir(parents=True)
    bundled.write_text("#!/bin/sh\n", encoding="utf-8")

    report = runtime_preflight(tmp_path, base_env={"HERMES_SWIPL": str(bundled)})

    assert report["portable_ready"] is True
    assert report["swipl_source"] == "bundled"
    assert report["swipl_path"] == str(bundled)


def test_runtime_preflight_reports_env_swipl_override(tmp_path):
    report = runtime_preflight(tmp_path, base_env={"HERMES_SWIPL": "/opt/swi/bin/swipl"})

    assert report["portable_ready"] is False
    assert report["swipl_source"] == "env"
    assert report["swipl_path"] == "/opt/swi/bin/swipl"
