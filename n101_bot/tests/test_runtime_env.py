from __future__ import annotations

from bridge.runtime_env import build_runtime_env, resolve_umedcta_root, runtime_preflight


def test_resolve_umedcta_root_prefers_environment_override(tmp_path):
    explicit = tmp_path / "custom-formalization"

    assert resolve_umedcta_root(tmp_path / "n101_bot", env={"UMEDCTA_ROOT": str(explicit)}) == explicit


def test_resolve_umedcta_root_prefers_flash_sibling_layout(tmp_path):
    app_root = tmp_path / "Hermes" / "n101_bot"
    expected = tmp_path / "Hermes" / "umedcta-formalization"
    expected.mkdir(parents=True)

    assert resolve_umedcta_root(app_root, env={}) == expected


def test_resolve_umedcta_root_prefers_source_sibling_layout(tmp_path):
    app_root = tmp_path / "Prolog" / "n101_bot"
    expected = tmp_path / "umedcta-formalization"
    expected.mkdir(parents=True)

    assert resolve_umedcta_root(app_root, env={}) == expected


def test_runtime_env_keeps_temp_and_cache_inside_app(tmp_path):
    app_root = tmp_path / "Hermes" / "n101_bot"
    app_root.mkdir(parents=True)

    env = build_runtime_env(app_root, base_env={})

    assert env["HERMES_APP_HOME"] == str(app_root)
    assert env["HERMES_DATA_ROOT"] == str(tmp_path / "Hermes" / "data")
    assert env["HERMES_RUNTIME_ROOT"] == str(tmp_path / "Hermes" / "runtime")
    assert env["HERMES_INPUT_ROOT"] == str(tmp_path / "Hermes" / "data" / "inputs")
    assert env["HERMES_OUTPUT_ROOT"] == str(tmp_path / "Hermes" / "data" / "outputs")
    assert env["HERMES_TMPDIR"] == str(tmp_path / "Hermes" / "runtime" / "tmp")
    assert env["TMPDIR"] == str(tmp_path / "Hermes" / "runtime" / "tmp")
    assert env["XDG_CACHE_HOME"] == str(tmp_path / "Hermes" / "runtime" / "cache")
    assert env["PYTHONPYCACHEPREFIX"] == str(tmp_path / "Hermes" / "runtime" / "pycache")
    assert env["UMEDCTA_ROOT"] == str(resolve_umedcta_root(app_root, env={}))
    assert (tmp_path / "Hermes" / "runtime" / "tmp").is_dir()
    assert (tmp_path / "Hermes" / "runtime" / "cache").is_dir()
    assert (tmp_path / "Hermes" / "runtime" / "pycache").is_dir()
    assert (tmp_path / "Hermes" / "data" / "derived").is_dir()
    assert (tmp_path / "Hermes" / "data" / "outputs").is_dir()
    assert not (tmp_path / "Hermes" / "data" / "inputs").exists()
    assert env["HERMES_MODEL"] == "gemma-4-31B-it"


def test_runtime_env_preserves_existing_model_and_reallms_config(tmp_path):
    env = build_runtime_env(
        tmp_path,
        base_env={
            "HERMES_MODEL": "custom-model",
            "REALLMS_BASE_URL": "https://example.test/v1",
            "REALLMS_API_KEY": "secret",
            "UMEDCTA_ROOT": "/Volumes/Hermes/Hermes/umedcta-formalization",
        },
    )

    assert env["HERMES_MODEL"] == "custom-model"
    assert env["REALLMS_BASE_URL"] == "https://example.test/v1"
    assert env["REALLMS_API_KEY"] == "secret"
    assert env["UMEDCTA_ROOT"] == "/Volumes/Hermes/Hermes/umedcta-formalization"


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
    app_root = tmp_path / "Hermes" / "n101_bot"
    app_root.mkdir(parents=True)

    report = runtime_preflight(app_root, base_env={})

    assert report["portable_ready"] is False
    assert report["swipl_source"] == "system"
    assert report["swipl_path"] == "swipl"
    assert report["bundled_swipl"]["path"] == str(
        app_root / "runtime" / "swi-prolog" / "bin" / "swipl"
    )
    assert report["bundled_swipl"]["exists"] is False
    assert report["local_runtime_dirs"] is True
    assert report["code_data_separated"] is True
    assert report["umedcta_root_source"] == "fallback"
    assert report["tmp_dir"] == str(tmp_path / "Hermes" / "runtime" / "tmp")
    assert report["cache_dir"] == str(tmp_path / "Hermes" / "runtime" / "cache")
    assert report["pycache_dir"] == str(tmp_path / "Hermes" / "runtime" / "pycache")
    assert report["data_root"]["path"] == str(tmp_path / "Hermes" / "data")
    assert report["input_root"]["exists"] is False


def test_runtime_preflight_reports_bundled_swipl_ready(tmp_path):
    app_root = tmp_path / "Hermes" / "n101_bot"
    bundled = app_root / "runtime" / "swi-prolog" / "bin" / "swipl"
    bundled.parent.mkdir(parents=True)
    bundled.write_text("#!/bin/sh\n", encoding="utf-8")

    report = runtime_preflight(app_root, base_env={})

    assert report["portable_ready"] is True
    assert report["swipl_source"] == "bundled"
    assert report["swipl_path"] == str(bundled)
    assert report["bundled_swipl"]["exists"] is True
    assert report["umedcta_root"]["path"] == str(resolve_umedcta_root(app_root, env={}))


def test_runtime_preflight_treats_env_pointing_to_bundled_swipl_as_bundled(tmp_path):
    app_root = tmp_path / "Hermes" / "n101_bot"
    bundled = app_root / "runtime" / "swi-prolog" / "bin" / "swipl"
    bundled.parent.mkdir(parents=True)
    bundled.write_text("#!/bin/sh\n", encoding="utf-8")

    report = runtime_preflight(app_root, base_env={"HERMES_SWIPL": str(bundled)})

    assert report["portable_ready"] is True
    assert report["swipl_source"] == "bundled"
    assert report["swipl_path"] == str(bundled)


def test_runtime_preflight_reports_env_swipl_override(tmp_path):
    report = runtime_preflight(tmp_path, base_env={"HERMES_SWIPL": "/opt/swi/bin/swipl"})

    assert report["portable_ready"] is False
    assert report["swipl_source"] == "env"
    assert report["swipl_path"] == "/opt/swi/bin/swipl"
