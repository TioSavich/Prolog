from __future__ import annotations

from pathlib import Path

import pytest

from bridge.path_contract import resolve_path_contract


def test_path_contract_defaults_to_package_sibling_roots_for_app_package(tmp_path):
    app_root = tmp_path / "Hermes" / "n101_bot"
    app_root.mkdir(parents=True)

    contract = resolve_path_contract(app_root, env={})

    assert contract.app_root == app_root
    assert contract.package_root == tmp_path / "Hermes"
    assert contract.data_root == tmp_path / "Hermes" / "data"
    assert contract.input_root == tmp_path / "Hermes" / "data" / "inputs"
    assert contract.derived_root == tmp_path / "Hermes" / "data" / "derived"
    assert contract.output_root == tmp_path / "Hermes" / "data" / "outputs"
    assert contract.runtime_root == tmp_path / "Hermes" / "runtime"
    assert contract.tmp_dir == tmp_path / "Hermes" / "runtime" / "tmp"
    assert contract.cache_dir == tmp_path / "Hermes" / "runtime" / "cache"
    assert contract.pycache_dir == tmp_path / "Hermes" / "runtime" / "pycache"
    assert contract.bundled_runtime_root == app_root / "runtime"


def test_path_contract_honors_data_and_runtime_overrides(tmp_path):
    app_root = tmp_path / "Hermes" / "n101_bot"
    app_root.mkdir(parents=True)
    data_root = tmp_path / "external-data"
    runtime_root = tmp_path / "external-runtime"

    contract = resolve_path_contract(
        app_root,
        env={
            "HERMES_DATA_ROOT": str(data_root),
            "HERMES_RUNTIME_ROOT": str(runtime_root),
        },
    )

    assert contract.data_root == data_root
    assert contract.input_root == data_root / "inputs"
    assert contract.output_root == data_root / "outputs"
    assert contract.runtime_root == runtime_root
    assert contract.tmp_dir == runtime_root / "tmp"
    assert contract.cache_dir == runtime_root / "cache"


def test_path_contract_rejects_data_root_inside_code_root(tmp_path):
    app_root = tmp_path / "Hermes" / "n101_bot"
    app_root.mkdir(parents=True)

    with pytest.raises(ValueError, match="HERMES_DATA_ROOT must not live inside app code root"):
        resolve_path_contract(
            app_root,
            env={"HERMES_DATA_ROOT": str(app_root / "inputs")},
        )


def test_path_contract_creates_runtime_and_output_roots_but_not_input_root(tmp_path):
    app_root = tmp_path / "Hermes" / "n101_bot"
    app_root.mkdir(parents=True)
    contract = resolve_path_contract(app_root, env={})

    contract.ensure_writable_roots()

    assert contract.tmp_dir.is_dir()
    assert contract.cache_dir.is_dir()
    assert contract.pycache_dir.is_dir()
    assert contract.derived_root.is_dir()
    assert contract.output_root.is_dir()
    assert not contract.input_root.exists()


def test_path_contract_preflight_payload_is_safe_and_relative(tmp_path):
    app_root = tmp_path / "Hermes" / "n101_bot"
    app_root.mkdir(parents=True)
    contract = resolve_path_contract(app_root, env={})
    payload = contract.preflight_payload()

    assert payload["data_root"]["path"] == str(tmp_path / "Hermes" / "data")
    assert payload["input_root"]["path"] == str(tmp_path / "Hermes" / "data" / "inputs")
    assert payload["input_root"]["exists"] is False
    assert payload["runtime_root"]["path"] == str(tmp_path / "Hermes" / "runtime")
    assert payload["code_data_separated"] is True
