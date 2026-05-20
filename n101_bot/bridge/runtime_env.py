"""Runtime environment helpers for portable Hermes launches."""
from __future__ import annotations

import os
from pathlib import Path
from typing import Mapping

from .path_contract import resolve_path_contract

DEFAULT_ROOT = Path(__file__).resolve().parent.parent


def bundled_swipl_path(root: Path | str = DEFAULT_ROOT) -> Path:
    contract = resolve_path_contract(Path(root), env={})
    return contract.bundled_runtime_root / "swi-prolog" / "bin" / "swipl"


def resolve_umedcta_root(
    root: Path | str = DEFAULT_ROOT,
    *,
    env: Mapping[str, str] | None = None,
) -> Path:
    app_root = Path(root)
    source_env = os.environ if env is None else env
    env_root = source_env.get("UMEDCTA_ROOT")
    if env_root:
        return Path(env_root)

    candidates = [
        app_root.parent / "umedcta-formalization",
        app_root.parent.parent / "umedcta-formalization",
    ]
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return app_root.parent.parent / "umedcta-formalization"


def umedcta_root_source(
    root: Path | str = DEFAULT_ROOT,
    *,
    env: Mapping[str, str] | None = None,
) -> str:
    app_root = Path(root)
    source_env = os.environ if env is None else env
    if source_env.get("UMEDCTA_ROOT"):
        return "env"
    if (app_root.parent / "umedcta-formalization").exists():
        return "package_sibling"
    if (app_root.parent.parent / "umedcta-formalization").exists():
        return "source_sibling"
    return "fallback"


def resolve_swipl(
    swipl: str | None = None,
    *,
    root: Path | str = DEFAULT_ROOT,
    env: Mapping[str, str] | None = None,
) -> str:
    if swipl:
        return swipl
    source_env = os.environ if env is None else env
    env_swipl = source_env.get("HERMES_SWIPL")
    if env_swipl:
        return env_swipl
    bundled = bundled_swipl_path(root)
    if bundled.exists():
        return str(bundled)
    return "swipl"


def build_runtime_env(root: Path | str, *, base_env: Mapping[str, str] | None = None) -> dict[str, str]:
    app_root = Path(root)
    env = dict(os.environ if base_env is None else base_env)
    contract = resolve_path_contract(app_root, env=env)
    contract.ensure_writable_roots()

    env["HERMES_APP_HOME"] = str(app_root)
    env["HERMES_DATA_ROOT"] = str(contract.data_root)
    env["HERMES_INPUT_ROOT"] = str(contract.input_root)
    env["HERMES_DERIVED_ROOT"] = str(contract.derived_root)
    env["HERMES_OUTPUT_ROOT"] = str(contract.output_root)
    env["HERMES_RUNTIME_ROOT"] = str(contract.runtime_root)
    env["HERMES_TMPDIR"] = str(contract.tmp_dir)
    env["TMPDIR"] = str(contract.tmp_dir)
    env["XDG_CACHE_HOME"] = str(contract.cache_dir)
    env["PYTHONPYCACHEPREFIX"] = str(contract.pycache_dir)
    env.setdefault("UMEDCTA_ROOT", str(resolve_umedcta_root(app_root, env=env)))
    env.setdefault("HERMES_MODEL", env.get("REALLMS_MODEL", "gemma-4-31B-it"))

    bundled_swipl = bundled_swipl_path(app_root)
    if "HERMES_SWIPL" not in env and bundled_swipl.exists():
        env["HERMES_SWIPL"] = str(bundled_swipl)

    return env


def runtime_preflight(root: Path | str, *, base_env: Mapping[str, str] | None = None) -> dict[str, object]:
    app_root = Path(root)
    incoming_env = dict(os.environ if base_env is None else base_env)
    contract = resolve_path_contract(app_root, env=incoming_env)
    env = build_runtime_env(app_root, base_env=incoming_env)
    bundled_swipl = bundled_swipl_path(app_root)

    if incoming_env.get("HERMES_SWIPL") == str(bundled_swipl):
        swipl_source = "bundled"
        swipl_path = str(bundled_swipl)
    elif "HERMES_SWIPL" in incoming_env:
        swipl_source = "env"
        swipl_path = incoming_env["HERMES_SWIPL"]
    elif bundled_swipl.exists():
        swipl_source = "bundled"
        swipl_path = str(bundled_swipl)
    else:
        swipl_source = "system"
        swipl_path = "swipl"

    runtime_dirs = [
        Path(env["HERMES_TMPDIR"]),
        Path(env["XDG_CACHE_HOME"]),
        Path(env["PYTHONPYCACHEPREFIX"]),
    ]
    local_runtime_dirs = all(
        directory.exists() and directory.is_relative_to(contract.runtime_root)
        for directory in runtime_dirs
    )

    report = {
        "portable_ready": swipl_source == "bundled" and local_runtime_dirs,
        "swipl_source": swipl_source,
        "swipl_path": swipl_path,
        "bundled_swipl": {
            "path": str(bundled_swipl),
            "exists": bundled_swipl.exists(),
        },
        "local_runtime_dirs": local_runtime_dirs,
        "tmp_dir": env["HERMES_TMPDIR"],
        "cache_dir": env["XDG_CACHE_HOME"],
        "pycache_dir": env["PYTHONPYCACHEPREFIX"],
        "umedcta_root": {
            "path": env["UMEDCTA_ROOT"],
            "exists": Path(env["UMEDCTA_ROOT"]).exists(),
        },
        "umedcta_root_source": umedcta_root_source(app_root, env=incoming_env),
    }
    report.update(contract.preflight_payload())
    return report
