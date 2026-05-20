"""Runtime environment helpers for portable Hermes launches."""
from __future__ import annotations

import os
from pathlib import Path
from typing import Mapping

DEFAULT_ROOT = Path(__file__).resolve().parent.parent


def bundled_swipl_path(root: Path | str = DEFAULT_ROOT) -> Path:
    return Path(root) / "runtime" / "swi-prolog" / "bin" / "swipl"


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
    runtime = app_root / "runtime"
    tmp = runtime / "tmp"
    cache = runtime / "cache"
    pycache = runtime / "pycache"
    for directory in (tmp, cache, pycache):
        directory.mkdir(parents=True, exist_ok=True)

    env["HERMES_APP_HOME"] = str(app_root)
    env["HERMES_TMPDIR"] = str(tmp)
    env["TMPDIR"] = str(tmp)
    env["XDG_CACHE_HOME"] = str(cache)
    env["PYTHONPYCACHEPREFIX"] = str(pycache)
    env.setdefault("HERMES_MODEL", "gemma:2b")

    bundled_swipl = bundled_swipl_path(app_root)
    if "HERMES_SWIPL" not in env and bundled_swipl.exists():
        env["HERMES_SWIPL"] = str(bundled_swipl)

    return env


def runtime_preflight(root: Path | str, *, base_env: Mapping[str, str] | None = None) -> dict[str, object]:
    app_root = Path(root)
    incoming_env = dict(os.environ if base_env is None else base_env)
    env = build_runtime_env(app_root, base_env=incoming_env)
    runtime = app_root / "runtime"
    bundled_swipl = bundled_swipl_path(app_root)

    if "HERMES_SWIPL" in incoming_env:
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
        directory.exists() and directory.is_relative_to(runtime)
        for directory in runtime_dirs
    )

    return {
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
    }
