"""Path contract for portable Hermes runtime packages."""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Mapping


DEFAULT_APP_ROOT = Path(__file__).resolve().parent.parent


@dataclass(frozen=True)
class HermesPathContract:
    app_root: Path
    package_root: Path
    data_root: Path
    input_root: Path
    derived_root: Path
    output_root: Path
    runtime_root: Path
    tmp_dir: Path
    cache_dir: Path
    pycache_dir: Path
    bundled_runtime_root: Path

    def ensure_writable_roots(self) -> None:
        for directory in (
            self.tmp_dir,
            self.cache_dir,
            self.pycache_dir,
            self.derived_root,
            self.output_root,
        ):
            directory.mkdir(parents=True, exist_ok=True)

    def preflight_payload(self) -> dict[str, object]:
        return {
            "data_root": _path_payload(self.data_root),
            "input_root": _path_payload(self.input_root),
            "derived_root": _path_payload(self.derived_root),
            "output_root": _path_payload(self.output_root),
            "runtime_root": _path_payload(self.runtime_root),
            "runtime_tmp_dir": _path_payload(self.tmp_dir),
            "runtime_cache_dir": _path_payload(self.cache_dir),
            "runtime_pycache_dir": _path_payload(self.pycache_dir),
            "bundled_runtime_root": _path_payload(self.bundled_runtime_root),
            "code_data_separated": not _is_relative_to(self.data_root, self.app_root),
        }


def resolve_path_contract(
    app_root: Path | str = DEFAULT_APP_ROOT,
    *,
    env: Mapping[str, str] | None = None,
) -> HermesPathContract:
    root = Path(app_root)
    source_env = {} if env is None else env
    package_root = root.parent if root.name == "n101_bot" else root
    explicit_data_root = source_env.get("HERMES_DATA_ROOT")
    data_root = Path(explicit_data_root or package_root / "data")
    runtime_root = Path(source_env.get("HERMES_RUNTIME_ROOT") or package_root / "runtime")

    if explicit_data_root and _is_relative_to(data_root, root):
        raise ValueError("HERMES_DATA_ROOT must not live inside app code root")

    return HermesPathContract(
        app_root=root,
        package_root=package_root,
        data_root=data_root,
        input_root=data_root / "inputs",
        derived_root=data_root / "derived",
        output_root=data_root / "outputs",
        runtime_root=runtime_root,
        tmp_dir=runtime_root / "tmp",
        cache_dir=runtime_root / "cache",
        pycache_dir=runtime_root / "pycache",
        bundled_runtime_root=root / "runtime",
    )


def _path_payload(path: Path) -> dict[str, object]:
    return {
        "path": str(path),
        "exists": path.exists(),
    }


def _is_relative_to(path: Path, parent: Path) -> bool:
    try:
        path.resolve().relative_to(parent.resolve())
    except ValueError:
        return False
    return True
