from __future__ import annotations

import importlib.util
import stat
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
APP_PATH = ROOT / "packaging" / "create_hermes_app.py"


def load_app_module():
    spec = importlib.util.spec_from_file_location("create_hermes_app", APP_PATH)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_create_app_bundle_writes_double_clickable_launcher(tmp_path):
    module = load_app_module()
    package_root = tmp_path / "Hermes"
    launcher = package_root / "n101_bot" / "scripts" / "launch_hermes.sh"
    icon = package_root / "Hermes_Icon.svg"
    _write(launcher, "#!/usr/bin/env bash\n")
    launcher.chmod(launcher.stat().st_mode | stat.S_IXUSR)
    _write(icon, "<svg xmlns='http://www.w3.org/2000/svg'></svg>\n")

    app_root = module.create_app_bundle(package_root, icon)

    executable = app_root / "Contents" / "MacOS" / "Hermes"
    plist = app_root / "Contents" / "Info.plist"
    resources = app_root / "Contents" / "Resources"
    assert app_root.name == "Hermes.app"
    assert executable.stat().st_mode & stat.S_IXUSR
    assert 'exec "$PACKAGE_ROOT/n101_bot/scripts/launch_hermes.sh"' in executable.read_text(
        encoding="utf-8"
    )
    assert "CFBundleName" in plist.read_text(encoding="utf-8")
    assert "Hermes" in plist.read_text(encoding="utf-8")
    assert (resources / "Hermes_Icon.svg").is_file()


def _write(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")
