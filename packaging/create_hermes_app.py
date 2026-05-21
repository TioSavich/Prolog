#!/usr/bin/env python3
"""Create a double-clickable macOS Hermes.app wrapper for a package root."""
from __future__ import annotations

import argparse
import shutil
import stat
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_PACKAGE_ROOT = ROOT


def create_app_bundle(package_root: Path | str, icon_path: Path | str | None = None) -> Path:
    package = Path(package_root).resolve()
    launcher = package / "n101_bot" / "scripts" / "launch_hermes.sh"
    if not launcher.is_file():
        raise FileNotFoundError(f"missing Hermes launcher: {launcher}")

    icon = Path(icon_path).resolve() if icon_path else package / "Hermes_Icon.svg"
    app_root = package / "Hermes.app"
    contents = app_root / "Contents"
    macos = contents / "MacOS"
    resources = contents / "Resources"
    macos.mkdir(parents=True, exist_ok=True)
    resources.mkdir(parents=True, exist_ok=True)

    _write_executable(macos / "Hermes")
    _write_plist(contents / "Info.plist")
    if icon.is_file():
        shutil.copy2(icon, resources / "Hermes_Icon.svg")
        _try_build_icns(icon, resources / "Hermes.icns")
    return app_root


def _write_executable(path: Path) -> None:
    path.write_text(
        """#!/usr/bin/env bash
set -euo pipefail

APP_BUNDLE="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PACKAGE_ROOT="$(cd "$APP_BUNDLE/.." && pwd)"
exec "$PACKAGE_ROOT/n101_bot/scripts/launch_hermes.sh" "$@"
""",
        encoding="utf-8",
    )
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)


def _write_plist(path: Path) -> None:
    path.write_text(
        """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleDevelopmentRegion</key>
  <string>en</string>
  <key>CFBundleExecutable</key>
  <string>Hermes</string>
  <key>CFBundleIconFile</key>
  <string>Hermes.icns</string>
  <key>CFBundleIdentifier</key>
  <string>local.hermes.console</string>
  <key>CFBundleName</key>
  <string>Hermes</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>CFBundleShortVersionString</key>
  <string>0.1.0</string>
  <key>LSMinimumSystemVersion</key>
  <string>12.0</string>
</dict>
</plist>
""",
        encoding="utf-8",
    )


def _try_build_icns(icon: Path, output: Path) -> None:
    if not shutil.which("iconutil") or not shutil.which("sips"):
        return
    with tempfile.TemporaryDirectory(prefix="hermes-icon-") as tmp_dir:
        tmp = Path(tmp_dir)
        base_png = _base_png(icon, tmp)
        if not base_png:
            return
        iconset = tmp / "Hermes.iconset"
        iconset.mkdir()
        for logical_size in (16, 32, 128, 256, 512):
            _sips_resize(base_png, iconset / f"icon_{logical_size}x{logical_size}.png", logical_size)
            _sips_resize(
                base_png,
                iconset / f"icon_{logical_size}x{logical_size}@2x.png",
                logical_size * 2,
            )
        subprocess.run(
            ["iconutil", "-c", "icns", str(iconset), "-o", str(output)],
            check=False,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )


def _base_png(icon: Path, tmp: Path) -> Path | None:
    if icon.suffix.lower() == ".png":
        return icon
    if icon.suffix.lower() == ".svg" and shutil.which("qlmanage"):
        before = set(tmp.glob("*.png"))
        subprocess.run(
            ["qlmanage", "-t", "-s", "1024", "-o", str(tmp), str(icon)],
            check=False,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        after = set(tmp.glob("*.png"))
        new_pngs = sorted(after - before)
        if new_pngs:
            return new_pngs[0]
    return None


def _sips_resize(source: Path, target: Path, size: int) -> None:
    subprocess.run(
        ["sips", "-z", str(size), str(size), str(source), "--out", str(target)],
        check=False,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--package-root", type=Path, default=DEFAULT_PACKAGE_ROOT)
    parser.add_argument("--icon", type=Path, default=None)
    args = parser.parse_args(argv)

    app_root = create_app_bundle(args.package_root, args.icon)
    print(app_root)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
