from __future__ import annotations

from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
CONSOLE = ROOT / "scripts" / "console.sh"
LAUNCH = ROOT / "scripts" / "launch_hermes.sh"
RALPH = ROOT / "scripts" / "ralph_verify.sh"


def _read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def test_console_script_delegates_to_canonical_launcher():
    script = _read(CONSOLE)

    assert 'exec "$ROOT/scripts/launch_hermes.sh" "$@"' in script


def test_launch_script_resolves_python_for_flash_package():
    script = _read(LAUNCH)

    assert "HERMES_PYTHON" in script
    assert 'PYTHON_BIN="$HERMES_PYTHON"' in script
    assert 'PYTHON_BIN="$ROOT/.venv/bin/python"' in script
    assert 'PYTHON_BIN="python3"' in script
    assert 'PACKAGE_ROOT="$(cd "$ROOT/.." && pwd)"' in script
    assert 'FLASH_ROOT="$(cd "$PACKAGE_ROOT/.." && pwd)"' in script
    assert 'for env_file in "$PACKAGE_ROOT/.env" "$FLASH_ROOT/.env" "$ROOT/.env"; do' in script
    assert 'export HERMES_DATA_ROOT="${HERMES_DATA_ROOT:-$PACKAGE_ROOT/data}"' in script
    assert 'export HERMES_RUNTIME_ROOT="${HERMES_RUNTIME_ROOT:-$PACKAGE_ROOT/runtime}"' in script
    assert 'export PYTHONPATH="$ROOT:$ROOT/vendor${PYTHONPATH:+:$PYTHONPATH}"' in script
    assert "HERMES_NO_OPEN" in script
    assert 'open "$URL"' in script
    assert 'exec "$PYTHON_BIN" -m bridge.hermes_console_server' in script


def test_ralph_verify_script_resolves_python_for_flash_package():
    script = _read(RALPH)

    assert "HERMES_PYTHON" in script
    assert 'PYTHON_BIN="$HERMES_PYTHON"' in script
    assert 'PYTHON_BIN="$ROOT/.venv/bin/python"' in script
    assert 'PYTHON_BIN="python3"' in script
    assert 'PACKAGE_ROOT="$(cd "$ROOT/.." && pwd)"' in script
    assert 'export HERMES_DATA_ROOT="${HERMES_DATA_ROOT:-$PACKAGE_ROOT/data}"' in script
    assert 'export HERMES_RUNTIME_ROOT="${HERMES_RUNTIME_ROOT:-$PACKAGE_ROOT/runtime}"' in script
    assert 'export PYTHONPATH="$ROOT:$ROOT/vendor${PYTHONPATH:+:$PYTHONPATH}"' in script
    assert '"$PYTHON_BIN" -m pytest -q' in script
    assert '"$PYTHON_BIN" ../packaging/check_hermes_packaging.py' in script
