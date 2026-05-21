from __future__ import annotations

from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "reallms_smoke.sh"


def test_reallms_smoke_script_uses_portable_python_and_module_entrypoint():
    script = SCRIPT.read_text(encoding="utf-8")

    assert "HERMES_PYTHON" in script
    assert 'PYTHON_BIN="$HERMES_PYTHON"' in script
    assert 'PYTHON_BIN="$ROOT/.venv/bin/python"' in script
    assert 'PYTHON_BIN="python3"' in script
    assert 'PACKAGE_ROOT="$(cd "$ROOT/.." && pwd)"' in script
    assert 'FLASH_ROOT="$(cd "$PACKAGE_ROOT/.." && pwd)"' in script
    assert 'for env_file in "$PACKAGE_ROOT/.env" "$FLASH_ROOT/.env" "$ROOT/.env"; do' in script
    assert 'export PYTHONPATH="$ROOT:$ROOT/vendor${PYTHONPATH:+:$PYTHONPATH}"' in script
    assert '"$PYTHON_BIN" -m bridge.reallms_smoke "$@"' in script
