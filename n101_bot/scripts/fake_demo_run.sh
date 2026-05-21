#!/usr/bin/env bash
# Explicit synthetic Hermes run; REALLMS revoice is opt-in via --revoice.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PACKAGE_ROOT="$(cd "$ROOT/.." && pwd)"
FLASH_ROOT="$(cd "$PACKAGE_ROOT/.." && pwd)"
cd "$ROOT"

for env_file in "$PACKAGE_ROOT/.env" "$FLASH_ROOT/.env" "$ROOT/.env"; do
  if [[ -f "$env_file" ]]; then
    set -a
    source "$env_file"
    set +a
  fi
done

if [[ -n "${HERMES_PYTHON:-}" ]]; then
  PYTHON_BIN="$HERMES_PYTHON"
elif [[ -x "$ROOT/.venv/bin/python" ]]; then
  PYTHON_BIN="$ROOT/.venv/bin/python"
else
  PYTHON_BIN="python3"
fi

export PYTHONPATH="$ROOT:$ROOT/vendor${PYTHONPATH:+:$PYTHONPATH}"

"$PYTHON_BIN" -m bridge.fake_demo_run "$@"
