#!/usr/bin/env bash
# Run Hermes from an explicit data/inputs manifest.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PACKAGE_ROOT="$(cd "$ROOT/.." && pwd)"
cd "$ROOT"

if [[ -f "$ROOT/.env" ]]; then
  set -a
  source "$ROOT/.env"
  set +a
fi

if [[ -n "${HERMES_PYTHON:-}" ]]; then
  PYTHON_BIN="$HERMES_PYTHON"
elif [[ -x "$ROOT/.venv/bin/python" ]]; then
  PYTHON_BIN="$ROOT/.venv/bin/python"
else
  PYTHON_BIN="python3"
fi

export PYTHONPATH="$ROOT:$ROOT/vendor${PYTHONPATH:+:$PYTHONPATH}"
export HERMES_DATA_ROOT="${HERMES_DATA_ROOT:-$PACKAGE_ROOT/data}"
export HERMES_INPUT_ROOT="${HERMES_INPUT_ROOT:-$HERMES_DATA_ROOT/inputs}"
export HERMES_DERIVED_ROOT="${HERMES_DERIVED_ROOT:-$HERMES_DATA_ROOT/derived}"
export HERMES_OUTPUT_ROOT="${HERMES_OUTPUT_ROOT:-$HERMES_DATA_ROOT/outputs}"
export HERMES_RUNTIME_ROOT="${HERMES_RUNTIME_ROOT:-$PACKAGE_ROOT/runtime}"
export HERMES_TMPDIR="${HERMES_TMPDIR:-$HERMES_RUNTIME_ROOT/tmp}"
export TMPDIR="$HERMES_TMPDIR"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HERMES_RUNTIME_ROOT/cache}"
export PYTHONPYCACHEPREFIX="${PYTHONPYCACHEPREFIX:-$HERMES_RUNTIME_ROOT/pycache}"

mkdir -p "$HERMES_TMPDIR" "$XDG_CACHE_HOME" "$PYTHONPYCACHEPREFIX" "$HERMES_DERIVED_ROOT" "$HERMES_OUTPUT_ROOT"

"$PYTHON_BIN" -m bridge.run_manifest "$@"
