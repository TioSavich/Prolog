#!/usr/bin/env bash
# Canonical one-click Hermes launcher.

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

HOST="${HERMES_HOST:-127.0.0.1}"
PORT="${HERMES_PORT:-8765}"
OPEN_BROWSER=1
SERVER_ARGS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --host)
      HOST="$2"
      shift 2
      ;;
    --port)
      PORT="$2"
      shift 2
      ;;
    --no-open)
      OPEN_BROWSER=0
      shift
      ;;
    *)
      SERVER_ARGS+=("$1")
      shift
      ;;
  esac
done

MODEL="${HERMES_MODEL:-${REALLMS_MODEL:-gemma-4-31B-it}}"
export HERMES_MODEL="$MODEL"
export HERMES_RENDERER="${HERMES_RENDERER:-reallms}"
export HERMES_APP_HOME="${HERMES_APP_HOME:-$ROOT}"
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

if [[ -z "${HERMES_SWIPL:-}" && -x "$ROOT/runtime/swi-prolog/bin/swipl" ]]; then
  export HERMES_SWIPL="$ROOT/runtime/swi-prolog/bin/swipl"
elif [[ -z "${HERMES_SWIPL:-}" && -x "$PACKAGE_ROOT/runtime/swi-prolog/bin/swipl" ]]; then
  export HERMES_SWIPL="$PACKAGE_ROOT/runtime/swi-prolog/bin/swipl"
fi

if [[ -n "${HERMES_SWIPL:-}" ]]; then
  SWIPL_HOME="$(cd "$(dirname "$HERMES_SWIPL")/.." && pwd)"
  SWI_LIB="$SWIPL_HOME/lib/swipl/lib/x86_64-darwin"
  if [[ -d "$SWI_LIB" ]]; then
    export DYLD_LIBRARY_PATH="$SWI_LIB${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
  fi
fi

if [[ -n "${HERMES_PYTHON:-}" ]]; then
  PYTHON_BIN="$HERMES_PYTHON"
elif [[ -x "$ROOT/.venv/bin/python" ]]; then
  PYTHON_BIN="$ROOT/.venv/bin/python"
else
  PYTHON_BIN="python3"
fi

export PYTHONPATH="$ROOT:$ROOT/vendor${PYTHONPATH:+:$PYTHONPATH}"

URL="http://$HOST:$PORT"
if [[ "$OPEN_BROWSER" == "1" && "${HERMES_NO_OPEN:-0}" != "1" ]]; then
  (sleep 2; open "$URL" >/dev/null 2>&1 || true) &
fi

if [[ ${#SERVER_ARGS[@]} -gt 0 ]]; then
  exec "$PYTHON_BIN" -m bridge.hermes_console_server --host "$HOST" --port "$PORT" "${SERVER_ARGS[@]}"
else
  exec "$PYTHON_BIN" -m bridge.hermes_console_server --host "$HOST" --port "$PORT"
fi
