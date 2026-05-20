#!/usr/bin/env bash
# Local Hermes console.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

if [[ -f "$ROOT/.env" ]]; then
  set -a
  source "$ROOT/.env"
  set +a
fi

MODEL="${HERMES_MODEL:-${REALLMS_MODEL:-gemma-4-31B-it}}"
export HERMES_MODEL="$MODEL"
export HERMES_RENDERER="${HERMES_RENDERER:-reallms}"
export HERMES_APP_HOME="${HERMES_APP_HOME:-$ROOT}"
export HERMES_TMPDIR="${HERMES_TMPDIR:-$ROOT/runtime/tmp}"
export TMPDIR="$HERMES_TMPDIR"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$ROOT/runtime/cache}"
export PYTHONPYCACHEPREFIX="${PYTHONPYCACHEPREFIX:-$ROOT/runtime/pycache}"

mkdir -p "$HERMES_TMPDIR" "$XDG_CACHE_HOME" "$PYTHONPYCACHEPREFIX"

if [[ -z "${HERMES_SWIPL:-}" && -x "$ROOT/runtime/swi-prolog/bin/swipl" ]]; then
  export HERMES_SWIPL="$ROOT/runtime/swi-prolog/bin/swipl"
fi

"$ROOT/.venv/bin/python" -m bridge.hermes_console_server "$@"
