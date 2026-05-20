#!/usr/bin/env bash
# Local Hermes console.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

MODEL="${HERMES_MODEL:-gemma:2b}"
export HERMES_MODEL="$MODEL"
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
