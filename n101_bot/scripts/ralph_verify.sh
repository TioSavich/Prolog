#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PACKAGE_ROOT="$(cd "$ROOT/.." && pwd)"
cd "$ROOT"

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

echo "[n101_bot] persistent worker and bridge tests"
"$PYTHON_BIN" -m pytest -q \
  ../packaging/tests/test_check_hermes_packaging.py \
  ../packaging/tests/test_flash_sync_package.py \
  tests/test_event_importer.py \
  tests/test_path_contract.py \
  tests/test_persistent_prolog.py \
  tests/test_hc_bot_offline.py \
  tests/test_runtime_env.py \
  tests/test_runtime_scripts.py \
  tests/test_runtime_swipl_commands.py \
  tests/test_console_runtime_preflight_endpoint.py \
  tests/test_console_json_headers.py \
  tests/test_console_html_hooks.py \
  tests/test_console_n103_workflows_endpoint.py \
  tests/test_console_pair_graph_endpoint.py \
  tests/test_reallms_revoicer.py \
  tests/test_reallms_smoke.py \
  tests/test_reallms_smoke_script.py \
  tests/test_console_revoice_endpoint.py \
  tests/test_console_mode_endpoint.py \
  tests/test_n103_run_loader.py \
  tests/test_n103_prolog_pipeline.py \
  tests/test_hermes_n103.py \
  tests/test_console_n103_pipeline_endpoint.py \
  tests/test_prolog_reasoning.py

echo "[n101_bot] packaging boundary"
"$PYTHON_BIN" ../packaging/check_hermes_packaging.py

echo "[n101_bot] verification complete"
