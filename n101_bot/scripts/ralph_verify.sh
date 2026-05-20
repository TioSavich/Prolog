#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

if [[ -n "${HERMES_PYTHON:-}" ]]; then
  PYTHON_BIN="$HERMES_PYTHON"
elif [[ -x "$ROOT/.venv/bin/python" ]]; then
  PYTHON_BIN="$ROOT/.venv/bin/python"
else
  PYTHON_BIN="python3"
fi

export PYTHONPATH="$ROOT:$ROOT/vendor${PYTHONPATH:+:$PYTHONPATH}"

echo "[n101_bot] persistent worker and bridge tests"
"$PYTHON_BIN" -m pytest -q \
  ../packaging/tests/test_check_hermes_packaging.py \
  tests/test_persistent_prolog.py \
  tests/test_hc_bot_offline.py \
  tests/test_runtime_env.py \
  tests/test_runtime_swipl_commands.py \
  tests/test_console_runtime_preflight_endpoint.py \
  tests/test_console_json_headers.py \
  tests/test_console_html_hooks.py \
  tests/test_console_n103_workflows_endpoint.py \
  tests/test_console_pair_graph_endpoint.py \
  tests/test_reallms_revoicer.py \
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
