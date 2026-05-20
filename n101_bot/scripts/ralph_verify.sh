#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

echo "[n101_bot] persistent worker and bridge tests"
"$ROOT/.venv/bin/python" -m pytest -q \
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

echo "[n101_bot] verification complete"
