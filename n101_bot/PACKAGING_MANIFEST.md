# Hermes Packaging Manifest

This manifest marks the source boundary for making `n101_bot` reproducible in git.
It is intentionally conservative: track code, small docs, tests, and synthetic fixtures;
do not track local runtimes, model weights, generated logs, caches, or student/course data.

## Track For The Push

- `n101_bot/PACKAGING_MANIFEST.md`
- `n101_bot/.gitignore`
- `n101_bot/README.md`
- `n101_bot/PLAN.md`
- `n101_bot/STATUS.md`
- `n101_bot/FOREST.md`
- `n101_bot/requirements.txt`
- `n101_bot/bridge/*.py`
  - representative required files: `n101_bot/bridge/hc_bot.py`, `n101_bot/bridge/hermes_console_server.py`, `n101_bot/bridge/runtime_env.py`, `n101_bot/bridge/path_contract.py`, `n101_bot/bridge/event_importer.py`, `n101_bot/bridge/reallms_smoke.py`
- `n101_bot/src/*.pl`
  - representative required files: `n101_bot/src/vocabulary.pl`, `n101_bot/src/hermes_worker.pl`
- `n101_bot/vocabularies/n101/*.pl`
  - representative required file: `n101_bot/vocabularies/n101/quantity.pl`
- `n101_bot/web/*.html`
  - representative required file: `n101_bot/web/hermes_gemma_console.html`
- `n101_bot/runtime/README.md`
- `n101_bot/reallms/*.md`
  - representative required file: `n101_bot/reallms/USAGE.md`
- `n101_bot/scripts/*.sh`
- `n101_bot/scripts/*.py`
  - representative required files: `n101_bot/scripts/console.sh`, `n101_bot/scripts/reallms_smoke.sh`
- `n101_bot/tests/*.py`
- `n101_bot/tests/*.pl`
  - representative required files: `n101_bot/tests/test_runtime_env.py`, `n101_bot/tests/test_path_contract.py`, `n101_bot/tests/test_event_importer.py`, `n101_bot/tests/test_runtime_scripts.py`, `n101_bot/tests/test_reallms_smoke.py`, `n101_bot/tests/test_reallms_smoke_script.py`
- `n101_bot/samples/*.json`
- `packaging/check_hermes_packaging.py`
- `packaging/flash_sync_manifest.json`
- `packaging/sync_flash_package.py`
- `packaging/tests/*.py`
  - representative required files: `packaging/tests/test_check_hermes_packaging.py`, `packaging/tests/test_flash_sync_package.py`

## Keep Out Of Git

- `n101_bot/.venv/`
- `n101_bot/models/`
- `n101_bot/llama32-1b.vindex/`
- `n101_bot/logs/`
- `n101_bot/.pytest_cache/`
- `n101_bot/**/__pycache__/`
- `n101_bot/**/*.pyc`
- `n101_bot/**/.DS_Store`
- local SWI-Prolog runtime payloads under `n101_bot/runtime/`, except `runtime/README.md`
- flash/package-level `data/` and writable `runtime/` roots
- local N101 student packets and course-note files

## Pre-Stage Check

Run this before staging the Hermes push:

```bash
python3 packaging/check_hermes_packaging.py
```

Then review what would be staged:

```bash
git status --short --untracked-files=all n101_bot packaging .gitignore
```

The check verifies that representative runtime/model/cache/student assets remain ignored
and representative Hermes source files are no longer hidden by `.gitignore`.
