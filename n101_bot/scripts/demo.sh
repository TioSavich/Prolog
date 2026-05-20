#!/usr/bin/env bash
# One-shot demo: ask the bot a single question, show answer and any violations.
# Usage: scripts/demo.sh "What is a quantity?"

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

QUESTION="${1:-What is a quantity?}"

"$ROOT/.venv/bin/python" -m bridge.cli "$QUESTION" --show-thinking
