#!/usr/bin/env bash
# Backward-compatible entry point for the canonical Hermes launcher.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
exec "$ROOT/scripts/launch_hermes.sh" "$@"
