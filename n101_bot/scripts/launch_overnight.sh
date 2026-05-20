#!/usr/bin/env bash
# Detach the overnight loop from the current terminal via nohup.
# Default ITERATIONS=5 → roughly 45-75 minutes of runtime with deepseek-r1:14b.
# Override: ITERATIONS=N ./scripts/launch_overnight.sh

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

ITERATIONS="${ITERATIONS:-5}"
STAMP="$(date +%Y%m%d_%H%M%S)"
DETACHED_LOG="$ROOT/logs/nohup_${STAMP}.log"

mkdir -p "$ROOT/logs"

echo "launching nohup overnight loop with ITERATIONS=$ITERATIONS"
echo "tail -f $DETACHED_LOG  # to watch live"
echo

ITERATIONS="$ITERATIONS" nohup bash "$ROOT/scripts/overnight_loop.sh" </dev/null >"$DETACHED_LOG" 2>&1 &
PID=$!
echo "started pid=$PID"
echo "$PID" > "$ROOT/logs/overnight.pid"
disown "$PID" 2>/dev/null || true
echo "pid saved to logs/overnight.pid"
echo "to cancel: kill $PID"
