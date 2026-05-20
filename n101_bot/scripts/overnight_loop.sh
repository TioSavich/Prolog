#!/usr/bin/env bash
# Overnight test loop. Runs every question in scripts/questions.txt through
# the bot, logs results, and writes a per-run CSV summary.
# Safe to nohup: writes all state to logs/ and exits cleanly.

set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

MODEL="${MODEL:-deepseek-r1:14b}"
# Slugify model name for log filenames (replace : and / with -)
MODEL_SLUG="$(echo "$MODEL" | tr ':/' '-')"
STAMP="$(date +%Y%m%d_%H%M%S)_${MODEL_SLUG}"
LOG="$ROOT/logs/overnight_${STAMP}.log"
CSV="$ROOT/logs/results_${STAMP}.csv"
JSON_DIR="$ROOT/logs/runs_${STAMP}"
mkdir -p "$JSON_DIR"

PY="$ROOT/.venv/bin/python"
QUESTIONS="$ROOT/scripts/questions.txt"

ITERATIONS="${ITERATIONS:-1}"

echo "n101_bot overnight run started $(date)" | tee -a "$LOG"
echo "model: $MODEL" | tee -a "$LOG"
echo "iterations: $ITERATIONS" | tee -a "$LOG"
echo "questions: $(wc -l < "$QUESTIONS")" | tee -a "$LOG"
echo "" | tee -a "$LOG"

# Regenerate artifacts once at start.
"$PY" -m bridge.prepare 2>&1 | tee -a "$LOG"
echo "" | tee -a "$LOG"

echo "timestamp,iteration,qindex,passed,violation_count,duration_ms,eval_tokens,question" > "$CSV"

i=0
total_pass=0
total_fail=0
total_q=0

for (( iter=1; iter<=ITERATIONS; iter++ )); do
    qidx=0
    while IFS= read -r question; do
        qidx=$((qidx + 1))
        total_q=$((total_q + 1))
        [ -z "$question" ] && continue

        ts="$(date +%H:%M:%S)"
        out_json="$JSON_DIR/iter${iter}_q${qidx}.json"

        echo "[$ts] iter=$iter q=$qidx: $question" | tee -a "$LOG"

        # Run the bot. Skip regenerate (already done).
        if "$PY" -m bridge.cli "$question" --model "$MODEL" --no-regenerate --json > "$out_json" 2>>"$LOG"; then
            passed=true
        else
            passed=false
        fi

        # Parse minimal fields from the json
        vcount="$("$PY" -c "import json; d=json.load(open('$out_json')); print(len(d.get('violations',[])))" 2>/dev/null || echo "-1")"
        dur="$("$PY" -c "import json; d=json.load(open('$out_json')); print(int(d.get('duration_ms',0)))" 2>/dev/null || echo "0")"
        toks="$("$PY" -c "import json; d=json.load(open('$out_json')); print(d.get('eval_tokens',0))" 2>/dev/null || echo "0")"

        # Escape commas/quotes in the question for CSV
        q_esc="$(printf '%s' "$question" | sed 's/"/""/g')"
        echo "$ts,$iter,$qidx,$passed,$vcount,$dur,$toks,\"$q_esc\"" >> "$CSV"

        if [ "$passed" = "true" ] && [ "$vcount" = "0" ]; then
            total_pass=$((total_pass + 1))
            echo "  ✓ no violations (${dur}ms, ${toks} tokens)" | tee -a "$LOG"
        else
            total_fail=$((total_fail + 1))
            echo "  ✗ ${vcount} violation(s) (${dur}ms, ${toks} tokens)" | tee -a "$LOG"
            "$PY" -c "import json; d=json.load(open('$out_json'));
for v in d.get('violations', []):
    print('     -', v['term'], '::', v['rule'])
    print('       trigger:', repr(v['trigger']))" 2>/dev/null | tee -a "$LOG"
        fi
        echo "" | tee -a "$LOG"
    done < "$QUESTIONS"
done

echo "=== summary ===" | tee -a "$LOG"
echo "total questions run: $total_q" | tee -a "$LOG"
echo "clean (zero violations): $total_pass" | tee -a "$LOG"
echo "violations or errors: $total_fail" | tee -a "$LOG"
echo "csv: $CSV" | tee -a "$LOG"
echo "per-run json: $JSON_DIR/" | tee -a "$LOG"
echo "finished $(date)" | tee -a "$LOG"
