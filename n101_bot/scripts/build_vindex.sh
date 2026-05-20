#!/usr/bin/env bash
# Convert the Ollama-local gemma:2b GGUF into a larql vindex.
# Assumes larql binary is built at larql/target/release/larql.
#
# Usage:
#   scripts/build_vindex.sh
#
# Result: /Users/tio/Documents/GitHub/Prolog/n101_bot/gemma-2b.vindex/

set -euo pipefail

ROOT="/Users/tio/Documents/GitHub/Prolog"
LARQL="$ROOT/larql/target/release/larql"
OLLAMA_BLOB="/Users/tio/.ollama/models/blobs/sha256-c1864a5eb19305c40519da12cc543519e48a0697ecd30e15d5ac228644957d12"
VINDEX_OUT="$ROOT/n101_bot/gemma-2b.vindex"

if [ ! -x "$LARQL" ]; then
    echo "larql not built at $LARQL" >&2
    exit 1
fi

if [ ! -f "$OLLAMA_BLOB" ]; then
    echo "ollama gemma:2b blob not found at $OLLAMA_BLOB" >&2
    exit 1
fi

if [ -d "$VINDEX_OUT" ]; then
    echo "vindex already exists at $VINDEX_OUT — delete it first to rebuild" >&2
    exit 1
fi

echo "converting gemma:2b GGUF to vindex at inference level (~2-3 GB output)"
echo "source:    $OLLAMA_BLOB"
echo "target:    $VINDEX_OUT"
echo

"$LARQL" convert gguf-to-vindex "$OLLAMA_BLOB" -o "$VINDEX_OUT" --level inference --f16

echo
echo "vindex built:"
ls -la "$VINDEX_OUT"
du -sh "$VINDEX_OUT"
