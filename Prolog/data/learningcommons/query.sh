#!/usr/bin/env bash
# Query LearningCommons Knowledge Graph JSONL data
# Usage:
#   ./query.sh standard <code> [jurisdiction]     — look up a standard
#   ./query.sh components <code> [jurisdiction]    — learning components for a standard
#   ./query.sh progression <code> [jurisdiction]   — what this standard builds toward
#   ./query.sh related <code> [jurisdiction]       — related standards
#   ./query.sh lessons <code> [jurisdiction]       — IM lessons aligned to this standard
#
# jurisdiction defaults to "Multi-State" (CCSS). Use "Indiana" for Indiana standards.
# Examples:
#   ./query.sh standard K.CC.A.1
#   ./query.sh standard K.NS.1 Indiana
#   ./query.sh components K.CC.A.1
#   ./query.sh progression K.CC.A.1

set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
NODES="$DIR/nodes.jsonl"
RELS="$DIR/relationships.jsonl"

if [ ! -f "$NODES" ] || [ ! -f "$RELS" ]; then
  echo "Data files not found. Download them:"
  echo '  curl -L "https://cdn.learningcommons.org/knowledge-graph/v1.7.0/exports/nodes.jsonl?ref=gh_curl" -o nodes.jsonl'
  echo '  curl -L "https://cdn.learningcommons.org/knowledge-graph/v1.7.0/exports/relationships.jsonl?ref=gh_curl" -o relationships.jsonl'
  exit 1
fi

CMD="${1:-help}"
CODE="${2:-}"
JURISDICTION="${3:-Multi-State}"

get_id() {
  jq -r "select(.properties.statementCode == \"$1\" and .properties.jurisdiction == \"$2\") | .identifier" "$NODES"
}

case "$CMD" in
  standard)
    [ -z "$CODE" ] && echo "Usage: $0 standard <code> [jurisdiction]" && exit 1
    jq -c "select(.properties.statementCode == \"$CODE\" and .properties.jurisdiction == \"$JURISDICTION\")" "$NODES" \
      | jq '{code: .properties.statementCode, jurisdiction: .properties.jurisdiction, description: .properties.description, uuid: .properties.caseIdentifierUUID, identifier: .identifier, gradeLevel: .properties.gradeLevel}'
    ;;

  components)
    [ -z "$CODE" ] && echo "Usage: $0 components <code> [jurisdiction]" && exit 1
    ID=$(get_id "$CODE" "$JURISDICTION")
    [ -z "$ID" ] && echo "Standard not found: $CODE ($JURISDICTION)" && exit 1
    jq -r "select(.target_identifier == \"$ID\" and .label == \"supports\") | .source_identifier" "$RELS" | while read -r comp_id; do
      jq -c "select(.identifier == \"$comp_id\") | {name: (.properties.name // .properties.description), identifier: .identifier}" "$NODES"
    done
    ;;

  progression)
    [ -z "$CODE" ] && echo "Usage: $0 progression <code> [jurisdiction]" && exit 1
    ID=$(get_id "$CODE" "$JURISDICTION")
    [ -z "$ID" ] && echo "Standard not found: $CODE ($JURISDICTION)" && exit 1
    echo "=== Builds Towards ==="
    jq -r "select(.source_identifier == \"$ID\" and .label == \"buildsTowards\") | .target_identifier" "$RELS" | while read -r target_id; do
      jq -c "select(.identifier == \"$target_id\") | {code: .properties.statementCode, jurisdiction: .properties.jurisdiction, description: .properties.description}" "$NODES"
    done
    echo ""
    echo "=== Built Upon By ==="
    jq -r "select(.target_identifier == \"$ID\" and .label == \"buildsTowards\") | .source_identifier" "$RELS" | while read -r source_id; do
      jq -c "select(.identifier == \"$source_id\") | {code: .properties.statementCode, jurisdiction: .properties.jurisdiction, description: .properties.description}" "$NODES"
    done
    ;;

  related)
    [ -z "$CODE" ] && echo "Usage: $0 related <code> [jurisdiction]" && exit 1
    ID=$(get_id "$CODE" "$JURISDICTION")
    [ -z "$ID" ] && echo "Standard not found: $CODE ($JURISDICTION)" && exit 1
    jq -r "select((.source_identifier == \"$ID\" or .target_identifier == \"$ID\") and .label == \"relatesTo\")" "$RELS" \
      | jq -r "if .source_identifier == \"$ID\" then .target_identifier else .source_identifier end" | sort -u | while read -r rel_id; do
      jq -c "select(.identifier == \"$rel_id\") | {code: .properties.statementCode, jurisdiction: .properties.jurisdiction, description: .properties.description}" "$NODES"
    done
    ;;

  lessons)
    [ -z "$CODE" ] && echo "Usage: $0 lessons <code> [jurisdiction]" && exit 1
    ID=$(get_id "$CODE" "$JURISDICTION")
    [ -z "$ID" ] && echo "Standard not found: $CODE ($JURISDICTION)" && exit 1
    jq -r "select(.target_identifier == \"$ID\" and .label == \"hasEducationalAlignment\") | .source_identifier" "$RELS" | while read -r lesson_id; do
      jq -c "select(.identifier == \"$lesson_id\") | {name: (.properties.name // .properties.title), identifier: .identifier}" "$NODES"
    done
    ;;

  *)
    echo "LearningCommons Knowledge Graph Query Tool"
    echo ""
    echo "Commands:"
    echo "  standard <code> [jurisdiction]    — look up a standard"
    echo "  components <code> [jurisdiction]   — learning components"
    echo "  progression <code> [jurisdiction]  — what it builds toward / is built upon by"
    echo "  related <code> [jurisdiction]      — related standards"
    echo "  lessons <code> [jurisdiction]      — IM lessons aligned"
    echo ""
    echo "Jurisdiction defaults to Multi-State (CCSS). Use 'Indiana' for Indiana."
    ;;
esac
