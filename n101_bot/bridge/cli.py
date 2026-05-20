"""CLI entry point: ask one question, route through Prolog, print result.

Flags:
  --reason-only    skip the LLM, just print what Prolog would detect/commit
                   on this utterance — useful for debugging triggers
  --json           structured JSON output
  --show-thinking  include DeepSeek's <think> scratchpad
"""
from __future__ import annotations

import argparse
import json
import sys

from .hc_bot import DEFAULT_MODEL, HermeneuticBot
from .ollama_client import OllamaError, ping
from .prolog import reason


def _print_reason_only(text: str) -> int:
    report = reason(text)
    print(json.dumps(report.as_dict(), indent=2))
    return 0 if not report.commitments else 1


def _print_state(record) -> None:
    if record.state_after.rendered:
        print("\n[dialogue state]")
        print(record.state_after.rendered)
        if record.state_after.near_cusp:
            print("  >>> approaching catastrophe — watch for aha or snap <<<")


def _print_human(record) -> int:
    if record.normalization.changed:
        print(f"Raw: {record.raw_question}")
        print(f"Q:   {record.question}  [normalized, {len(record.normalization.applied_rules)} rule(s) fired]")
    else:
        print(f"Q: {record.question}")
    print(f"   move: {record.move.kind} → {record.move.move_tag} "
          f"({'assessing' if record.assessing else 'answering'})")
    if record.detected_terms:
        print(f"   detected terms: {', '.join(record.detected_terms)}")
    else:
        print("   detected terms: none")
    if record.assessing:
        print("\n(Amy's move grammar said: don't answer — ask.)")
        print(record.final_answer)
        if record.final_commitments:
            print(f"\n{len(record.final_commitments)} commitment(s) fired on the rendered question:")
            for c in record.final_commitments:
                print(f"  [{c.term}] {c.rule}")
            _print_state(record)
            return 1
        _print_state(record)
        return 0
    if record.repaired:
        print("\n--- first answer (rejected) ---")
        print(record.first_answer)
        print(f"\nfired {len(record.first_commitments)} commitment(s):")
        for c in record.first_commitments:
            print(f"  [{c.term}] {c.rule}")
            print(f"    triggered by: {c.trigger!r}")
            print(f"    Amy's correction: {c.correction}")
        print("\n--- repaired answer ---")
    else:
        print()
    print(record.final_answer)
    if record.final_commitments:
        print(f"\n{len(record.final_commitments)} commitment(s) still firing after repair:")
        for c in record.final_commitments:
            print(f"  [{c.term}] {c.rule}")
        _print_state(record)
        return 1
    print("\nno violations.")
    _print_state(record)
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Ask the N101 bot a question.")
    parser.add_argument("question", nargs="?")
    parser.add_argument("--model", default=DEFAULT_MODEL)
    parser.add_argument("--temperature", type=float, default=0.2)
    parser.add_argument("--json", action="store_true")
    parser.add_argument("--show-thinking", action="store_true")
    parser.add_argument("--reason-only", action="store_true", help="skip LLM, just run Prolog reasoning")
    parser.add_argument(
        "--audience",
        choices=["teacher", "student"],
        default="teacher",
        help="teacher (default): output is a suggested question for the teacher. student: address the student directly.",
    )
    args = parser.parse_args(argv)

    if not args.question:
        parser.error("question is required")

    if args.reason_only:
        return _print_reason_only(args.question)

    if not ping():
        print("ollama daemon not reachable at localhost:11434", file=sys.stderr)
        return 2

    bot = HermeneuticBot(model=args.model, audience=args.audience)
    try:
        record = bot.ask(args.question, temperature=args.temperature)
    except OllamaError as e:
        print(f"ollama error: {e}", file=sys.stderr)
        return 3

    if args.json:
        print(json.dumps(record.as_dict(), indent=2))
        return 0 if record.passed else 1

    if args.show_thinking and record.final_thinking:
        print("[think]")
        print(record.final_thinking)
        print("[/think]\n")

    return _print_human(record)


if __name__ == "__main__":
    sys.exit(main())
