"""CLI for the near-term N103 Hermes pairer."""
from __future__ import annotations

import argparse
import json
import sys

from .hermes_n103 import (
    analysis_payload,
    analyze_events,
    load_events,
    recommend_pairs,
    render_markdown,
)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Recommend N103 geometry discussion pairs from forum/Zoom text."
    )
    parser.add_argument("input", help="JSON, CSV, or Zoom-style transcript text")
    parser.add_argument("--json", action="store_true", help="emit structured JSON")
    parser.add_argument(
        "--all-candidates",
        action="store_true",
        help="do not greedily reserve students; return every scored candidate",
    )
    parser.add_argument("--max-pairs", type=int, default=None)
    parser.add_argument("--min-score", type=float, default=3.0)
    args = parser.parse_args(argv)

    try:
        events = load_events(args.input)
        profiles = analyze_events(events)
        recommendations = recommend_pairs(
            profiles,
            max_pairs=args.max_pairs,
            min_score=args.min_score,
            exclusive=not args.all_candidates,
        )
    except (OSError, ValueError) as exc:
        print(f"n103 pairer error: {exc}", file=sys.stderr)
        return 2

    if args.json:
        print(json.dumps(analysis_payload(profiles, recommendations), indent=2))
    else:
        print(render_markdown(recommendations, profiles), end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

