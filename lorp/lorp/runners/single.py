"""Single-passage runner.

Usage:
    python -m lorp.runners.single --passage path/to/passage.txt [--stem NAME]
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from ..config import load as load_config
from ..models import backend_for
from ..pipeline import (
    validate_immanence,
    validate_syntax,
    write_outputs,
)
from ..pipeline.gloss import append_training_corpus_entry
from ..voices.immanent import ImmanentVoice


MAX_RETRIES = 2


def process_passage(
    *,
    passage_path: Path,
    config_path: Path | None = None,
    stem_override: str | None = None,
) -> int:
    """Return 0 on success, 1 on rejection (still writes artifacts)."""
    config = load_config(config_path)
    backend = backend_for(config)
    voice = ImmanentVoice(config=config, backend=backend)

    passage_text = passage_path.read_text()
    stem = stem_override or passage_path.stem
    output_dir = config.output_base_dir / stem

    candidate = None
    attempts = 0
    syntax_verdict = None
    immanence_verdict = None
    last_error_hint = ""

    for attempt in range(1, MAX_RETRIES + 2):  # 1 initial + MAX_RETRIES retries
        attempts = attempt
        user_suffix = ""
        if last_error_hint:
            user_suffix = (
                "\n\n## Retry context\n"
                "The previous candidate was rejected with the following feedback. "
                "Re-emit, preserving the rule that safe predicates describe sentences.\n\n"
                f"```\n{last_error_hint}\n```"
            )
        # Inject retry context by appending to passage text (simplest path).
        candidate = voice.decompose(
            passage=passage_text + user_suffix,
            passage_stem=stem,
        )

        if not candidate.prolog:
            last_error_hint = (
                "Your response did not contain a parsable <PROLOG>...</PROLOG> block. "
                "Emit the two blocks in exactly the required format."
            )
            syntax_verdict = type("S", (), {"ok": False, "error_text": "no <PROLOG> block parsed"})()
            immanence_verdict = type("I", (), {"ok": True, "violations": []})()
            continue

        syntax_verdict = validate_syntax(candidate.prolog)
        if not syntax_verdict.ok:
            last_error_hint = (
                "SWI-Prolog rejected the candidate. Fix and re-emit:\n"
                + syntax_verdict.error_text
            )
            immanence_verdict = validate_immanence(candidate.prolog)
            continue

        immanence_verdict = validate_immanence(candidate.prolog)
        if not immanence_verdict.ok:
            last_error_hint = (
                "The candidate contains a predicate that attributes structure/function "
                "to a concept. Rewrite at the sentence level.\n"
                + "\n".join(f"- {pat}: {exp}" for pat, exp in immanence_verdict.violations)
            )
            continue

        # Both passed.
        break

    artifacts = write_outputs(
        output_dir=output_dir,
        stem=stem,
        passage_text=passage_text,
        decomposer=candidate,
        syntax=syntax_verdict,
        immanence=immanence_verdict,
        attempts=attempts,
    )
    append_training_corpus_entry(
        index_path=config.corpus_index_path,
        passage_path=passage_path,
        artifacts=artifacts,
        syntax=syntax_verdict,
        immanence=immanence_verdict,
        attempts=attempts,
        model=config.get_voice("immanent").model,
    )

    status = "PASS" if (syntax_verdict.ok and immanence_verdict.ok) else "REJECT"
    print(
        f"[{status}] {passage_path}\n"
        f"  attempts: {attempts}\n"
        f"  prolog:     {artifacts.prolog_path}\n"
        f"  gloss:      {artifacts.gloss_path}\n"
        f"  validation: {artifacts.validation_path}\n"
        f"  raw:        {artifacts.raw_path}  (audit: provider's echoed model name)\n"
        + (f"  rejected:   {artifacts.rejected_path}\n" if artifacts.rejected_path else "")
    )
    return 0 if (syntax_verdict.ok and immanence_verdict.ok) else 1


def _main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Run immanent-reader on a single passage.")
    parser.add_argument("--passage", required=True, type=Path, help="Path to the passage file.")
    parser.add_argument("--stem", default=None, help="Output stem (default: passage filename stem).")
    parser.add_argument("--config", default=None, type=Path, help="Path to config.yaml.")
    args = parser.parse_args(argv)

    if not args.passage.exists():
        print(f"passage file not found: {args.passage}", file=sys.stderr)
        return 2

    return process_passage(
        passage_path=args.passage.resolve(),
        config_path=args.config,
        stem_override=args.stem,
    )


if __name__ == "__main__":
    sys.exit(_main(sys.argv[1:]))
