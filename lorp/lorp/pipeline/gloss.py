"""Output writer: the per-passage output tree described in the spec."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path

from ..voices.immanent import DecomposerResult
from .validator import ImmanenceVerdict, SyntaxVerdict


@dataclass
class RunArtifacts:
    prolog_path: Path
    gloss_path: Path
    validation_path: Path
    raw_path: Path
    """The raw provider response as JSON. Audit trail — confirms which
    model actually answered (the response's `model` field is echoed by
    the server, not taken from our config)."""

    rejected_path: Path | None
    """If the run rejected the candidate, this points to where the bad
    Prolog was saved for forensics. None on success."""


def write_outputs(
    *,
    output_dir: Path,
    stem: str,
    passage_text: str,
    decomposer: DecomposerResult,
    syntax: SyntaxVerdict,
    immanence: ImmanenceVerdict,
    attempts: int,
) -> RunArtifacts:
    output_dir.mkdir(parents=True, exist_ok=True)

    prolog_path = output_dir / f"{stem}.pl"
    gloss_path = output_dir / f"{stem}_gloss.md"
    validation_path = output_dir / f"{stem}_validation.md"
    rejected_path: Path | None = None

    if syntax.ok and immanence.ok:
        prolog_path.write_text(decomposer.prolog + "\n" if decomposer.prolog else "")
    else:
        # Save the candidate for forensics even if it failed validation.
        rejected_path = output_dir / f"{stem}_rejected.pl"
        rejected_path.write_text(decomposer.prolog + "\n" if decomposer.prolog else "")
        # Also write an empty (but well-formed) stem.pl so downstream
        # tooling that consults the output tree doesn't error on missing file.
        prolog_path.write_text(
            f":- module(list_{stem}, []).\n"
            f"% Decomposition rejected at syntax or immanence check.\n"
            f"% See {rejected_path.name} and {validation_path.name}.\n"
        )

    gloss_path.write_text(
        f"# Gloss — {stem}\n\n"
        f"## Passage\n\n"
        f"```\n{passage_text.strip()}\n```\n\n"
        f"## Substrate + justifications\n\n"
        f"{decomposer.gloss or '(no gloss returned)'}\n"
    )

    validation_md = [
        f"# Validation — {stem}",
        "",
        f"- Attempts: {attempts}",
        f"- Syntax: {'pass' if syntax.ok else 'fail'}",
        f"- Immanence: {'pass' if immanence.ok else 'fail'}",
        "",
    ]
    if not syntax.ok:
        validation_md += ["## Syntax error", "", "```", syntax.error_text, "```", ""]
    if not immanence.ok:
        validation_md += [
            "## Immanence violations",
            "",
            *[f"- `{pat}` — {exp}" for pat, exp in immanence.violations],
            "",
        ]
    if decomposer.thinking:
        validation_md += [
            "## Model thinking trace (for debugging)",
            "",
            "```",
            decomposer.thinking,
            "```",
            "",
        ]
    validation_path.write_text("\n".join(validation_md))

    raw_path = output_dir / f"{stem}_raw.json"
    raw_path.write_text(json.dumps(decomposer.raw_response.raw, indent=2, default=str))

    return RunArtifacts(
        prolog_path=prolog_path,
        gloss_path=gloss_path,
        validation_path=validation_path,
        raw_path=raw_path,
        rejected_path=rejected_path,
    )


def append_training_corpus_entry(
    *,
    index_path: Path,
    passage_path: Path,
    artifacts: RunArtifacts,
    syntax: SyntaxVerdict,
    immanence: ImmanenceVerdict,
    attempts: int,
    model: str,
) -> None:
    index_path.parent.mkdir(parents=True, exist_ok=True)
    entry = {
        "passage_path": str(passage_path),
        "prolog_path": str(artifacts.prolog_path),
        "gloss_path": str(artifacts.gloss_path),
        "validation_path": str(artifacts.validation_path),
        "rejected_path": str(artifacts.rejected_path) if artifacts.rejected_path else None,
        "syntax_ok": syntax.ok,
        "immanence_ok": immanence.ok,
        "syntax_error": syntax.error_text if not syntax.ok else "",
        "immanence_violations": [
            {"pattern": p, "explanation": e} for p, e in immanence.violations
        ],
        "attempts": attempts,
        "model": model,
    }
    with index_path.open("a") as fh:
        fh.write(json.dumps(entry) + "\n")
