"""Validators for the two-step check from the immanent-reader skill.

Step 2a — syntax. Shell out to SWI-Prolog. Load the candidate file as a
module. If it loads without error, syntax passes. Capture stderr on
failure.

Step 2b — immanence. Regex-scan for predicates in the unsafe list from
references/immanence_rule.md. If any unsafe predicate appears in a
non-comment position, fail and report which.
"""

from __future__ import annotations

import re
import subprocess
from dataclasses import dataclass
from pathlib import Path


# Unsafe predicates from references/immanence_rule.md.
# Match the predicate name at a word boundary followed by '(' to catch
# functor calls. Comment lines (% ...) are stripped before this check.
_UNSAFE_PATTERNS = [
    (re.compile(r"\bfixed_point\s*\("), "fixed_point/N attributes fixed-point status to a concept"),
    (re.compile(r"\bself_names\s*\("), "self_names/1 slides into concept-level self-reference"),
    (re.compile(r"\bis_[a-zA-Z_]+_function\s*\("),
     "is_X_function/1 attributes function to a concept"),
    (re.compile(r"\bgaifman_structure\s*\("),
     "gaifman_structure/N imposes Gaifman's apparatus on a concept"),
    (re.compile(r"\bessence\s*\("), "essence/N is classical metaphysics"),
    (re.compile(r"\bstructure\s*\("),
     "structure/N applied to a concept slips from text-level to concept-level"),
    (re.compile(r"\bfunction\s*\("),
     "function/N applied to a concept slips from text-level to concept-level"),
    (re.compile(r"\blocated_in\s*\("),
     "located_in/2 locates a concept's identity in some feature"),
]

# Strip block and line comments so false positives inside comments don't trip
# the immanence check.
_LINE_COMMENT_RE = re.compile(r"%[^\n]*")
_BLOCK_COMMENT_RE = re.compile(r"/\*.*?\*/", re.DOTALL)


@dataclass
class SyntaxVerdict:
    ok: bool
    error_text: str
    """Empty when ok; otherwise swipl stderr captured for retry prompts."""


@dataclass
class ImmanenceVerdict:
    ok: bool
    violations: list[tuple[str, str]]
    """List of (predicate_pattern_matched, explanation)."""


def _strip_comments(source: str) -> str:
    source = _BLOCK_COMMENT_RE.sub("", source)
    source = _LINE_COMMENT_RE.sub("", source)
    return source


def validate_syntax(prolog_source: str, *, timeout_seconds: int = 15) -> SyntaxVerdict:
    """Write source to a temp file and ask swipl to consult it.

    We don't run any query — only the consult step, which surfaces parse
    errors and missing-operator errors. We use -q to suppress welcome
    messages and -t halt to exit immediately.
    """
    import tempfile
    with tempfile.NamedTemporaryFile("w", suffix=".pl", delete=False) as fh:
        fh.write(prolog_source)
        path = fh.name

    try:
        result = subprocess.run(
            ["swipl", "-q", "-t", "halt", "-g", "true", path],
            capture_output=True,
            text=True,
            timeout=timeout_seconds,
        )
    except subprocess.TimeoutExpired as e:
        return SyntaxVerdict(ok=False, error_text=f"swipl timed out: {e}")
    finally:
        Path(path).unlink(missing_ok=True)

    # SWI-Prolog returns 0 on successful consult even if the file prints
    # warnings. Warnings go to stderr but don't set exit code. We treat
    # non-zero returncode OR stderr containing "ERROR:" as failure.
    has_error = result.returncode != 0 or "ERROR:" in result.stderr
    if has_error:
        return SyntaxVerdict(ok=False, error_text=result.stderr.strip() or result.stdout.strip())
    return SyntaxVerdict(ok=True, error_text="")


def validate_immanence(prolog_source: str) -> ImmanenceVerdict:
    """Scan (comment-stripped) source for unsafe predicate patterns."""
    stripped = _strip_comments(prolog_source)
    violations: list[tuple[str, str]] = []
    for pattern, explanation in _UNSAFE_PATTERNS:
        match = pattern.search(stripped)
        if match:
            violations.append((match.group(0), explanation))
    return ImmanenceVerdict(ok=not violations, violations=violations)
