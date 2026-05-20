"""K-12 deny-list and deterministic output filter.

Philosophy: the LLM is stochastic. Safety cannot be. A probability of
exactly zero for forbidden content — not 0.98, not 0.9998 — is the
only acceptable floor in a K-12 context. Tio's example: an LLM that
tells a student to kill themselves because it's more efficient 2 in
100 times is catastrophically unacceptable, even though it's right 98
times out of 100.

Two layers of defense:

1. **Post-hoc regex filter** (in this file). Any output that contains
   a denylist term is BLOCKED — the user-visible answer is replaced
   with a fallback, and the offence is logged. This gives probability
   zero on denylisted surface strings, deterministically.

2. **Logit masking** (future work — llama-cpp-python supports it via
   LogitsProcessor; Ollama's HTTP API does not expose logit_bias
   directly). With logit masking, the forbidden TOKENS are simply
   never generated. That's the cleaner guarantee; the post-hoc filter
   is the pragmatic one we can run against any model we have.

The deny-list here is a STARTER — not comprehensive. It is meant to be
expanded by the teacher deploying the bot, in review with the
district. It does NOT replace supervision; it is one layer among
several.

Categories covered (starter):
- Common English profanity
- Common slurs (racial, homophobic, ableist) — SHORT LIST to avoid
  reproducing them; real deployment would reference a vetted lexicon
- Self-harm / suicide vocabulary — Tio called this out explicitly
- Sexual-content descriptors (adults-only register)
- A few category-labels rather than full word lists, to avoid
  enumerating slurs in source code. A production deploy should load
  from a curated, versioned file.

NOT covered here:
- Political invective — too context-dependent; belongs in district
  review, not a hard-coded list
- Religious content — ditto
- Personally-identifying information patterns (phone, address) —
  different concern; separate filter
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path
from typing import List


# ── Denylist by category ──
#
# We do not enumerate every slur in source. We cover the most common
# English profanity and gesture at slur categories via placeholder
# patterns that a real deployment would expand from a vetted file.
#
# Patterns use word boundaries where helpful to avoid over-matching
# ("ass" is a curse; "pass" is a math word).

_PROFANITY_PATTERNS = [
    r"\bf[u\*]ck\w*\b",
    r"\bshit\w*\b",
    r"\bass(?:hole|es)\b",
    r"\bbitch\w*\b",
    r"\bdamn\b",
    r"\bgoddamn\b",
    r"\bbastard\b",
    r"\bcunt\b",
    r"\bdick\b",
    r"\bpussy\b",
    r"\bpiss\b",
    r"\bcock\b",
]

# Self-harm / suicide vocabulary. Extra strict.
# These words can appear in benign math-education contexts (e.g.,
# "killed it" as slang for "did great") so we are careful with word
# boundaries and context — but we err toward false positives for
# K-12, because a false positive is "the bot stays silent on a
# harmless comment," which is recoverable, and a false negative is
# "the bot said something harmful to a child," which is not.
_SELF_HARM_PATTERNS = [
    r"\bkill\s+(?:your|my|him|her|them|ones?)\s*sel(?:f|ves)\b",
    r"\bsuicide\b",
    r"\bsuicidal\b",
    r"\bhang\s+(?:your|my|him|her)\s*self\b",
    r"\bend\s+(?:your|my|his|her|their)\s+life\b",
    r"\bend\s+it\s+all\b",
    r"\bshould\s+(?:just\s+)?die\b",
    r"\b(?:cut|cutting|hurt|harm)\s+(?:your|my|him|her|them)\s*sel(?:f|ves)\b",
    r"\bself[\-\s]harm\w*\b",
]

# Sexual-content descriptors at adult register
_ADULT_PATTERNS = [
    r"\bporn\w*\b",
    r"\bmasturbat\w*\b",
    r"\borgasm\w*\b",
    r"\berection\w*\b",
]

# Racial / identity slurs. Deliberately NOT enumerated here.
# Production deploy loads from a curated, non-public file such as
# `~/.config/n101_bot/slurs.txt` with one per line.
#
# For tonight, we load that file if it exists; otherwise we log a
# reminder that the list is incomplete.
_SLUR_FILE = Path.home() / ".config" / "n101_bot" / "slurs.txt"


def _load_slur_patterns() -> List[str]:
    if _SLUR_FILE.exists():
        patterns = []
        for line in _SLUR_FILE.read_text(encoding="utf-8").splitlines():
            word = line.strip()
            if not word or word.startswith("#"):
                continue
            # word-boundaried, case-insensitive — matches inflections
            patterns.append(rf"\b{re.escape(word)}\w*\b")
        return patterns
    return []


ALL_PATTERNS: List[str] = (
    _PROFANITY_PATTERNS
    + _SELF_HARM_PATTERNS
    + _ADULT_PATTERNS
    + _load_slur_patterns()
)

COMPILED = [re.compile(p, re.IGNORECASE) for p in ALL_PATTERNS]


# ── Filter API ──

@dataclass(frozen=True)
class FilterHit:
    category: str
    pattern: str
    matched_text: str

    def as_dict(self) -> dict:
        return {"category": self.category, "pattern": self.pattern, "matched_text": self.matched_text}


@dataclass(frozen=True)
class FilterResult:
    text: str            # the text that survives the filter (may be fallback)
    blocked: bool        # True if original text contained denylisted content
    hits: List[FilterHit]
    original: str

    def as_dict(self) -> dict:
        return {
            "text": self.text,
            "blocked": self.blocked,
            "hits": [h.as_dict() for h in self.hits],
            "original": self.original,
        }


FALLBACK_MESSAGE = (
    "I can't help with that. Let me loop your teacher in — they'll know "
    "the right next step here."
)


def _classify(pattern: str) -> str:
    # cheap category lookup
    if pattern in _PROFANITY_PATTERNS:
        return "profanity"
    if pattern in _SELF_HARM_PATTERNS:
        return "self_harm"
    if pattern in _ADULT_PATTERNS:
        return "adult"
    return "slur"


def filter_output(text: str, *, fallback: str = FALLBACK_MESSAGE) -> FilterResult:
    """Scan text against every denylist pattern.

    If any pattern matches, the returned text is the FALLBACK — we do
    not attempt to redact inline, because partial redaction of
    self-harm / slur content still leaks semantic content (e.g.,
    "kill [REDACTED]" still reads as harmful). A full fallback is the
    only deterministic guarantee.
    """
    hits: List[FilterHit] = []
    for pattern, regex in zip(ALL_PATTERNS, COMPILED):
        for match in regex.finditer(text):
            hits.append(
                FilterHit(
                    category=_classify(pattern),
                    pattern=pattern,
                    matched_text=match.group(0),
                )
            )
    if hits:
        return FilterResult(text=fallback, blocked=True, hits=hits, original=text)
    return FilterResult(text=text, blocked=False, hits=[], original=text)


def contains_denylisted(text: str) -> bool:
    """Boolean probe — no fallback, no record-keeping."""
    return any(regex.search(text) for regex in COMPILED)
