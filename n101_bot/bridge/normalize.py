"""Kid-talk normalizer — rule-based only.

Kids don't type much, and when they do it's mangled by autocorrect,
voice-to-text, chat shorthand, and the specific colloquial register of
elementary math talk. We do NOT want to train a model for this; Tio has
said so explicitly. Instead, we maintain a small hand-curated set of
replacement rules that preserve meaning while making the utterance
classifier-legible.

The philosophy:
  - Replace obvious typos / contractions (cuz → because)
  - Normalize math notation (×, ÷, equals spellings)
  - Expand a short CGI-strategy colloquialism lexicon so downstream
    strategy_mentioned/2 in Prolog fires reliably
  - Preserve the raw utterance alongside the normalized form, because
    the raw version matters for transcripts and for the teacher
    dashboard ("the student said it THIS way").

This is deliberately non-semantic. No grammar parsing. No LLM. If a
rule doesn't fit a case, the case falls through unchanged. Add rules
when you see a pattern repeat in real transcripts.
"""
from __future__ import annotations

import re
from dataclasses import dataclass


# ── Typo / contraction corrections ──
#
# Case-insensitive. Keys are regex patterns (word-boundaried to avoid
# spurious matches inside longer words).
_TYPOS: list[tuple[str, str]] = [
    (r"\bcuz\b", "because"),
    (r"\bcause\b(?!\s+of)", "because"),  # "cause I did" but not "cause of"
    (r"\bcoz\b", "because"),
    (r"\bcos\b(?!\w)", "because"),
    (r"\bur\b", "your"),
    (r"\bu\b", "you"),
    (r"\br\b", "are"),
    (r"\bn\b", "and"),
    (r"\bimma\b", "I'm going to"),
    (r"\bgonna\b", "going to"),
    (r"\bwanna\b", "want to"),
    (r"\bgotta\b", "got to"),
    (r"\bkno\b", "know"),
    (r"\btho\b", "though"),
    (r"\bthx\b", "thanks"),
    (r"\bidk\b", "I don't know"),
    (r"\bidr\b", "I don't remember"),
    (r"\bty\b", "thank you"),
    # kid past-tense overregularization: "maked" → "made", "knowed" → "knew"
    (r"\bmaked\b", "made"),
    (r"\bknowed\b", "knew"),
    (r"\bgoed\b", "went"),
    (r"\bthinked\b", "thought"),
]


# ── Math notation normalization ──
_MATH_NOTATION: list[tuple[str, str]] = [
    (r"\s*×\s*", " x "),
    (r"\s*∗\s*", " * "),
    (r"\s*÷\s*", " / "),
    (r"\s*−\s*", " - "),  # unicode minus
    (r"\s*＝\s*", " = "),
    (r"\bplus\b", "+"),
    (r"\bminus\b", "-"),
    (r"\btimes\b", "x"),
    (r"\bequals\b", "="),
    (r"\bis equal to\b", "="),
]


# ── CGI-strategy colloquialism lexicon ──
#
# Kid-reported → Prolog-legible phrasing. The goal is not to reduce to
# canonical tags (that's move_grammar.pl's job); it's to make sure
# something the strategy_mentioned/2 clauses can latch onto is in the
# text. We bias toward adding *both* phrasings (original + canonical)
# to avoid erasing the kid's voice from downstream logs.
_STRATEGY_COLLOQUIALISMS: list[tuple[str, str]] = [
    # "broke 8 into 2 and 6" → adds "decomposed" marker
    (r"\bbroke\s+\d+\s+into\b", "broke apart"),
    (r"\bsplit\s+\d+\s+into\b", "split"),
    # "took 2 from the 8" → making-ten signature
    (r"\btook\s+\d+\s+from\b", "take from"),
    (r"\bgave\s+\d+\s+to\b", "gave 2 to"),
    # "started at 8 and went 9 10 11 12 13" → counting_on
    (r"\bstarted\s+at\s+\d+\s+and\s+went\b", "started at and counted on"),
    # "I used my fingers"
    (r"\bused\s+my\s+fingers\b", "counted from 1"),
    (r"\bcounted\s+on\s+my\s+fingers\b", "counted on"),
    # "it's just 6 and 6 plus one"
    (r"\bit['']?s\s+just\s+\d+\s+and\s+\d+\s+plus\b", "near doubles"),
    # "I know 6 plus 6"
    (r"\bi\s+know\s+\d+\s*\+\s*\d+\b", "doubles I know"),
]


@dataclass(frozen=True)
class Normalization:
    raw: str
    normalized: str
    applied_rules: list[str]

    @property
    def changed(self) -> bool:
        return self.raw != self.normalized

    def as_dict(self) -> dict:
        return {
            "raw": self.raw,
            "normalized": self.normalized,
            "applied_rules": self.applied_rules,
            "changed": self.changed,
        }


def normalize(utterance: str) -> Normalization:
    """Run the rule pipeline. Returns both versions plus which rules fired."""
    applied: list[str] = []
    current = utterance

    for pattern, replacement in _TYPOS:
        new = re.sub(pattern, replacement, current, flags=re.IGNORECASE)
        if new != current:
            applied.append(f"typo:{pattern}→{replacement}")
            current = new

    for pattern, replacement in _MATH_NOTATION:
        new = re.sub(pattern, replacement, current, flags=re.IGNORECASE)
        if new != current:
            applied.append(f"math:{pattern}→{replacement}")
            current = new

    for pattern, annotation in _STRATEGY_COLLOQUIALISMS:
        # Strategy phrases are *appended* as an annotation, not substituted —
        # we want the kid's original words to survive for the transcript.
        if re.search(pattern, current, flags=re.IGNORECASE):
            current = f"{current} [{annotation}]"
            applied.append(f"strategy:{pattern}→{annotation}")

    # Light whitespace cleanup
    current = re.sub(r"\s+", " ", current).strip()

    return Normalization(raw=utterance, normalized=current, applied_rules=applied)
