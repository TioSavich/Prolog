"""N103 geometry context for Hermes — Prolog-query-layer + card rendering.

This module replaces the older regex-based geometry context with a mode-aware
pipeline that calls into the eight query predicates exposed by
`umedcta-formalization/geometry/query.pl` (loaded by `src/geometry_runner.pl`)
and renders the results as LLM-friendly cards.

Public surface, in order of preference:

* `geometry_context(text, *, mode="auto", grade_band=None, standard_code=None)`
  Returns the prompt-injectable string under the `--- N103 GEOMETRY FACTS ---`
  header, or "" when nothing relevant fires. This is the form `hc_bot` calls
  today and the existing tests assert against.

* `geometry_context_with_cards(text, *, mode="auto", grade_band=None,
                              standard_code=None)`
  Same logic, but returns a `(prompt_text, cards_used)` tuple. `cards_used`
  is a list of dicts (kind, id, ...) matching the structured records the
  query layer returned, so the bot can stash them on `TurnRecord`.

The legacy file-scraping helpers — `load_geometry_cards()`,
`relevant_geometry_cards()`, and the `GeometryCard` dataclass — are still
exposed because `tests/test_geometry_context.py` calls them directly. They
read the on-disk Prolog files via regex, not the live KB; they're fine as
secondary scaffolding.

Design: docs/superpowers/specs/2026-05-04-hermes-chatbot-substrate-design.md
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

from .prolog import PrologError, geometry_query
from .runtime_env import resolve_umedcta_root


REPO_ROOT = Path(__file__).resolve().parents[2]
UMEDCTA_ROOT = resolve_umedcta_root(REPO_ROOT)

SOURCE_FILES = [
    REPO_ROOT / "Prolog" / "formalization" / "axioms_geometry.pl",
    REPO_ROOT / "Prolog" / "misconceptions" / "misconceptions_geometry.pl",
    REPO_ROOT / "Prolog" / "misconceptions" / "misconceptions_geometric_batch_1.pl",
    REPO_ROOT / "Prolog" / "misconceptions" / "misconceptions_geometric_batch_2.pl",
    UMEDCTA_ROOT / "formalization" / "axioms_geometry.pl",
    UMEDCTA_ROOT / "geometry" / "concepts" / "area_perimeter.pl",
    UMEDCTA_ROOT / "geometry" / "concepts" / "shape_recognition.pl",
    UMEDCTA_ROOT / "geometry" / "concepts" / "coordinate_geometry.pl",
    UMEDCTA_ROOT / "geometry" / "concepts" / "volume_surface_area.pl",
    UMEDCTA_ROOT / "standards" / "ccss" / "geometry.pl",
    UMEDCTA_ROOT / "standards" / "indiana" / "geometry.pl",
    UMEDCTA_ROOT / "misconceptions" / "misconceptions_geometry.pl",
    UMEDCTA_ROOT / "misconceptions" / "misconceptions_geometric_batch_1.pl",
    UMEDCTA_ROOT / "misconceptions" / "misconceptions_geometric_batch_2.pl",
]

STOPWORDS = {
    "about", "after", "again", "also", "because", "could", "does", "from",
    "have", "into", "just", "like", "more", "right", "same", "shape",
    "should", "student", "than", "that", "their", "there", "these", "they",
    "think", "this", "unit", "units", "what", "when", "where", "which",
    "with", "would", "your",
}

GEOMETRY_VOCAB = {
    "angle", "area", "axis", "circle", "cone", "cube", "cubic", "cylinder",
    "diamond", "edge", "face", "geometry", "line", "parallel", "perimeter",
    "polyhedron", "prism", "pyramid", "rectangle", "rhombus", "slope",
    "solid", "sphere", "square", "surface", "triangle", "vertex", "volume",
    "quadrilateral", "trapezoid", "polygon", "tilted", "rotation",
    # Pedagogical / methods vocabulary — also gates geometry context
    # because methods-instructor questions about levels, misconceptions,
    # and lessons are exactly what the chatbot is for.
    "hiele", "level", "misconception", "lesson", "construction",
    "transition", "reflection", "translation", "symmetry", "vertices",
    "definition", "classification", "inclusive", "exclusive",
    "congruent", "similar", "scale", "transformation",
}

VALID_MODES = {"auto", "check_answers", "ask_good_questions", "lesson_plan"}

# Per-mode card budget — see spec Section 2 for the priority table.
# Each entry is (kind, count) ordered by priority (highest first).
MODE_PLANS: dict[str, list[tuple[str, int]]] = {
    "check_answers": [
        ("misconception", 3),
        ("vh_marker", 2),
        ("arc", 1),
        ("concept", 1),
    ],
    "ask_good_questions": [
        ("bootstrap", 3),
        ("vh_marker", 2),
        ("concept", 1),
        ("arc", 1),
    ],
    "lesson_plan": [
        ("concept", 1),
        ("misconception", 2),
        ("bootstrap", 3),
        ("vh_marker", 2),
        ("arc", 1),
        ("pck", 1),
    ],
    "auto": [
        ("concept", 1),
        ("misconception", 2),
        ("bootstrap", 2),
        ("arc", 1),
    ],
}

# Approximate prompt budget — chars / 4 ≈ tokens. The spec calls for
# ~1500 tokens, so cap at ~6000 characters of *card* text (the unit-fact
# prelude and the header are extra and small).
CARD_BUDGET_CHARS = 6000


# ── Public API ───────────────────────────────────────────────────────


def geometry_context(
    text: str,
    *,
    mode: str = "auto",
    grade_band: list[int] | None = None,
    standard_code: tuple[str, str] | None = None,
) -> str:
    """Build the geometry context block for an LLM prompt.

    Returns the empty string when no card fires.
    """
    prompt, _ = geometry_context_with_cards(
        text,
        mode=mode,
        grade_band=grade_band,
        standard_code=standard_code,
    )
    return prompt


def geometry_context_with_cards(
    text: str,
    *,
    mode: str = "auto",
    grade_band: list[int] | None = None,
    standard_code: tuple[str, str] | None = None,
) -> tuple[str, list[dict]]:
    """Same as `geometry_context` but also returns the structured cards used.

    `cards_used` is a list of dicts shaped like the records the Prolog query
    layer returned — each has a `kind` field ("concept", "misconception",
    "vh_marker", "bootstrap", "arc", "pck", "bundle", or "unit_fact") plus
    record-specific fields. The bot stashes this on `TurnRecord` so the UI
    can show what the bot looked at.
    """
    if mode not in VALID_MODES:
        mode = "auto"

    cards_used: list[dict] = []
    rendered: list[str] = []

    # Layer 1 — quick unit-of-measure cards. These are deterministic and
    # cheap; existing tests assert their wording, so keep them as-is.
    lower = text.lower()
    for unit_card in _unit_fact_cards(lower):
        cards_used.append(unit_card)
        rendered.append(unit_card["text"])

    # Layer 2 — Prolog-driven cards. Skip the swipl round-trip when the
    # text doesn't look geometric to keep latency down on unrelated turns.
    if standard_code is not None or _looks_geometric(lower):
        resolved_mode = _resolve_mode(mode, lower, standard_code)
        try:
            kb_cards, kb_rendered = _kb_cards(
                text, resolved_mode, grade_band, standard_code
            )
        except PrologError:
            # Fall back gracefully if the geometry runner isn't reachable
            # — better to lose enrichment than break the turn.
            kb_cards, kb_rendered = [], []
        cards_used.extend(kb_cards)
        rendered.extend(kb_rendered)

    if not rendered:
        return "", []

    body = "\n".join(f"- {chunk}" for chunk in rendered)
    prompt = (
        "\n\n--- N103 GEOMETRY FACTS (authoritative; do not contradict) ---\n"
        + body
    )
    return prompt, cards_used


# ── Mode resolution ──────────────────────────────────────────────────


CHECK_ANSWERS_HINTS = (
    "student says", "kid says", "student wrote", "first-grader",
    "second-grader", "third-grader", "fourth-grader", "fifth-grader",
    "kindergartner", "child says", "is this right", "did i grade",
    "is this correct", "what did they get wrong", "is this an error",
)
ASK_QUESTIONS_HINTS = (
    "what should i ask", "give me questions", "good question",
    "probing question", "what would you ask", "questions to ask",
    "how do i probe", "what should i probe",
)
LESSON_PLAN_HINTS = (
    "lesson plan", "i'm planning a lesson", "planning a lesson",
    "design a lesson", "build a lesson", "lesson on",
    "scope and sequence", "unit plan",
    # "walk me through ... lesson" and "in a Nth grade lesson" patterns
    # are lesson-context cues even without the literal "lesson plan" phrase.
    "walk me through", "grade lesson", "in a kindergarten lesson",
    "transition between", "exclusive→inclusive", "exclusive to inclusive",
    "how to handle", "how to teach",
    # Definition / explanation questions — when the methods-instructor
    # user asks the bot to explain something ("what is X", "what does X
    # look like", "tell me about X"), they want content, not a probing
    # question reflected back. Route to lesson_plan so the assessing
    # pre-flight bypasses.
    "what is a", "what is the", "what does a", "what does the",
    "what are the", "what's a", "what's the", "explain",
    "tell me about", "look like", "look like at", "describe",
)
CCSS_RX = re.compile(
    r"\b[k0-9]\.[a-z]+(?:\.[a-z]+)?\.\d+[a-z]?\b", re.IGNORECASE
)


def resolve_mode(
    mode: str,
    text: str,
    standard_code: tuple[str, str] | None = None,
) -> str:
    """Public mode-resolution: turns ``auto`` into a concrete mode based on
    input shape. Used by both ``geometry_context_with_cards`` (to pick the
    card plan) and by ``hc_bot.HermeneuticBot.ask`` (to decide whether to
    bypass the assessing pre-flight). Returns one of
    {"auto", "check_answers", "ask_good_questions", "lesson_plan"}.

    Stays ``auto`` only when no hint fires — the caller can decide whether
    to treat that as "use mixed plan" or fall through to default behavior.
    """
    if mode != "auto":
        return mode
    lower = text.lower()
    if standard_code is not None:
        return "lesson_plan"
    if any(hint in lower for hint in LESSON_PLAN_HINTS):
        return "lesson_plan"
    if CCSS_RX.search(lower):
        return "lesson_plan"
    if any(hint in lower for hint in CHECK_ANSWERS_HINTS):
        return "check_answers"
    if any(hint in lower for hint in ASK_QUESTIONS_HINTS):
        return "ask_good_questions"
    return "auto"


def _resolve_mode(mode: str, lower: str, standard_code) -> str:
    """Backwards-compat shim — internal callers pass already-lowered text."""
    return resolve_mode(mode, lower, standard_code)


# ── KB card pull-and-render ──────────────────────────────────────────


def _kb_cards(
    text: str,
    mode: str,
    grade_band: list[int] | None,
    standard_code: tuple[str, str] | None,
) -> tuple[list[dict], list[str]]:
    """Pull the KB records the mode wants and render them.

    Returns (cards_used, rendered_strings). Both lists are kept in sync
    so the bot can correlate which prose came from which record.
    """
    plan = MODE_PLANS.get(mode) or MODE_PLANS["auto"]
    needed = {kind for kind, _ in plan}
    pools = _gather_pools(text, grade_band, standard_code, needed)
    return _select_and_render(pools, plan)


def _gather_pools(
    text: str,
    grade_band: list[int] | None,
    standard_code: tuple[str, str] | None,
    needed: set[str],
) -> dict[str, list[dict]]:
    """Run only the query predicates the mode actually plans to consume.

    The standards-bundle path expands into all six pools in a single Prolog
    round-trip; the token-driven path scopes its calls to `needed` so each
    geometry-context build is a small handful of swipl invocations rather
    than the full eight.
    """
    pools: dict[str, list[dict]] = {
        "concept": [],
        "misconception": [],
        "vh_marker": [],
        "bootstrap": [],
        "arc": [],
        "pck": [],
    }

    if standard_code is not None:
        framework, code = standard_code
        bundle = geometry_query("standards_bundle_for", [framework, code])
        if bundle:
            pools["concept"].append(bundle.get("concept", {}))
            pools["misconception"].extend(bundle.get("misconceptions") or [])
            pools["vh_marker"].extend(bundle.get("vh_markers") or [])
            pools["bootstrap"].extend(bundle.get("bootstraps") or [])
            arc = bundle.get("arc")
            if arc:
                pools["arc"].append(arc)
            pck = bundle.get("pck")
            if pck:
                pools["pck"].append(pck)
        return pools

    tokens = list(_tokens(text))
    gb = grade_band if grade_band else None  # None → JSON null → any
    raw_concepts = geometry_query("matching_concepts", [tokens, gb]) or []
    raw_concepts.sort(key=lambda c: -int(c.get("score") or 0))
    top_concepts = raw_concepts[:5]
    pools["concept"] = top_concepts

    if not top_concepts:
        return pools

    concept_ids = [c["id"] for c in top_concepts]
    top_concept_id = top_concepts[0]["id"]

    # Expand the *top* concept's neighborhood via material_inference +
    # developmental_marker links. This bridges cases where the user mentions
    # a phenomenon concept (e.g., `tilted_square_as_diamond`) but the
    # misconceptions / markers / bootstraps live under the property concept
    # (`orientation_invariant_naming`). See concepts/cross_links.pl for the
    # explicit cross-link records that drive this expansion.
    #
    # Why expand only the top concept and not all five: misconception
    # rendering is budget-capped (3 per turn), and expanding all five floods
    # the budget with misconceptions linked to lower-scored matches that
    # may not be relevant. The top concept's neighborhood is the highest-
    # signal expansion.
    top_expanded = (
        geometry_query("concepts_in_neighborhood", [[top_concept_id], 1])
        or [top_concept_id]
    )
    if not isinstance(top_expanded, list):
        top_expanded = [top_concept_id]
    # Misconception lookup uses top-concept neighborhood + remaining
    # top-N concept IDs (so we don't lose the breadth, but the top
    # neighborhood's misconceptions get priority by virtue of Prolog
    # enumeration ordering).
    misc_ids = list(dict.fromkeys(top_expanded + concept_ids))

    if "misconception" in needed:
        triggered = (
            geometry_query(
                "applicable_misconceptions", [text, misc_ids]
            ) or []
        )
        linked = (
            geometry_query("linked_misconceptions", [misc_ids]) or []
        )
        merged = _merge_misconceptions(triggered, linked)
        # Sort misconceptions so those linked to higher-priority concepts
        # come first. Priority = position in misc_ids (top concept's
        # neighborhood first, then remaining matched concepts in score
        # order). Triggered misconceptions stay at the top regardless,
        # since _merge_misconceptions placed them there.
        priority = {cid: i for i, cid in enumerate(misc_ids)}
        triggered_ids = {t.get("id") for t in triggered}
        merged.sort(key=lambda m: (
            0 if m.get("id") in triggered_ids else 1,
            priority.get(m.get("concept_id"), len(priority)),
        ))
        pools["misconception"] = merged

    if "vh_marker" in needed:
        pools["vh_marker"] = (
            geometry_query("vh_markers_for", [top_concept_id, "any"]) or []
        )

    if "bootstrap" in needed:
        pools["bootstrap"] = (
            geometry_query(
                "bootstraps_for", [top_concept_id, "any", "any"]
            ) or []
        )

    if "arc" in needed:
        arc = geometry_query("developmental_arc_for", [top_concept_id])
        if arc:
            pools["arc"].append(arc)

    if "pck" in needed:
        pck = geometry_query("pck_synthesis_for", [top_concept_id])
        if pck:
            pools["pck"].append(pck)

    return pools


def _merge_misconceptions(
    triggered: list[dict], linked: list[dict]
) -> list[dict]:
    """Triggered misconceptions outrank linked-only ones; dedupe by id."""
    seen: set[str] = set()
    merged: list[dict] = []
    for m in list(triggered) + list(linked):
        mid = m.get("id")
        if not mid or mid in seen:
            continue
        seen.add(mid)
        merged.append(m)
    return merged


def _select_and_render(
    pools: dict[str, list[dict]],
    plan: list[tuple[str, int]],
) -> tuple[list[dict], list[str]]:
    chosen: list[dict] = []
    rendered: list[str] = []
    char_budget = CARD_BUDGET_CHARS
    for kind, count in plan:
        for record in pools.get(kind, [])[:count]:
            text = _render_record(kind, record)
            if not text:
                continue
            cost = len(text) + 4  # bullet/newline overhead
            if cost > char_budget and chosen:
                # Budget exhausted; stop adding lower-priority cards.
                return chosen, rendered
            chosen.append(record)
            rendered.append(text)
            char_budget -= cost
    return chosen, rendered


# ── Per-kind renderers ───────────────────────────────────────────────


def _render_record(kind: str, record: dict) -> str:
    if not record:
        return ""
    table = {
        "concept": _render_concept,
        "misconception": _render_misconception,
        "vh_marker": _render_vh_marker,
        "bootstrap": _render_bootstrap,
        "arc": _render_dev_arc,
        "pck": _render_pck,
    }
    fn = table.get(kind)
    return fn(record) if fn else ""


def _render_concept(c: dict) -> str:
    cid = c.get("id", "?")
    name = (c.get("name") or "").strip()
    topic = c.get("topic") or "?"
    return f"CONCEPT {cid} ({topic}): {_squeeze(name)}"


def _render_misconception(m: dict, matched_trigger: str | None = None) -> str:
    mid = m.get("id", "?")
    cid = m.get("concept_id") or "?"
    name = _squeeze(m.get("name") or "")
    repair = _squeeze(m.get("repair") or "")
    tier = m.get("tier")
    trigger = matched_trigger or m.get("matched_trigger")
    head = f"MISCONCEPTION {mid} (under {cid}; tier {tier}): {name}."
    if trigger and trigger != "all":
        head += f' Trigger: "{_squeeze(trigger)}".'
    if repair:
        head += f" Repair: {repair}"
    return head


def _render_vh_marker(v: dict) -> str:
    level = v.get("level")
    phrases = v.get("phrases") or []
    citation = ", ".join(v.get("citation") or [])
    tier = v.get("tier")
    sample = "; ".join(_squeeze(p) for p in phrases[:3])
    head = f"VH-LEVEL-{level} marker (tier {tier}): {sample}"
    if citation:
        head += f" [{citation}]"
    return head


def _render_bootstrap(b: dict) -> str:
    bid = b.get("id", "?")
    bs_kind = b.get("bs_kind") or "activity"
    prompt = _squeeze(b.get("prompt") or "")
    tools = ", ".join(b.get("tools") or [])
    citation = ", ".join(b.get("citation") or [])
    tier = b.get("tier")
    head = f"BOOTSTRAP {bid} ({bs_kind}; tier {tier}): {prompt}"
    if tools:
        head += f" [tools: {tools}]"
    if citation:
        head += f" [{citation}]"
    return head


def _render_dev_arc(a: dict) -> str:
    arc_id = a.get("arc_concept_id", "?")
    fr = _squeeze(a.get("from") or "?")
    to = _squeeze(a.get("to") or "?")
    evidence = a.get("evidence") or []
    head = f"DEV-ARC {arc_id}: {fr} → {to}"
    if evidence:
        sample = "; ".join(_squeeze(e) for e in evidence[:2])
        head += f". Evidence: {sample}"
    return head


def _render_pck(p: dict) -> str:
    kkt = p.get("key_kid_thinking") or []
    ktm = p.get("key_teacher_moves") or []
    arc = _squeeze(p.get("developmental_arc") or "")
    citation = ", ".join(p.get("citation") or [])
    parts = []
    if kkt:
        parts.append("key kid thinking: " + "; ".join(kkt[:3]))
    if ktm:
        parts.append("key teacher moves: " + "; ".join(ktm[:3]))
    if arc:
        parts.append(f"arc: {arc}")
    body = " | ".join(parts) if parts else ""
    head = f"PCK synthesis: {body}"
    if citation:
        head += f" [{citation}]"
    return head


# ── Unit-fact prelude (kept for backwards compatibility with tests) ──


def _unit_fact_cards(lower: str) -> list[dict]:
    cards: list[dict] = []
    if _unit_question(lower, "area"):
        cards.append({
            "kind": "unit_fact",
            "id": "area_unit_fact",
            "text": (
                "AREA UNIT FACT: Area is measured in square units or unit "
                "squares. If the length unit is inches, the area unit is "
                "square inches; if the length unit is centimeters, the area "
                "unit is square centimeters. Do not answer an area-unit "
                "question with a linear unit such as inches, centimeters, "
                "paperclips, or popsicle sticks unless you explicitly square "
                "that unit."
            ),
        })
    if _unit_question(lower, "perimeter"):
        cards.append({
            "kind": "unit_fact",
            "id": "perimeter_unit_fact",
            "text": (
                "PERIMETER UNIT FACT: Perimeter is boundary length, so it is "
                "measured in linear units such as inches, centimeters, feet, "
                "paperclips, or other repeatable length units."
            ),
        })
    if _unit_question(lower, "volume"):
        cards.append({
            "kind": "unit_fact",
            "id": "volume_unit_fact",
            "text": (
                "VOLUME UNIT FACT: Volume is measured in cubic units, such as "
                "cubic inches, cubic centimeters, or unit cubes."
            ),
        })
    if _unit_question(lower, "angle"):
        cards.append({
            "kind": "unit_fact",
            "id": "angle_unit_fact",
            "text": (
                "ANGLE UNIT FACT: Angle measure is commonly expressed in "
                "degrees in elementary geometry. The unit describes "
                "turn/opening, not the side lengths of the angle drawing."
            ),
        })
    return cards


def _unit_question(text: str, quantity: str) -> bool:
    if quantity not in text:
        return False
    if "unit" in text or "units" in text:
        return True
    return bool(re.search(rf"\b(measure|measured|measuring)\b.{0,40}\b{quantity}\b", text))


# ── Tokenization & geometry-detection ────────────────────────────────


def _tokens(text: str) -> set[str]:
    raw = re.findall(r"[a-zA-Z][a-zA-Z0-9]{2,}", text.lower().replace("_", " "))
    tokens = {t for t in raw if t not in STOPWORDS}
    tokens.update(t[:-1] for t in list(tokens) if t.endswith("s") and len(t) > 4)
    if "square" in tokens and "area" in tokens:
        tokens.add("area_unit_is_a_square")
    return tokens


def _looks_geometric(lower: str) -> bool:
    return bool(_tokens(lower) & GEOMETRY_VOCAB)


def _squeeze(s: str) -> str:
    return re.sub(r"\s+", " ", s).strip()


# ── Legacy file-scraping helpers ─────────────────────────────────────
#
# Older callers (and `tests/test_geometry_context.py`) import these to
# pull cards directly out of the on-disk Prolog source. They sit alongside
# the live KB query layer rather than replacing it because the test suite
# asserts on names extracted by these regexes.


@dataclass(frozen=True)
class GeometryCard:
    kind: str
    name: str
    topic: str
    body: str
    source: Path


def relevant_geometry_cards(text: str, *, limit: int = 4) -> list[GeometryCard]:
    if not _is_geometry_query(text):
        return []
    query = _tokens(text)
    if not query:
        return []
    scored = []
    lower = text.lower()
    for card in load_geometry_cards():
        score = _score_card(query, lower, card)
        if score > 0:
            scored.append((score, card))
    scored.sort(key=lambda item: (-item[0], item[1].kind, item[1].name))
    return [card for _score, card in scored[:limit]]


def load_geometry_cards() -> list[GeometryCard]:
    """Read the on-disk Prolog files each call so repo edits stay visible."""
    cards: list[GeometryCard] = [_quadrilateral_axiom_card()]
    for path in SOURCE_FILES:
        if not path.exists() or path.suffix != ".pl":
            continue
        try:
            content = path.read_text(encoding="utf-8")
        except OSError:
            continue
        cards.extend(_concept_cards(content, path))
        cards.extend(_misconception_cards(content, path))
        cards.extend(_inference_cards(content, path))
        cards.extend(_standard_cards(content, path))
    return cards


def _concept_cards(content: str, source: Path) -> list[GeometryCard]:
    pattern = re.compile(
        r"geom_concept\(\s*([a-zA-Z0-9_]+)\s*,\s*\"((?:[^\"\\]|\\.)*)\"\s*,\s*([a-zA-Z0-9_]+)",
        re.DOTALL,
    )
    return [
        GeometryCard("concept", name, topic, _clean(desc), source)
        for name, desc, topic in pattern.findall(content)
    ]


def _misconception_cards(content: str, source: Path) -> list[GeometryCard]:
    pattern = re.compile(
        r"geom_misconception\(\s*([a-zA-Z0-9_]+)\s*,\s*([a-zA-Z0-9_]+)\s*,\s*\"((?:[^\"\\]|\\.)*)\"\s*,\s*\[(.*?)\]\s*,\s*\"((?:[^\"\\]|\\.)*)\"",
        re.DOTALL,
    )
    cards = []
    for name, concept, desc, trigger_block, repair in pattern.findall(content):
        triggers = ", ".join(
            _clean(s) for s in re.findall(r"\"((?:[^\"\\]|\\.)*)\"", trigger_block)[:3]
        )
        body = f"{_clean(desc)}. Triggers: {triggers}. Teacher move: {_clean(repair)}"
        cards.append(GeometryCard("misconception", name, concept, body, source))
    return cards


def _inference_cards(content: str, source: Path) -> list[GeometryCard]:
    pattern = re.compile(
        r"material_inference\(\s*([a-zA-Z0-9_]+)\s*,\s*\"((?:[^\"\\]|\\.)*)\"\s*,\s*\"((?:[^\"\\]|\\.)*)\"\s*,\s*([a-zA-Z0-9_]+)\s*\)",
        re.DOTALL,
    )
    return [
        GeometryCard(
            "inference",
            name,
            name,
            f"{status}: if {_clean(premise)}, then {_clean(conclusion)}",
            source,
        )
        for name, premise, conclusion, status in pattern.findall(content)
    ]


def _standard_cards(content: str, source: Path) -> list[GeometryCard]:
    pattern = re.compile(
        r"standard_anchor\(\s*([a-zA-Z0-9_]+)\s*,\s*([a-zA-Z0-9_]+)\s*,\s*\"([^\"]+)\"\s*,\s*\"((?:[^\"]|\"\")*)\"\s*\)",
        re.DOTALL,
    )
    return [
        GeometryCard(
            "standard",
            f"{jurisdiction}:{code}",
            name,
            _clean(desc.replace('""', '"')),
            source,
        )
        for name, jurisdiction, code, desc in pattern.findall(content)
    ]


def _quadrilateral_axiom_card() -> GeometryCard:
    return GeometryCard(
        "axiom",
        "quadrilateral_taxonomy_via_incompatibility",
        "inclusive_shape_hierarchy",
        (
            "Current Prolog geometry axiom: a shape entails another shape when it "
            "rejects all restrictions the target shape rejects. Square entails "
            "rectangle, rhombus, parallelogram, and quadrilateral; rectangle "
            "entails parallelogram and quadrilateral; rhombus entails "
            "parallelogram and quadrilateral; every listed shape entails "
            "quadrilateral."
        ),
        REPO_ROOT / "Prolog" / "formalization" / "axioms_geometry.pl",
    )


def _score_card(query: set[str], lower_text: str, card: GeometryCard) -> int:
    haystack = " ".join([card.name, card.topic, card.body]).lower()
    tokens = _tokens(haystack)
    score = len(query & tokens) * 3
    for phrase in (
        "area", "perimeter", "square", "rectangle", "triangle", "circle",
        "volume", "slope", "prism", "pyramid", "unit", "polyhedron",
    ):
        if phrase in lower_text and phrase in haystack:
            score += 4
    if "unit" in lower_text and (
        "unit" in haystack or "square units" in haystack or "cubic units" in haystack
    ):
        score += 5
    if any(word in lower_text for word in ("misconception", "confused", "wrong", "not", "isn't", "doesn't")) and card.kind == "misconception":
        score += 3
    if any(word in lower_text for word in ("standard", "grade", "indiana", "ccss")) and card.kind == "standard":
        score += 5
    if re.search(r"\b(what is|define|definition|tell me about)\b", lower_text):
        if card.kind == "concept":
            score += 5
        elif card.kind == "misconception":
            score -= 2
    return score


def _is_geometry_query(text: str) -> bool:
    return bool(_tokens(text.lower()) & GEOMETRY_VOCAB)


def _clean(text: str) -> str:
    return re.sub(r"\s+", " ", text.replace('\\"', '"')).strip()


def _short_source(path: Path) -> str:
    try:
        return str(path.relative_to(GITHUB_ROOT))
    except ValueError:
        return path.name


__all__ = [
    "GeometryCard",
    "geometry_context",
    "geometry_context_with_cards",
    "load_geometry_cards",
    "relevant_geometry_cards",
    "resolve_mode",
    "STOPWORDS",
    "MODE_PLANS",
    "VALID_MODES",
]
