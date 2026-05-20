"""HermeneuticBot — Prolog-in-the-loop round-trip.

Flow per turn:
  1. Prolog detects which vocabulary terms the student mentioned
  2. Compose focused system prompt (just those terms' cards)
  3. Generate with Ollama
  4. Prolog scans the answer for incompatibility triggers ("commitments")
  5. If anything fired, repair turn: append "Amy would correct this" and
     regenerate once
  6. Record the surviving commitments to the session ledger

Non-monotonic: each turn's ledger grows; a new commitment can revoke an
entitlement set by an earlier turn (that logic lives in Prolog but is
only partially modeled tonight — see material_consequences/2).
"""
from __future__ import annotations

import os
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional, Tuple

from .normalize import Normalization, normalize
from .ollama_client import ChatResult, chat
from .prolog import (
    Commitment,
    DialogueState,
    EntitlementCheck,
    MoveDecision,
    commitments,
    detect,
    entitlement,
    focused_system_prompt,
    move_for,
    state_init,
    state_step,
)
from . import geometry_context as _geometry_context_mod
from .geometry_context import geometry_context
from .validator import strip_thinking


# Subagent 1 owns geometry_context.py. Their rewrite may expose either:
#   - geometry_context_with_cards(text, *, mode, ...) -> (str, list)
#   - or just geometry_context(text, *, mode, ...) -> str
# We probe at call time so this module works against either interface.
_GEOMETRY_CONTEXT_WITH_CARDS = getattr(
    _geometry_context_mod, "geometry_context_with_cards", None
)


ROOT = Path(__file__).resolve().parent.parent
LOGS = ROOT / "logs"
DEFAULT_MODEL = os.environ.get("HERMES_MODEL", "gemma:2b")


@dataclass
class TurnRecord:
    question: str
    raw_question: str
    normalization: Normalization
    detected_terms: List[str]
    entitlements: List[EntitlementCheck]
    move: MoveDecision
    first_answer: str
    first_thinking: str
    first_commitments: List[Commitment]
    final_answer: str
    final_thinking: str
    final_commitments: List[Commitment]
    repaired: bool
    assessing: bool
    state_before: DialogueState
    state_after: DialogueState
    model: str
    duration_ms: float
    eval_tokens: int
    mode: str = "auto"
    cards_used: list = field(default_factory=list)

    @property
    def passed(self) -> bool:
        return len(self.final_commitments) == 0

    def as_dict(self) -> dict:
        return {
            "question": self.question,
            "raw_question": self.raw_question,
            "normalization": self.normalization.as_dict(),
            "detected_terms": self.detected_terms,
            "entitlements": [e.as_dict() for e in self.entitlements],
            "move": self.move.as_dict(),
            "first_answer": self.first_answer,
            "first_thinking": self.first_thinking,
            "first_commitments": [c.as_dict() for c in self.first_commitments],
            "final_answer": self.final_answer,
            "final_thinking": self.final_thinking,
            "final_commitments": [c.as_dict() for c in self.final_commitments],
            "repaired": self.repaired,
            "assessing": self.assessing,
            "state_before": self.state_before.as_dict(),
            "state_after": self.state_after.as_dict(),
            "passed": self.passed,
            "model": self.model,
            "duration_ms": self.duration_ms,
            "eval_tokens": self.eval_tokens,
            "mode": self.mode,
            "cards_used": self.cards_used,
        }


@dataclass
class Session:
    model: str = DEFAULT_MODEL
    history: List[TurnRecord] = field(default_factory=list)
    ledger: List[Commitment] = field(default_factory=list)
    engaged_terms: List[str] = field(default_factory=list)
    state: DialogueState = field(default_factory=state_init)

    def ledger_rules(self) -> set:
        return {(c.term, c.rule) for c in self.ledger}

    def engage(self, terms: List[str]) -> None:
        """Add to the running list of terms the speaker has engaged.

        Used by the entitlement graph: a term is entitled-for-use only
        after the speaker has engaged its required relata. De-duplicated
        to preserve the set-like semantics without losing order.
        """
        for t in terms:
            if t not in self.engaged_terms:
                self.engaged_terms.append(t)


TEACHER_AUDIENCE_NOTE = (
    "\n\n--- AUDIENCE: TEACHER ---\n"
    "You are producing a SUGGESTED QUESTION OR OBSERVATION for a "
    "classroom teacher to consider using with a student. Frame your "
    "response in the third person ('You might ask: ...' or "
    "'Consider probing how they...'). Do NOT address the student "
    "directly. The teacher decides whether to use your suggestion. "
    "Do not invent a student name; if no name was given, say 'the student' "
    "or leave the name out. If a move instruction says to let the student "
    "generate an example, do not provide your own example and do not write "
    "'for example'."
)

# Lesson-plan audience framing. When the teacher is planning a lesson, the
# default "You might ask: ..." framing produces a probing question instead
# of actual lesson content. This audience note tells the model to produce
# a structured lesson-planning brief addressed to the teacher directly.
TEACHER_LESSON_PLAN_AUDIENCE_NOTE = (
    "\n\n--- AUDIENCE: TEACHER PLANNING A LESSON ---\n"
    "You are producing a STRUCTURED LESSON-PLANNING BRIEF for a teacher. "
    "Address the teacher directly in second person ('You can…', 'For "
    "this lesson…'). Do NOT frame your response as a question to ask "
    "the student. Do NOT respond with 'You might ask…'. Produce concrete "
    "answers, recommendations, and content the teacher can use to plan: "
    "anchor concept, prerequisites, common misconceptions to watch for, "
    "recommended activities, transition targets, and standards "
    "alignment if known. Reference the geometry facts above by name."
)


# Mode-specific framing fragments. Appended to the system prompt when the
# caller picks a non-auto mode. `auto` falls back to TEACHER_AUDIENCE_NOTE
# (which is appended separately for audience='teacher').
MODE_FRAMING = {
    "check_answers": (
        "\n\n--- MODE: CHECK ANSWERS ---\n"
        "The input is kid-talk or student work. Identify the concept(s) "
        "engaged, the likely van Hiele level, any misconception "
        "(reference the matched trigger if one fired), and a teacher "
        "repair move. Address the teacher in third person."
    ),
    "ask_good_questions": (
        "\n\n--- MODE: ASK GOOD QUESTIONS ---\n"
        "The teacher wants probing questions to advance student "
        "thinking. Tie each question to a specific concept and a target "
        "vH-level transition where applicable. Frame as 'You might "
        "ask…' or 'Consider asking…'."
    ),
    "lesson_plan": (
        "\n\n--- MODE: LESSON PLAN ---\n"
        "The teacher is planning a lesson. Use the standards bundle to "
        "organize: anchor concept, prerequisites, common misconceptions "
        "to watch for, recommended activities, and any developmental "
        "transition target. Produce a structured lesson-planning brief, "
        "not a list of questions to ask. Give actual answers and "
        "concrete recommendations."
    ),
    "auto": "",  # falls back to TEACHER_AUDIENCE_NOTE
}


# Recognized standard-code framework prefixes for _extract_standard_code.
# Order matters: longer / more specific patterns first.
_STANDARD_CODE_PATTERNS = [
    # Indiana math: e.g. "4.G.1", "K.G.2", "6.G.A.1"
    ("in_indiana", re.compile(r"\b(?:IN[.\s-]*)?([K\d]+\.G(?:\.[A-Z])?\.\d+[a-z]?)\b")),
    # CCSS geometry: e.g. "5.G.B.3", "CCSS.MATH.CONTENT.5.G.B.3"
    (
        "ccss",
        re.compile(
            r"\b(?:CCSS(?:\.MATH(?:\.CONTENT)?)?[.\s-]*)?"
            r"([K\d]+\.G\.[A-Z]\.\d+[a-z]?)\b"
        ),
    ),
]


# Grade-band recognition patterns. Each callable returns a list[int] | None.
_GRADE_BAND_PATTERNS = [
    # "K-2", "3-5", "K–2"
    (
        re.compile(r"\b([K\d])\s*[-–]\s*(\d)\b", re.IGNORECASE),
        lambda m: _expand_grade_band(m.group(1), m.group(2)),
    ),
    # "kindergarten", "kindergartener"
    (
        re.compile(r"\bkinder(?:garten(?:er)?|gartner)?\b", re.IGNORECASE),
        lambda m: [0],
    ),
    # "first-grader", "first grader", "first grade", "1st grade"
    (
        re.compile(
            r"\b(first|second|third|fourth|fifth|sixth|seventh|eighth|"
            r"ninth|tenth|eleventh|twelfth)[ -]?grad(?:e|er)\b",
            re.IGNORECASE,
        ),
        lambda m: [_word_to_grade(m.group(1))],
    ),
    # "1st grade", "4th grader", "12th grade"
    (
        re.compile(r"\b(\d{1,2})(?:st|nd|rd|th)[ -]?grad(?:e|er)\b", re.IGNORECASE),
        lambda m: [int(m.group(1))],
    ),
    # "grade 4", "grade K"
    (
        re.compile(r"\bgrade\s+([K\d]{1,2})\b", re.IGNORECASE),
        lambda m: [_grade_token_to_int(m.group(1))],
    ),
]


def _word_to_grade(word: str) -> int:
    table = {
        "first": 1, "second": 2, "third": 3, "fourth": 4, "fifth": 5,
        "sixth": 6, "seventh": 7, "eighth": 8, "ninth": 9, "tenth": 10,
        "eleventh": 11, "twelfth": 12,
    }
    return table[word.lower()]


def _grade_token_to_int(tok: str) -> int:
    if tok.upper() == "K":
        return 0
    return int(tok)


def _expand_grade_band(lo: str, hi: str) -> list:
    a = _grade_token_to_int(lo)
    b = _grade_token_to_int(hi)
    if a > b:
        a, b = b, a
    return list(range(a, b + 1))


class HermeneuticBot:
    def __init__(
        self,
        model: str = DEFAULT_MODEL,
        *,
        audience: str = "teacher",
    ):
        """audience='teacher' (default): output framed as a suggestion for
        the teacher. audience='student': output framed to address the
        student directly (mostly for demos; Tio's explicit design
        preference is teacher-facing)."""
        self.session = Session(model=model)
        self.audience = audience

    # public API

    def ask(
        self,
        question: str,
        *,
        mode: str = "auto",
        temperature: float = 0.2,
    ) -> TurnRecord:
        """Run a turn through the bot.

        mode ∈ {"auto", "check_answers", "ask_good_questions",
        "lesson_plan"}. Default is "auto" — preserves existing behavior
        for backward compat. `lesson_plan` bypasses the assessing
        pre-flight so the bot gives concrete answers and recommendations
        rather than turning the prompt back as a question.
        """
        if mode not in MODE_FRAMING:
            raise ValueError(
                f"unknown mode {mode!r}; expected one of {sorted(MODE_FRAMING)}"
            )
        # Raw kid-talk → normalized before Prolog sees it. The normalized
        # form drives classification/detection; the raw form survives in
        # the record so the teacher dashboard can show both.
        norm = normalize(question)
        normalized_question = norm.normalized
        detected = detect(normalized_question)
        # The session's engaged-term list grows with every utterance that
        # mentions a vocabulary term. Entitlement is computed against
        # this accumulated set — that's the non-monotonic piece: a term
        # becomes entitled only when its required relata have been
        # engaged across turns, and can be revoked by a later commitment.
        self.session.engage(detected)
        entitlements = [
            entitlement(t, self.session.engaged_terms, self.session.ledger)
            for t in detected
        ]
        move = move_for(normalized_question)
        sys_prompt = focused_system_prompt(detected)
        sys_prompt = self._inject_entitlement_cues(sys_prompt, entitlements)

        # Geometry context — call S1's mode-aware renderer if available,
        # otherwise fall back to the legacy single-arg signature so this
        # module works against either interface.
        ctx_text, cards_used = self._build_geometry_context(
            normalized_question, mode=mode, question=question,
        )
        sys_prompt = sys_prompt + ctx_text

        # Mode framing fragment. `auto` is empty string; the existing
        # TEACHER_AUDIENCE_NOTE still appends below for teacher audience,
        # so default behavior is unchanged.
        # Resolve the effective mode early so all downstream framing
        # (mode-specific note, audience note, assessing bypass) uses the
        # same view of the user's intent. When mode == "auto", we run
        # geometry_context's resolver to detect lesson-plan / check-answers
        # / ask-good-questions cues from the input shape.
        try:
            from .geometry_context import resolve_mode as _resolve_mode_fn
            effective_mode = _resolve_mode_fn(mode, normalized_question)
        except Exception:
            effective_mode = mode

        if effective_mode != "auto":
            sys_prompt = sys_prompt + MODE_FRAMING[effective_mode]
        if self.audience == "teacher":
            # lesson_plan needs the lesson-planning audience note instead of
            # the default "you might ask:" framing — otherwise the model
            # produces a probing question even though the bypass fired.
            if effective_mode == "lesson_plan":
                sys_prompt = sys_prompt + TEACHER_LESSON_PLAN_AUDIENCE_NOTE
            else:
                sys_prompt = sys_prompt + TEACHER_AUDIENCE_NOTE
        state_before = self.session.state

        # ── Pre-flight: Amy's move grammar may ask us NOT to answer ──
        #
        # When the classifier labels the prompt as an assessing situation
        # (FMST: we don't know the student's thinking yet; PS: we want to
        # perturb), the bot renders an Amy-style question from the move
        # template rather than giving the answer. The "hermeneutic
        # calculator": it listens before it computes.
        #
        # `lesson_plan` mode bypasses this pre-flight: the teacher is
        # planning a lesson and wants concrete answers + recommendations,
        # not a question reflected back. Other modes keep the assessing
        # route — probing observations and probing questions are the
        # deliverables.
        if move.assessing and effective_mode != "lesson_plan":
            rendered = self._render_assessing(normalized_question, sys_prompt, move, temperature)
            final_answer, final_think = strip_thinking(rendered.content)
            final_commits = commitments(final_answer)
            state_after = state_step(state_before, move.move_tag, len(final_commits))
            record = TurnRecord(
                question=normalized_question,
                raw_question=question,
                normalization=norm,
                detected_terms=detected,
                entitlements=entitlements,
                move=move,
                first_answer=final_answer,
                first_thinking=final_think,
                first_commitments=[],
                final_answer=final_answer,
                final_thinking=final_think,
                final_commitments=final_commits,
                repaired=False,
                assessing=True,
                state_before=state_before,
                state_after=state_after,
                model=rendered.model,
                duration_ms=rendered.total_duration_ms,
                eval_tokens=rendered.eval_count,
                mode=mode,
                cards_used=cards_used,
            )
            self.session.history.append(record)
            self.session.ledger.extend(record.final_commitments)
            self.session.state = state_after
            return record

        # ── Advancing / direct answer path ──
        #
        # LST and AQST moves DO produce an answer, but the move template
        # shapes it — e.g., an AQST vocab answer should give Amy's
        # definition AND close with an "apply this to an example you
        # choose" probe. We inject the template as an addendum.
        advance_sys = sys_prompt
        if move.template_prompt:
            advance_sys = (
                sys_prompt
                + "\n\n--- MOVE INSTRUCTION ---\n"
                + f"Classification: {move.kind} (move: {move.move_tag}).\n"
                + "Follow this template as hidden guidance. Do not quote the template, "
                + "classification, move tag, or phrases like 'AQST style' in the visible answer. "
                + "Visible output should be only the teacher-facing suggestion.\n"
                + "Template instruction: " + move.rendered_template()
            )
        first = chat(self.session.model, advance_sys, normalized_question, temperature=temperature)
        first_answer, first_think = strip_thinking(first.content)
        first_commits = commitments(first_answer)

        # ── When a commitment fires on the answer ──
        #
        # Previously we ran a "repair" turn: ask the LLM to rewrite
        # without the trigger phrase. That was sanitization, not
        # engagement. Amy/Hackenberg's move when a misconception
        # appears is NOT to scrub the trace; it's to surface it. The
        # aha-catastrophe — the "oh!" moment when a student sees their
        # own thinking from a new angle — is the PURPOSE of the
        # pedagogy. Deflecting that is pedagogical malpractice.
        #
        # New behavior: if a commitment fires, we do not re-generate
        # an answer. We pivot the turn into an ASSESSING question that
        # surfaces the misconception — and we mark the turn as
        # assessing, so the dialogue-state tracker sees it correctly.
        if first_commits:
            pivot = self._pivot_to_fmst(
                normalized_question, sys_prompt, first_answer, first_commits, temperature,
            )
            final_answer, final_think = strip_thinking(pivot.content)
            final_commits = commitments(final_answer)
            repaired_flag = True  # the turn was pivoted, not "repaired"
            assessing_flag = True  # pivot IS an assessing move
            move_tag_for_state = "FMST"
            model_used = pivot.model
            duration = first.total_duration_ms + pivot.total_duration_ms
            tokens = first.eval_count + pivot.eval_count
        else:
            final_answer, final_think = first_answer, first_think
            final_commits = []
            repaired_flag = False
            assessing_flag = False
            move_tag_for_state = move.move_tag
            model_used = first.model
            duration = first.total_duration_ms
            tokens = first.eval_count

        state_after = state_step(state_before, move_tag_for_state, len(final_commits))
        record = TurnRecord(
            question=normalized_question,
            raw_question=question,
            normalization=norm,
            detected_terms=detected,
            entitlements=entitlements,
            move=move,
            first_answer=first_answer,
            first_thinking=first_think,
            first_commitments=first_commits,
            final_answer=final_answer,
            final_thinking=final_think,
            final_commitments=final_commits,
            repaired=repaired_flag,
            assessing=assessing_flag,
            state_before=state_before,
            state_after=state_after,
            model=model_used,
            duration_ms=duration,
            eval_tokens=tokens,
            mode=mode,
            cards_used=cards_used,
        )
        self.session.history.append(record)
        self.session.ledger.extend(first_commits)
        self.session.ledger.extend(final_commits)
        self.session.state = state_after
        return record

    # ── Helpers: geometry context, grade band, standard code ──

    def _build_geometry_context(
        self,
        normalized_question: str,
        *,
        mode: str,
        question: str,
    ) -> Tuple[str, list]:
        """Call the geometry context renderer.

        Subagent 1's rewrite of geometry_context.py may expose a
        cards-returning function; if so, prefer it. Otherwise fall back
        to the legacy single-argument signature and synthesize an empty
        cards_used list.

        We pull grade-band and standard-code hints from the original
        (un-normalized) question text; that's where teachers tend to
        write things like "I'm planning a 4th-grade lesson on 5.G.B.3"
        and the normalizer can strip useful surface cues.
        """
        grade_band = self._infer_grade_band(question)
        standard_code = self._extract_standard_code(question)
        if _GEOMETRY_CONTEXT_WITH_CARDS is not None:
            try:
                ctx_text, cards_used = _GEOMETRY_CONTEXT_WITH_CARDS(
                    normalized_question,
                    mode=mode,
                    grade_band=grade_band,
                    standard_code=standard_code,
                )
                return ctx_text, list(cards_used) if cards_used else []
            except TypeError:
                # Interface mismatch — fall through to legacy path.
                pass
        # Legacy fallback: single-arg geometry_context. Try the new
        # mode-aware kwargs first (in case S1 extended geometry_context
        # itself rather than adding a sister function), and gracefully
        # degrade if that signature isn't present.
        try:
            ctx_text = geometry_context(
                normalized_question,
                mode=mode,
                grade_band=grade_band,
                standard_code=standard_code,
            )
        except TypeError:
            ctx_text = geometry_context(normalized_question)
        return ctx_text, []

    def _infer_grade_band(self, question: str) -> Optional[List[int]]:
        """Pull a grade band from text like 'first-grader' / '4th grade' / 'K-2'.

        Returns None if no band is found. Returns a list of grade ints
        (K = 0). For ranges, returns the inclusive range expanded.
        """
        for pattern, builder in _GRADE_BAND_PATTERNS:
            match = pattern.search(question)
            if match:
                try:
                    band = builder(match)
                except (KeyError, ValueError):
                    continue
                if band:
                    return band
        return None

    def _extract_standard_code(
        self, question: str
    ) -> Optional[Tuple[str, str]]:
        """Extract a CCSS or Indiana standard code if one is present.

        Returns (framework, code) like ('ccss', '5.G.B.3') or
        ('in_indiana', '4.G.1'). Returns None if no recognized code is
        found. The framework is inferred from surrounding context cues
        ('CCSS' / 'common core' → ccss; 'Indiana' / 'IN' → in_indiana);
        otherwise we default to ccss for the longer A-letter pattern
        and in_indiana for the bare numeric pattern.
        """
        lower = question.lower()
        explicit_indiana = bool(
            re.search(r"\bindiana\b|\bin[-.\s]+grade\b", lower)
        )
        explicit_ccss = bool(
            re.search(r"\bccss\b|\bcommon\s*core\b", lower)
        )
        # CCSS pattern is more specific (X.G.A.N) — try it first.
        ccss_match = _STANDARD_CODE_PATTERNS[1][1].search(question)
        if ccss_match:
            framework = "ccss"
            if explicit_indiana and not explicit_ccss:
                framework = "in_indiana"
            return (framework, ccss_match.group(1))
        # Bare X.G.N (no letter) → in_indiana by default.
        in_match = _STANDARD_CODE_PATTERNS[0][1].search(question)
        if in_match:
            framework = "in_indiana"
            if explicit_ccss and not explicit_indiana:
                framework = "ccss"
            return (framework, in_match.group(1))
        return None

    def _render_assessing(
        self,
        question: str,
        sys_prompt: str,
        move: MoveDecision,
        temperature: float,
    ) -> ChatResult:
        """Ask the LLM to render an assessing question from the move template.

        The LLM is given the full vocabulary context (sys_prompt) for
        register, plus a narrow instruction telling it to PRODUCE A
        QUESTION (not answer the student). The prompt intentionally
        forbids handing over the computed answer.
        """
        rendered = move.rendered_template()
        move_sys = (
            sys_prompt
            + "\n\n--- MOVE INSTRUCTION ---\n"
            + f"The student's utterance has been classified as: {move.kind} (move: {move.move_tag}).\n"
            + "Do NOT compute or hand over the answer. Instead, render the assessing question below "
            + "in Amy's voice: warm, specific, open-ended, one or two sentences, no emoji. "
            + "Do not mention the move tag, classification, or template label in the visible answer. "
            + "If N103 geometry facts are present above, use them to make the question concrete; "
            + "do not fall back to a generic noticing question.\n\n"
            + "Template instruction: " + rendered
        )
        move_user = f"Student utterance: {question}\n\nRender the assessing question now."
        return chat(self.session.model, move_sys, move_user, temperature=temperature)

    # private

    def _inject_entitlement_cues(
        self, sys_prompt: str, entitlements: List[EntitlementCheck]
    ) -> str:
        """Augment the system prompt with per-term entitlement status.

        A term with unmet requires signals: "the student has not yet
        engaged this term's supporting machinery; before handing them
        a definition, probe one piece of that machinery." This is a
        pedagogical priority — Amy does not front-load full
        definitions onto students who haven't yet engaged the pieces.

        A term that's NOT entitled because an incompatibility fired
        signals: "the prior session has a live tension on this term;
        surface it."
        """
        if not entitlements:
            return sys_prompt
        notes = []
        for e in entitlements:
            if e.entitled:
                continue
            if e.missing_requirements:
                notes.append(
                    f"- {e.term}: speaker has not yet engaged "
                    f"{', '.join(e.missing_requirements)}. "
                    "Before giving a full definition, probe one of those."
                )
            else:
                notes.append(
                    f"- {e.term}: a prior commitment on this term is "
                    "in tension with Amy's frame. Surface the tension; "
                    "don't paper over it."
                )
        if not notes:
            return sys_prompt
        return (
            sys_prompt
            + "\n\n--- ENTITLEMENT STATUS (for your pedagogical reasoning) ---\n"
            + "\n".join(notes)
        )

    def _pivot_to_fmst(
        self,
        question: str,
        sys_prompt: str,
        unsanitized_answer: str,
        fired: List[Commitment],
        temperature: float,
    ) -> ChatResult:
        """Pivot turn: a commitment fired, so surface it rather than scrub it.

        Amy's move when a misconception appears: ask about it. Don't
        bury it. The "catastrophe" — the student feeling the ground
        shift under a belief — is the pedagogical goal, not the
        failure mode.

        We render a new assessing question that engages the student on
        exactly the commitment that fired. The LLM is told what Amy
        would correct to, but it is asked to produce a QUESTION, not a
        correction. The student arrives at the correction themselves.
        """
        rule_text = "\n".join(
            f"  - {c.rule}\n    triggered by: {c.trigger!r}\n    Amy's frame: {c.correction}"
            for c in fired
        )
        pivot_sys = (
            sys_prompt
            + "\n\n--- PIVOT INSTRUCTION (catastrophe-preserving) ---\n"
            + "A commitment fired on a previous draft of the answer. That means the "
            + "conversation is at a place where a student could genuinely learn from "
            + "surfacing the tension. DO NOT scrub the trigger away. DO NOT hand over "
            + "a corrected answer. Instead, produce an assessing question that invites "
            + "the student to articulate what they mean by the triggered phrasing. Keep "
            + "Amy's frame in view for your own reasoning, but let the student arrive "
            + "at the distinction themselves.\n\n"
            + "Rules that fired and Amy's framing (for your reasoning, not for quoting):\n"
            + rule_text
            + "\n\nRender one or two sentences, warm and curious, no emoji, no answer."
        )
        pivot_user = (
            f"Student utterance: {question}\n\n"
            f"Previous draft (you are rewriting this as an assessing question, not a corrected answer):\n{unsanitized_answer}"
        )
        return chat(
            self.session.model,
            pivot_sys,
            pivot_user,
            temperature=temperature,
        )
