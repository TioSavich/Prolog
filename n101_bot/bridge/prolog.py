"""Thin wrapper around swipl CLI invocations of src/vocabulary.pl.

Every call shells out to swipl. Per-turn latency is ~100 ms on this box;
fine for a prototype. If the bot becomes interactive at classroom scale,
switch to a long-running swipl worker (PL_HTTP_Server or piped repl).
"""
from __future__ import annotations

import json
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import List

from .runtime_env import build_runtime_env, resolve_swipl

ROOT = Path(__file__).resolve().parent.parent
VOCAB_PL = ROOT / "src" / "vocabulary.pl"
GEOMETRY_RUNNER_PL = ROOT / "src" / "geometry_runner.pl"


class PrologError(RuntimeError):
    pass


def _run(command: str, *args: str) -> str:
    """Invoke `swipl -q -g cli_main <vocab.pl> -- <command> <arg>...`."""
    cmd = [
        resolve_swipl(root=ROOT),
        "-q",
        "-g",
        "cli_main",
        str(VOCAB_PL),
        "--",
        command,
        *args,
    ]
    result = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        cwd=ROOT,
        env=build_runtime_env(ROOT),
    )
    if result.returncode != 0:
        raise PrologError(
            f"swipl {command} exit {result.returncode}\n"
            f"stderr: {result.stderr.strip()}"
        )
    return result.stdout


def detect(text: str) -> List[str]:
    """Return the list of vocabulary terms (or aliases) mentioned in text."""
    out = _run("detect", text)
    return json.loads(out)


def focused_system_prompt(terms: List[str]) -> str:
    """Compose a system prompt limited to the given terms. Empty list → full prompt."""
    csv = ",".join(terms)
    return _run("system", csv).rstrip("\n")


@dataclass(frozen=True)
class Commitment:
    term: str
    rule: str
    trigger: str
    correction: str

    @classmethod
    def from_dict(cls, d: dict) -> "Commitment":
        return cls(term=d["term"], rule=d["rule"], trigger=d["trigger"], correction=d["correction"])

    def as_dict(self) -> dict:
        return {"term": self.term, "rule": self.rule, "trigger": self.trigger, "correction": self.correction}


def commitments(text: str) -> List[Commitment]:
    out = _run("commitments", text)
    raw = json.loads(out)
    return [Commitment.from_dict(d) for d in raw]


@dataclass(frozen=True)
class ReasoningReport:
    detected: List[str]
    commitments: List[Commitment]
    consequences: List[dict]  # {term, rule, trigger, status, correction}

    def as_dict(self) -> dict:
        return {
            "detected": self.detected,
            "commitments": [c.as_dict() for c in self.commitments],
            "consequences": self.consequences,
        }


def reason(text: str) -> ReasoningReport:
    out = _run("reason", text)
    raw = json.loads(out)
    return ReasoningReport(
        detected=raw.get("detected", []),
        commitments=[Commitment.from_dict(c) for c in raw.get("commitments", [])],
        consequences=raw.get("consequences", []),
    )


# ── Hermeneutic-calculator move grammar ──

@dataclass(frozen=True)
class MoveDecision:
    """What Amy's move grammar says about a prompt.

    kind: arithmetic_computation | vocabulary_question | strategy_report | unclear
    move_tag: FMST | LST | AQST | PS
    template_prompt: the English instruction the LLM will render
    slots: filled-in placeholders the LLM can reference
    assessing: True when the move grammar says DON'T answer; ask first
    class_info: the full classifier dict (for logging / advanced use)
    """
    kind: str
    move_tag: str
    template_prompt: str
    slots: dict
    class_info: dict

    @property
    def assessing(self) -> bool:
        return self.move_tag in {"FMST", "PS"}

    def rendered_template(self) -> str:
        body = self.template_prompt
        for k, v in self.slots.items():
            body = body.replace("{{" + str(k) + "}}", str(v))
        return body

    def as_dict(self) -> dict:
        return {
            "kind": self.kind,
            "move_tag": self.move_tag,
            "assessing": self.assessing,
            "template_prompt": self.template_prompt,
            "rendered_template": self.rendered_template(),
            "slots": self.slots,
            "class_info": self.class_info,
        }


def move_for(text: str) -> MoveDecision:
    """Classify a prompt and pick an Amy-style move template."""
    out = _run("move", text)
    raw = json.loads(out)
    cls = raw.get("class", {})
    tmpl = raw.get("template", {})
    return MoveDecision(
        kind=cls.get("kind", "unclear"),
        move_tag=raw.get("move_tag", "FMST"),
        template_prompt=tmpl.get("prompt", ""),
        slots=tmpl.get("slots", {}) or {},
        class_info=cls,
    )


# ── ZCM dialogue state ──

@dataclass(frozen=True)
class DialogueState:
    """Opaque 4-tuple (assessing_pull, advancing_pull, temperature, history).

    Produced by Prolog's dialogue_state module. Python treats it as an
    opaque blob — don't peek inside unless you're also updating the
    Prolog model.
    """
    a: float
    v: float
    t: float
    history: list
    rendered: str = ""
    near_cusp: bool = False

    def as_prolog_dict(self) -> dict:
        return {"a": self.a, "v": self.v, "t": self.t, "history": self.history}

    def as_dict(self) -> dict:
        return {
            "a": self.a,
            "v": self.v,
            "t": self.t,
            "history": self.history,
            "rendered": self.rendered,
            "near_cusp": self.near_cusp,
        }


def state_init() -> DialogueState:
    out = _run("state_init")
    raw = json.loads(out)
    return DialogueState(
        a=raw["a"], v=raw["v"], t=raw["t"], history=raw["history"],
    )


def state_step(prev: DialogueState, move_tag: str, commit_count: int) -> DialogueState:
    payload = json.dumps(prev.as_prolog_dict())
    out = _run("state_step", payload, move_tag, str(commit_count))
    raw = json.loads(out)
    return DialogueState(
        a=raw["a"], v=raw["v"], t=raw["t"],
        history=raw["history"],
        rendered=raw.get("rendered", ""),
        near_cusp=raw.get("near_cusp", False),
    )


# ── Entitlement ──

@dataclass(frozen=True)
class EntitlementCheck:
    term: str
    entitled: bool
    missing_requirements: List[str]

    def as_dict(self) -> dict:
        return {
            "term": self.term,
            "entitled": self.entitled,
            "missing_requirements": self.missing_requirements,
        }


# ── Geometry KB query layer ──
#
# Wraps the eight query predicates in the geometry query layer under
# $UMEDCTA_ROOT/geometry/query.pl via a swipl subprocess that emits a single
# JSON document per call. See `n101_bot/src/geometry_runner.pl`.

GEOMETRY_PREDICATES = {
    "matching_concepts",            # (tokens, grade_band)
    "applicable_misconceptions",    # (user_text, concept_ids)
    "linked_misconceptions",        # (concept_ids[, max_tier])
    "vh_markers_for",               # (concept_id, level_opt)
    "bootstraps_for",               # (concept_id, transition, kind)
    "developmental_arc_for",        # (concept_or_arc_id,)
    "pck_synthesis_for",            # (concept_id,)
    "standards_bundle_for",         # (framework, code)
    "concepts_in_neighborhood",     # (concept_ids, depth) → list of concept atoms
}


def geometry_query(predicate: str, args: list):
    """Call one of the geometry KB query predicates.

    Returns:
        - list[dict] for predicates that return a list of records
        - dict for predicates that return a single record
        - None when the underlying Prolog returns `none` / `not_found`

    Latency is dominated by KB load (~1–2 s on first call, every call as
    long as we shell out per query). If the bot becomes interactive at
    classroom scale, switch to a long-running swipl worker — `geom_main`
    is structured so a future REPL loop is a simple drop-in.
    """
    if predicate not in GEOMETRY_PREDICATES:
        raise ValueError(f"unknown geometry predicate: {predicate}")
    payload = json.dumps(args, ensure_ascii=False)
    cmd = [
        resolve_swipl(root=ROOT),
        "-q",
        "-g",
        "geom_main",
        str(GEOMETRY_RUNNER_PL),
        "--",
        predicate,
        payload,
    ]
    result = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        cwd=ROOT,
        env=build_runtime_env(ROOT),
    )
    if result.returncode != 0:
        raise PrologError(
            f"swipl geometry_query {predicate} exit {result.returncode}\n"
            f"stderr: {result.stderr.strip()}"
        )
    out = result.stdout.strip()
    if not out or out == "null":
        return None
    return json.loads(out)


def entitlement(term: str, engaged: List[str], fires: List[Commitment]) -> EntitlementCheck:
    """Query Prolog whether the speaker is entitled to use `term`.

    Entitled iff every term in `term`'s `requires` list appears in
    `engaged` AND no incompatibility-fire on `term` appears in `fires`.
    """
    engaged_csv = ",".join(engaged)
    fires_csv = ";".join(f"{c.term}:{c.rule.replace(' ', '_')}" for c in fires)
    out = _run("entitlement", term, engaged_csv, fires_csv)
    raw = json.loads(out)
    return EntitlementCheck(
        term=term,
        entitled=bool(raw.get("entitled", False)),
        missing_requirements=list(raw.get("missing_requirements", [])),
    )
