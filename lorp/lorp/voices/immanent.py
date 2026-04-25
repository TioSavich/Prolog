"""The immanent-decomposer voice.

Loads the system prompt + canonical skill references (copied from
~/.claude/skills/immanent-reader/references/ into this package), calls
the configured model backend, and parses the structured output into a
DecomposerResult.

The voice does no validation on its own. Validation is a pipeline step
that consumes DecomposerResult.prolog.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

from ..config import LoRPConfig
from ..models import ModelBackend, ModelResponse

_PROMPTS_DIR = Path(__file__).resolve().parent / "prompts"

_PROLOG_RE = re.compile(r"<PROLOG>(.*?)</PROLOG>", re.DOTALL)
_GLOSS_RE = re.compile(r"<GLOSS>(.*?)</GLOSS>", re.DOTALL)


@dataclass
class DecomposerResult:
    prolog: str
    """The body of the <PROLOG> block, stripped. Empty string means the
    voice failed to emit a parsable block."""

    gloss: str
    """The body of the <GLOSS> block, stripped."""

    thinking: str | None
    """Raw <think>...</think> trace if the model emitted one."""

    raw_response: ModelResponse


class ImmanentVoice:
    def __init__(self, config: LoRPConfig, backend: ModelBackend):
        self.config = config
        self.backend = backend
        self.voice_cfg = config.get_voice("immanent")
        self.system_prompt = self._build_system_prompt()

    def _build_system_prompt(self) -> str:
        base = (_PROMPTS_DIR / "immanent_system.md").read_text()
        safe_predicates = (_PROMPTS_DIR / "safe_predicates.md").read_text()
        immanence_rule = (_PROMPTS_DIR / "immanence_rule.md").read_text()
        example = (_PROMPTS_DIR / "example_derrida.pl").read_text()
        rich_examples: list[str] = []
        for name in ("example_vp_originary_supplement.pl", "example_vp_trace.pl"):
            p = _PROMPTS_DIR / name
            if p.exists():
                rich_examples.append(
                    f"### {name}\n\n```prolog\n{p.read_text()}\n```"
                )
        parts = [
            base,
            "## Safe predicate catalog (authoritative)\n\n" + safe_predicates,
            "## Immanence rule (authoritative)\n\n" + immanence_rule,
            "## Canonical worked example (short)\n\n```prolog\n" + example + "\n```",
        ]
        if rich_examples:
            parts.append(
                "## Fuller worked examples — match this density and in-file commentary style\n\n"
                "These are prior immanent-reader outputs on Derridean passages. Emulate the "
                "`%` catalog-version commentary blocks, the `:- discontiguous` declarations for "
                "multi-fact predicates, the mode-selection reasoning comments on `authorial_move/4`, "
                "and the explicit passage-ID stability across related facts.\n\n"
                + "\n\n".join(rich_examples)
            )
        return "\n\n".join(parts)

    def decompose(self, passage: str, passage_stem: str) -> DecomposerResult:
        """Call the backend on a passage and parse the response.

        passage_stem is embedded in the system prompt's instruction about
        the module declaration (`:- module(list_<stem>, []).`). Use a
        short identifier like 'vp_ch1' or the text filename stem.
        """
        user = (
            f"Passage stem: {passage_stem}\n\n"
            f"Passage text:\n\n{passage.strip()}\n\n"
            "Emit the <PROLOG> and <GLOSS> blocks now."
        )

        resp = self.backend.complete(
            system_prompt=self.system_prompt,
            user_message=user,
            model=self.voice_cfg.model,
            temperature=self.voice_cfg.temperature,
            max_tokens=self.voice_cfg.max_tokens,
            think_mode=self.voice_cfg.think_mode,
        )

        prolog_match = _PROLOG_RE.search(resp.text)
        gloss_match = _GLOSS_RE.search(resp.text)

        prolog = prolog_match.group(1).strip() if prolog_match else ""
        gloss = gloss_match.group(1).strip() if gloss_match else ""

        return DecomposerResult(
            prolog=prolog,
            gloss=gloss,
            thinking=resp.thinking,
            raw_response=resp,
        )
