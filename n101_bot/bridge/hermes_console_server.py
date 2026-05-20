"""Local Hermes console server.

Runs a no-dependency HTTP server around the existing Prolog-in-the-loop bot.
The default renderer is REALLMS; Ollama is available only by explicit
``HERMES_RENDERER=ollama`` override.
"""
from __future__ import annotations

import json
import mimetypes
import os
import re
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Any

from .event_importer import assert_pair_graph_safe, events_from_payload
from .hc_bot import DEFAULT_MODEL, HermeneuticBot
from .hermes_n103 import (
    analysis_payload,
    analyze_events,
    recommend_pairs,
    render_markdown,
)
from .n103_prolog_pipeline import run_prolog_pair_pipeline
from .ollama_client import OllamaError, list_models, ping
from .persistent_prolog import PersistentPrologWorker
from .prolog import reason
from .reallms_revoicer import (
    RealLMSError,
    RealLMSRevoicer,
    RevoiceSafetyError,
    reallms_api_key_configured,
)
from .runtime_env import runtime_preflight


ROOT = Path(__file__).resolve().parent.parent
WEB_ROOT = ROOT / "web"

VALID_MODES = ("auto", "check_answers", "ask_good_questions", "lesson_plan")
TRANSCRIPT_SPEAKER_RE = re.compile(
    r"^\s*(student\s*\d+|s\d+|[A-Za-z][A-Za-z .'-]{0,40})\s*:\s+\S",
    re.IGNORECASE,
)
NON_SPEAKER_LABELS = {
    "answer",
    "answers",
    "note",
    "prompt",
    "question",
    "response",
    "state",
    "trace",
}

N103_WORKFLOW_PACKET = {
    "course": "N103",
    "privacy": "metadata_only_no_student_work",
    "event_schema": {
        "required": ["event_id", "actor", "source", "symbolic", "pml"],
        "notes": [
            "Use pseudonyms only in actor fields.",
            "Send symbolic commitments and PML annotations, not student prose.",
            "Keep raw forum text, names, source paths, and evidence out of console payloads.",
        ],
    },
    "workflows": [
        {
            "unit": "Unit 3",
            "title": "Inclusive and exclusive definitions of quadrilaterals",
            "cluster": "defining_attributes_classification",
            "prompt_focus": "Pair prototype reasoning with inclusive hierarchy reasoning.",
            "pairing_use": "Use when events mention square/rectangle/rhombus membership, orientation, or definition-image tension.",
            "question_moves": ["FMST", "AQST"],
            "sample_event_count": 2,
        },
        {
            "unit": "Unit 5",
            "title": "Area on the geoboard and perimeter-area distinctions",
            "cluster": "area_tiling_unit_iteration",
            "prompt_focus": "Pair boundary-focused reasoning with region/unit-iteration reasoning.",
            "pairing_use": "Use when events distinguish or conflate area, perimeter, length, unit, and measured region.",
            "question_moves": ["AQST", "FQST"],
            "sample_event_count": 2,
        },
        {
            "unit": "Unit 8",
            "title": "Transformations, congruence, and symmetry",
            "cluster": "transformations_congruence_similarity",
            "prompt_focus": "Pair visual congruence claims with transformation-based justifications.",
            "pairing_use": "Use when events invoke turns, flips, slides, same shape, or orientation-invariant properties.",
            "question_moves": ["FMST", "GQST"],
            "sample_event_count": 2,
        },
    ],
}


def _resolve_mode(value: object) -> str:
    """Coerce an incoming mode field to one of the four supported modes.

    Unknown / missing / null values resolve to ``auto`` so the caller does
    not have to special-case them. The four allowed values match the spec's
    UI dropdown.
    """
    if value is None:
        return "auto"
    candidate = str(value).strip().lower()
    if candidate in VALID_MODES:
        return candidate
    return "auto"


def _looks_like_discussion_transcript(text: str) -> bool:
    """Detect speaker-labeled discussion text before it reaches chat rendering."""
    labels: list[str] = []
    for raw_line in text.splitlines():
        match = TRANSCRIPT_SPEAKER_RE.match(raw_line)
        if not match:
            continue
        label = re.sub(r"\s+", " ", match.group(1).strip().lower())
        if label in NON_SPEAKER_LABELS:
            continue
        labels.append(label)
    return len(set(labels)) >= 2


def _resolve_optional_model(value: object) -> str | None:
    if value is None:
        return None
    model = str(value).strip()
    if not model or model.lower() == "default":
        return None
    return model


def _bot_ask_with_mode(bot, question: str, *, temperature: float, mode: str):
    """Call ``bot.ask`` while staying compatible with both old and new signatures.

    Subagent 2 is in flight wiring ``mode`` into ``HermeneuticBot.ask``.
    Until that lands, fall back to the existing positional signature so the
    server keeps responding instead of crashing. The mode echo in the
    response is still correct because the server resolves it locally.
    """
    try:
        return bot.ask(question, temperature=temperature, mode=mode)
    except TypeError:
        return bot.ask(question, temperature=temperature)


def _extract_cards_used(record) -> list:
    """Best-effort fetch of ``record.cards_used`` with a graceful default.

    Subagent 1 surfaces a structured cards list via geometry_context;
    Subagent 2 exposes it on TurnRecord. Either parallel agent may be
    mid-flight when this server runs, so missing fields collapse to ``[]``
    instead of bringing the endpoint down.
    """
    cards = getattr(record, "cards_used", None)
    if cards is None:
        return []
    if isinstance(cards, list):
        return cards
    # Defensive: if S2 hands us a tuple/iterable, normalize to a list.
    try:
        return list(cards)
    except TypeError:
        return []


class ConsoleState:
    def __init__(self) -> None:
        self.model = DEFAULT_MODEL
        self.audience = "teacher"
        self.bot = HermeneuticBot(model=self.model, audience=self.audience)

    def get_bot(self, model: str, audience: str) -> HermeneuticBot:
        if model != self.model or audience != self.audience:
            self.model = model
            self.audience = audience
            self.bot = HermeneuticBot(model=model, audience=audience)
        return self.bot

    def reset(self, model: str, audience: str) -> None:
        self.model = model
        self.audience = audience
        self.bot = HermeneuticBot(model=model, audience=audience)


STATE = ConsoleState()


class HermesHandler(BaseHTTPRequestHandler):
    server_version = "HermesConsole/0.1"

    def do_OPTIONS(self) -> None:
        self.send_response(204)
        self._send_cors_headers()
        self.send_header("Content-Length", "0")
        self.end_headers()

    def do_GET(self) -> None:
        if self.path == "/":
            self._send_file(WEB_ROOT / "hermes_gemma_console.html")
            return
        if self.path == "/api/models":
            renderer = os.environ.get("HERMES_RENDERER", "").strip().lower() or "reallms"
            if renderer == "ollama":
                models = list_models()
                renderer_ready = ping()
                legacy_ollama_reachable = renderer_ready
            else:
                renderer = "reallms"
                models = []
                renderer_configured = reallms_api_key_configured()
                renderer_ready = renderer_configured and os.environ.get("HERMES_FORCE_OFFLINE") != "1"
                legacy_ollama_reachable = False
            if DEFAULT_MODEL not in models:
                models.insert(0, DEFAULT_MODEL)
            self._send_json(
                {
                    "default_model": DEFAULT_MODEL,
                    "models": models,
                    "renderer": renderer,
                    "renderer_ready": renderer_ready,
                    "renderer_configured": reallms_api_key_configured(),
                    "force_offline": os.environ.get("HERMES_FORCE_OFFLINE") == "1",
                    "reallms_configured": reallms_api_key_configured(),
                    "ollama_reachable": legacy_ollama_reachable,
                }
            )
            return
        if self.path == "/api/n103_workflows":
            self._handle_n103_workflows({})
            return
        if self.path == "/api/runtime_preflight":
            self._handle_runtime_preflight()
            return
        path = WEB_ROOT / self.path.lstrip("/")
        if path.is_file() and path.resolve().is_relative_to(WEB_ROOT.resolve()):
            self._send_file(path)
            return
        self._send_json({"error": "not found"}, status=404)

    def do_POST(self) -> None:
        try:
            payload = self._read_json()
            if self.path == "/api/chat":
                self._handle_chat(payload)
                return
            if self.path == "/ask":
                self._handle_ask(payload)
                return
            if self.path == "/api/reason":
                self._handle_reason(payload)
                return
            if self.path == "/api/pair":
                self._handle_pair(payload)
                return
            if self.path == "/api/pair_graph":
                self._handle_pair_graph(payload)
                return
            if self.path == "/api/n103_pipeline":
                self._handle_n103_pipeline(payload)
                return
            if self.path == "/api/revoice":
                self._handle_revoice(payload)
                return
            if self.path == "/api/reset":
                model = str(payload.get("model") or DEFAULT_MODEL)
                audience = str(payload.get("audience") or "teacher")
                STATE.reset(model, audience)
                self._send_json({"ok": True})
                return
            self._send_json({"error": "not found"}, status=404)
        except RevoiceSafetyError as exc:
            self._send_json(
                {"error": str(exc), "error_type": "revoice_safety"},
                status=400,
            )
        except RealLMSError as exc:
            self._send_json(
                {"error": str(exc), "error_type": "reallms"},
                status=502,
            )
        except Exception as exc:  # keep the local demo server honest
            self._send_json({"error": str(exc)}, status=500)

    def log_message(self, fmt: str, *args: Any) -> None:
        return

    def _handle_chat(self, payload: dict) -> None:
        message = str(payload.get("message") or "").strip()
        if not message:
            self._send_json({"error": "message is required"}, status=400)
            return
        if _looks_like_discussion_transcript(message):
            self._send_json(
                {
                    "error": (
                        "This looks like speaker-labeled student discussion text. "
                        "Use the N103 Prolog analyzer so Hermes can canonicalize it "
                        "before any REALLMS revoicing."
                    ),
                    "error_type": "chat_transcript_safety",
                    "route": "n103_pipeline",
                },
                status=400,
            )
            return
        model = str(payload.get("model") or DEFAULT_MODEL)
        audience = str(payload.get("audience") or "teacher")
        temperature = float(payload.get("temperature", 0.2))
        mode = _resolve_mode(payload.get("mode"))
        bot = STATE.get_bot(model, audience)
        try:
            record = _bot_ask_with_mode(bot, message, temperature=temperature, mode=mode)
        except (OllamaError, RealLMSError) as exc:
            self._send_json({"error": str(exc), "renderer_ready": False}, status=502)
            return
        record_dict = record.as_dict()
        # Echo mode + cards_used at the top level so the UI can render them
        # without having to dig into the record. Defensive defaults if S2's
        # TurnRecord hasn't yet grown the new fields.
        record_dict.setdefault("mode", mode)
        record_dict.setdefault("cards_used", _extract_cards_used(record))
        self._send_json(
            {
                "record": record_dict,
                "mode": record_dict["mode"],
                "cards_used": record_dict["cards_used"],
            }
        )

    def _handle_ask(self, payload: dict) -> None:
        """Mode-aware /ask endpoint per the chatbot-substrate spec.

        Accepts {question, mode?} and returns the answer + thinking +
        commitments + mode + cards_used + matched_concepts. The shape is
        what `tests/test_console_mode_endpoint.py` (S5) and the Hermes
        Console UI consume.
        """
        question = str(payload.get("question") or payload.get("message") or "").strip()
        if not question:
            self._send_json({"error": "question is required"}, status=400)
            return
        model = str(payload.get("model") or DEFAULT_MODEL)
        audience = str(payload.get("audience") or "teacher")
        temperature = float(payload.get("temperature", 0.2))
        mode = _resolve_mode(payload.get("mode"))
        bot = STATE.get_bot(model, audience)
        try:
            record = _bot_ask_with_mode(bot, question, temperature=temperature, mode=mode)
        except (OllamaError, RealLMSError) as exc:
            self._send_json({"error": str(exc), "renderer_ready": False}, status=502)
            return
        self._send_json(
            {
                "answer": record.final_answer,
                "thinking": record.final_thinking,
                "commitments": [c.as_dict() for c in record.final_commitments],
                "mode": getattr(record, "mode", mode) or mode,
                "cards_used": _extract_cards_used(record),
                "matched_concepts": record.detected_terms,
            }
        )

    def _handle_reason(self, payload: dict) -> None:
        text = str(payload.get("text") or payload.get("message") or "")
        self._send_json({"reason": reason(text).as_dict()})

    def _handle_pair(self, payload: dict) -> None:
        self._send_json(
            {
                "error": (
                    "Legacy /api/pair accepts raw text and can echo evidence. "
                    "Use /api/pair_graph with metadata-only event packets."
                ),
                "error_type": "pair_safety",
            },
            status=400,
        )

    def _handle_pair_graph(self, payload: dict) -> None:
        events = payload.get("events")
        if not isinstance(events, list):
            self._send_json({"error": "events list is required"}, status=400)
            return
        try:
            assert_pair_graph_safe(events)
        except ValueError as exc:
            self._send_json(
                {"error": str(exc), "error_type": "pair_graph_safety"},
                status=400,
            )
            return
        worker = PersistentPrologWorker()
        try:
            pairs = worker.request("pair_score", events=events)
            graph = worker.request("pair_graph", events=events)
        finally:
            worker.close()
        self._send_json({"pairs": pairs, "graph": graph})

    def _handle_n103_workflows(self, payload: dict) -> None:
        self._send_json(N103_WORKFLOW_PACKET)

    def _handle_n103_pipeline(self, payload: dict) -> None:
        raw = payload.get("events")
        if raw is None:
            raw = payload.get("transcript") or payload.get("text")
        if raw is None:
            self._send_json(
                {
                    "error": "transcript or events are required",
                    "error_type": "n103_pipeline_input",
                },
                status=400,
            )
            return
        try:
            events = events_from_payload(raw)
        except ValueError as exc:
            self._send_json(
                {"error": str(exc), "error_type": "n103_pipeline_input"},
                status=400,
            )
            return
        if not events:
            self._send_json(
                {
                    "error": "no events could be parsed",
                    "error_type": "n103_pipeline_input",
                },
                status=400,
            )
            return
        self._send_json(run_prolog_pair_pipeline(events))

    def _handle_revoice(self, payload: dict) -> None:
        if os.environ.get("HERMES_FORCE_OFFLINE") == "1":
            self._send_json(
                {
                    "error": "revoicing disabled by HERMES_FORCE_OFFLINE",
                    "error_type": "reallms_offline",
                },
                status=503,
            )
            return
        question_move = payload.get("question_move")
        pair_context = payload.get("pair_context")
        if not isinstance(question_move, dict) or not isinstance(pair_context, dict):
            self._send_json(
                {"error": "question_move and pair_context are required"},
                status=400,
            )
            return
        revoicer = RealLMSRevoicer(model=_resolve_optional_model(payload.get("model")))
        result = revoicer.revoice(
            question_move=question_move,
            pair_context=pair_context,
        )
        self._send_json({"revoice": result.as_dict()})

    def _handle_runtime_preflight(self) -> None:
        self._send_json({"runtime": runtime_preflight(ROOT)})

    def _read_json(self) -> dict:
        length = int(self.headers.get("Content-Length", "0"))
        raw = self.rfile.read(length).decode("utf-8")
        return json.loads(raw or "{}")

    def _send_file(self, path: Path) -> None:
        data = path.read_bytes()
        ctype = mimetypes.guess_type(str(path))[0] or "text/html; charset=utf-8"
        self.send_response(200)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _send_cors_headers(self) -> None:
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")

    def _send_json(self, payload: dict, *, status: int = 200) -> None:
        data = json.dumps(payload).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self._send_cors_headers()
        self.send_header("Cache-Control", "no-store, max-age=0")
        self.send_header("Pragma", "no-cache")
        self.send_header("Expires", "0")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

def main(argv: list[str] | None = None) -> int:
    import argparse

    parser = argparse.ArgumentParser(description="Run the local Hermes REALLMS console.")
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8765)
    args = parser.parse_args(argv)

    server = ThreadingHTTPServer((args.host, args.port), HermesHandler)
    print(f"Hermes console: http://{args.host}:{args.port}")
    print(f"Default renderer: {os.environ.get('HERMES_RENDERER', '').strip().lower() or 'reallms'}")
    print(f"Default model: {DEFAULT_MODEL}")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
