"""REALLMS-backed revoicing boundary for Hermes.

The Prolog worker decides pairings and question moves. This module gives the
GUI a narrow prose layer: send only safe metadata to a REALLMS/OpenAI-compatible
chat endpoint, then filter the model output before returning it.
"""
from __future__ import annotations

import json
import os
import re
import time
from dataclasses import dataclass
from typing import Any, Callable

import requests

from .denylist import FilterResult, filter_output
from .ollama_client import ChatResult


DEFAULT_REALLMS_BASE_URL = "https://reallms.rescloud.iu.edu/direct/v1"
DEFAULT_REALLMS_MODEL = "gemma-4-31B-it"

FORBIDDEN_INPUT_KEYS = {
    "raw_text",
    "text",
    "actor_id",
    "student",
    "student_id",
    "student_name",
    "source_id",
    "path",
    "evidence",
}


class RevoiceSafetyError(ValueError):
    pass


class RealLMSError(RuntimeError):
    pass


def resolve_chat_completions_url(base_url: str | None = None) -> str:
    value = (base_url or os.environ.get("REALLMS_BASE_URL") or DEFAULT_REALLMS_BASE_URL).strip()
    if not value:
        value = DEFAULT_REALLMS_BASE_URL
    value = value.rstrip("/")
    if value.endswith("/chat/completions"):
        return value
    if value.endswith("/v1"):
        return f"{value}/chat/completions"
    return f"{value}/v1/chat/completions"


def resolve_reallms_model(model: str | None = None) -> str:
    value = (model or os.environ.get("REALLMS_MODEL") or DEFAULT_REALLMS_MODEL).strip()
    return value or DEFAULT_REALLMS_MODEL


def reallms_api_key_configured(api_key: str | None = None) -> bool:
    api_key = (api_key if api_key is not None else os.environ.get("REALLMS_API_KEY") or "").strip()
    if not api_key:
        return False
    if api_key in {"YOUR_KEY_HERE", "PASTE_KEY_HERE"}:
        return False
    return not api_key.startswith("sk-PASTE")


@dataclass(frozen=True)
class RevoiceResult:
    content: str
    provider: str
    model: str
    blocked: bool
    filter_result: FilterResult

    def as_dict(self) -> dict[str, Any]:
        return {
            "content": self.content,
            "provider": self.provider,
            "model": self.model,
            "blocked": self.blocked,
            "filter_result": self.filter_result.as_dict(),
        }


def build_revoicing_payload(
    *,
    question_move: dict[str, Any],
    pair_context: dict[str, Any],
) -> dict[str, Any]:
    """Build the safe metadata payload sent to the prose layer."""
    _assert_safe_metadata(question_move)
    _assert_safe_metadata(pair_context)
    return {
        "question_move": question_move,
        "pair_context": pair_context,
        "style": {
            "audience": "instructor_review",
            "tone": "warm, concise, non-diagnostic",
            "student_work_policy": "Do not quote or reconstruct student work.",
        },
    }


class RealLMSRevoicer:
    def __init__(
        self,
        *,
        base_url: str | None = None,
        api_key: str | None = None,
        model: str | None = None,
        timeout: float = 60.0,
        http_post: Callable[..., Any] = requests.post,
    ) -> None:
        self.chat_url = resolve_chat_completions_url(base_url)
        self.api_key = api_key if api_key is not None else os.environ.get("REALLMS_API_KEY")
        self.model = resolve_reallms_model(model)
        self.timeout = timeout
        self._http_post = http_post

    def revoice(
        self,
        *,
        question_move: dict[str, Any],
        pair_context: dict[str, Any],
    ) -> RevoiceResult:
        if not reallms_api_key_configured(self.api_key):
            raise RealLMSError("REALLMS_API_KEY is not configured")
        safe_payload = build_revoicing_payload(
            question_move=question_move,
            pair_context=pair_context,
        )
        response = _post_chat_payload(
            self._http_post,
            self.chat_url,
            headers=self._headers(),
            payload={
                "model": self.model,
                "messages": [
                    {"role": "system", "content": _SYSTEM_PROMPT},
                    {
                        "role": "user",
                        "content": json.dumps(safe_payload, ensure_ascii=False, sort_keys=True),
                    },
                ],
                "temperature": 0.2,
            },
            timeout=self.timeout,
        )
        content = _extract_chat_content(_response_json(response))
        filtered = filter_output(content)
        return RevoiceResult(
            content=filtered.text,
            provider="reallms",
            model=self.model,
            blocked=filtered.blocked,
            filter_result=filtered,
        )

    def _headers(self) -> dict[str, str]:
        headers = {"Content-Type": "application/json"}
        if self.api_key:
            headers["Authorization"] = f"Bearer {self.api_key}"
        return headers


class RealLMSChatClient:
    """OpenAI-compatible REALLMS chat client for Hermes' prose renderer."""

    def __init__(
        self,
        *,
        base_url: str | None = None,
        api_key: str | None = None,
        model: str | None = None,
        timeout: float = 120.0,
        http_post: Callable[..., Any] = requests.post,
    ) -> None:
        self.chat_url = resolve_chat_completions_url(base_url)
        self.api_key = api_key if api_key is not None else os.environ.get("REALLMS_API_KEY")
        self.model = resolve_reallms_model(model)
        self.timeout = timeout
        self._http_post = http_post

    def chat(
        self,
        system_prompt: str,
        user_message: str,
        *,
        temperature: float = 0.2,
    ) -> ChatResult:
        if not self.api_key or not reallms_api_key_configured(self.api_key):
            raise RealLMSError("REALLMS_API_KEY is not configured")
        start = time.monotonic()
        response = _post_chat_payload(
            self._http_post,
            self.chat_url,
            headers=self._headers(),
            payload={
                "model": self.model,
                "messages": [
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_message},
                ],
                "temperature": temperature,
            },
            timeout=self.timeout,
        )
        duration_ms = (time.monotonic() - start) * 1000
        content = _extract_chat_content(_response_json(response))
        filtered = filter_output(content)
        return ChatResult(
            content=filtered.text,
            raw_content=content,
            model=self.model,
            total_duration_ms=duration_ms,
            eval_count=0,
            filter_result=filtered,
        )

    def _headers(self) -> dict[str, str]:
        headers = {"Content-Type": "application/json"}
        if self.api_key:
            headers["Authorization"] = f"Bearer {self.api_key}"
        return headers


_SYSTEM_PROMPT = (
    "You are Hermes' prose revoicer for instructor review. Use only the JSON "
    "metadata provided. Do not quote, reconstruct, or infer student work. "
    "Return one concise teacher-facing prompt sentence."
)


def _assert_safe_metadata(value: Any, *, path: str = "$") -> None:
    if isinstance(value, dict):
        for key, child in value.items():
            key_text = str(key)
            if key_text in FORBIDDEN_INPUT_KEYS:
                raise RevoiceSafetyError(f"unsafe revoicing field at {path}.{key_text}")
            _assert_safe_metadata(child, path=f"{path}.{key_text}")
        return
    if isinstance(value, list):
        for index, child in enumerate(value):
            _assert_safe_metadata(child, path=f"{path}[{index}]")
        return
    if isinstance(value, (str, int, float, bool)) or value is None:
        return
    raise RevoiceSafetyError(f"unsupported revoicing value at {path}: {type(value).__name__}")


def _extract_chat_content(payload: dict[str, Any]) -> str:
    choices = payload.get("choices")
    if not choices:
        raise RealLMSError(f"reallms response missing choices: {payload}")
    message = choices[0].get("message") or {}
    content = message.get("content")
    if not isinstance(content, str) or not content.strip():
        raise RealLMSError(f"reallms response missing content: {payload}")
    return content.strip()


def _post_chat_payload(
    http_post: Callable[..., Any],
    chat_url: str,
    *,
    headers: dict[str, str],
    payload: dict[str, Any],
    timeout: float,
) -> Any:
    try:
        response = http_post(
            chat_url,
            headers=headers,
            json=payload,
            timeout=timeout,
        )
    except requests.RequestException as exc:
        raise RealLMSError(f"reallms request failed: {_sanitize_error_text(str(exc))}") from exc
    if response.status_code != 200:
        text = _sanitize_error_text(getattr(response, "text", "")[:400])
        raise RealLMSError(f"reallms returned {response.status_code}: {text}")
    return response


def _response_json(response: Any) -> dict[str, Any]:
    try:
        payload = response.json()
    except ValueError as exc:
        text = _sanitize_error_text(getattr(response, "text", ""))
        raise RealLMSError(f"reallms returned invalid JSON: {text[:400]}") from exc
    if not isinstance(payload, dict):
        raise RealLMSError(f"reallms returned invalid JSON payload: {type(payload).__name__}")
    return payload


def _sanitize_error_text(text: str) -> str:
    redacted = re.sub(
        r"(Received API Key:\s*)[^.\s]+",
        r"\1[redacted]",
        text,
        flags=re.IGNORECASE,
    )
    redacted = re.sub(
        r"(Key Hash \(Token\):\s*)[^.\s]+",
        r"\1[redacted]",
        redacted,
        flags=re.IGNORECASE,
    )
    redacted = re.sub(r"Bearer\s+[A-Za-z0-9._~+/=-]+", "Bearer [redacted]", redacted)
    redacted = re.sub(r"\bsk-[A-Za-z0-9._~+/=-]+", "sk-[redacted]", redacted)
    return redacted
