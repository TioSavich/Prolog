"""REALLMS-backed revoicing boundary for Hermes.

The Prolog worker decides pairings and question moves. This module gives the
GUI a narrow prose layer: send only safe metadata to a REALLMS/OpenAI-compatible
chat endpoint, then filter the model output before returning it.
"""
from __future__ import annotations

import json
import os
from dataclasses import dataclass
from typing import Any, Callable

import requests

from .denylist import FilterResult, filter_output


DEFAULT_REALLMS_BASE_URL = os.environ.get("REALLMS_BASE_URL", "https://reallms.uits.iu.edu/v1")
DEFAULT_REALLMS_MODEL = os.environ.get("REALLMS_MODEL", "default")

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
        base_url: str = DEFAULT_REALLMS_BASE_URL,
        api_key: str | None = None,
        model: str = DEFAULT_REALLMS_MODEL,
        timeout: float = 60.0,
        http_post: Callable[..., Any] = requests.post,
    ) -> None:
        self.base_url = base_url.rstrip("/")
        self.api_key = api_key if api_key is not None else os.environ.get("REALLMS_API_KEY")
        self.model = model
        self.timeout = timeout
        self._http_post = http_post

    def revoice(
        self,
        *,
        question_move: dict[str, Any],
        pair_context: dict[str, Any],
    ) -> RevoiceResult:
        safe_payload = build_revoicing_payload(
            question_move=question_move,
            pair_context=pair_context,
        )
        response = self._http_post(
            f"{self.base_url}/chat/completions",
            headers=self._headers(),
            json={
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
        if response.status_code != 200:
            raise RealLMSError(f"reallms returned {response.status_code}: {response.text[:400]}")
        content = _extract_chat_content(response.json())
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
