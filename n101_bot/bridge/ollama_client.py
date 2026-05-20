"""Minimal Ollama HTTP client — single /api/chat call, no streaming.

Every response from the LLM is passed through the K-12 denylist filter
before being returned. If any forbidden pattern matches, `content` is
replaced with a fallback message and `filter_result` records what
happened. The filter is deterministic — probability exactly zero of
denylisted surface strings reaching a student.
"""
from __future__ import annotations

import json
import os
from dataclasses import dataclass

import requests

from .denylist import FilterResult, filter_output


OLLAMA_URL = "http://localhost:11434"
DEFAULT_NUM_PREDICT = int(os.environ.get("HERMES_NUM_PREDICT", "220"))


@dataclass
class ChatResult:
    content: str                   # filtered text — safe to show
    raw_content: str               # what the LLM actually produced
    model: str
    total_duration_ms: float
    eval_count: int
    filter_result: FilterResult    # record of whether the filter fired

    @property
    def blocked(self) -> bool:
        return self.filter_result.blocked


class OllamaError(RuntimeError):
    pass


def chat(
    model: str,
    system_prompt: str,
    user_message: str,
    *,
    temperature: float = 0.2,
    timeout: float = 240.0,
) -> ChatResult:
    """Call Ollama /api/chat and return the assistant's content."""
    payload = {
        "model": model,
        "messages": [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_message},
        ],
        "stream": False,
        # Gemma 4 / newer Ollama thinking-capable models may otherwise put
        # tokens in message.thinking while leaving message.content empty.
        "think": False,
        "options": {"temperature": temperature, "num_predict": DEFAULT_NUM_PREDICT},
    }
    try:
        resp = requests.post(
            f"{OLLAMA_URL}/api/chat",
            json=payload,
            timeout=timeout,
        )
    except requests.RequestException as e:
        raise OllamaError(f"ollama request failed: {e}") from e

    if resp.status_code != 200:
        raise OllamaError(
            f"ollama returned {resp.status_code}: {resp.text[:400]}"
        )

    try:
        data = resp.json()
    except json.JSONDecodeError as e:
        raise OllamaError(f"ollama returned non-json: {resp.text[:400]}") from e

    message = data.get("message", {})
    content = message.get("content", "")
    if not content:
        raise OllamaError(f"ollama returned empty content: {data}")

    filtered = filter_output(content)

    return ChatResult(
        content=filtered.text,     # safe-to-show
        raw_content=content,       # what the LLM produced
        filter_result=filtered,
        model=data.get("model", model),
        total_duration_ms=data.get("total_duration", 0) / 1e6,
        eval_count=data.get("eval_count", 0),
    )


def ping() -> bool:
    """Return True if the Ollama daemon is reachable."""
    try:
        resp = requests.get(f"{OLLAMA_URL}/api/tags", timeout=5.0)
        return resp.status_code == 200
    except requests.RequestException:
        return False


def list_models() -> list[str]:
    """Return local Ollama model names, or an empty list if unavailable."""
    try:
        resp = requests.get(f"{OLLAMA_URL}/api/tags", timeout=5.0)
        if resp.status_code != 200:
            return []
        data = resp.json()
    except (requests.RequestException, json.JSONDecodeError):
        return []
    return [m.get("name", "") for m in data.get("models", []) if m.get("name")]
