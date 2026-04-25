"""Ollama backend.

Uses the /api/chat endpoint. Splits <think>...</think> out of the primary
text for reasoning models (DeepSeek-R1 et al), putting the reasoning
trace in ModelResponse.thinking and the post-think content in
ModelResponse.text.
"""

from __future__ import annotations

import re

import requests

from .protocol import ModelResponse


_THINK_RE = re.compile(r"<think>(.*?)</think>", re.DOTALL)


class OllamaBackend:
    def __init__(self, endpoint: str, timeout_seconds: int):
        self.endpoint = endpoint.rstrip("/")
        self.timeout = timeout_seconds

    def complete(
        self,
        system_prompt: str,
        user_message: str,
        *,
        model: str,
        temperature: float = 0.0,
        max_tokens: int = 4096,
        think_mode: bool = False,  # informational; handled by the model itself
    ) -> ModelResponse:
        payload = {
            "model": model,
            "stream": False,
            "messages": [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_message},
            ],
            "options": {
                "temperature": temperature,
                "num_predict": max_tokens,
            },
        }

        resp = requests.post(
            f"{self.endpoint}/api/chat",
            json=payload,
            timeout=self.timeout,
        )
        resp.raise_for_status()
        body = resp.json()
        full_text = (body.get("message") or {}).get("content", "")

        thinking = None
        m = _THINK_RE.search(full_text)
        if m:
            thinking = m.group(1).strip()
            full_text = _THINK_RE.sub("", full_text).strip()

        return ModelResponse(text=full_text, thinking=thinking, raw=body)
