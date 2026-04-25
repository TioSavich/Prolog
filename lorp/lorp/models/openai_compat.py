"""OpenAI-compatible backend.

Stub for running against vLLM, TGI, SGLang, or similar on Big Red.
Not exercised in the Phase 1 PoC — included so that the config switch
is a one-line change when the HPC setup is ready.
"""

from __future__ import annotations

import os
import re

import requests

from .protocol import ModelResponse


_THINK_RE = re.compile(r"<think>(.*?)</think>", re.DOTALL)


class OpenAICompatBackend:
    def __init__(self, endpoint: str, timeout_seconds: int):
        self.endpoint = endpoint.rstrip("/")
        self.timeout = timeout_seconds
        self.api_key = os.environ.get("LORP_OPENAI_COMPAT_KEY", "")

    def complete(
        self,
        system_prompt: str,
        user_message: str,
        *,
        model: str,
        temperature: float = 0.0,
        max_tokens: int = 4096,
        think_mode: bool = False,
    ) -> ModelResponse:
        headers = {"Content-Type": "application/json"}
        if self.api_key:
            headers["Authorization"] = f"Bearer {self.api_key}"

        payload = {
            "model": model,
            "messages": [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_message},
            ],
            "temperature": temperature,
            "max_tokens": max_tokens,
            "stream": False,
        }

        resp = requests.post(
            f"{self.endpoint}/v1/chat/completions",
            headers=headers,
            json=payload,
            timeout=self.timeout,
        )
        resp.raise_for_status()
        body = resp.json()

        choice = (body.get("choices") or [{}])[0]
        full_text = (choice.get("message") or {}).get("content", "")

        thinking = None
        m = _THINK_RE.search(full_text)
        if m:
            thinking = m.group(1).strip()
            full_text = _THINK_RE.sub("", full_text).strip()

        return ModelResponse(text=full_text, thinking=thinking, raw=body)
