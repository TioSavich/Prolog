"""Model backend protocol + selector."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Protocol

from ..config import LoRPConfig


@dataclass
class ModelResponse:
    text: str
    """Primary model output with <think>...</think> removed for reasoning models."""

    thinking: str | None = None
    """Content of the <think>...</think> block if the model emitted one."""

    raw: dict[str, Any] = field(default_factory=dict)
    """Provider-specific full response. Retained for debugging and corpus collection."""


class ModelBackend(Protocol):
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
        ...


def backend_for(config: LoRPConfig) -> ModelBackend:
    """Instantiate the backend named in config. Deferred import keeps the
    dependency graph shallow when only one backend is configured."""
    if config.backend == "ollama":
        from .ollama import OllamaBackend
        return OllamaBackend(endpoint=config.endpoint, timeout_seconds=config.timeout_seconds)
    if config.backend == "openai_compat":
        from .openai_compat import OpenAICompatBackend
        return OpenAICompatBackend(
            endpoint=config.endpoint, timeout_seconds=config.timeout_seconds
        )
    raise ValueError(f"unknown backend: {config.backend!r}")
