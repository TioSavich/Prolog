"""Configuration loader.

Reads config.yaml from the package root (or a path supplied by the caller)
and exposes a typed view. Voices read their own sub-config via get_voice().
"""

from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import yaml


@dataclass(frozen=True)
class VoiceConfig:
    name: str
    model: str
    temperature: float
    max_tokens: int
    think_mode: bool


@dataclass(frozen=True)
class LoRPConfig:
    backend: str
    endpoint: str
    timeout_seconds: int
    voices: dict[str, VoiceConfig]
    output_base_dir: Path
    corpus_index_path: Path
    _raw: dict[str, Any]

    def get_voice(self, name: str) -> VoiceConfig:
        if name not in self.voices:
            raise KeyError(f"voice '{name}' not configured; known voices: {sorted(self.voices)}")
        return self.voices[name]


def _default_config_path() -> Path:
    env = os.environ.get("LORP_CONFIG")
    if env:
        return Path(env).expanduser().resolve()
    # Walk up from this file to find config.yaml at the lorp/ package root.
    here = Path(__file__).resolve().parent
    for parent in [here, *here.parents]:
        candidate = parent / "config.yaml"
        if candidate.exists():
            return candidate
    raise FileNotFoundError(
        "config.yaml not found; set LORP_CONFIG or place config.yaml at lorp/ root"
    )


def load(path: Path | str | None = None) -> LoRPConfig:
    cfg_path = Path(path).expanduser().resolve() if path else _default_config_path()
    with cfg_path.open() as fh:
        raw = yaml.safe_load(fh)

    voices_raw = raw.get("voices") or {}
    voices: dict[str, VoiceConfig] = {}
    for name, spec in voices_raw.items():
        voices[name] = VoiceConfig(
            name=name,
            model=spec["model"],
            temperature=float(spec.get("temperature", 0.0)),
            max_tokens=int(spec.get("max_tokens", 4096)),
            think_mode=bool(spec.get("think_mode", False)),
        )

    output_base = Path(raw.get("output", {}).get("base_dir", "./immanent_output")).expanduser()
    corpus_index = Path(
        raw.get("corpus_hook", {}).get("index_path", "./training_corpus/index.jsonl")
    ).expanduser()

    # Resolve relative paths against the config file's directory so the
    # package works from any cwd.
    if not output_base.is_absolute():
        output_base = (cfg_path.parent / output_base).resolve()
    if not corpus_index.is_absolute():
        corpus_index = (cfg_path.parent / corpus_index).resolve()

    return LoRPConfig(
        backend=raw.get("backend", "ollama"),
        endpoint=raw.get("endpoint", "http://localhost:11434"),
        timeout_seconds=int(raw.get("timeout_seconds", 600)),
        voices=voices,
        output_base_dir=output_base,
        corpus_index_path=corpus_index,
        _raw=raw,
    )
