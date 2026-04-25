"""LoRP — open-source-backed port of the immanent-reader skill.

Phase 1 scope: decompose a passage into Prolog substrate facts via a
configurable LLM backend (Ollama locally, OpenAI-compatible for HPC),
validate syntax with SWI-Prolog, validate immanence against the skill's
canonical rule.

See ../docs/superpowers/specs/2026-04-24-lorp-immanent-reader-port-design.md
for the design; ../lorp/TODO.md for deferred phases.
"""

__version__ = "0.1.0"
