"""Model backends.

`ModelBackend` is a Protocol. Implementations (ollama, openai_compat)
live in sibling modules and are selected by lorp.config.backend at runtime.
"""

from .protocol import ModelBackend, ModelResponse, backend_for  # noqa: F401
