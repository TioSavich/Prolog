"""Pipeline steps: decomposer (in voices/), validator, gloss writer."""

from .validator import (  # noqa: F401
    ImmanenceVerdict,
    SyntaxVerdict,
    validate_immanence,
    validate_syntax,
)
from .gloss import write_outputs  # noqa: F401
