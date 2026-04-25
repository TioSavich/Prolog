# LoRP

Standalone port of the `immanent-reader` Claude skill to a Python pipeline
that runs against open-source LLM backends.

Takes a philosophical passage and emits Prolog substrate facts describing
what the sentences *do* (naming, quotation, adjacent assertions,
procedural directives, cross-language pairs) without attributing
function or structure to the author's concepts.

Inspired by LoRP (Di et al., 2025, *Knowledge-Based Systems*), scoped to
decomposition only. No theorem-proving, no voting, no rhythm-axiom
polarization. Those belong to `pml-reader` and to the engine in the
`Prolog/` submodule, and are deferred to later phases (see `TODO.md`).

## Quickstart

```bash
cd lorp
python3 -m venv .venv
. .venv/bin/activate
pip install -r requirements.txt

# ensure ollama is running and the configured model is available
ollama list | grep deepseek-r1

# run on the test fixture
python -m lorp.runners.single --passage tests/fixtures/minimal_passage.txt

# outputs land under ./immanent_output/<stem>/
ls immanent_output/minimal_passage/
```

## Swapping the model

Edit `config.yaml`:

```yaml
voices:
  immanent:
    model: qwen3:32b          # or gemma4:4b once pulled, or any Ollama tag
```

Or point at a self-hosted OpenAI-compatible endpoint (vLLM, TGI, SGLang):

```yaml
backend: openai_compat
endpoint: http://bigred.iu.edu:8000
voices:
  immanent:
    model: google/gemma-4-27b
```

No code changes. Set `LORP_OPENAI_COMPAT_KEY` in the environment if your
endpoint requires a bearer token.

## Batch run

```bash
python -m lorp.runners.batch --corpus ../Prolog/misconceptions/pdfs --glob '*.txt'
```

Walks the directory, processes each match, writes per-file artifacts under
`immanent_output/`, appends (passage, axioms, verdict) quadruples to
`training_corpus/index.jsonl`.

## Output per passage

```
immanent_output/<stem>/
├── <stem>.pl              # Prolog substrate (or a stub if rejected)
├── <stem>_gloss.md        # verbatim passage + fact-by-fact justification
├── <stem>_validation.md   # syntax + immanence verdicts, thinking trace
└── <stem>_rejected.pl     # only if rejected; the bad candidate for forensics
```

## What the pipeline does (per passage)

1. Voice (`voices/immanent.py`) loads the system prompt and sends the
   passage to the configured model.
2. Validator runs two checks:
   - **Syntax:** `swipl -q -t halt -g true <scratch>.pl`
   - **Immanence:** regex scan for unsafe predicates
     (`fixed_point/N`, `self_names/1`, `is_*_function/1`,
     `gaifman_structure/N`, `essence/N`, `structure/N`, `function/N`,
     `located_in/2`) in non-comment positions.
3. On failure, the runner retries up to twice, re-prompting with the
   error. After that it writes the candidate to `<stem>_rejected.pl`
   and moves on.
4. Gloss emits the three artifacts above.
5. Corpus hook appends a JSONL line for downstream training.

## Testing

```bash
pytest tests/
```

No model dependency — tests cover the syntax and immanence validators
only. End-to-end smoke testing goes through the `runners.single` CLI.

## Relationship to the other artifacts in this repo

- `../Prolog/` (submodule → `umedcta-formalization`) — the kernel the
  substrate facts eventually feed into. The PoC does **not** load
  substrate into the kernel; that belongs to `pml-reader`'s rhythm-axiom
  layer, which is deferred.
- `../n101_bot/` (gitignored) — the Gemma/Prolog/DeepSeek prototype this
  pipeline generalizes. Kept locally as a sketch.
- `../larql/` — weight-editing infrastructure, orthogonal to this port.
- `~/.claude/skills/immanent-reader/` — the canonical source-of-truth
  skill. This port mirrors `references/*` into `lorp/voices/prompts/`;
  when the skill updates, the mirror updates.

## References

- Spec: `../docs/superpowers/specs/2026-04-24-lorp-immanent-reader-port-design.md`
- Skill source: `~/.claude/skills/immanent-reader/SKILL.md`
- LoRP paper: `../LoRP_LLM_Prolog.pdf` (Di et al., 2025)
