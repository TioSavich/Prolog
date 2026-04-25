# LoRP — immanent-reader port (Phase 1 PoC)

**Date:** 2026-04-24
**Status:** Design
**Scope:** Proof-of-concept port of the `immanent-reader` Claude skill to a standalone Python pipeline that runs against open-source LLMs via Ollama (locally) or any OpenAI-compatible endpoint (Big Red, vLLM, TGI).

## Problem

The `pml-reader` and `immanent-reader` skills already describe the pipeline — decompose a passage into Prolog substrate facts, validate against an immanence rule, emit a human-readable gloss — but both run inside Claude via the Agent tool. That couples the reader to one provider and to an interactive session. For research throughput over the `ASKTM_Data` and `misconceptions` corpora, and for running on institutional hardware, the pipeline needs to be callable from a shell with a configurable backend.

LoRP (Di et al., 2025) named the pipeline shape: translate → validate → infer → vote. The `immanent-reader` skill is already scoped to the first two steps and explicitly declines the last two. Porting it is the narrowest useful first cut.

## Non-goals for this PoC

- No `pml-reader` port (rhythm axioms, four-voice convergence, CUSP handling). The immanent substrate is the first deliverable; polarized rhythm axioms are a separate skill that layers on top.
- No voting. A single decomposition per passage. LoRP's vote step addresses translation instability; the immanence rule's re-decompose-twice policy handles that locally.
- No engine inference. The emitted substrate is declarative. No `proves/1`. (That's the lesson of the Derrida convergence — keep substrate and rhythm-axiom emission apart from engine engagement until the engine's inference table covers what the readings need.)
- No fine-tuning. See "training track" below.
- No NotebookLM integration. The skill supports it via `--notebook`; the port defers it until the local pipeline is stable.
- No commentator-register alignment in the first pass. `references/commentator_register.md` is the authoritative spec when we add `--commentator`.
- No teacher chat REPL. The batch runner is the only user-facing entry for now.

## Architecture

```
lorp/
├── config.yaml                        # model name, endpoint, backend, timeouts
├── requirements.txt
├── README.md
├── TODO.md                            # deferred phases, training track
├── lorp/
│   ├── models/
│   │   ├── protocol.py                # ModelBackend protocol
│   │   ├── ollama.py                  # Ollama implementation
│   │   └── openai_compat.py           # OpenAI-compatible (for vLLM / TGI / Big Red)
│   ├── voices/
│   │   ├── immanent.py                # loads system prompt + references, calls model
│   │   └── prompts/
│   │       ├── immanent_system.md     # derived from SKILL.md, with anti-bloat rules
│   │       ├── safe_predicates.md     # mirror of skill references (checked into lorp/)
│   │       ├── immanence_rule.md      # mirror
│   │       └── example_derrida.pl     # canonical few-shot example
│   ├── pipeline/
│   │   ├── decomposer.py              # Step 1: passage → Prolog candidate
│   │   ├── validator.py               # Step 2a: SWI syntax; Step 2b: immanence rule
│   │   └── gloss.py                   # Step 4: gloss.md writer
│   ├── runners/
│   │   ├── single.py                  # one passage → one output directory
│   │   └── batch.py                   # directory of passages → parallel or sequential
│   └── corpus_hook.py                 # writes (text, axioms, verdicts) quadruples
└── tests/
    ├── test_validator.py              # SWI + immanence validator unit tests
    └── fixtures/
        └── minimal_passage.txt
```

## Model backend protocol

```python
class ModelBackend(Protocol):
    def complete(self, system_prompt: str, user_message: str,
                 *, temperature: float = 0.0, max_tokens: int = 4096,
                 think_mode: bool = False) -> ModelResponse: ...

class ModelResponse:
    text: str                     # the model's primary output
    thinking: str | None          # <think>...</think> content for reasoning models
    raw: dict                     # provider-specific full response
```

Implementations:

- **OllamaBackend** — `POST /api/chat` against `http://localhost:11434`. Strips `<think>...</think>` from `text` and moves it to `thinking` for DeepSeek-R1 style models.
- **OpenAICompatBackend** — `POST /v1/chat/completions` against a configurable base URL. Works against vLLM, TGI, SGLang, or any OpenAI-compatible endpoint on Big Red.

Selection is via `config.yaml`:

```yaml
backend: ollama            # or: openai_compat
model: deepseek-r1:14b     # default; overridable per-voice
endpoint: http://localhost:11434  # ignored for ollama local; used for openai_compat
timeout_seconds: 300
voices:
  immanent:
    model: deepseek-r1:14b
    temperature: 0.0
    think_mode: true
```

## Pipeline flow for a single passage

1. **Load.** Read the passage text. Create `<output_dir>/<stem>/`.
2. **Decompose.** Call `voices.immanent` with the system prompt and the passage. The voice returns candidate Prolog.
3. **Validate syntax.** Write the candidate to a scratch file and shell out to `swipl -q -t halt -g "[scratch_file]"` (under a short timeout). If it fails, capture the error and re-invoke the voice with the error text; retry up to two times. After two failures, write the candidate unchanged to `<stem>_rejected.pl` and log.
4. **Validate immanence.** Parse the candidate for any predicate in the unsafe list (`fixed_point/2`, `self_names/1`, `is_*_function/1`, `gaifman_structure/2`, `essence/*`, `structure/*` applied to a concept, `function/*` applied to a concept). If any unsafe predicate is present, re-invoke the voice with a targeted error. If that retry still fails, log the passage as "cannot be decomposed under the immanence rule" and move on.
5. **Gloss.** Emit `<stem>_gloss.md` with three columns: verbatim passage / substrate facts / one-line justification citing the marker. Emit `<stem>_validation.md` with the syntax and immanence check results.
6. **Corpus hook.** Append a quadruple `(passage_path, axioms_path, syntax_verdict, immanence_verdict)` to `lorp/training_corpus/index.jsonl`. Downstream (Phase 2) uses this as the SFT corpus.

## Default model choices (rationale)

Available locally via Ollama: `deepseek-r1:14b`, `qwen3:32b`, `gemma:2b`, `mistral-small`, `llama3.1:8b`.

- **Decomposer voice default: `deepseek-r1:14b`.** Immanent decomposition is careful analytical work; a reasoning-trained model with `<think>` traces is the right tool. 9 GB, runs on the user's laptop.
- **Fallback / speed option: `qwen3:32b`.** Larger, slower, but strong at structured output. Configurable via `config.yaml`.
- **Gemma 4 (E4B / 4B) not yet pulled.** Once `ollama pull gemma4:4b` lands, it's a one-line config change. Good candidate for eventual Drafter role in Phase 2 when `pml-reader` port is added.

## Training track — deferred but not forgotten

The LoRP paper and `niklasm222/gsm8k-prolog-prover` (ACL 2026 Findings) demonstrate that GRPO with the interpreter as reward oracle produces measurable gains on NL-to-Prolog translation, at the cost of an "accuracy vs auditability" trade-off. The pipeline built here is the substrate for doing that, not a substitute for it:

- **Phase 2 (next project, not this one):** once the PoC has processed ~20 texts, the `corpus_hook.py` output becomes an SFT corpus. Fine-tune Gemma-4-E4B or Qwen2.5-3B-Instruct on positive examples (passage → legal substrate, validated syntactically and under immanence). Run SFT locally or on Big Red.
- **Phase 3:** GRPO with engine verification (SWI syntax pass + immanence rule pass + number of engine-rule-engagements) as reward. Full-parameter on Big Red; LoRA on laptop as sanity check.
- **Separate track for student-math data:** the gsm8k-prolog-prover dataset is math-word-problem-domain, directly relevant for ingesting `ASKTM_Data` student responses. A second fine-tuned model targeting math-word-problem → `clpq`-Prolog is a viable parallel workstream.

These are tracked in `lorp/TODO.md` and in the user's persistent memory so they resurface when we pick the next phase.

## Success criteria for the PoC

- `python -m lorp.runners.single --passage <path>` emits `<stem>.pl`, `<stem>_gloss.md`, `<stem>_validation.md` for a hand-picked passage and the Prolog is syntax-valid.
- `python -m lorp.runners.batch --corpus <dir>` walks a directory, processes each `.txt` / `.md`, and writes a summary of pass/reject.
- Swapping `model: deepseek-r1:14b` to `model: qwen3:32b` in `config.yaml` changes behavior without any code edits.
- A minimal test suite runs `pytest` green: syntax validator catches a malformed file, immanence validator catches a known-unsafe predicate.

## Out-of-scope failure modes the PoC will accept

- **Occasional empty emissions.** Some passages genuinely don't contain the markers the skill looks for; the skill's own policy accepts "passage cannot be decomposed under the immanence rule" as a legitimate outcome.
- **Model refuses.** If the open-source model refuses to emit on content-policy grounds (unlikely for philosophy passages, but possible), log and skip.
- **Slow passes.** `deepseek-r1:14b` in think-mode will take 30-90 seconds per passage on a laptop. The batch runner is sequential and un-optimized by design; parallelism is a later ops concern.

## Self-review

- No placeholders or TBDs.
- Scope is consistent: immanent substrate only, no rhythm-axiom emission, no engine inference.
- Architecture matches the stated problem: adapter protocol + voice + pipeline + runners is what "config-swappable model backend" requires.
- Ambiguity check: "Phase 2 / 3" are clearly deferred and tracked; not promised here.
- The training track is named with enough specificity (`niklasm222/gsm8k-prolog-prover`, GRPO, Qwen2.5-3B / Gemma-4-E4B) to pick up again later without re-research.
