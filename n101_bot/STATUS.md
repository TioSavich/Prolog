# n101_bot — build status

Date: 2026-04-16, overnight build session.

## UPDATE: deterministic round-trip reached retrieval layer, not generation layer

See [logs/deterministic_roundtrip_report.md](logs/deterministic_roundtrip_report.md) for the full write-up. Summary:

- ✅ Prolog → LQL → larql patch → vindex round-trip works end-to-end.
- ✅ `DESCRIBE "quantity"` on the patched vindex returns 9 authored edges at Layer 10, gate 9.0-9.5. Vanilla vindex returns "no edges found."
- ✅ 91 operations, 1.0 MB `.vlp` patch file, idempotent.
- ⚠️ `WALK` and `INFER` show bit-identical activations before/after patch — larql tags the insertions as `retrieval-override`, a KNN store overlay that DESCRIBE consults but the forward pass ignores.
- ⚠️ Adding `ALPHA 0.30` per larql's README made no difference on Llama; the README examples use Gemma 4B. Llama path in this build does not implement weight-blend INSERTs.

What this means: we have **deterministic vocabulary as structured, retrievable knowledge attached to the model**. Any consumer that queries the patched graph gets Amy's authored edges. Weight-level generation determinism is the next step and requires either the Gemma `--level all` path (license-gated), patching larql's Llama path, or an inference-time wrapper.

Report: [logs/deterministic_roundtrip_report.md](logs/deterministic_roundtrip_report.md)

## What works tonight

### End-to-end
- `/.venv/bin/python -m bridge.cli "What is a quantity?"` runs the full pipeline: Prolog loads vocabulary, composes system prompt, DeepSeek-R1:14b answers via Ollama, Python validates against incompatibilities, prints the result.
- First live smoke test passed: the bot gave an Amy-faithful answer distinguishing quantity from measure with zero violations.

### Prolog layer ([src/vocabulary.pl](src/vocabulary.pl))
- Loads 10 per-term modules from [vocabularies/n101/](vocabularies/n101/).
- Each term declares positive definitions, required-related-terms, incompatibility rules with substring triggers, and source citations pointing into `N101coursenotes_f24.md` with line ranges.
- `load_vocabulary/1` — enumerates all term definitions.
- `validate_response/3` — scans a string and returns every fired incompatibility.
- `system_prompt/1` — composes the Amy-style system prompt (~7.4k characters).
- `export_json/1` — writes `logs/vocabulary.json`.
- `emit_lql/1` — writes `logs/vocabulary.lql`, an LQL patch script that would inject these edges into a Gemma vindex. Not yet executed against larql (see deferred, below).

### Python bridge ([bridge/](bridge/))
- `bridge.prepare` shells to `swipl` to regenerate `logs/vocabulary.json`, `logs/vocabulary.lql`, `logs/system_prompt.txt`. Single source of truth: Prolog.
- `bridge.validator` is a Python mirror of the Prolog matcher with case-insensitive substring semantics.
- `bridge.ollama_client` talks to `localhost:11434` via `/api/chat`.
- `bridge.hc_bot.HermeneuticBot` wraps the whole call, also strips DeepSeek-R1's `<think>...</think>` scratchpad from validation (the thinking is exploratory; only the visible answer is constrained).
- `bridge.cli` is the CLI entry point.

### Tests ([tests/](tests/))
- 20 pytest tests covering validator shape, trigger matching, false-positive resistance on faithful answers, Prolog-Python validator agreement, and LQL emitter output.
- All passing as of the snapshot before the overnight loop started.
- Run: `.venv/bin/python -m pytest tests/ -v`

### Overnight test runner ([scripts/overnight_loop.sh](scripts/overnight_loop.sh))
- Reads [scripts/questions.txt](scripts/questions.txt) (15 questions, a mix of Amy-softball and trap questions).
- Runs each through the bot, logs per-question JSON to `logs/runs_<timestamp>/`, appends a row to `logs/results_<timestamp>.csv`, and prints a running summary to `logs/overnight_<timestamp>.log`.
- `ITERATIONS` env var controls how many passes.

## What is stubbed (tonight)

### No weight editing
The LQL emitter in [src/vocabulary.pl](src/vocabulary.pl) produces real LQL text. It is **not** piped into a running `larql` binary against a Gemma vindex. Reasons:
- Only 17 GB of disk free on a 99%-full drive. A Gemma 3-4B vindex is ~3-6 GB. A debug cargo build of larql's workspace adds several more GB of `target/` artifacts. Not safe to start these unattended overnight.
- No Gemma vindex was present on disk.
- No Gemma model was pulled in Ollama either (available: deepseek-r1:14b, qwen3:32b, mistral-small, llama variants).

The runtime tonight is therefore **Ollama + DeepSeek-R1:14b + system-prompt injection + post-hoc incompatibility validation**, NOT weight editing. This is honest scaffolding — the Prolog authoring layer and the LQL emitter are production-shaped; the runtime path is a different (simpler) mechanism.

### Keyword triggers are blunt
Incompatibility matching is case-insensitive substring. A trigger that reads `"because it is easier"` fires on any text containing that substring. It does not fire on semantic paraphrases. Real parsing is later work. Triggers are explicit in each `vocabularies/n101/*.pl` file; adding more paraphrases is cheap but remains keyword-level.

### No question-posing talk moves
Tonight's bot answers questions. The Hackenberg et al. 2024 decentering vocabulary (PS → LST → FMST, or PS → FMST → LST) is the year-2 NSF endpoint target and was not built tonight. A future `vocabularies/decentering/` module would encode those moves as their own protected vocabulary.

### No instructor-publication ingestion
The design has a softer "expert-context" vocabulary layer pulled from an instructor's reading list via MCP Consensus. Not built.

## Morning review: what to look at

1. `logs/overnight_<latest>.log` — full transcript of the test run, per-question pass/fail with violation details.
2. `logs/results_<latest>.csv` — one row per (iteration, question), columns: timestamp, iteration, qindex, passed, violation_count, duration_ms, eval_tokens, question.
3. `logs/runs_<latest>/iter<N>_q<N>.json` — full JSON for each call, including the model's answer, its `<think>` scratchpad, and every violation triggered.
4. Particularly interesting rows are questions 2, 4, 6 from the set — the trap questions that should either reveal good Amy-move answers OR catch the model in an incompatibility. Patterns across iterations tell you whether DeepSeek-R1 is stable on the traps.

## What to change tomorrow

- Pull `gemma:2b` (`ollama pull gemma:2b` — 1.6 GB) for a cheaper conversational target. DeepSeek-R1:14b is thinking-heavy, ~20s per answer; Gemma will be ~3s.
- Free disk, then build larql: `cd ../larql && cargo build --release` (likely 5-15 min). Then extract a Gemma vindex from the Ollama-cached model or from HuggingFace.
- Actually apply the LQL patch: `cd ../larql && cargo run --release -p larql-cli -- lql < ../n101_bot/logs/vocabulary.lql`. Compare model behavior before and after.
- Start the decentering-move vocabulary as `vocabularies/decentering/` — PS, AQST, FMST, LST, PS as Prolog term definitions with their own incompatibilities (e.g., "AQCA is a lower-potential move per Bas-Ader and Carlson 2022").
- Wire in the E343_Cleanup talk-moves data. Claude Desktop + MCP project.
- Consider connecting to the repo's existing `pml/pml_operators.pl` — the PML compressive/expansive operator family may be the right frame for classifying the bot's OWN moves (is it compressing a student's exploration too early?).

## Honest caveats

- This is a five-hour overnight build, not a validated prototype. The vocabulary is 10 terms drawn from pages 1-12 of Amy's coursenotes. Amy has not reviewed it.
- The incompatibility triggers were written by Claude reading the notes, not by Amy specifying them. Amy would likely edit them heavily.
- DeepSeek-R1:14b is a reasoning model with its own tendencies; results with a different base model may differ materially.
- The `.lql` emitter output is plausible but has not been round-tripped through larql, so its edge schema may not perfectly match what larql expects. First actual `larql lql` run will reveal mismatches.
- The bot has no memory across turns. Each question is independent.

## Files in this directory

```
n101_bot/
├── PLAN.md                       # overnight plan (design intent)
├── README.md                     # how to run the demo
├── STATUS.md                     # this file — what works, what's stubbed
├── requirements.txt              # python deps (requests, pytest)
├── .venv/                        # python venv
├── vocabularies/n101/
│   ├── _index.pl                 # list of term files
│   ├── quantity.pl
│   ├── measurement_unit.pl
│   ├── measurement_process.pl
│   ├── measure.pl
│   ├── counting.pl
│   ├── base.pl
│   ├── base_five.pl
│   ├── explanation.pl
│   ├── strategy.pl
│   └── creative_activity.pl
├── src/
│   └── vocabulary.pl             # prolog loader, validator, exporters
├── bridge/
│   ├── __init__.py
│   ├── prepare.py                # regenerate artifacts from prolog
│   ├── validator.py              # python mirror validator
│   ├── ollama_client.py          # ollama http wrapper
│   ├── hc_bot.py                 # HermeneuticBot class
│   └── cli.py                    # `.venv/bin/python -m bridge.cli`
├── tests/
│   ├── __init__.py
│   ├── test_validator.py         # 11 tests
│   ├── test_emitter.py           # 5 tests
│   └── test_prolog_mirror.py     # 4 tests
├── scripts/
│   ├── questions.txt             # 15 N101 questions
│   └── overnight_loop.sh         # test runner
└── logs/
    ├── vocabulary.json           # generated by bridge.prepare
    ├── vocabulary.lql            # generated by bridge.prepare
    ├── system_prompt.txt         # generated by bridge.prepare
    ├── overnight_<ts>.log        # overnight transcript
    ├── results_<ts>.csv          # overnight CSV
    └── runs_<ts>/                # per-question json
```
