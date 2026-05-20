# n101_bot — overnight plan (2026-04-16)

## Status: experimental. Not part of the UMEDCTA formalization.

This directory is experimental code built 2026-04-16 to test the
vocabulary-protection thesis: can a professor-authored incompatibility
set render an LLM's output faithful to their course's key terms?

## What this is

A minimal end-to-end demo:

1. Per-term Prolog files in `vocabularies/n101/` encode N101 terms
   as `term(Name, PositiveDefs, Requires, Incompatibilities, Sources)`.
   Sources point into `N101coursenotes_f24.md` with line numbers so
   every claim is auditable.
2. A Prolog loader (`src/vocabulary.pl`) validates term files, exports
   JSON, and emits a stub LQL patch file (`.vlp`) in larql's format.
   The emitter is real code; it simply isn't executed against a vindex
   tonight because (a) no Gemma vindex is on disk and (b) only 17 GB
   of free disk remains.
3. A Python bridge (`bridge/`) loads the JSON, composes a system prompt
   from the positive definitions + "don't say" rules, calls DeepSeek-R1:14b
   via the local Ollama instance, and runs the Prolog validator over the
   model's response. Violations are surfaced, not hidden.
4. Tests (`tests/`) cover the loader, the validator's incompatibility
   triggers, the emitter's output shape, and one end-to-end question
   against the running Ollama model.
5. An overnight runner (`scripts/overnight_loop.sh`) cycles through a
   small question set, logs to `logs/`, and leaves a morning-readable
   report.

## What this is NOT

- Not weight editing. Runtime is system-prompt injection + post-hoc
  incompatibility check. The larql `.vlp` emitter exists as code; it
  is not wired to a vindex tonight.
- Not inferentialism in a strong philosophical sense. Incompatibilities
  are keyword-triggered. A real Brandom-native implementation would
  require genuine parsing.
- Not a tutoring bot. The decentering move grammar (Hackenberg et al.
  2024: PS → LST → FMST, or PS → FMST → LST) is the NSF-project target,
  not tonight's target. Tonight's bot answers questions; it doesn't
  pose them using formal teacher moves.
- Not integrated with the strategies/, arche-trace/, or learner/
  modules. If this experiment matures, integration would be a separate
  decision with its own design doc.

## Constraints accepted

- Disk is at 99%. No vindex download tonight.
- DeepSeek-R1:14b in Ollama is the runtime.
- Gemma 2B was the conversational target but can be added later with
  `ollama pull gemma:2b` when disk permits.
- Per-term-per-file for the authoring layer.
- Plain-string keyword triggers for the validator tonight; real parsing
  later.

## Morning outputs to review

- `STATUS.md` — honest pass/fail report, what's stubbed, what isn't.
- `logs/overnight_<timestamp>.log` — full transcript of the test loop.
- `logs/results.csv` — one row per question with pass/fail and
  incompatibility hits.

## Next session (not tonight)

- Pull `gemma:2b` and `gemma2:2b` once disk permits.
- Build the larql Rust workspace (`cargo build --release`).
- Actually apply a `.vlp` patch and diff model behavior before/after.
- Wire in the decentering-move vocabulary from Hackenberg et al. 2024
  (AQST, FMST, LST, PS, MG) so the bot can QUESTION, not just answer.
- Import instructor's reading list from syllabus; pull via MCP Consensus
  into a softer-protection expert-context vocabulary layer.
- Talk-moves module probably lives in a new `vocabularies/decentering/`
  folder parallel to `n101/`.
