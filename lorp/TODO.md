# LoRP — deferred phases

Phase 1 (the current PoC) is scoped narrowly. These items are deliberately
*not* in Phase 1, but were identified during design and should not be lost.

## Phase 1.5 — engine extensions (Prolog/ submodule)

The Derrida convergence report at
`~/Documents/GitHub/pml-testing-bridge/diagnostics/convergence/derrida.md`
identifies specific gaps in the PML engine. Addressing these expands
what the submodule can reason about and is a prerequisite for a
productive `pml-reader` port.

- Extend `is_incoherent/1` to see inside modal wrappers
  (`comp_nec(P)` versus `exp_nec(¬P)` on identical content;
  `comp_nec(iff(A, B))` versus downstream asymmetry).
- Add `dialectical_transition/2` rules for content atoms beyond
  `u, a, lg, t, u_prime` so engine engagement isn't limited to
  rhythm-specific texts.
- Formalize `iff`-inside-`comp_nec` as an anti-pattern (detector rule)
  for asymmetric-contamination cases.
- Canonicalize atom discipline: `indication` vs `indication_in_expression`
  should not live in parallel universes.

## Phase 2 — pml-reader port

Build on Phase 1 + 1.5. Port the four-voice pipeline
(Anticipation → Drafter → Textual → Brandomian) to run against
open-source backends the same way this PoC does. Voice prompts live at
`~/.claude/skills/pml-reader/voices/*.md`. Reuse them verbatim.

Optional: add voting (LoRP step 4) at this phase since rhythm axioms
have higher variance than substrate facts.

## Phase 3 — training

Once Phase 1 has produced ~500 quadruples in `training_corpus/index.jsonl`
(passage + axioms + syntax + immanence verdicts), we have a domain
fine-tune target.

### PML-specific SFT + GRPO

- Reference method: Mellgren et al., *Training Language Models to Use
  Prolog as a Tool*, ACL 2026 Findings (arxiv 2512.07407).
- Dataset released as
  [`niklasm222/gsm8k-prolog-prover`](https://huggingface.co/datasets/niklasm222/gsm8k-prolog-prover)
  — 7,473 (problem, prolog) pairs in SWI-`clpq` style.
- Method: GRPO with the interpreter as reward oracle. Paper reports an
  accuracy-versus-auditability trade-off; for this project,
  auditability is the point, so reward the structure side.
- Base model candidates: `Qwen2.5-3B-Instruct` (matches the paper),
  `Gemma 4 E4B` (smaller, laptop-runnable at inference).
- Where: Big Red HPC, full-parameter or LoRA.

### Math-domain path for student data (ASKTM)

- A **separate** fine-tune, same technique, different corpus.
- gsm8k-prolog-prover is directly relevant to student math-word-problem
  ingestion. Useful for coding ASKTM responses into structured form
  for analysis.
- Different target than the PML fine-tune; run as parallel workstream.

## Phase 4 — teacher chat REPL

- `n101_bot/` was the prototype. Re-implement with the Phase 1 adapter
  + Phase 2 four-voice pipeline.
- Translator (Drafter, small model, fast) converts NL → query against
  the kernel; prover runs `proves/1`; resituator (Brandomian voice,
  thinking model) renders the result in teacher voice.

## Phase 5 — parallel dispatch

Apply the pattern documented in
`~/.claude/projects/-Users-tio-Documents-GitHub-umedcta-formalization/memory/feedback_agent_dispatch_methodology.md`:

- Setup → N parallel agents → consolidator.
- Each agent writes to its own file, never a shared append.
- Harness runs on the aggregate.

For this pipeline: a `--parallel N` mode on the batch runner that
splits the corpus into N shards and dispatches them concurrently.

## Phase 6 — NotebookLM integration

The skill supports `--notebook <id>` for textual grounding during the
Textual voice. The port defers this until the local pipeline is stable
so we can iterate on prompts without an external dependency.
