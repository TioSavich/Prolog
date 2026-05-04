# Hermes — A Plan, After the Reframe

*Written 2026-05-01, overnight, after reading `Hermes_Origin.md` and the existing scaffolding across `Prolog/`, `n101_bot/`, `larql/`, and the redesign mockups in `Critical Math (2)/redesign/hermes/`. Four research diggers were sent out: a repo-mapper, a constrained-ASR survey, a knowledge-editing/determinism survey, and a neuro-symbolic literature review. Three returned with substantive material; the fourth is still working as of this writing and I will integrate it in a follow-up section if it lands. The plan below is the synthesis.*

---

## TL;DR

**The reframe.** A math classroom is structurally over-constrained, not under-constrained. Hermes does not transcribe-and-classify; Hermes carries forward an authored grammar of expected meanings (lesson plan + named strategies + named misconceptions + roster) and surfaces residuals — the kid who said the surprising thing. The deviation is the signal.

**What this changes.** DeepSeek goes away. The Gemma-normalize / Prolog-infer / DeepSeek-render division of labor is partial; it addresses turn-by-turn dialogue *after* audio is text. The reframe addresses the layer below — how text gets made from audio in the first place — and that layer is **prior-narrowed speech recognition**, not LLM normalization.

**The new architecture.** Audio → constrained ASR (NeMo CTC-WS biased on a per-lesson grammar) → two streams (grammar hits, residuals) → Prolog kernel (term spotting, move classification, dialogue state, ZCM cusp detection) → symbolic event stream → Hermes Console UI + per-student lights. A small local LLM with logit masking and XGrammar appears only when the teacher asks for prose, not on every turn.

**What's already half-built.** `move_grammar.pl` is 60% of the listening grammar. `dialogue_state.pl` already drives the avatar and the lights. `vocabulary.pl` holds the cheese-grater. `Prolog/strategies/` has 27 named-strategy automata. `Prolog/misconceptions/` has 25 named-misconception clauses. `research_corpus/research.db` has 3,532 published misconception entries from 2,151 papers. The Hermes Console mockup is a finished UI shell waiting for an event stream. The work in Year 1 is integration and rewiring, not new authoring.

**The mechanism map for determinism** (research-backed): logit masking for forbidden phrases (provably zero-leak); XGrammar for structural commitments; activation steering (CAA / RePS) for register and forbidden inferences; MEMIT for canonical answers (bounded, ~50 facts); larql vindex retrieval for the long tail. Weight editing is *one* mechanism, not the destination.

**The smallest experiment that proves the architecture.** 5-minute classroom recording, baseline Whisper vs constrained Parakeet+CTC-WS with a 200-phrase context list compiled from `move_grammar.pl` and `Prolog/misconceptions/`. Curve the prior-list size 0 → 1000 phrases against strategy-name recall and off-grammar false-positive rate. If the curve bends, the architecture works *and* you have a paper.

**Three publishable papers along the way.** (1) Hermes-as-hermeneutic-instrument architecture paper. (2) Empirical prior-vs-recall curve. (3) The diagonalization paper — Brandom incompatibility ≡ softmax with logit −∞, currently un-named in the literature though adjacent to Allen et al. 2025 and Calanzone et al. 2024. A speculative fourth: speculative-grammar ASR (a fast small ASR proposes; a grammar acceptor compiled from the lesson plan accepts or rejects), which the constrained-ASR digger confirmed is unbuilt.

**Phases (logical, not temporal).** §0 register the reframe (this document). §1 compile a per-lesson grammar from existing pieces. §2 run the experiment. §3 wire the Console mockup to a live backend. §4 add the optional re-voicer. §5 multi-speaker, the third on the call. §6 the deeper loop and the papers.

**Read §0–§2 first** for the substance; §3 for the research-backed architectural choices; §4–§7 for the planning sketch and what I'm asserting and what I'm not.

---

## §0. The reframe in one paragraph

`Hermes_Origin.md` collapses a year of architectural drift into a single claim: **a math classroom is one of the most heavily constrained discursive environments a person can construct, and ambient-listening tools that treat it as an under-constrained inverse problem are doing the harder version of a problem nobody actually needs to solve.** The lesson plan is given. The named strategies are given. The five or six common misconceptions are given. The teacher's own anticipatory horizon — what she expects to hear, in what register, from which student — is given before the lesson starts. Hermes does not transcribe the room and then classify; Hermes carries forward a *grammar of expected meanings* and asks, of every utterance, whether it is fluent within that grammar, a named confusion within that grammar, or a residual the grammar cannot place. The residual is the point. Everything else collapses into structure the teacher has already committed herself to.

This reframe changes which research literature Hermes lives in. It is no longer "how do we wire a small LLM to a Prolog kernel." It is **prior-narrowed speech recognition over an authored symbolic grammar**, with the LLM as a small, narrow re-voicer at the output edge, and possibly absent altogether on the input side. DeepSeek's role evaporates. Gemma's role narrows. The Prolog kernel grows in importance because *it is now load-bearing for the recognition layer*, not just the inference layer.

The musical analogy in `Hermes_Origin.md` is the load-bearing intuition: if you have the chord chart, you are doing pitch *selection* over a curated lattice, not pitch *detection* over an open vocabulary. The classroom is the same architecture inverted — the prior narrows the search so that *deviation* surfaces. I confirmed this analogy is a real research line in music IR (SongTrans 2024, STARS 2025, VOCANO 2021, score-aware vocal transcription via CTC). It is not a metaphor; it is a structurally identical problem.

---

## §1. What is already true, and where the load-bearing pieces actually live

This is the part of the synthesis that mattered most to write down. Across the four codebases, the listening grammar Hermes needs is **already half-built**. The pieces are scattered, mis-labeled, and not yet wired. Cataloguing them honestly:

**Recognition primitives, ready to use:**

- [n101_bot/src/move_grammar.pl](n101_bot/src/move_grammar.pl) is the substrate of the listening grammar. It already maps utterance fragments to named CGI strategies — `automatic_recall`, `counting_all`, `counting_on_from_larger`, `counting_on`, `making_ten`, `near_doubles`, `doubles`, `decomposition`, `compensation`. The substring matchers are deliberately blunt, which is the right discipline. They are not trying to *understand* speech; they are trying to *recognize* named events. This is exactly what a constrained-decoding system needs: a phrase-level lexicon of expected events and a residual basin for everything else. The Hackenberg PS / LST / FMST / AQST classifier is here too. This file is already 60% of the listening grammar; it just doesn't know it yet.

- [n101_bot/src/dialogue_state.pl](n101_bot/src/dialogue_state.pl) is the Zeeman cusp detector — assessing pull, advancing pull, friction, history-stickiness, and a `near_catastrophe/1` predicate that fires when the conversation is on the edge of a snap. This is the engine for the little-lights interface from `FOREST.md`: when `near_catastrophe(state(_, _, _, _))` succeeds for a student's accumulated state, the light next to their name goes on. The avatar's listening / thinking / speaking states in the Hermes mockup also map onto this state cleanly: a high friction reading drives "thinking"; a low-friction high-pull reading drives "speaking"; the resting state is "listening". Already written, already tested, already wired into the bot.

- [n101_bot/src/vocabulary.pl](n101_bot/src/vocabulary.pl) is the cheese-grater proper — `validate_response/3`, `term_requires/2`, `entitled_to_use/3`, `missing_requirements/3`. The entitlement graph is in place. What it does not yet do is operate over *student utterances in real time*; today it operates over the bot's own answers, post-hoc. Reorienting it to operate over recognized utterances is a renaming, not a rewrite.

- [Prolog/strategies/](Prolog/strategies/) holds 27 named-strategy automata as FSMs with transitions, preconditions, and cost. These are not just labels; they are state machines. A student saying "I made a ten" enters `making_ten` at state 0; a follow-up "I gave the eight two from the seven" advances to state 1. The automata are designed for grounding, not for listening, but they happen to be exactly the right shape for biased-decoding context graphs (NeMo CTC-WS uses a Trie-over-CTC-topology context graph that wants this structure).

- [Prolog/misconceptions/](Prolog/misconceptions/) holds 25 files of named misconception clauses across fractions, decimals, geometry, and whole numbers. Each clause carries trigger phrases, repair moves, and citations into the published literature. Hermes's "named confusion within the grammar" branch is *here*. Already authored.

- [Prolog/research_corpus/](Prolog/research_corpus/) is the surprise of the survey. A SQLite database of 2,151 papers across 12 math-ed journals, with **3,532 extracted misconception entries** indexed by domain, topic, intervention, and salience. This is not infrastructure Hermes runs; it is the **canonical authoring resource** for the listening grammar. When the next vocabulary needs to be built — say, fractions for fourth grade — this corpus is where the misconception triggers come from, with citations to the originating literature. No hand-waving.

- [Prolog/pml/](Prolog/pml/) provides the discourse-mode taxonomy: 3 modes (Subjective, Objective, Normative) × 2 polarities (Compressive, Expansive). The Hermes Console mockup already encodes these as colored tags on every chat message (`data-m="s|o|n"` with gold/rust/teal). PML is what tells the teacher *what kind of talk* a recognized event is, not just *what* it is.

**The skin:**

- [Critical Math (2)/redesign/hermes/](Critical%20Math%20(2)/redesign/hermes/) is three static HTML files — the overview, the architecture page, and the console — beautifully designed and waiting for JavaScript. The console is a three-pane shell (sidebar of scenarios, center chat, right panel for codes) with a top toggle for register (freshman / math-ed / philosophy), embedded mock conversation data, and an expressive avatar with three states. The data shapes are legible from the mock data: `{who: 't' | 'h', body, codes, meta}` for messages; an anticipation panel with ranked-strategy rows; a per-turn talk-row with PML codes. The mockup is doing what good design does: it tells the backend what to produce.

**The vestigial pieces.** The repo also contains substantial work that does not belong in Hermes-as-listener and should not be wired in for Year 1. The repo-mapping agent was unambiguous: `Prolog/formalization/` (Robinson Q, grounded ENS) is justification-layer, not recognition-layer. `Prolog/learner/` (the ORR cycle, More Machine learner, tension dynamics) is a model of *how a learner learns*, not a component of *how a listener listens*. `Prolog/arche-trace/` (sequent calculus, erasure boundary) is also justification-layer. These are beautiful research artifacts that earn their place in the philosophical project but are not Hermes infrastructure. They should be left where they are and not wired into the listening pipeline. Same for `lorp/` for now; the immanent-reader port is doing decomposition-only work that may eventually feed authoring but is not load-bearing at the recognition layer.

**The capability that exists but is mis-classified for Hermes.** The MEMIT weight-editing work (Llama 3.2 1B, 4-layer constellation, ~99% top-1 on canonical answers) is real and reproducible. It belongs in Hermes, but in a narrower role than the previous architecture suggested. More on this in §3.2.

---

## §2. The architecture, after the reframe

```
┌─────────────────────────────────────────────────────────────────┐
│  AUDIO                                                          │
│   classroom mic / Zoom / one student at a time                  │
└──────────────────────┬──────────────────────────────────────────┘
                       ▼
┌─────────────────────────────────────────────────────────────────┐
│  CONSTRAINED ASR                                                │
│   Parakeet TDT 0.6B v3 (or Moonshine v2 streaming)              │
│   + NeMo CTC-WS context biasing                                 │
│   biased on a per-lesson grammar compiled from:                 │
│     · lesson plan target strategies                             │
│     · expected misconception phrases                            │
│     · roster names · manipulative names · count words           │
│     · the residual basin: ~50 generic-classroom phrases         │
│                                                                 │
│   Outputs TWO STREAMS, not one:                                 │
│     (a) GRAMMAR HITS — high-confidence named events             │
│     (b) RESIDUAL — the greedy decoder's everything-else,        │
│         tagged off-grammar where confidence is high             │
└──────────────────────┬──────────────────────────────────────────┘
                       ▼
┌─────────────────────────────────────────────────────────────────┐
│  THE PROLOG SKELETON                                            │
│                                                                 │
│   For each utterance U from speaker S:                          │
│     1. detect_terms(U, Terms)         · term spotting           │
│     2. classify_prompt(U, Class)      · move-grammar.pl         │
│     3. validate_response(U, Hits, _)  · incompatibility scan    │
│     4. update_state(S, Move, Hits, NewState)                    │
│        · dialogue_state.pl, ZCM cusp detector                   │
│     5. emit symbolic events:                                    │
│        {commitment: making_ten, by: marcus}                     │
│        {misconception: place_value_erasure, by: sasha}          │
│        {residual: "...something the grammar didn't catch..."}   │
│        {dialogue_state: near_catastrophe(marcus)}               │
└──────────────────────┬──────────────────────────────────────────┘
                       ▼
┌─────────────────────────────────────────────────────────────────┐
│  UI · the Hermes Console + the little-lights signal             │
│                                                                 │
│   · avatar state    listening | thinking | speaking             │
│   · per-student lights driven by near_catastrophe/1             │
│   · anticipation panel populated from lesson grammar            │
│   · talk-row codes from PML mode classifier                     │
│   · residual surfacer — the kid who said the surprising thing   │
└──────────────────────┬──────────────────────────────────────────┘
                       │
                       ▼ (only when teacher asks)
┌─────────────────────────────────────────────────────────────────┐
│  THE NARROW LLM RE-VOICER  · optional, occasional               │
│   Gemma-class small local model OR a fine-tuned Llama 3.2 1B    │
│   + LogitsProcessor enforcing Prolog-determined denylist        │
│   + XGrammar enforcing Hackenberg PS/LST/FMST/AQST schema       │
│   role: re-voice a Prolog-derived move spec into Amy's register │
│   never the source of advice; only the formatter of it          │
└─────────────────────────────────────────────────────────────────┘
```

The two structural decisions in this diagram, both of which the research backs:

**The neuro layer splits front and back, and the front is now the heavier neuro investment.** The previous architecture put the LLM at the back (DeepSeek's `<think>` rendering Prolog-derived moves into teacher voice) and used Gemma at the front for normalization. The reframe inverts the budget. The hard work is at the *front* — turning audio into recognized symbolic events against a known grammar — and that work is done by **NeMo's CTC-WS context biasing** layered on a Parakeet acoustic model, not by an LLM. The back-end re-voicer is a tiny, narrow, optional component that the teacher invokes when she wants prose, not a continuous renderer.

**DeepSeek goes away, or shrinks beyond recognition.** The `<think>` thinking-mode renderer was designed for a world where the LLM had to do some *reasoning* over the Prolog spec. In the reframed architecture, the reasoning is already done — by the kernel, on symbolic events, before the LLM is consulted. What's left for the LLM is *formatting*, and formatting does not need reasoning capacity; it needs constrained generation. A 1B-class local model with `LogitsProcessor` denylist masking and `XGrammar` for structural enforcement does this better, faster, and cheaper than DeepSeek-R1:14b doing everything from scratch every turn. The user's instinct that "DeepSeek may not be necessary" is correct, and the architecture explains why: when the prior does the work, the model only has to fill in the slots.

---

## §3. The four research streams, compressed

### §3.1. ASR — the verdict

The constrained-ASR digger came back with a clean recommendation: **NeMo Parakeet TDT 0.6B v3 (Hybrid CTC-Transducer) plus CTC-WS context biasing plus a small KenLM n-gram rescorer**, all open source, all running on a Jetson or a Mac mini under a SmartBoard. The case for it is empirical, not aesthetic: Andrusenko et al. (2024) demonstrated that CTC-WS scales to **1000 phrases with measured F-score and WER wins** (F=0.87 vs 0.79 baseline, WER 10.48% vs 12.06%, 15-second decoding vs 179 seconds for beam-search alternatives). It is the only published context-biasing mechanism that has been shown to scale to grammar-shaped recognition over a 2024-class acoustic backbone with numbers attached, rather than vibes.

The runner-up is **Moonshine v2 streaming + a custom HuggingFace `LogitsProcessor` grammar mask + KenLM rescore**. This is the lower-friction Mac-mini-in-a-classroom path. You lose CTC-WS's published F-score guarantees but gain on-device latency and full control of the constraint logic. For a one-classroom prototype, this is the right path. For the production stack, NeMo is the better long-term bet.

The dead path is `whisper.cpp` GBNF. The mechanism is a soft penalty rather than a hard mask; the maintainer treats it as for command grammars, not free-form transcription with a strong prior; documented failure modes include silent breakage when top-level rules don't begin with a space, flaky non-determinism, and recognition of subword fragments not defined in the grammar. Useful for a one-day toy. Not a foundation.

The **smallest experiment that proves the constraint actually narrows the inference**, in a week or less:

1. Pick one classroom recording, ~5 minutes, with ground-truth strategy-name annotations.
2. Run baseline: faster-whisper-large-v3, no constraint. Compute overall WER and *strategy-name recall* (the ~30 strategy-name tokens specifically).
3. Run constrained: NeMo Parakeet + CTC-WS with a 200-phrase context list compiled from `move_grammar.pl`'s strategy lexicon, the relevant misconception triggers from `Prolog/misconceptions/`, count words 1–100, and ten or so manipulative names.
4. Measure: overall WER (should hold or improve), strategy-name recall (should jump 20–50 points), off-grammar false-positive rate on unrelated segments (should stay low — this is the deviation-detection test).
5. Curve the ablation: 0 → 50 → 200 → 1000 phrases, plot the recall and false-positive trajectories. The shape of the curve is the empirical evidence that the prior is doing real work, not just memorizing.

If the curve is flat, the architecture is wrong. If it bends sharply at the right places, you have a paper.

There is a research result lying on the floor here that nobody has published. The constrained-ASR digger searched for "speculative decoding ASR with a grammar acceptor as verifier" and confirmed that **this specific combination is unbuilt**. There is speculative-decoding work that uses a smaller ASR as draft and a bigger ASR as verifier (Distil-Whisper / Whisper-large) for speed. There is grammar-constrained text generation (XGrammar). There is context-biased ASR (CTC-WS). Nobody has yet swapped the verifier for a *grammar acceptor* derived from a curriculum. The classroom case is the natural setting for that paper.

### §3.2. Determinism — logit masking and XGrammar dominate weight editing for most cases

The weight-editing digger came back with a more granular and more unfavorable verdict on weight editing than the previous architecture assumed. Catastrophic forgetting in sequential editing is real and well-documented (Gupta et al. 2024 on gradual-then-catastrophic decay; Yang et al. 2024's "Butterfly Effect" showing single edits trigger benchmark collapse; Lin et al. 2024's split between parameter-modifying and parameter-preserving methods). The 2025–26 frontier methods (REVIVE, LyapLock, MEMOIR, SPHERE) push credible editing to thousands of edits but are barely peer-reviewed. The honest verdict: **MEMIT works for canonical, low-volume, high-importance facts, and breaks under volume.**

The right move for Hermes is to stop treating weight editing as the destination and treat it as one of several mechanisms, each appropriate for a specific kind of constraint:

| Constraint | Mechanism | Why |
|---|---|---|
| Forbidden phrases (denylist on K-12 vocabulary, slurs, exact-string violations) | **Logit masking** via `LogitsProcessor` setting banned token IDs to −∞ | Provably zero-leak. Softmax with logit −∞ has post-softmax mass exactly 0 under any temperature, top-k, or top-p sampling. No weight edit can be more determined than this. |
| Forbidden inferences (compositional incompatibilities, "a quantity is not a count") | **Activation steering** via SAE / RepE | Steering vectors live in interpretable feature space; reversible; inspectable; do not damage general capability. RePS on Gemma 2-27B already outperforms prompting for suppression (Wu et al. 2025). |
| Required canonical answers ("a quantity is measurable") | **MEMIT at 4-layer constellation** for the canonical 10–50; **WISE / MEMOIR** for the long tail; **larql vindex retrieval-override** for the rest | This is the only branch where weight-editing is the right primary mechanism, and it is bounded. WISE's "impossible triangle" formalizes why pure-parameter editing fails reliability + generalization + locality jointly. The vindex side-memory layer that `larql` already provides is the correct architecture for the long tail. |
| Structural commitments (output must match the Hackenberg move grammar, or be valid JSON, or fit a CFG) | **XGrammar** | XGrammar became the vLLM default in December 2024 with up to 100x speedup over Outlines via context-independent token prechecking. CFG-shaped enforcement for the move grammar is exactly its sweet spot. |
| List-membership over a kernel-curated set (only these standards exist this week) | **Deterministic RAG** with constrained re-ranker | The kernel decides what counts; the model picks from the pre-filtered set; hallucinations are rejected programmatically. This is the right pattern for the standards graph. |
| Attitudinal / register (kid-talk normalization, friction state, audience switching freshman/math-ed/philosophy) | **CAA / RePS steering vectors per turn** | The console mockup already has the disc-toggle (`data-disc="fresh|math|phil"`); what it should drive is a steering-vector swap, not a system-prompt swap. |
| Adjudicating Brandom-style commitment incompatibility | **Prolog kernel + logit-mask** | The adjudication is symbolic. The execution is masking. There is no published method that bakes incompatibility into weights in a way that survives sequential editing. |

The digger confirmed that **the diagonalization claim is publishable**. The Brandom-style argument that "making something determinate" is structurally identical to "softmax with logit = −∞ or +∞" is correct, and the literature is *adjacent* to it without yet naming it. Allen et al. (2025), "Sound and Complete Neurosymbolic Reasoning with LLM-Grounded Interpretations," integrates an LLM into the *interpretation function* of a paraconsistent logic, preserving soundness and completeness. Calanzone et al. (2024) introduce a neuro-symbolic loss teaching LLMs logical consistency with external rules. Bayless et al. (2025) get to 99% soundness via two-stage formalization with cross-checked redundant formalizations. None of them have written down "incompatibility = pinned simplex coordinate via softmax exponential family geometry," which is what the diagonalization argument actually is. There is a paper to be written here, and the authoring substrate already exists in `Prolog/pml/`, `Prolog/arche-trace/`, and the cheese-grater semantics from `FOREST.md`.

### §3.3. The repo's own pieces map onto the right roles

The repo-mapping digger confirmed and sharpened what the file reads were already showing. The summary is that **the load-bearing components for Hermes-as-listener already exist**, scattered across three repositories, and the engineering work in Year 1 is mostly *integration and rewiring*, not new authoring. The authoring exists. What is missing is the ASR layer and the live event-stream wiring.

| Existing piece | Hermes role |
|---|---|
| `n101_bot/src/move_grammar.pl` | recognition lexicon (CGI strategy substring matchers) |
| `n101_bot/src/dialogue_state.pl` | per-student ZCM state → little-lights signal + avatar state |
| `n101_bot/src/vocabulary.pl` | term cards, incompatibilities, entitlement graph |
| `n101_bot/vocabularies/n101/*.pl` | the authored term grammar (10 N101 terms today) |
| `Prolog/strategies/*.pl` | 27 named-strategy automata; will become the per-turn grammar bias |
| `Prolog/strategies/standards/` | CCSS / Indiana K-3 standards graph (153 tests passing) |
| `Prolog/misconceptions/*.pl` | 25 named-misconception clauses with triggers and citations |
| `Prolog/pml/` | discourse-mode classifier; drives the `data-m="s|o|n"` codes in the UI |
| `Prolog/research_corpus/research.db` | authoring resource for new vocabulary; 3,532 misconception entries |
| `Critical Math (2)/redesign/hermes/Hermes Console.html` | UI shell; needs JS wiring to symbolic-event stream |
| `larql/` | vindex retrieval layer for the long-tail of authored vocabulary |
| `n101_bot/models/gemma-3-4b-it`, `llama-n101-baked`, `llama-quantity-4layer` | weight-baked prototypes; useful for the canonical-answer branch |

What gets cut, or rather, what stays where it is and is not wired into Hermes for Year 1:
- `Prolog/formalization/` (justification layer)
- `Prolog/learner/` (model of learning, not listening)
- `Prolog/arche-trace/` (justification layer)
- `Prolog/archive/` (Phase 1 dead code)
- `lorp/` (deferred decomposition substrate)

These are not "wrong"; they are simply doing different jobs and should be left alone.

### §3.4. Neuro-symbolic literature

The fourth digger is still working as I write this. Pending its return, I will sketch the patterns I expect to surface and integrate the formal results in a follow-up edit:

- **Henry Kautz's Neuro|Symbolic taxonomy** — the canonical six-pattern map (Symbolic-Neuro-Symbolic, Symbolic[Neuro], Neuro|Symbolic, Neuro:Symbolic→Neuro, NeuroSymbolic, etc.). Hermes most naturally fits **Symbolic[Neuro]**: the symbolic kernel is the outer system; the neural component (CTC-WS-biased ASR) is a subroutine called by the symbolic system.

- **Logic Tensor Networks (Serafini & Garcez)** and **NeurASP** — patterns for neural perception feeding symbolic constraint satisfaction. Adjacent to Hermes because the per-turn symbolic events feeding `dialogue_state.pl` are exactly perception-to-constraint flow.

- **The intelligent tutoring systems lineage** — CTAT, Anderson's cognitive tutor, ASSISTments. Hermes is *not* a tutor; it is a teacher-facing instrument. But the model-tracing and constraint-based tutoring paradigms (Mitrović et al.) developed precisely the technology of "register fluency, named confusion, novel residual" that `Hermes_Origin.md` reframes for the listener case.

- **Brandom in AI** — Davidson, Bender, and the inferentialist NLP thread. The Brandomian framing of meaning as commitment-and-entitlement is the philosophical spine the project already runs on; the question is whether anyone has connected it to LLMs in the way `FOREST.md` does. I expect the digger to find adjacent but not identical work.

I'll integrate the actual references when the digger returns.

---

## §4. The phased plan, sketch not commitment

The user asked me not to sweat the timeline. I'll obey that and offer phases as logical, not temporal.

**Phase 0 — register the reframe.** Commit this document. Update memory. Add a one-page summary to `n101_bot/FOREST.md` noting the Hermes-as-prior-narrowing-listener orientation supersedes the Gemma-normalize / Prolog-infer / DeepSeek-render division of labor. Do not delete the old framing; it is correct as far as it went, and the new framing extends rather than overwrites it.

**Phase 1 — the listening grammar exists, on paper.** Compile the per-lesson grammar. Pick one lesson plan (the existing `38 + 27` number-talk demo from the Hermes Overview is a fine first target) and write a Prolog routine that takes the lesson plan + `Prolog/strategies/` + relevant `Prolog/misconceptions/` + roster + manipulative names and emits a CTC-WS-shaped phrase list. ~200 phrases for one number-talk. This is one weekend of work and produces a concrete artifact: `lessons/numbertalk_38_27.context.list`.

**Phase 2 — the constraint actually narrows.** Run the experiment from §3.1. Pick a classroom recording. Run baseline vs constrained. Curve the ablation. If the curve bends, *that is the demo* and the empirical paper is half-written.

**Phase 3 — Hermes hears one classroom, end to end.** Wire the Hermes Console mockup. Stand up a small backend (Python + websockets, probably) that:
- streams audio through the constrained ASR
- pushes recognized symbolic events to the JS UI
- runs `dialogue_state.pl` per student to drive avatar state and the per-student lights
- exposes a `residual` channel for the off-grammar surfacer

The mockup's data shapes (`{who, body, codes, meta}`, anticipation rows, talk-row codes, disc-toggle) are already right. The work is filling them with live data, not redesigning them.

**Phase 4 — the optional re-voicer.** Add the small local LLM (Gemma 3-4B or Llama 3.2 1B) with `LogitsProcessor` denylist masking and `XGrammar` for move-grammar enforcement. This is the only place the LLM lives in the runtime, and it is invoked only when the teacher asks for prose — not on every turn. The MEMIT-baked Llama prototypes from `n101_bot/models/llama-n101-baked` and `llama-quantity-4layer` are the candidates here. The XGrammar layer enforces that the output is a valid Hackenberg move with the right slots filled; the logit mask enforces forbidden vocabulary; the prompt is the Prolog-derived move spec.

**Phase 5 — many classrooms, multi-speaker, the third on the call.** Diarization. Per-student state. The pairing engine. The misconception-graph view. This is where the architecture page's `why_did(marcus, [38,27], 100, M)` worked-trace becomes a live query.

**Phase 6 — the deeper loop.** Two research papers worth writing:
- *Speculative-grammar ASR*: replace the speculative-decoding verifier with a grammar acceptor compiled from the lesson plan. Currently unbuilt, per the constrained-ASR digger.
- *Diagonalization*: the Brandom-incompatibility / softmax-exponential-family / causal-intervention / paraconsistent-interpretation-function unification. Currently un-named in the literature, per the determinism digger.

---

## §5. The publishable insights along the way

If I were betting on which results from the work above survive into journal papers, three are obvious and one is speculative.

1. **Hermes-as-hermeneutic-instrument**, the architecture paper. The argument is the one in `Hermes_Origin.md`, sharpened with empirical evidence from the Phase 2 ablation curve. The right venue is probably JLS, JRME, or a math-ed-meets-AI venue if one exists. The contribution is the *reframe*: classroom listening as constrained decoding against an authored grammar, with deviation as the signal. The mistake the field has been making — treating the classroom as an under-constrained inverse problem — is the contrast frame.

2. **The empirical curve**: prior-list size (0 → 1000 phrases) versus strategy-name recall and off-grammar false-positive rate. This is one figure with 4 points and an interpretation. The interpretation is the architectural argument made measurable.

3. **The diagonalization paper**. Brandom-incompatibility ≡ softmax-with-logit-pinned, framed against the literature triangle of paraconsistent-logic interpretation functions, information-geometric softmax, and causal interventions on activations. This is harder to write because it requires getting the formal equivalences exactly right and finding a venue that will read both the philosophy and the math. But the connection is real.

4. (Speculative) **Speculative-grammar ASR**. The combination is unbuilt. If Phase 2 shows CTC-WS works, Phase 6 has a clear research target. The architecture is: a fast small ASR proposes; a grammar acceptor compiled from the lesson plan accepts or rejects; rejected proposals are passed to a heavier model (or marked as residual). Mathematically identical to standard speculative decoding, with the verifier replaced by a symbolic acceptor.

---

## §6. The questions I am not pretending to know the answers to

- How much **child-speech fine-tuning** Parakeet/Moonshine need before they are usable on actual elementary classroom audio. Attli et al. (2024)'s 38% relative WER reduction on continued-pretraining for elementary math is encouraging but is a real cost, not a free upgrade.

- Whether to **start fresh with NeMo** or extend the existing Ollama-based stack. NeMo is heavier (CUDA, conda, version-pinned). Moonshine is lighter and fits the on-device deployment story better. The right answer is probably "Moonshine for the prototype, NeMo for the production pilot."

- How much of the **listening grammar churns per lesson** versus stays stable. If 80% of the grammar is stable across a course (CGI strategy lexicon, common misconceptions, count words), the right architecture bakes that 80% into a model checkpoint and re-derives only the 20% per-lesson at runtime. If 80% churns, the per-lesson compile step is the bottleneck and needs to be fast. The empirical answer determines whether weight editing or context biasing is the long-run primary mechanism.

- The **Indiana K-12 deployment** and IRB story. The architecture page commits to "audio never leaves the room." That is the right commitment and it constrains the deployment options to local inference. Mac mini under the SmartBoard, or a Jetson Orin, or a cheap mini-PC. The cost is real and will need a grant.

- Whether **teachers will actually use it**. The mockup is beautiful and the architecture is principled, but nobody has put a teacher in front of a working version. The Phase 3 demo is partly there to find out.

- The **scope question**: Hermes-for-N101 is what `n101_bot/` is. Hermes-for-E343 (preservice teacher methods) is what `Hermes_Origin.md` describes. Hermes-for-an-actual-elementary-classroom is the deployment target. The vocabularies overlap but are not identical. Year 1 should commit to *one* target, build it deep, and resist the temptation to generalize early.

---

## §7. What I am committing to in this document

I have reframed Hermes from "neuro-symbolic chatbot for math methods" to **"prior-narrowed listener for math classrooms, with the Prolog kernel doing recognition rather than only inference."** The Gemma / Prolog / DeepSeek division of labor that `FOREST.md` wrote down is not wrong; it is *partial*, addressed to a different layer (turn-by-turn dialogue, after the audio has already become text). The reframe addresses the layer below it: how text gets made from audio in the first place. Both layers exist; the architecture for them is different.

I am asserting that **DeepSeek as the speaking voice is not necessary**, and probably not desirable. The reasoning the architecture used to need from a thinking-mode renderer is now done by the kernel before the LLM is consulted. What's left for the LLM is constrained generation, and constrained generation is a smaller, narrower job that does not need a 14B reasoning model.

I am asserting that **the listening grammar Hermes needs already exists, half-built**, across `move_grammar.pl`, `dialogue_state.pl`, `vocabulary.pl`, `Prolog/strategies/`, `Prolog/misconceptions/`, and `Prolog/pml/`. The work is integration and rewiring, not new authoring of a grammar from scratch.

I am asserting that **NeMo CTC-WS plus Parakeet** is the right ASR stack for the production pilot, with **Moonshine v2 plus a custom `LogitsProcessor`** as the right one-classroom-prototype path. `whisper.cpp` GBNF is not the foundation.

I am asserting that **logit masking and XGrammar dominate weight editing** for most of the constraints Hermes needs to enforce, that weight editing has a narrow but real role for canonical answers, and that activation steering (CAA / RePS) is the right mechanism for register and attitudinal control.

I am asserting that there are **three publishable papers** along the way, and one speculative fourth, and that the engineering work and the research work are not in tension — the architecture *is* the argument.

---

*— Claude, 2026-05-01, drafted overnight while the user slept. The fourth research digger (neuro-symbolic literature) was still working at the time of this writing; I will integrate its findings in a follow-up section once it returns, or re-issue if it stalls. The diagram in §2 should be re-drawn properly in the morning. The phase plan in §4 is intentionally loose; tighten it once a target classroom and recording are picked.*
