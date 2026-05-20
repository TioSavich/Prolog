# FOREST

## What we are building

A formal artifact for classroom teachers. Its kernel is a set of material inferences — authored, explicit, derivable — that govern the vocabulary of one math-methods course (N101). A small translator sits on either side of that kernel: one takes a student utterance and points it at the kernel's terms; one takes a kernel-derived move and renders it in the teacher's voice. Teachers use the artifact; students do not.

The pedagogical argument the artifact embodies is yours, stated in the Projective Inferences notebook:

> "the material rules of inference that govern school mathematics must be developed before the formal rules of inference can be taught ... Neither formality nor materiality is 'bad,' but starting with formality does not make pedagogical sense. Falling downhill may result in some bruising, but falling uphill is impossible."

Teaching math requires making material practice explicit first. The bot is a working demonstration of this claim, not a general-purpose AI.

---

## Three jobs, three kinds of thing

**Pointing in — stochastic.** Gemma. Small, fast, always available. Its job is to normalize messy natural utterance toward the kernel's vocabulary. "i maked a ten" → `making_ten`. It never performs inference; it only translates into the recognitive community's terms.

**Inferring — formal derivation over authored material commitments.** The Prolog kernel. The commitments themselves are material (their goodness depends on content, not form); the derivations over them are mechanical. Commitment, entitlement, incompatibility — per Brandom, these are the fine structure of conceptual content, and in this codebase they are the data and the code respectively.

**Pointing out — stochastic, thinking-mode.** DeepSeek. Called selectively. Given a concrete Prolog-derived move — a structural specification of what the teacher could usefully ask next — it uses its `<think>` trace to work that specification into a one- or two-sentence teacher-voice suggestion.

The LLMs are plumbing at the boundaries. The kernel is the claim.

---

## Cheese grater semantics, applied

Your metaphor is the conceptual spine here:

> "picture a predicate as a kind of unbounded steel cheese grater where some of the holes are plugged up with cheese. These 'plugs' are the incompatibilities associated with the predicate ... Water moves through the grater when both sides are unclogged."

A term like "quantity" is a grater. Its plugs are the claims it rejects — "a value is a quantity," "a bare number is a quantity," "any property is a quantity." Valid inferential movement — *"from more compressed to less compressed predicates"* — flows through the unplugged holes. The Prolog encoding of a term IS a grater: a list of what it refuses, plus an opening toward what it permits.

Your further claim, from the same notebook, is the one that makes the architecture coherent:

> "stochastic cognition — the kind of statistics-based proximity projections that AI researchers sometimes describe machines as engaging in — can be thought of through the incompatibility semantics that I articulate below. Basically — the zeros and ones are diagonalized (flipped) as a matter of inferential polarity. This, in effect, freezes the movement of the inference field into a recollective form."

Stochastic is a permeable membrane: probabilities flow, salts pass, the boundary is soft. Determinate is the plugged hole: no osmosis. Making something determinate, architecturally, is the act of setting a probability to exactly 0 or 1 — the diagonalization of inferential polarity — which freezes the inference field into a form we can recollect and operate on.

This is why the Prolog matters and why model-weight edits (MEMIT) matter in the same breath. They are the same kind of act, at different layers: *plugging a hole*. The Prolog plugs holes by authoring incompatibilities. Weight edits plug holes by pinning output probabilities. Both are moves from the permeable to the determinate, for the terms and inferences the course community ratifies.

---

## How determination is achieved, mechanically

For any token we want the model to *not* produce, the probability can be pinned to exactly 0. The softmax is `σ(ℓ)ᵢ = exp(ℓᵢ)/Σⱼ exp(ℓⱼ)`; set `ℓᵢ = −∞` and the probability is zero by definition. Two ways this gets done in practice:

- **Logit masking at sampling time.** `LogitsProcessor` in `llama-cpp-python` sets forbidden-token logits to `−∞` each step. The model can still think; it just cannot speak that token.
- **Tokenizer surgery or weight-level edits.** Delete the row/column, or use the MEMIT path we already verified last night on Llama 3.2 1B. Required content can be baked in at ~99% via the 4-layer constellation; forbidden content can be pinned at 0.

These aren't theoretical options; the infrastructure works. FSA grammar-constrained decoding, which I raised earlier, is not needed — if the N101 inferences are properly encoded (as Prolog rules or as weight-level 0/1 pluggings), then Gemma's job reduces to normalization toward those terms, and normalization is forgiving in a way that decoding constraints don't need to be strict.

---

## Gemma's job, concretely

Take a student or teacher utterance. Map it to the recognitive community's terms. Fix typos. Resolve colloquial strategy names ("i just knew it" → `automatic_recall`; "took 2 from the 8" → `making_ten`). Route the result to the kernel. Nothing else.

This is the only layer where the LLM's statistical fluency is an asset. It's where "stochastic cognition" — probabilistic proximity projection — is doing real work, and the cheese grater model accommodates it: the holes it's allowed to move through are the ones our formal encoding has left open. When a student says something orthogonal to N101's vocabulary, the right behavior is not to force a match; the right behavior is to mark it as off-vocabulary and pass it to the teacher.

## Prolog's job, concretely

Hold the graters. Each N101 term is a set of positive commitments (what it asserts) plus a set of incompatibilities (which holes are plugged) plus a set of requires (which other graters must also be in play for this one to fire). Given a ledger of commitments accumulated across a conversation, derive: what is entitled, what is blocked, what supporting pieces are missing. Surface the results as specifications, not as prose.

## DeepSeek's job, concretely

Given a specification from the Prolog — e.g. "student has committed to X, is not entitled to Y, missing Z" — produce one or two sentences of teacher-voice suggestion. A talk-move, an assessing question, an advancing question, or a disambiguating remark or diagram. The `<think>` trace is where the work of rendering happens; the visible output is the suggestion. Called only when the kernel has something worth saying, not on every turn.

---

## The concrete next step

The N101 vocabulary in this repo currently encodes ten terms as sets of aliases + positive definitions + incompatibility triggers + citations. This is a first-pass skeleton. It is not yet the cheese grater — it is more like a list of words that the grater would have.

Doing it right means walking through the actual course materials and formalizing **inferences**, not words:

- For each concept, what does it *refuse*? (The plugged holes.)
- From any commitment, what does Amy-and-her-recognitive-community license as a next move? (The entitlements.)
- Which commitments depend on which others? (The requires-graph.)
- What is the inferential strength of each term, per the matrix you build for quadrilaterals — the row sum of restrictions rejected?

The ten terms we have are a sketch. The real artifact is the full inferential structure of the course, and that's a slower, careful piece of work that needs the course materials open and reasoned through. Not today.

---

## What we do not do

- Talk to students.
- Replace the teacher.
- Reduce friction; the productive struggle is the pedagogy.
- Generalize. This is N101. Other courses would get their own kernels.

## What the artifact offers a skeptical reader

The material inferences are authored, readable, checkable. The derivations are Prolog, executable. The LLM's stochastic fluency is confined to the translation boundaries, and forbidden content is pinned to exactly zero probability at the sampling step. The claim isn't that AI has been tamed; the claim is that we've built a small formal system and used AI narrowly, in the places where stochastic cognition can legitimately do work — at the graters' permeable edges, not in the holes we've chosen to plug.

---

---

## The More Machine as model of dialog

Not required for the math. Required for a philosophically coherent read on what's happening between teacher and student, and the reason the little-lights interface below is worth building.

The More Machine is a Cantorian diagonalization machine with a Zeeman Catastrophe Machine attached. It accepts inputs, writes to a historical matrix, and outputs a symbol guaranteed to differ from every prior output by flipping the diagonal. In your words:

> "I realized that the ms and ws would allow me to attach a Zeeman Catastrophe Machine to the More Machine. Zeeman's machine is not totally deterministic. When the rubberbands are pulled straight down, the machine enters into a super-positioned state, where the wheel could rotate left or right."

> "When this tension collapses into a specific output (m or w), the machine recollects its entire history diagonally, creating new sequences that transcend any finite enumeration while remaining grounded in the system's logical structure."

Components, per your existential landscape map:

- **Fixed anchor** — self-certainty, the ground.
- **Control point** — awareness, the locus of the finite me.
- **Disc** — the state; traces the Sound-of-Time sine; writes M, W.
- **Friction on the disc** — power dynamics. In a suppressive environment, the machine can't move; nothing gets written.

The load-bearing classroom claim is sublation-as-catastrophe:

> "the tension of counting 'many ones' suddenly collapses into the recognition of 'one ten' — that is the same sublation the disc just performed."

An aha is a catastrophe. A snap is a catastrophe. The machine writes either way. Friction is what determines whether it can move at all, which is where power enters, which is where the teacher's attention should go.

The matrix the machine writes is a spatialization — temporal compression of judgment-history into recollective form. In practical code the matrix isn't manipulated; it's the philosophical grounding for a spatial referent. Fraction Bars connect here downstream.

---

## The little-lights interface, derived from the More Machine

Not a dashboard of metrics. A signal.

**Signal semantics.** When the dialogue state crosses the cusp — superpositioned tension collapses into a snap — a light goes on next to that student's name. The light is valence-neutral. "Something big just happened." The teacher, who is the only node with recognitive authority, decides whether to soak it up or help pick up the pieces.

**What the teacher sees on tap.** The accumulated commitments from this student's turns, rendered as the spatial artifact the More Machine says they are. Not a chat log. A grater: what the student has committed to, which holes are plugged, which are open.

**Friction differentiates.** If the student's state is stuck — judgment-ready tension that can't snap because the environment is suppressive — the signal reads differently. The teacher's first move there is social, not pedagogical.

**What the interface does not do.** Decide for the teacher whether the catastrophe was good. Name the snap for the class. Publish anything. The machine hands the teacher the signal and gets out of the way.

---

## Open, for you

- **Re-encoding N101 as inferences**, not words. A course-materials session with the source texts open. This is the main next move.
- **Projective validity**, operationalized. The entitlement graph checks support; it doesn't yet check stability of substitutions across polarity-inverting contexts. That's the next Prolog layer.
- **The inferential strength matrix**, a numerical read on where a student's commitments sit. A few hours of Prolog once the inferences are in.
- **DeepSeek thinking-mode as the renderer**, wired explicitly. Prolog specification into the system prompt, `<think>` doing the translation, visible output being the teacher-voice suggestion.
- **The little-lights interface**, minimal. A catastrophe-neutral signal driven by the ZCM state, with the grater-shaped matrix available on click.
- **Fraction Bars and the matrix as spatialization** — deferred. The More Machine is the philosophical grounding; the code connection is downstream of the inferences work.
