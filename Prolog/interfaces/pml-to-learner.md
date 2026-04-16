# PML to Learner: Modal Polarity Modulates Inference Cost

## The Claim

PML operators modulate the learner's inference cost through a single working
wire: the compressive/expansive context multiplier. When a goal is wrapped in
`comp_nec(X)` or `comp_poss(X)`, the meta-interpreter switches to compressive
context and all subsequent operations cost more. When wrapped in `exp_nec(X)` or
`exp_poss(X)`, the context switches to expansive and operations cost less.

This is the one connection between PML and the learner model that exists as
running code. Everything else — mode-dependent strategy selection, meaning
fields, decentering modulation — is either planned or hypothetical.

## What Exists in Code

### Modal Context in the Meta-Interpreter

`learner/meta_interpreter.pl`, `solve/6` (lines 124-143), handles PML operators
as special goals:

```prolog
solve(s(comp_nec(X)), CtxIn, CtxOut, I_In, I_Out, [modal_trace(...)]) :-
    CtxOut = compressive,
    cognitive_cost(modal_shift, ShiftCost),    % 3, from config.pl
    AdjustedCost is ShiftCost * CtxMul,
    ...
    solve(X, compressive, _, I_Mid, I_Out, SubTrace).
```

When the meta-interpreter encounters a modal operator:
1. It charges a `modal_shift` cost (default 3, from `learner/config.pl`)
2. The cost is multiplied by the current context multiplier
3. The sub-goal `X` is solved under the new context
4. After the sub-goal returns, the **original context is restored**

Context multipliers for cost, embedded in `solve/6`:
- Compressive: 2x (each `incur_cost` signal doubled)
- Neutral: 1x
- Expansive: 1x

### Tension Dynamics: Sharper Multipliers

`learner/tension_dynamics.pl` (lines 102-134) uses steeper multipliers for
tension accumulation:

```
compressive:  2.5x tension growth
neutral:      1.0x
expansive:    0.3x
```

Tension is a continuous value tracked over a rolling window of the last 20
values. The stability check (`tension_dynamics.pl:181-211`) computes the second
derivative of tension over time. When stability < 0 (tension accelerating) and
the history window has 9+ entries, the system throws
`perturbation(tension_instability)`.

Under compressive context, tension accumulates 2.5x faster, meaning the
catastrophe threshold is reached sooner. Under expansive context, tension grows
at 0.3x — almost an order of magnitude slower. The modal context set by PML
operators directly determines how quickly the learner approaches crisis.

### Cost Types and Their Values

`learner/config.pl` defines the embodied cost model:

| Cost Type | Value | Emitted By |
|-----------|-------|-----------|
| `unit_count` | 5 | Grounded counting operations (successor, predecessor) |
| `slide_step` | 2 | Number line movement |
| `inference` | 1 | Logical reasoning steps |
| `fact_retrieval` | 1 | Compressed fact access |
| `modal_shift` | 3 | Context transitions (the PML wire) |
| `recollection_step` | 1 | Tally manipulation |
| `norm_check` | 2 | Normative validation |

When a strategy step emits `incur_cost(unit_count)` under compressive context,
the effective cost is 5 * 2 = 10 (meta-interpreter) and tension grows by
5 * 2.5 = 12.5 (tension dynamics). Under expansive context: effective cost
5 * 1 = 5, tension growth 5 * 0.3 = 1.5.

### The Embodied Prover's Parallel

`arche-trace/embodied_prover.pl:46-48` has its own cost multiplier:

```prolog
get_inference_cost(compressive, 2).
get_inference_cost(expansive, 1).
get_inference_cost(neutral, 1).
```

And its own modal context determination (`embodied_prover.pl:58-63`):

```prolog
determine_modal_context(comp_nec(_), compressive).
determine_modal_context(comp_poss(_), compressive).
determine_modal_context(exp_nec(_), expansive).
determine_modal_context(exp_poss(_), expansive).
```

This is the same wire implemented independently. The meta-interpreter and the
embodied prover both route PML operators to cost modulation, but through
separate code paths (see `arche-trace-to-learner.md` for the gap between them).

### Modal Trace Output

When the meta-interpreter processes a modal operator, it emits a
`modal_trace/4` term in the execution trace:

```prolog
modal_trace(operator, context_before, context_after, sub_trace)
```

This trace is included in the computation's event log and is visible through
the HTTP API (`GET /api/events`). The ORR visualization on the server's
timeline page can display modal context transitions.

## Where the Claim Holds

### The Cost Multiplier Is Real Arithmetic

The wire is not metaphorical. When the meta-interpreter runs
`s(comp_nec(add(recollection([t,t,t]), recollection([t,t]), Result)))`, the
modal context switches to compressive, and every `incur_cost(unit_count)` in
the addition's execution is doubled. The total cost of the operation is
literally higher. The tension dynamics literally accumulate faster. Crisis is
literally more likely.

This means: embedding a computation in a compressive modal frame makes that
computation more expensive and more crisis-prone. Embedding it in an expansive
frame makes it cheaper and more stable. PML operators are not annotations —
they change the computational economics of the operation they wrap.

### Tension Instability Is Context-Sensitive

Because compressive context multiplies tension growth by 2.5x while expansive
multiplies by 0.3x, the same sequence of operations can trigger crisis under
one context and complete stably under the other. A counting_on(7, 5) that
succeeds under expansive context (low tension growth per step) may exceed the
stability threshold under compressive context (high tension growth per step).

The tension dynamics' second-derivative check means that not just total tension
but its *acceleration* matters. Under compressive context, tension accelerates
faster, making the stability check more likely to go negative. This models the
intuition that reasoning under pressure (compressive: "you MUST get this right")
is more crisis-prone than reasoning under exploration (expansive: "let's find
out what happens").

## Where the Claim Exceeds What's Formalized

### The Wire Is Polarity-Only

PML has 12 operators (4 polarities x 3 modes). The cost multiplier responds to
polarity (compressive vs. expansive) but is blind to mode (S, O, N). This means:

- `s(comp_nec(X))` — teacher asserting about their own understanding
- `o(comp_nec(X))` — teacher asserting about the student's procedure
- `n(comp_nec(X))` — teacher invoking curricular authority

These are different pedagogical acts with different consequences for student
learning, but the cost model treats them identically. The mode distinction, which
is central to PML's discourse-analytic power, has no computational effect in the
learner model.

### No Meaning Fields

The spec mentions "what meaning fields would add if they existed." Meaning fields
are a hypothetical extension where each PML mode opens a different space of
available strategies:

- **S-mode**: only strategies the learner has internalized (learned strategies in
  `more_machine_learner:run_learned_strategy/5`)
- **O-mode**: strategies observable in another's behavior (would require
  multi-agent architecture)
- **N-mode**: strategies sanctioned by the curriculum (the standards modules in
  `strategies/standards/`)

None of this is implemented. The meta-interpreter's strategy selection
(`solve/6:180-207`) checks learned strategies via LIFO ordering but does not
filter by modal mode.

### Decentering Is Not Modeled

PML's discourse-analytic claim is that the decentering signature — a teacher's
ability to adopt the student's modal position — is measurable through the
S/O/N mode distribution. A decentered teacher spends more time in O-mode
(attending to the student's reasoning) and less in S-mode (asserting their own
understanding).

The learner model has no representation of decentering. The oracle server
(`learner/oracle_server.pl`) provides strategies as a black box. It does not
model the teacher's modal stance or adjust its behavior based on the learner's
current PML mode. The oracle is modally neutral.

### Person-Position Tracking Absent

PML's person-position markers (first person = S, second person = O,
third person = N) would track *whose reasoning* is being executed:

- My strategy (S-mode): I discovered this approach
- Your strategy (O-mode): I am following what you showed me
- The strategy (N-mode): this is how the textbook says to do it

The learner does not track strategy provenance at this level. When a strategy
is learned from the oracle, it is asserted as a clause in
`more_machine_learner`. The fact that it came from an external source (O-mode
or N-mode) versus being synthesized internally (S-mode) is not recorded.

### Context Restoration Limits Modal Flow

When `solve/6` encounters a modal operator, it switches context for the
sub-goal and then restores the original context. This means modal effects are
local: a compressive frame around one operation does not bleed into subsequent
operations. In natural discourse, modal stance often persists — a teacher who
shifts to compressive mode tends to stay there for a stretch. The
meta-interpreter's context restoration prevents this kind of modal momentum.

## What Would Strengthen This Interface

1. **Mode-sensitive cost model.** Add a second multiplier keyed to S/O/N mode.
   One possibility: S-mode operations cost more (internal reasoning is
   cognitively demanding), O-mode operations cost less (applying someone
   else's procedure is cheaper), N-mode is intermediate. Alternatively:
   mode determines which cost *types* are active (S-mode charges
   `recollection_step`, O-mode charges `fact_retrieval`, N-mode charges
   `norm_check`).

2. **Strategy provenance tracking.** When a strategy is learned from the oracle,
   tag it with its source mode: oracle-provided (O-mode), standard-matched
   (N-mode), or synthesized (S-mode). Allow mode-based filtering: under
   S-mode context, only try S-tagged strategies first.

3. **Modal persistence option.** Add a configuration flag: when modal context
   changes, does it reset after the sub-goal (current behavior) or persist
   until the next modal operator? Persistent mode would model the discourse
   phenomenon of modal momentum.

4. **Oracle decentering.** When the oracle provides a strategy, have it adjust
   its interpretation based on the learner's current modal context. Under
   S-mode, provide a first-person narrative ("You count on from here...").
   Under O-mode, provide an observational account ("Watch: starting at 8,
   we count 6 more"). Under N-mode, provide a normative statement
   ("The standard method is to decompose into tens and ones").

5. **Meaning field prototype.** Implement the simplest version: under N-mode
   context, restrict strategy selection to those that match a loaded
   standards module. If `current_domain(n)` and the current standard is
   `standard_1_ca_1`, only try strategies listed in that standard's exports.
   This would make PML's N-mode computationally meaningful without requiring
   full meaning field infrastructure.
