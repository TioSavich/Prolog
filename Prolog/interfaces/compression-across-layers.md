# Compression Across Layers: The Shared Polarity

## The Claim

PML's compressive/expansive polarity and the strategy automata's temporal
compression/decompression are the same structure operating at different levels of
description. Teacher discourse and student arithmetic share this polarity.

A teacher saying "This IS seven plus five equals twelve" (compressive necessity,
categorical) and a child using rounding to compute 7 + 5 (compressed strategy,
O(1) cost) are doing analogous things: collapsing a possibility space that was
previously open. A teacher saying "I wonder what would happen if..." (expansive
possibility, hedging) and a child counting on by ones to compute 7 + 5 (expanded
strategy, O(B) cost) are doing the complementary thing: holding open what the
outcome might be.

This is the project's core cross-level claim. Whether it is true — whether
"same structure" means formal isomorphism or productive analogy — is the open
question this interface document frames.

## What Exists in Code

### Compression in Strategy Automata

The 27 strategy automata form a developmental trajectory from expanded to
compressed:

| Strategy | Operation | Temporal Cost | Character |
|----------|-----------|---------------|-----------|
| `sar_add_counting_on` | A + B | O(B) steps | Maximally expanded: one successor per count |
| `sar_add_cobo` | A + B | O(B/10 + B%10) | Partial compression: count by bases then ones |
| `sar_add_chunking` | A + B | O(digits) | Further compressed: decompose, chunk, recombine |
| `sar_add_rounding` | A + B | O(1) + adjustment | Near-maximally compressed: round, compute, adjust |

Each strategy module in `strategies/math/` implements this as an FSM with
`transition/3` or `transition/4`. The number of transitions to reach
`accept_state/1` decreases as strategies become more compressed.

Every transition calls grounded arithmetic operations from
`formalization/grounded_arithmetic.pl`, each of which emits `incur_cost/1`
signals. The total cost of a strategy execution is the sum of these signals.

### Compression in PML

The four modal operators encode polarity:

- **Compressive** (`comp_nec`, `comp_poss`): narrowing, fixation, crystallization
- **Expansive** (`exp_nec`, `exp_poss`): opening, release, liquefaction

Defined in `pml/pml_operators.pl:24-33` as prefix operators. The dialectical
rhythm in `pml/axioms_eml.pl` models how states cycle between compressed and
expanded forms:

```
s(u)  =>  s(comp_nec(a))     Unity compresses into awareness
s(a)  =>  s(exp_poss(lg))    Awareness can expand toward release
s(a)  =>  s(comp_poss(t))    Awareness can compress toward fixation
```

The bad infinite (`axioms_eml.pl:40-41`) is oscillation trapped in compression:
`s(t_b) => s(comp_nec(t_n))` and back. No expansion occurs.

### Strategy Automata Emit PML Operators

The connection between PML and strategy execution is not merely structural —
the strategy automata themselves emit PML modal operators during their FSM
transitions. Examples from the actual strategy code:

- `sar_add_cobo.pl`: entering base-counting emits
  `s(comp_nec(focus_on_bases))`; switching to ones emits
  `s(exp_poss(shift_to_ones))`
- `sar_add_chunking.pl`: adding a base chunk emits
  `s(comp_nec(adding_complete_base_chunk))`
- `sar_sub_cbbo_take_away.pl`: counting back by bases emits
  `s(comp_nec(applying_embodied_base_subtraction))`

These calls are embedded directly in the `transition/3` and `transition/4`
rules. They execute during strategy FSM traversal, not during sequent
calculus proof. The PML operators are part of the strategy's choreography.

### The Wire: Modal Context Multiplies Cost

The meta-interpreter (`learner/meta_interpreter.pl`, `solve/6`) tracks modal
context through proof execution. When a PML operator appears in a goal
(including those emitted by strategy transitions):

- `comp_nec(X)` or `comp_poss(X)` sets context to **compressive**
- `exp_nec(X)` or `exp_poss(X)` sets context to **expansive**

The embodied prover (`arche-trace/embodied_prover.pl:46-48`) charges different
costs per context:

```prolog
get_inference_cost(compressive, 2).   % compression is taxing
get_inference_cost(expansive, 1).     % expansion flows
get_inference_cost(neutral, 1).
```

The tension dynamics (`learner/tension_dynamics.pl:102-134`) use sharper
multipliers:

```
compressive context:  2.5x tension accumulation
neutral context:      1.0x
expansive context:    0.3x
```

This means: reasoning under compressive modality (categorical, fixed, necessary)
burns resources faster and builds tension more quickly. Reasoning under expansive
modality (possible, open, exploratory) conserves resources and accumulates
tension slowly. The modal context set by PML operators directly modulates the
computational economics of strategy execution.

### Cost Tracking in Strategies

Each strategy step signals its cost type via `incur_cost/1`:

| Cost Type | Value (config.pl) | Character |
|-----------|-------------------|-----------|
| `unit_count` | 5 | Embodied counting action, temporally extended |
| `slide_step` | 2 | Mental number line step, spatial |
| `inference` | 1 | Abstract logical reasoning |
| `fact_retrieval` | 1 | Compressed fact access |
| `modal_shift` | 3 | Cognitive context transition |
| `recollection_step` | 1 | Tally manipulation |

A counting_on execution of 7 + 5 incurs 5 `unit_count` costs (5 successor
operations at cost 5 each = 25 total). A rounding execution incurs a few
`inference` costs plus one `fact_retrieval` (approximately 4 total). The ratio
of these totals IS the compression ratio — expanded strategies cost more because
they do more temporal work.

## Where the Claim Holds

### The Cost Multiplier Works

When the meta-interpreter processes a goal wrapped in `s(comp_nec(...))`, the
context switches to compressive, and all subsequent `incur_cost` signals are
multiplied. A counting_on strategy run under compressive context costs 2.5x what
it costs under expansive context. This is not metaphor — it is arithmetic in the
tension dynamics.

### The Developmental Trajectory Is Real

Children learn counting strategies first (expanded) and develop toward
compressed strategies over time. This is documented in CGI research
(Carpenter/Fennema via Hackenberg). The strategy automata in `strategies/math/`
are ordered by this developmental trajectory, and the grounding work (all 27
automata converted, `strategies/GROUNDING_STATUS.md`) preserves the cost
differential: expanded strategies literally take more grounded operations.

### The Bad Infinite Is Compressive Stagnation

The critique module (`arche-trace/critique.pl:100-107`) defines a bad infinite
as a proof cycle where every node is compressive:

```prolog
is_bad_infinite(Cycle) :-
    Cycle \= [],
    forall(member(Node, Cycle), is_compressive_node(Node)).
```

A learner stuck in the bad infinite is cycling between compressed states without
ever expanding. This corresponds to the classroom phenomenon of a student
applying the same (wrong) procedure repeatedly, never stepping back to
reconsider from an open stance.

## Where the Claim Exceeds What's Formalized

### "Same Structure" Is Not Proved

The cost multiplier connects PML polarity to strategy execution cost. But
"same structure" is a stronger claim than "connected." A formal isomorphism
would require:

1. A compression measure on FSM traces (number of transitions, total cost,
   trace length)
2. A compression measure on discourse sequences (categorical density, hedge
   ratio, tempo markers)
3. A proof that these two measures satisfy the same algebraic properties
   (associativity, ordering, compositionality)

None of this exists. The connection is through a shared scalar multiplier, not
through a demonstrated structural identity.

### The Scalar Multiplier Does Not Distinguish Modes

PML has 12 operators: 4 polarities x 3 modes. The cost multiplier responds only
to polarity (compressive/expansive). It does not distinguish whether a
compressive utterance is S-mode (the teacher fixing their own understanding),
O-mode (the teacher fixing the student's procedure), or N-mode (the teacher
invoking a standard). These are different discursive acts with different
pedagogical consequences, but the cost model treats them identically.

### Temporal Compression and Discursive Compression May Differ in Kind

A child's shift from counting_on to rounding is a change in procedure — fewer
physical or mental actions to reach the same result. A teacher's shift from
hedging to categorical assertion is a change in stance — a different relationship
to the claim being made. Whether "fewer actions" and "stronger commitment" are
the same kind of compression, or merely share the word "compression," is the
question the claim depends on.

The project's answer — following Brandom — would be that both are changes in
inferential commitment structure. A compressed strategy carries more inferential
commitments per step (each step presupposes the validity of prior compressions).
A categorical assertion carries more inferential commitments per utterance
(asserting P categorically commits the speaker to everything P entails). If
inferential commitment density is the shared measure, then the claim holds at
the level of Brandom's pragmatic expressive bootstrapping.

But this argument is philosophical, not formalized.

### No Empirical Connection

No study has measured teacher discourse compression (via PML or any coding
scheme) alongside student strategy compression (via the automata or any other
instrument) to test whether they covary. The catastrophe theory literature
(van der Maas & Molenaar 1992, Tall 1977) provides operationalizable criteria
for developmental transitions — bimodality, hysteresis, sudden jumps — but
these have not been applied to the PML-strategy interface.

## What Would Strengthen This Interface

1. **Define compression formally on both sides.** For strategies: compression
   ratio = (counting_on cost for inputs A, B) / (strategy cost for A, B).
   For discourse: compression ratio = (proportion of compressive-polarity
   codings in a segment). Show these ratios have the same monotonicity
   properties under composition.

2. **Mode-sensitive cost model.** Extend the cost multiplier to distinguish S,
   O, and N modes. One possibility: S-mode compressive costs more (internal
   fixation is cognitively expensive), O-mode compressive costs less (applying
   a known procedure to the other), N-mode compressive is intermediate
   (invoking authority). Test whether this better predicts crisis onset.

3. **Empirical covariation study.** Code 5-10 instructional episodes with PML
   (teacher discourse) and record which strategies students use before and
   after. Test whether episodes with more expansive teacher discourse correlate
   with student strategy expansion (trying new approaches), and whether
   compressive teacher discourse correlates with strategy consolidation.

4. **Catastrophe theory bridge.** Use van der Maas & Molenaar's (1992)
   criteria to operationalize the transition between expanded and compressed
   strategies. Check whether the transition exhibits bimodality (students use
   either the old or the new strategy, not a blend) and hysteresis (the
   transition point depends on direction). If so, the More Machine's Zeeman
   catastrophe surface is not just a metaphor — it is a measurement model
   for the compression transition.

## The Fractal Hypothesis

If the compression/expansion polarity is genuinely the same at both levels, then
the relationship is fractal: each strategy automaton is a disc on a Zeeman
catastrophe surface, transitions between states are springs, nesting of
strategies (chunking calls counting_on as a subroutine) is stacked machines, and
spring tension is PML polarity. Teacher discourse tunes the springs.

This is the vision described in the project's fractal catastrophe architecture.
Whether it coheres as formal mathematics or remains a generative metaphor for
further formalization is not yet determined.
