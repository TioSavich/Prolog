# Assessment: Crisis-Driven Fraction Learning

*Written 2026-03-20 after wiring Jason PFS/FCS into the oracle.*

## The Question

Can the existing ORR (Observe-Reorganize-Reflect) cycle learn fractional reasoning
the same way it learns whole-number strategies? The manuscript claims fractions are
"fractal expansion of the base dialectic" — iterative decompression past the unit
boundary. If that's right, the same crisis-driven architecture should generalize.

## What's Already Working

The oracle now speaks "fraction." Two strategies are registered:

```prolog
?- query_oracle(fraction(3,7), 'PFS', R, I).
% R = 0.4286..., I = 'Partitive fractional scheme: Partition...'

?- query_oracle(fraction_composition(3-4, 1-4), 'FCS', R, I).
% R = 0.1875, I = 'Fractional composition scheme: Find 3/4 of 1/4...'
```

The grounded ENS operations (`grounded_ens_operations.pl`), fraction semantics
(`fraction_semantics.pl`), normalization, and composition modules are implemented
and tested. The `jason.pl` modern version uses recollection structures. The
`jason_fsm.pl` has the full FSM with traces.

## What's Missing: Three Gaps

### Gap 1: No fraction goals in the crisis pipeline (easy, ~2 hours)

The meta-interpreter doesn't know about `fraction/3` as a goal type. The
`execution_handler.pl` has no `consult_oracle_for_solution/4` clause for fractions.
No `object_level:fraction/3` stub exists to trigger exhaustion or unknown_operation.

This is pure wiring. Structurally identical to how subtract, multiply, and divide
were added. The oracle side is already done.

### Gap 2: Representation bridge (medium, design decision required)

The ORR pipeline lives in the Peano/integer world:
- Meta-interpreter operates on `s(s(s(0)))` terms
- `consult_oracle_for_solution` converts Peano → integer for oracle calls
- Oracle-backed strategies convert integer → Peano for results

The fraction modules live in the recollection/unit world:
- `grounded_ens_operations.pl` uses `recollection([t,t,t])` structures
- `jason_fsm.pl` uses `unit(Value, History)` with rational arithmetic
- `jason.pl` (modern) uses recollection-based ENS

These worlds are not connected through the ORR cycle. The curriculum_processor
shows they *can* connect (it converts integers to recollections and calls
`partitive_fractional_scheme`), but the crisis pipeline has no such bridge.

**The design question**: Does the learner encounter fractions as Peano terms? As
recollections? As raw integers? This is not a technical question — it is a
philosophical one about what "encountering a fraction" means for a system that
started with tally marks.

### Gap 3: No true FSM synthesis for fractions (hard, the real research)

The `fsm_synthesis_engine.pl` synthesizes FSMs for addition using three patterns:
`count_on_bigger`, `make_base`, `commutative_swap`. Its primitives are
`successor/2`, `predecessor/2`, `decompose_base10/3`. It knows nothing about
`ens_partition`, `ens_disembed`, or `ens_iterate`.

The oracle-backed path (which subtract/multiply/divide use today) sidesteps this:
it asserts a strategy that just calls the oracle at runtime. This works but is
philosophically unsatisfying — the learner hasn't actually learned anything, it has
just memorized a phone number.

For the system to *genuinely learn* fractional reasoning, the synthesis engine
needs new primitives (partition, disembed, iterate), new hint detection for
fractional vocabulary, and new `synthesize_*` predicates that build fraction FSMs
from ENS operations. This is where the real intellectual work is.

## Verdict: Medium Difficulty

| Component | Difficulty | Status |
|-----------|-----------|--------|
| Oracle speaks fraction | Done | Wired in this session |
| Grounded ENS operations | Done | Existing code, tested |
| Fraction semantics / normalization | Done | Existing code, tested |
| Crisis pipeline wiring (Gap 1) | Easy | ~2 hours of dispatch clauses |
| Representation bridge (Gap 2) | Medium | Design decision needed |
| True FSM synthesis (Gap 3) | Hard | New synthesis primitives needed |

**The oracle-backed path** (Gaps 1+2 only) could be wired in an afternoon. The
system would "learn" fractions the same way it "learns" multiplication today:
by asserting a clause that calls the oracle. Functional, demonstrable, philosophically
hollow.

**True synthesis** (all three gaps) is where the manuscript's thesis gets tested.
The claim is that fractions are the same dialectical pattern as whole numbers —
just decompressed past the unit boundary. If that's true, the existing synthesis
architecture should generalize with new primitives. If it's not, the synthesis
engine will fail in instructive ways, and *that failure is the interesting result*.

## Why This Is Still Challenging

The user's analog-watch insight is suggestive: "the denominator is ALMOST like a
base system; 1/7th is kinda like 1 unit in base 7." The counting PDA encounters
the paradox of the one and the many when carrying (10 ones → 1 ten). Fractions
encounter the same paradox when finding common denominators (1/7 + 1/5 requires
finding 1/35 as the common unit).

But "almost like" is doing a lot of work. Three specific challenges:

1. **The base is not fixed.** In whole-number counting, base 10 is the normative
   standard — crises happen at 9→10, 99→100. In fractions, the "base" (denominator)
   varies per operation. The synthesis engine's `make_base` strategy hardcodes
   base 10 as the target. For fractions, the target base is the LCM of the
   denominators, which is a second-order operation the current system cannot
   discover.

2. **Three levels of units.** Hackenberg and Steffe describe fraction reasoning as
   requiring coordination of three unit levels simultaneously: the unit fraction
   (1/n), the composite fraction (m/n), and the referent whole. The current
   synthesis engine coordinates two levels (ones and tens). Adding a third level
   is not a quantitative extension — it is a qualitative shift in the FSM's
   state space.

3. **Metamorphic accommodation.** The FCS demonstrates this: the result of one
   PFS call becomes the input whole for the next PFS call. This is recursion
   over the strategic shell, not over the iterative core. The synthesis engine
   currently cannot synthesize recursive strategies — it produces flat FSMs.
   Synthesizing an FSM that *calls another FSM as a subroutine* is a different
   class of problem.

## The Deeper Problem: Where Does the Divisible Object Come From?

Human subjects encounter fractions in the context of fair-sharing — seven kids
want to share a cake. The cake is a *spatial referent with multiple access*: an
object that can be divided. Without it, there is nothing to partition.

The system currently has `ens_partition/3`, which mechanically divides a unit
into N parts. But there is no philosophical grounding for WHY the system can
divide a unit. The operation exists as a primitive, but a primitive needs an
origin story within the crisis-driven architecture. Children don't start knowing
how to partition — they learn it through the embodied act of cutting, folding,
sharing physical objects. The system skips this.

### Denominator-Specific Automata: A Possible Bridge

The counting PDA (`counting2.pl`) implements base-10 counting with carry states.
When nine ones become one ten, this is sublation — negation, preservation,
elevation. The automaton's stack encodes the compressed history of counting.

A fraction crisis might generate *new counting automata parameterized by the
denominator*. A "base-7 counter" carries at 7 instead of 10. A "base-5 counter"
carries at 5. Each such automaton is a recollection — a compressed temporal
experience of "what it means to count in groups of 7."

Under this reading:
- 1/7 is the first tick of the base-7 automaton
- 3/7 is three ticks of the base-7 automaton
- The automaton itself IS the denominator, understood as a recollected process
- Common denominators (adding 1/7 + 1/5) require synthesizing a meta-automaton
  that subsumes both base-7 and base-5 counting — the LCM as dialectical
  synthesis of two incommensurable counting norms

This aligns with the manuscript's claim (UMEDCA_Concatenated_fixed.tex, line 4393):
"in base 7, seven itself is (10)_7, and 1/7 becomes (0.1)_7 — the fundamental
unit fraction of that base system."

### The Spatial Extant Problem

The manuscript's "Sound of Time" chapter (UMEDCA_Concatenated_fixed.tex, line 572)
develops the claim that spatial extants are recollections of temporally extended
experiences. "50 miles" compresses the journey from Bloomington to Indianapolis as
a recollection. If this is right, then a "cake" (the divisible spatial referent)
is not a primitive — it is itself a recollection of some temporally extended
experience (consuming it, measuring it, traversing it).

But the system has no primitive for "continuous divisible quantity." Its recollections
are discrete (tally marks). The `unit(whole)` in the fraction modules is declared
by fiat, not constructed through crisis. This may be the deepest gap: the system
needs a crisis that produces the *concept* of a divisible whole before it can
encounter fractions as crises over that whole.

Alternatively, the counting automaton itself could serve as the spatial referent.
An automaton for base-10 "contains" 10 ticks. This is a bounded, structured
object that can be partitioned. "1/7 of the base-10 automaton" would be a
meaningful (if strange) operation — it asks: what happens when you try to divide
a 10-tick process into 7 equal parts? The answer is: you can't do it evenly,
and the resulting crisis forces a context shift from N to Q.

### A Caution About Representation

Hackenberg and Steffe's emphasis on three-level unit coordination is empirically
grounded, but it inherits radical constructivism's Kantian picture-thinking — the
idea that mathematical knowledge is structured by internal representations that
mirror external objects. The manuscript's Hegelian alternative claims that
mathematical objects ARE the processes that generate them (UMEDCA_Concatenated_fixed.tex,
line 4401: "A repeating decimal is not a representation of a rational number — it
is the rational number as a stabilized loop in the division algorithm. The number
is the process that generates it").

If we take this seriously, then the three "levels of units" are not static
representational structures to be coordinated. They are moments in a temporal
process that the automaton enacts. The unit fraction 1/7 is not a representation
held in a register — it is the first tick of a base-7 counting process. This
reframing might simplify the synthesis problem (no need to coordinate static
registers), while making the divisible-whole problem harder (the "whole" is not
a given object but must be constructed as the recollection of a completed
counting process).

## Where to Find the Theoretical Pieces

*For future sessions that need to pick up this thread:*

### The Three Registers (Subjective, Normative, Objective)

The polarized modal logic (PML) implements three validity modes. These are the
philosophical backbone of the system, not an implementation detail.

- **Canonical definition**: `Prolog/pml_operators.pl` lines 43-73 — `s/1` (Subjective),
  `o/1` (Objective), `n/1` (Normative), with four modal operators (`comp_nec`,
  `exp_nec`, `exp_poss`, `comp_poss`)
- **Dialectical rhythm**: `Prolog/semantic_axioms.pl` lines 34-67 — transition facts
  encoding the compressive/expansive oscillation and inter-modal dynamics
  (e.g., Oobleck: `s(comp_nec P) -> o(comp_nec P)`)
- **PML prover**: `Prolog/incompatibility_semantics.pl` lines 137-164 — the rhythm
  structural rule that drives modal transitions during proof search
- **Manuscript exposition**: `Modal_Logic/UMEDCA_Concatenated_fixed.tex` lines 4820-4857
  — "Subjective Validity: The Embodied Ground," "Normative Validity: Material
  Inferences," "Objective Validity: Formal Automata as Choreography"

### The Counting Automaton (Base-10 PDA)

The existing whole-number counting automaton. Relevant because denominator-specific
variants may be the path to fraction crises.

- **Unidirectional PDA**: `Prolog/math/counting2.pl` — `run_counter/2`, states
  `q_start → q_idle → q_inc_tens → q_inc_hundreds`, stack carries at 9→0
- **Bidirectional PDA** (tick/tock): `Prolog/math/counting_on_back.pl` — adds
  `tock` (decrement) with borrow states `q_dec_tens`, `q_dec_hundreds`
- **Theoretical paper**: `Modal_Logic/counting.pdf` (16 pages) — formal transition
  function δ, sublation when carrying, "diagonalization as the one and the many"
- **LaTeX source**: `Modal_Logic/counting.tex` line 96 — "Counting is not merely
  an accumulation of marks — it is a process that both preserves and transforms"

### Jason's Fractional Schemes (Steffe's ENS)

Two implementations: the FSM version (now in the oracle) and the grounded version.

- **FSM version (oracle)**: `Prolog/math/jason_fsm.pl` — module `jason_fsm`,
  `run_pfs/5` (Partitive Fractional Scheme), `run_fcs/5` (Fractional Composition
  Scheme). Uses `unit(Value, History)` with rational arithmetic. Full state
  machine traces. Wired into `oracle_server.pl` as strategies 'PFS' and 'FCS'.
- **Grounded version**: `Prolog/math/jason.pl` — module `jason`,
  `partitive_fractional_scheme/4`. Uses recollection structures. Calls
  `grounded_ens_operations:ens_partition/3`.
- **ENS operations**: `grounded_ens_operations.pl` — `ens_partition/3` creates
  nested `unit(partitioned(N, InputUnit))` structures using recollection arithmetic
- **Fraction equivalence**: `fraction_semantics.pl` — `apply_equivalence_rule/3`
  with Grouping (reconstitution) and Composition (integration) rules
- **Theoretical paper**: `Modal_Logic/Jason.pdf` (6 pages) — formal translation
  of Steffe's account into automata, ENS as primitive operations, PFS as finite
  automaton, FCS as nested PFS (metamorphic accommodation)
- **Architecture diagram**: `Modal_Logic/jason_automaton_picture.pdf` — Strategic
  Shell (PFS) over Iterative Core (ENS) with three Cognitive Registers

### Fractions as Fractal Expansion of the Base Dialectic

The manuscript's central claim about how fractions relate to whole numbers.

- **Core section**: `Modal_Logic/UMEDCA_Concatenated_fixed.tex` lines 4380-4423
  — "Fractions as Fractal Expansion of the Base Dialectic"
- **Key passage** (line 4393): "in base 7, seven itself is (10)_7, and 1/7
  becomes (0.1)_7 — the fundamental unit fraction of that base system"
- **Self-similar ratio chain** (line 4397): (Hundreds:Tens)::(Tens:Ones)::(Ones:Tenths)
- **Incommensurable norms** (lines 4405-4421): adding 1/7 + 1/5 as dialectical
  synthesis; common denominator as sublation
- **Three-level restructuring** (line 4376): "a dialectical reorganization...
  such that the cognitive automaton can support simultaneous three-tiered
  coordination prior to operating"

### The Representation Bridge (curriculum_processor.pl)

The only place where all three number layers connect end-to-end.

- `curriculum_processor.pl` lines 110-120 — `process_task(fraction(Num, Den))`
  converts integers to recollections via counting, then calls
  `partitive_fractional_scheme(TallyNum, TallyDen, [unit(whole)], Result)`
- Lines 142-158 — `process_task(fraction_of_fraction(...))` for recursive fractions
- **Conversion utilities**: `grounded_utils.pl` lines 149-163 —
  `peano_to_recollection/2` and `recollection_to_peano/2`

### The Sound of Time / Spatial Extants as Recollections

The philosophical move that might unstick the "where does the divisible object
come from" problem.

- **Chapter**: `Modal_Logic/UMEDCA_Concatenated_fixed.tex` line 572 — "The Sound
  of Time" chapter
- **Central metaphor** (lines 722-735): circular unified experience "unrolled"
  through time into linear oscillation — compression and rarefaction
- **Number as recollected counting** (line 2553): "The child who counts 'one,
  two, three, four' is not just establishing reference to discrete cardinalities.
  They are building through iteration."
- **Number IS the process** (line 4401): "A repeating decimal is not a
  representation of a rational number — it is the rational number as a stabilized
  loop in the division algorithm."
- **Motion-to-space compression** (line 4627): "numerical thinking compresses the
  temporal process of counting into a spatial form"
- **Reflective transducers** (lines 2641-2649): automata as "recursive loops where
  outputs feed back as inputs, where states include monitoring states"

### The ORR Crisis Pipeline (execution path)

For wiring fractions into the crisis architecture.

- **Crisis catch**: `execution_handler.pl` line 108 — `handle_perturbation/3`
- **Oracle consultation**: `execution_handler.pl` lines 239-313 —
  `consult_oracle_for_solution/4` (add/subtract/multiply/divide clauses)
- **FSM synthesis**: `fsm_synthesis_engine.pl` lines 62-105 — true synthesis for
  addition; lines 107-170 — oracle-backed strategies for other operations
- **Synthesis primitives**: `fsm_synthesis_engine.pl` lines 259+ —
  `synthesize_count_on_bigger`, `synthesize_make_base`, `synthesize_commutative`
- **Unknown operation trigger**: `meta_interpreter.pl` lines 205-217 — throws
  `perturbation(unknown_operation(Op, Goal))` when no object_level clause exists
- **Learned strategy LIFO**: `meta_interpreter.pl` line 167 — checks
  `more_machine_learner:run_learned_strategy` before falling through to primordial

## Recommended Path

1. Wire the oracle-backed path (Gaps 1+2) to get a working fraction demo.
2. Add ENS primitives to `fsm_synthesis_engine.pl` so the synthesizer can
   *attempt* true fraction synthesis.
3. Explore denominator-specific counting automata as a mechanism for constructing
   the divisible whole from the counting process itself.
4. Document where it fails. Those failure points are the manuscript's evidence
   that formalization breaks productively at the fraction boundary.
