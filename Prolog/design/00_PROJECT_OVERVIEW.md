# Strategy Connection Redesign — Project Overview

## What this is

Planning documents for redesigning how the Prolog formalization's arithmetic
strategies connect to one another. The current system has ~25 strategy automata
that operate as islands — each takes inputs, runs transitions, produces a result,
but nothing one strategy learns is available to another. The teacher masks this by
handing back answers. The synthesis engine wraps teacher calls rather than building
from primitives. The system demonstrates *that* crisis-driven learning works as
an architecture, but not *how* one piece of mathematical knowledge becomes
another.

## The central insight

Strategies are **sentence frames** (in Brandom's sense) with adicities. Each FSM
is a Q(α, β) — "given numbers α, compute using strategy β." The strategies don't
connect because we haven't formalized whether the rules governing their
subsentential elements *extend* across strategies. That extension is what Savich
(2022) calls **projective validity** — a species of normative validity testing
whether rules are extendable into new material circumstances.

## The architectural claim

The redesign is organized around three philosophical structures and one
paradox:

### 1. Meaning fields for numerals

Each numeral has a **meaning field** — a set of compossible interpretations that
starts dense (confusion) and acquires internal structure through interaction.
`two` begins with a meaning field containing one element (the counting trace).
Teacher interactions depopulate the field (ruling out bad inferences) and relax
it (accepting new co-referentialities). The field doesn't just shrink; it
acquires AND/OR/XOR structure between elements.

Learning is the depopulation of a meaning field through information. Confusion
is the 2^n combinatorial explosion when the learner doesn't know which
interpretations are compossible (AND/OR) vs. incompossible (XOR). See Savich
(2022, Ch. 3) on hybridized models and meaning field analysis, drawing on
Carspecken (1995).

### 2. Projective validity as the connection mechanism

A strategy learned for `eight + five` projects into `seven + six` if and only
if the rules governing the subsentential elements (partition facts, counting
operations) extend to the new context. That projection is testable — it is
falsifiable at three points (Savich, 2022, Ch. 3):

- The substitution inference a → a' (does the description of the student's
  algorithm accurately capture the original?)
- The substitution inference b → b' (does the machine code reproduce what the
  student would do with new inputs?)
- The inference Qa so Qb (is the inference from original to new context good?)

The teacher's role is to **recognize when projections are worth attempting**
and to **validate or reject** the student's results. The student's prior
activity creates the conditions for the teacher's recognition. The teacher
says "look at this"; the student looks again; the teacher says yes or no.

### 3. Three modes of validity (Habermas, via PML)

Every numeral and every strategy simultaneously raises three validity claims:

- **Objective (o/1)**: Prolog's native arithmetic. The world-check. `2 + 3 =:= 5`
  just works. Inert but authoritative. The "stick-in-itself" from Brandom's
  bent-stick example.
- **Normative (n/1)**: The teacher's vocabulary and rules. The community's
  scorekeeping. Material inferences, incompatibilities, what counts as a good
  move. The second-person layer.
- **Subjective (s/1)**: The counting trace. The embodied experience. The ability
  to rationally reconstruct from output to the history that produced it.
  "Connected knowing." The first-person layer.

The `ought-not` → `cannot` transition (Savich, 2022, pp. 228-229) is the
developmental moment where normative recognition becomes alethic fact. This
maps onto the deontic→alethic shift in the PML.

### 4. The paradox: depopulation AND relaxation

Learning is simultaneously:

- **Depopulation** (compressive, `comp_nec`): The teacher's "no" rules out
  alternatives, reducing 2^n confusion toward determinate structure.
- **Relaxation** (expansive, `exp_nec`): New co-referentialities are accepted.
  `two` becomes compatible with `one-plus-one`, `three-minus-one`,
  `ten-minus-eight`. The inferential role grows richer.

These go in opposite directions. Yet they are both learning. The same event can
be both: "yes, `ten` is `seven-plus-three`" is a relaxation (new connection) AND
a depopulation (the meaning field for `ten` is now more structured, ruling out
interpretations incompatible with this partition).

This maps onto Hegel's **determinate negation** — negation that produces a richer
concept rather than mere absence. The formalization should enact this paradox,
not resolve it.

## What exists and what doesn't

### Exists, keep as-is or with minor modification:
- Strategy automata in `Prolog/math/` — these are the sentence frames
- Grounded arithmetic (`grounded_arithmetic.pl`) — recollection-based counting
- Counting DPDAs (`counting2.pl`, `counting_on_back.pl`) — trace producers
- PML operators (`pml_operators.pl`) — s/1, o/1, n/1 wrappers
- Incompatibility semantics (`incompatibility_semantics.pl`) — entailment framework
- Pragmatic/semantic axioms — dialectical drive (unsatisfiable desire, etc.)
- Highlander and equality-iterator automata — structural rules for practices
- FSM engine (`fsm_engine.pl`) — unified execution for strategy automata

### Exists, needs significant redesign:
- Oracle server (`oracle_server.pl`) — currently gives answers; design/03 calls
  for redesign as a teacher that monitors traces and prompts reflection
- Meta-interpreter (`meta_interpreter.pl`) — crisis detection and modal
  context tracking work; meaning field integration (design/01) remains
  hypothetical
- Execution handler (`execution_handler.pl`) — ORR cycle works; projective
  validity testing (design/02) remains hypothetical

### Partially exists:
- Number-word layer — implemented as the curriculum module
  `strategies/standards/standard_k_ns_2.pl` (10 tests). Teacher teaches names
  incrementally via `learn_numeral/2`; names are normative assignments.

### Does not yet exist, needs to be built:
- Meaning field module — per-numeral meaning fields with AND/OR/XOR structure
  (design/01)
- Projective validity tester — the three-point falsifiability protocol
  (design/02)
- Reflection mechanism — how counting traces become structural facts
  (design/06, "the hard problem")
- More/less relational vocabulary — `more(five, three, two)` as the first
  asymmetric predicate over numbers
- Level-transition mechanism — V→P→V' cycle with generated (not prescribed)
  curriculum

### Resolved since this overview was written:
- Lazy automata — all 27 strategy automata grounded (2026-04-14). No `is/2`
  in strategy code.
- Synthesis engine — archived as dead code (2026-04-14). `fsm_synthesis_engine.pl`
  is a shim preserving the module interface; the teacher-wrapped path in
  `execution_handler.pl` is what actually runs.
- `synthesized_paper.md` — moved to archive (Phase 1).
- Overlapping architecture docs — consolidated; Phase 2 modularization complete.

## Naming caution

The codebase uses "arche-trace" and "trace" as terms. These are suggestive of
Derrida's concepts but the implementation (SWI-Prolog attributed variables that
resist unification with concrete terms) does not constitute a philosophically
serious engagement with Derrida's notion of trace/différance. The mechanism is
interesting on its own terms — a variable that propagates through unification but
fails when grounded — but claiming Derridean lineage overstates what the code
does. Future documentation should describe the mechanism honestly and note the
resonance without overclaiming.

Similarly, the dialectical engine and PML do not "implement" Hegelian dialectics.
They model specific structural features (compression/expansion, crisis/
accommodation, the three validity modes) that can be made to behave like certain
reasoning patterns under controlled conditions. The interesting thing is where
these formalizations fail or oversimplify.

## The reflection problem

The hardest piece of this redesign is the **reflection mechanism** — how does the
system look at its own counting traces and notice structural patterns (partitions,
place value, commutativity)?

We do not yet have a satisfactory answer to this. Possible approaches:

1. **Hard-coded reflection templates** per level — honest but limited
2. **Diagonalization over the inference/meaning field** — see Carspecken (2013)
   on recursion and self-reference in meaning. This may be the most
   philosophically appropriate approach but is difficult to implement.
3. **Teacher-prompted reflection** — the teacher asks questions that force the
   system to examine its own traces. The conversation IS the reflection. This is
   implementable but shifts the intelligence to the teacher's question-selection.

Any of these may turn out to be the right approach, or none of them. The point of
the formalization is partly to discover where reflection resists formalization.
That breakdown is itself a finding.

## Document index

- `00_PROJECT_OVERVIEW.md` — this file
- `01_MEANING_FIELDS.md` — meaning field module design
- `02_PROJECTIVE_VALIDITY.md` — projective validity tester design
- `03_ORACLE_REDESIGN.md` — teacher module (recognition-triggered interaction)
- `04_NUMBER_WORDS.md` — number-word layer and numeral semantics
- `05_COUNTING_TRACES.md` — counting engine and trace production
- `06_REFLECTION.md` — reflection mechanism (the hard problem)
- `07_PML_INTEGRATION.md` — how the PML drives the system
- `08_PRUNING.md` — what to remove or reorganize

## References

- Savich, T.M. (2022). *Towards a Critical Mathematics*. Indiana University.
  (Dissertation)
- Brandom, R.B. (2000). *Articulating Reasons*. Harvard University Press.
- Brandom, R.B. (2008). *Between Saying and Doing*. Oxford University Press.
- Brandom, R.B. (2019). *A Spirit of Trust*. Harvard University Press.
- Carspecken, P.F. (1995). *Critical Ethnography in Educational Research*.
  Routledge.
- Carspecken, P.F. (2013). Reference cited re: recursion/diagonalization over
  meaning fields. PDF not yet in repository.
- Habermas, J. (1971). *Knowledge and Human Interests*. Beacon Press.
