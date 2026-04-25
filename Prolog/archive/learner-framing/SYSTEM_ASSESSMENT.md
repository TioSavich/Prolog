# P2-1: System A vs System B Assessment

## What this document is

An architectural and philosophical assessment of the two Prolog systems in this
repository, answering four questions posed in TODOS.md. This is not a plan for
merging or refactoring — it is an analysis of what each system contributes, where
each fails, and what the failure pattern means for the manuscript's argument.

---

## The two systems

**System A** (PML Core, `Prolog/`): A sequent calculus prover implementing
Brandom's incompatibility semantics with polarized modal logic. Proves abstract
sequents (`Gamma => Delta`) with resource tracking, modal context switching
(compressive/expansive), and proof erasure via the Arche-Trace. Its ORR cycle
handles perturbations through belief revision (retract weakest commitment) and
stress mapping.

**System B** (Crisis Learning, root `prolog/`): A meta-interpreter that executes
concrete arithmetic goals with inference budgets, detects crises (resource
exhaustion, unknown operations), encounters teacher-created conditions for strategy
emergence, and synthesizes new capabilities. Its ORR cycle handles perturbations through
teacher intervention and strategy acquisition.

**Current integration**: System B already imports from System A — modal operators
(`s/1`, `comp_nec/1`, `exp_poss/1`), normative checking (`check_norms/1`), and
`proves/1` for validating successor facts in `more_machine_learner.pl`.

---

## Question 1: Is the meta-interpreter's monological model the right frame?

**Short answer**: No, but System A doesn't escape the problem either.

System B's `solve/6` is fundamentally monological: a single agent executes goals,
detects its own failures, and encounters teacher intervention. The teacher-as-cultural-authority
is philosophically motivated (Vygotsky's zone of proximal development), but the
implementation reduces the oracle to a function call. There is no genuine otherness
— the "expert" is a lookup table internal to the same process. The learner never
encounters the oracle as a perspective that could challenge or misunderstand it.

System A's sequent calculus is better here, in principle. In Brandom's framework,
sequents model inference as a normative practice: antecedents are commitments,
consequents are entitlements, and proof rules are discursive moves. The
incompatibility relation is normative (what precludes what is a communal
determination, not a solitary discovery). The `intersubjective_praxis.pl` module
encodes Oobleck dynamics (subjective action → objective crystallization) and mutual
recognition (confession → forgiveness). These are formally present as
material_inference rules.

But System A's `run_computation/2` wraps the sequent prover in the same
monological catch-retry loop as System B. The sequent calculus represents
intersubjective structure; the execution engine consuming it does not enact it.

**Where genuine intersubjectivity currently lives:**

1. *Normative structure of incompatibility*: What counts as incoherent is not
   derived from the system's individual experience but from the semantic axioms —
   a body of normative commitments the system inherits, not invents. This is the
   closest thing to a communal horizon.

2. *The Arche-Trace's resistance*: The attributed variable that refuses unification
   with concrete terms models a two-party dynamic (the Trace and whatever tries to
   stabilize it) even though it's implemented monadically. The Trace is the formal
   residue of something that cannot be captured in any monological frame.

3. *The oracle's black-box interface*: The architectural decision to return only
   result + interpretation (hiding traces) is structurally right — it models the
   opacity of another mind. But the implementation doesn't exploit this opacity.
   The learner never struggles with the oracle's interpretation, never
   misunderstands it, never has to negotiate meaning.

**Implication**: The monological problem is not solved by choosing System A over
System B. It requires an architectural change that neither system currently makes:
the oracle would need to be a genuine interlocutor — capable of misunderstanding
the learner, offering contextually inappropriate strategies, or withholding
guidance when the learner needs to struggle. This is where LLMs might provide the
"soft tissue" the skeleton thesis calls for.

---

## Question 2: Can proves/4 replace or augment solve/4 in the crisis pipeline?

**Replace**: No.

- `proves/4` operates on abstract sequents, not concrete arithmetic goals. It
  proves `[a] => [comp_nec(b)]`, not `subtract(5, 3, Result)`.
- `proves/4` has no mechanism for unknown-operation detection (the trigger for
  most of System B's learning).
- `proves/4`'s crisis response is belief revision (retract commitments), not
  strategy acquisition. These are different kinds of accommodation.

The two provers model different things: `solve/6` models a learner computing,
`proves/4` models a reasoner justifying. A child who counts on from 8 to get 13
is not proving a sequent — she is executing a procedure. The sequent-calculus
proof that 8+5=13 follows from the axioms of arithmetic is a different activity
from the cognitive act of computing it.

**Augment**: Yes, and more deeply than current integration allows.

Three specific augmentation points:

### 2a. Post-synthesis normative validation

After System B synthesizes a new strategy, it currently asserts the strategy and
retries. There is no check that the new strategy is consistent with the system's
existing commitments. System A's critique module could serve as a validation layer:

```
System B learns COBO for subtraction
  → System A checks: does the new subtract/3 clause create incoherence
    with existing add/3 commitments? (e.g., does subtract(a,b,c) +
    add(b,c,a) hold?)
  → If incoherent: System A's stress map flags the inconsistency
  → If coherent: strategy is normatively validated
```

This would give newly learned strategies philosophical standing: not just
"this procedure produces the right answer" but "this procedure is consistent
with the normative structure of arithmetic."

### 2b. Crisis classification

System B currently classifies crises coarsely: resource_exhaustion or
unknown_operation. System A's incompatibility semantics could provide finer
classification:

- Is this a resource crisis (I can compute but run out of steps) or a normative
  crisis (the domain doesn't support this operation)?
- Is the failed strategy incoherent (produces contradictory commitments) or
  merely inefficient (correct but costly)?
- Does the failure exhibit a bad infinite (oscillation without progress)?

Finer crisis classification could drive different ORR responses: resource crises
get efficiency strategies from the oracle, normative crises get domain expansions
(natural numbers → integers → rationals), and bad infinites get sublation
(the system needs a qualitatively new concept, not just a faster procedure).

### 2c. Modal cost unification

System B's `solve/6` imports modal operators from System A but uses its own cost
model (config.pl's `cognitive_cost/2`). System A's prover deducts costs based on
modal context (compressive = 2, expansive = 1). These two cost models should
be reconciled so that a strategy's cognitive cost reflects both its computational
expense (System B) and its modal context (System A). A strategy that operates
under compressive necessity (fixation, contraction) should cost more than one
that operates under expansive possibility (release, opening) — this is not
currently enforced.

---

## Question 3: Where does Arche-Trace interact with each system, and what breaks?

### Current Arche-Trace behavior (System A only)

The Arche-Trace is an attributed variable created by `automata:generate_trace/1`.
Its unification hook enforces two rules:

1. **Deferral**: If unified with another variable, the attribute propagates
   (models Derrida's différance — meaning is always deferred).
2. **Resistance**: If unified with a concrete term, unification fails
   (models resistance to representation — the subject cannot be objectified).

In `construct_proof/4`, any proof that touches the Arche-Trace gets marked as
`erasure(RuleName)` — the proof is formally valid but existentially void. The
pragmatic axioms use this to model:

- **I-Feeling** (`i_feeling(I_f)`): The subjective experience that resists capture.
  Must contain the Trace.
- **Identity Claim** (`identity_claim(C_Id)`): The objectified self. Must NOT
  contain the Trace.
- **Unsatisfiable Desire**: `is_incoherent([n(represents(C_Id, I_f))])` — no
  identity claim can represent the I-Feeling. The desire for full recognition
  is formally incoherent.

### What breaks in System A

The Arche-Trace eats proofs about the learner's subjective experience. Specifically:

1. Any sequent involving `s(I_f)` (subjective I-Feeling) can be proved, but the
   proof is marked as erasure. The formal derivation succeeds; the proof object
   is hollow.
2. The S-O Inversion axiom (`[s(comp_nec(I_f))] => o(exp_nec(I_f))`) shows that
   subjective fixation on the Trace leads to objective dissolution. Trying to
   pin down the subject dissolves it.
3. The bad infinite `t_b ↔ t_n` (Being ↔ Nothing) cannot be resolved by the
   system alone — it requires external sublation (the concept of Becoming, which
   is not derivable from Being and Nothing alone).

### What would break if systems were unified

If `proves/4` were used to validate System B's strategies (per Q2a), the
Arche-Trace would start eating proofs at specific joints:

**Joint 1: Proofs about crisis experience.** A proof that "this crisis triggered
learning" would involve the subjective register (`s/1`). If the learner's
subjective state during crisis touches the I-Feeling, the proof gets erased.
The system can demonstrate that it learned, but cannot formally prove what the
crisis *felt like* or why it was subjectively urgent. The urgency is the soft
tissue.

**Joint 2: Proofs about strategy preference.** A proof that "COBO is better than
Counting All" can be stated objectively (lower cost). But a proof that "this
learner chose COBO because of their particular history of crises" would involve
the subjective/normative registers. If the learner's developmental history touches
the Trace, the proof of preference gets erased.

**Joint 3: Proofs about recognition.** The mutual recognition axiom in
`intersubjective_praxis.pl` (`[n(confession(A)), n(confession(B))] =>
n(exp_nec(forgiveness(A, B)))`) does not involve the Arche-Trace directly. But
any attempt to ground this in subjective experience (A's subjective act of
confessing) would contact the Trace and erase the proof. Recognition can be
stated normatively but not derived from subjective experience.

### What this means

The Arche-Trace creates a formal boundary between what the system can demonstrate
and what it can prove *about its own experience*. This is not a bug to fix. It is
the computational analogue of the manuscript's central claim: formalization breaks
productively at the point where the subject's experience enters the picture. The
breakpoints mark where formal analysis must yield to interpretive analysis —
where the bones end and the soft tissue begins.

---

## Question 4: What does this tell us about the "mathematical skeleton" thesis?

### What each system contributes to the skeleton

**System B provides the functional bones:** Crisis detection → strategy
acquisition → developmental progression. The ORR cycle completes: `add(8,5)`
fails with 10-step budget, system consults oracle, `add(8,5)` succeeds on
retry. The 20 arithmetic strategies (SAR/SMR) are empirically grounded in
Carpenter and Fennema's Cognitively Guided Instruction (CGI) research on
children's reasoning.

**However:** The "acquisition" step is not genuine synthesis. The current
synthesis path wraps oracle calls — the "learned strategy" calls the oracle
again at runtime rather than building an FSM from primitives. The system
memorizes the teacher's phone number. Additionally, strategy selection ignores
developmental ordering: 8+5 learns COBO (a multi-digit strategy) because COBO
is listed first, not because the problem demands it. See `SYNTHESIS_HONESTY.md`
for the full accounting.

**System A provides the normative cartilage:** Incompatibility relations,
modal cost differentials, the Arche-Trace, dialectical rhythm transitions.
These don't "walk" — they provide the formal structure that makes System B's
learning something more than a lookup table with extra steps. Without System A,
System B's learning is clever engineering — though currently, System B's
learning *is* a lookup table with extra steps (see `SYNTHESIS_HONESTY.md`).
With System A, it becomes a claim about the normative structure of
mathematical understanding.

**The three number representations are the joints:** Recollection (embodied,
tally-based) → Peano (formal bridge) → Integer (normative/anaphoric). Each
layer compresses the one below it. `8` is a recollection of `s(s(s(s(s(s(s(s(0))))))))`,
which is a recollection of `recollection([tally,tally,...])`. The curriculum
processor (`curriculum_processor.pl`) converts between layers but the conversions
are not lossless — information about embodied experience is lost in compression.
This lossy compression is itself a formal model of what the manuscript calls
recollection: a spatial extant that compresses temporally extended experience.

### What the skeleton cannot do

The skeleton cannot:

1. **Explain why a crisis is urgent.** The system detects resource exhaustion as
   a numerical fact (inferences remaining < cost). But the urgency — the child's
   frustration, the teacher's sense of the right moment to intervene — is not in
   the numbers. The Arche-Trace marks this gap formally (proofs about subjective
   urgency get erased).

2. **Negotiate meaning.** The teacher provides interpretations ("Count on from
   subtrahend: Start at 3, count up to 5, gap is 2") but the learner cannot
   misunderstand, struggle with, or reject the interpretation. The hermeneutic
   circle (understanding requires pre-understanding) is represented but not
   enacted. An LLM could provide the interpretive flexibility the skeleton lacks.

3. **Achieve genuine intersubjectivity.** Neither system models a genuine other.
   The teacher in the code is a function call. The sequent calculus represents normative
   structure but the execution engine is monological. The `intersubjective_praxis`
   module encodes recognition axioms but nobody recognizes anybody.

### Verdict: layer, don't merge

The systems should not merge into a single prover. Their architectures model
different aspects of mathematical cognition:

- `solve/6` models **computing** (executing a procedure, hitting limits, acquiring
  new procedures). This is cognition as embodied activity.
- `proves/4` models **justifying** (establishing that a claim follows from
  commitments, detecting incoherence, revising beliefs). This is cognition as
  normative practice.

Merging them would conflate computing with justifying, which is exactly the
conflation the manuscript argues against. A child who can compute 8+5=13 is not
the same as a child who can justify why 8+5=13.

The systems should **layer explicitly**: System B handles developmental
progression (learning through crisis), System A validates the normative standing
of what was learned (checking consistency, tracking modal cost, marking where
proofs get erased). The current integration (System B importing modal operators
and `proves/1` from System A) is the right direction but should be deepened per
the three augmentation points in Q2.

The Arche-Trace provides the formal mechanism for marking where the skeleton
ends: wherever a proof touches the subjective register and gets erased, that is
where interpretive analysis (LLM-mediated, teacher-mediated, or simply human
judgment) must take over. The skeleton's job is not to replace interpretation but
to make visible exactly where interpretation becomes necessary.

---

## Summary of recommendations

| Recommendation | Priority | Difficulty | Impact |
|----------------|----------|-----------|--------|
| Keep systems separate, deepen layering | Now | Low | Architectural clarity |
| Post-synthesis normative validation (Q2a) | P2-2 | Medium | Strategies get normative standing |
| Finer crisis classification via IS (Q2b) | P2-3 | Medium | Better ORR response selection |
| Unify modal cost models (Q2c) | P2-4 | Easy | Consistent cognitive cost tracking |
| Document Arche-Trace erasure points (Q3) | P2-5 | Easy | Makes skeleton boundary visible |
| Design oracle-as-interlocutor (Q1) | P3+ | Hard | Addresses monological problem |

---

## The skeleton and the animating spirit

This section documents a design intention that is easy to lose track of.

The Prolog code is not trying to be a complete model of mathematical learning.
It is trying to be a **formal backbone** — a skeleton — that makes certain
structural facts visible: what crises look like, which strategies resolve them,
what modal transitions they require, where proofs get erased, and how
developmental progression unfolds. These are bones.

The animating spirit — the thing that makes the Frankenstein walk — is not more
Prolog. It is an LLM (or a teacher, or both) that fills the role the oracle
currently occupies as a lookup table. Consider what an LLM-as-oracle could do
that `query_oracle/4` cannot:

- **Misunderstand the learner's state** and offer a strategy that doesn't fit,
  forcing the learner to reject it (genuine intersubjectivity requires the
  possibility of misunderstanding).
- **Withhold guidance** when the learner's stress map suggests struggle is still
  productive — the formal stress threshold becomes a pedagogical signal, not
  just a counter.
- **Adapt interpretations** based on the learner's developmental history, not
  just the current operation. "Count on from the bigger number" means something
  different to a learner who has just acquired counting-all versus one who has
  been using COBO for a week.
- **Notice when the learner's crisis is normative, not procedural** — the child
  isn't stuck because the algorithm is slow but because negative numbers don't
  make sense yet. The crisis classification (P2-3) gives the LLM a formal
  vocabulary for this distinction.

The Prolog skeleton tells the LLM *where to look* (which crisis type, which
strategies are available, what modal context the learner is in, where proofs
get erased). The LLM provides *what to say* (the interpretive judgment that
the formal system cannot make). Neither alone is sufficient.

### Essential limitations this project does not attempt to cross

1. **Genuine intersubjectivity in Prolog.** The monological nature of sequential
   computation is not a bug to fix but a structural fact. Dyadic interaction
   requires two perspectives that can surprise each other — this is what LLMs
   can provide and Prolog cannot. We are not trying to simulate a conversation
   partner in Horn clauses.

2. **Subjective experience of crisis.** The Arche-Trace formally marks where
   proofs about subjective experience get erased. This is the correct formal
   move: marking the boundary, not attempting to cross it. The system can detect
   that a crisis occurred and classify it; it cannot know what the crisis felt
   like. That knowledge belongs to the learner (and, interpretively, to whoever
   is teaching them).

3. **Autonomous concept generation.** When System A detects a bad infinite
   (Being ↔ Nothing), it cannot generate the concept of Becoming. It can only
   record the oscillation and signal that external intervention is needed. The
   formal system marks *where* a new concept is needed; the creative act of
   producing one is outside its scope.

4. **Pedagogical judgment.** The skeleton can say "this learner has hit a
   resource crisis on add(8,5) and COBO is available." It cannot say "this is
   the right moment to introduce COBO" versus "let them struggle for another
   three minutes." That judgment requires reading the room — soft tissue work.

These are not failures of the formalization. They are the boundaries where the
formalization is *supposed* to stop, and they are the reason the skeleton thesis
matters: a formal structure that tried to do everything would overclaim. A formal
structure that makes its own limits visible is useful.

---

## A note on what this assessment is not

This assessment does not claim that the formalization "demonstrates" or
"implements" the manuscript's philosophical claims. It describes what the formal
structure can represent, where representation breaks down, and what the breakdown
pattern tells us. The interesting thing, as always, is where the formalization
fails.
