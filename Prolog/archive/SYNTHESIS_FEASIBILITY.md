# Genuine Synthesis: Feasibility Assessment

## What this document is

An analysis of whether the system can genuinely synthesize strategies from primitives,
based on tracing through every strategy automaton, the grounded arithmetic layer, and
the meta-interpreter. Written March 2026 alongside SYNTHESIS_HONESTY.md.

---

## The three tiers of strategy discovery

### Tier 1: Synthesizable from successor/predecessor

These strategies are compositions of operations the learner starts with. A search
loop that enumerates compositions of `successor`, `predecessor`, and `comparison`
could find them.

| Strategy | Operation | How it composes | Search space |
|----------|-----------|-----------------|--------------|
| Counting On | add | Iterate successor(A) B times | Tiny |
| Counting Back | subtract | Iterate predecessor(A) B times | Tiny |
| Dealing by Ones | divide | predecessor + round-robin grouping | Small |
| UCR (Commutative) | divide | Repeated addition + equality check | Small |
| CBO (Mult) | multiply | Grouping/regrouping via grounded add/subtract | Medium |

**Critical gap:** Counting On does not exist as a separate strategy automaton.
It is the first thing children learn after Counting All and the first strategy
that is genuinely synthesizable from `successor`. The system jumps from
Counting All directly to COBO, skipping the foundational rung.

### Tier 2: Requires a new primitive (the concept of a base)

These strategies require `decompose_base10` — the ability to see 55 as
"5 tens and 5 ones." This concept is not derivable from counting. It is the
dialectical synthesis of the one-and-the-many: ten ones *are* a ten. This is
itself a learning event.

| Strategy | Operation | Prerequisite primitive |
|----------|-----------|----------------------|
| COBO | add | decompose_base10 + Counting On |
| CBBO Take Away | subtract | decompose_base10 + Counting Back |
| COBO Missing Addend | subtract | decompose_base10 + Counting On |
| ABAO (Add Bases Add Ones) | add | decompose_base10 + addition within columns |

A learner that has Counting On but not `decompose_base10` would face a *different
kind of crisis* when given 38+55: "I can count on, but 55 steps exceeds my budget.
I don't have a way to take bigger steps." The oracle's response is not just "93" but
the *concept* of decomposing into bases — a new primitive, not a new composition.

### ~~Tier 3: Requires taught abstract properties~~ [CORRECTED]

**Update (March 2026):** The original Tier 3 was wrong. RMB, Chunking, Rounding,
and DR are NOT about arbitrary "friendly numbers" or abstract properties. They are
ALL about making bases — the same concept as Tier 2 (decompose_base10), applied in
different contexts. The entire N101 curriculum is oriented toward bases.

Once the learner has decompose_base10, these strategies become **synthesizable**:
- **RMB**: rearrange to make a base (8+5 → 10+3) — compose decompose + transfer
- **Chunking**: chunk to hit base landmarks — compose decompose + strategic iteration
- **Rounding**: round to a base, compute, adjust — compose round + COBO + count back
- **DR**: split multiplicand using bases (4×9 = 4×10 - 4×1) — discoverable via
  inference cost comparison, not taught property

What remains genuinely teachable (requires conceptual reframe, not just composition):
- **Sliding**: The constant-difference invariant (M+K)-(S+K) = M-S
- **Decomposition**: Column reasoning + regrouping (borrowing)
- **COBO Missing Addend**: Reframing subtraction as "what must I add?"
- **IDP**: Division as inverse multiplication (requires prior multiplication facts)
- **UCR (Division)**: Transforming sharing division into measurement division

See CURRICULUM_MAP.md for the full classification of all 25 strategies.

---

## The N101 curriculum as prerequisite graph

The developmental sequence from Tio's N101 course maps to a prerequisite graph.
Each step presupposes capabilities from previous steps.

```
ADDITION (SAR — Strategic Additive Reasoning)
  1. Counting All           [given: enumerate + recursive_add]
  2. Counting On            [synthesize: iterate successor]          ← MISSING
  3. Counting Back          [synthesize: iterate predecessor]        ← MISSING
  4. RMB                    [teach: friendly number heuristic]
  5. COBO                   [teach: decompose_base10, then synthesize from Counting On]
  6. Chunking               [teach: strategic targeting, presupposes COBO]
  7. Rounding & Adjusting   [teach: compensation, presupposes COBO]
  8. ABAO                   [teach: column operation, presupposes decompose_base10]

SUBTRACTION (SAR)
  9. CBBO Take Away         [synthesize from Counting Back + decompose_base10]
 10. COBO Missing Addend    [synthesize from Counting On + decompose_base10]
 11. Chunking A/B/C         [teach: strategic chunking variants]
 12. Rounding               [teach: double-rounding compensation]
 13. Sliding                [teach: constant-difference invariant]
 14. Decomposition          [teach: column reasoning + regrouping]

MULTIPLICATION (SMR — Strategic Multiplicative Reasoning)
 15. C2C (Count by ones)    [synthesize: iterate addition]
 16. Strategic Counting     [teach: use addition strategies to support C2C]
 17. CBO (Bases & Ones)     [synthesize: grouping/regrouping from grounded ops]
 18. Commutative            [teach: A×B = B×A]
 19. DR (Distributive)      [teach: distributive property]

DIVISION (SMR)
 20. CBO (Measurement)      [teach: base-10 divisibility]
 21. IDP                    [teach: inverse of multiplication facts]
 22. Dealing by Ones        [synthesize: predecessor + round-robin]
 23. UCR (Commutative)      [synthesize: repeated addition + equality check]
```

Entries marked "synthesize" can be built from the learner's current primitives.
Entries marked "teach" require the oracle to introduce a new concept.

---

## What genuine synthesis requires (implementation sketch)

### 1. A Counting On strategy automaton

The system needs `sar_add_counting_on.pl`:
```prolog
% States: q_init → q_count → q_accept
% q_init: Start at A, set counter to B
% q_count: successor(Current, Next), decrement counter, emit step
% q_accept: counter = 0, return Current
```

This is the simplest possible addition strategy beyond Counting All.
It is synthesizable from `successor/2` alone.

### 2. A primitive-composition search loop

Given:
- Current primitives: {successor, predecessor, comparison, ...}
- Target: produce Result from (A, B)

Search:
```prolog
try_composition(Primitives, A, B, TargetResult, Strategy) :-
    % Generate candidate FSMs from primitives
    compose_fsm(Primitives, CandidateFSM),
    % Execute candidate on inputs
    execute_fsm(CandidateFSM, A, B, ActualResult),
    % Check result
    ActualResult == TargetResult,
    % Check cost (must be cheaper than what failed)
    fsm_cost(CandidateFSM, A, B, Cost),
    Cost =< MaxBudget,
    Strategy = CandidateFSM.
```

The search space for Tier 1 strategies is small because the primitives are few.
The hard part is `compose_fsm/2` — defining the grammar of FSM compositions.
For the simple cases (iterate successor, iterate predecessor, iterate addition),
the grammar is: `loop(Primitive, Counter)`.

### 3. A developmental state tracker

```prolog
:- dynamic learner_has_primitive/1.
:- dynamic learner_has_concept/1.

% Initial state
learner_has_primitive(successor).
learner_has_primitive(predecessor).
learner_has_primitive(comparison).

% After learning Counting On:
% assert learner_has_primitive(count_on)

% After being taught decompose_base10:
% assert learner_has_concept(base_10_decomposition)
% assert learner_has_primitive(decompose_base10)
```

### 4. Crisis type distinction

```prolog
classify_synthesis_failure(Goal, Primitives, CrisisType) :-
    (   can_solve_but_too_expensive(Goal, Primitives)
    ->  CrisisType = resource_exhaustion
        % Learner needs a more efficient composition of what it has
    ;   cannot_solve_at_all(Goal, Primitives)
    ->  CrisisType = conceptual_insufficiency
        % Learner needs a new primitive/concept from oracle
    ).
```

This distinction — "I need a faster way" vs. "I need a new idea" — is the
developmental boundary. Resource exhaustion leads to synthesis. Conceptual
insufficiency leads to teaching.

---

## What this would look like as an "on-rails" demonstration

### Scene 1: Counting All (given)
- Problem: 2+3, budget 30
- Learner: enumerate(2), enumerate(3), recursive_add → 13 inferences → success
- No crisis. Strategy works.

### Scene 2: Crisis → Counting On (synthesized)
- Problem: 8+5, budget 15
- Learner: enumerate(8) + enumerate(5) + recursive_add → ~21 inferences → CRISIS
- Oracle: "The answer is 13. Count on from the first number."
- Synthesis search: "Can I reach 13 from 8 using successor?" → 5 steps → YES
- Learned: iterate_successor(A, B_times) → Counting On
- Retry: 8, 9, 10, 11, 12, 13 → 5 inferences → success

### Scene 3: Counting On works for a while
- Problem: 7+4, budget 15 → Counting On → 4 steps → success
- Problem: 12+6, budget 20 → Counting On → 6 steps → success

### Scene 4: Crisis → Concept needed (taught)
- Problem: 38+55, budget 20
- Counting On: 55 successor calls → CRISIS
- Synthesis search: "Can I compose successor more efficiently?" → No.
  All compositions of successor are O(B). I need a way to take bigger steps.
- CrisisType: conceptual_insufficiency
- Oracle teaches: decompose_base10 (the concept of a base)
- With new primitive: synthesize COBO → count on 5 tens then 5 ones → 10 steps
- Retry: 38, 48, 58, 68, 78, 88, 89, 90, 91, 92, 93 → success

### Scene 5: COBO works for multi-digit addition
- Problem: 24+33, budget 20 → COBO → 3+3=6 steps → success

### Scenes 6+: Subtraction crisis, multiplication crisis, etc.
Following the N101 sequence, each new operation type triggers a crisis that either:
- Leads to synthesis from current primitives (Tier 1), or
- Reveals conceptual insufficiency requiring teaching (Tier 2-3)

---

## Feasibility verdict

| Component | Feasibility | Effort |
|-----------|------------|--------|
| Counting On automaton | Trivial | 1 file, ~50 lines |
| Counting Back automaton | Trivial | 1 file, ~50 lines |
| Primitive-composition search (Tier 1) | Moderate | Grammar of FSM compositions is the hard part |
| Developmental state tracker | Easy | Dynamic predicates tracking acquired primitives |
| Prerequisite graph (N101 curriculum) | Easy | Encode the sequence from Tio's course |
| Crisis type distinction | Moderate | Requires synthesis-failure detection |
| Tier 2 concept teaching | Hard | Oracle must convey new primitives, not just results |
| Tier 3 heuristic teaching | Very hard | Requires the oracle to teach *when* to apply, not just *what* |
| "Showing" the strategy (trace observation) | Moderate-hard | Learner must parse oracle traces into patterns |
| Full on-rails demo (counting → division) | Moderate | Composing all the above |

**Overall: the system has the raw materials. The grounded primitives are correct.
The strategy automata are correct. The meta-interpreter meters correctly. What's
missing is the connective tissue: the search loop, the developmental tracker, and
the prerequisite graph. These are buildable.**

**The philosophically interesting boundary** is between Tier 1 (genuine synthesis)
and Tier 2-3 (concept teaching). The system can honestly claim synthesis for
Counting On, Counting Back, Dealing by Ones, UCR, and CBO Multiplication. For
everything else, it should honestly claim "the oracle taught me this concept."
That honesty — about where synthesis stops and teaching begins — is the formal
analogue of the manuscript's claim about where formalization breaks.

---

## Relationship to "agents running automata"

The idea of spawning agents, each responsible for one strategy automaton, and
looking for where they meet, would reconstruct the prerequisite graph empirically.
Each agent could answer:
- "What primitives do I need that I don't have?"
- "What can I do that agent X can't?"
- "If agent X gave me their output, could I use it?"

The result would be a verified map of inter-strategy dependencies — essentially,
a machine-generated version of the N101 curriculum sequence. This is a discovery
tool for validating the prerequisite graph, not a mechanism for the Prolog learner
to use at runtime.

---

## What this document is not

This is not a plan to implement genuine synthesis. Whether to implement it —
or whether the gap between oracle-wrapping and genuine synthesis is itself the
point — is a decision that belongs to the manuscript's argument. This document
assesses what is technically feasible, not what should be built.
