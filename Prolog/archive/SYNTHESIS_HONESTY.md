# What the Synthesis Engine Actually Does

## What this document is

An honest accounting of the gap between what the ORR cycle claims to do and what it
actually does, written March 2026 after tracing through `fsm_synthesis_engine.pl`,
`execution_handler.pl`, and `oracle_server.pl`. This is not a post-mortem or a plan.
It is a description of what is happening, for the purpose of deciding what to do next.

---

## The claim

The system claims crisis-driven accommodation: a primordial learner exhausts its
resources on Counting All, enters crisis, encounters teacher intervention, and *synthesizes* a new
strategy from what the teacher made available. The module header of `fsm_synthesis_engine.pl`
says:

> Unlike pattern-matching approaches, this engine constructs Finite State Machine (FSM)
> strategies by searching the space of possible primitive operation compositions.

And:

> ANTI-PATTERNS TO AVOID:
> - No hard-coded strategy templates (violates emergence)
> - No pattern matching on traces (innate knowledge)
> - No lookup tables (defeats bootstrapping)

These are stated commitments. Here is what actually happens.

---

## What actually happens

### The teacher-wrapping path (always used)

`execution_handler.pl` line 549 contains this comment:

```prolog
% ALWAYS use oracle-backed synthesis for consistency
% FSM synthesis has bugs with Peano conversion, and oracle-backed is simpler
```

The 5-argument `synthesize_strategy_from_oracle/5` is the only synthesis path that
runs. It calls `assert_oracle_backed_strategy/3` (line 134), which does this:

1. Builds a clause head: `Op(A, B, Result)` (e.g., `add(A, B, Result)`)
2. Builds a clause body that:
   - Converts A and B from Peano to integers
   - Calls `oracle_server:query_oracle(OpInt, StrategyName, IntResult, Interpretation)`
   - Converts the result back to Peano
3. Asserts this as both `object_level:Op(A,B,Result)` and
   `more_machine_learner:run_learned_strategy/5`

**The "learned strategy" is a procedure that calls the oracle again at runtime.**

The system has not learned to count on by bases and ones. It has memorized the
teacher's phone number. Every time it "uses" the learned strategy, it calls the oracle
with the same strategy name and gets the answer from the same lookup table. The
meta-interpreter's second pass succeeds not because the learner has new capabilities
but because the new clause bypasses the inference budget entirely — the oracle call is
not metered.

### The FSM synthesis path (never used)

The 4-argument `synthesize_strategy_from_oracle/4` (line 62) does attempt genuine
synthesis: it extracts hints from the oracle's interpretation, searches an FSM space
using `synthesize_fsm/5`, validates the result, and asserts a strategy built from
primitives (`successor/2`, `predecessor/2`, `decompose_base10/3`).

This path is never called. The comment says "FSM synthesis has bugs with Peano
conversion." Whether those bugs are fixable is an open question. But the effect is
that the system's only synthesis mechanism is oracle-wrapping.

### The reflection path (no-op)

`more_machine_learner.pl` defines:

```prolog
reflect_and_learn(_Result) :- true.
```

The "Reflect" phase of ORR does nothing. There is no reflection.

---

## The strategy ordering problem

`oracle_server.pl` lists addition strategies as:

```prolog
list_available_strategies(add, ['COBO', 'RMB', 'Chunking', 'Rounding']).
```

When the system has no strategy name from the oracle response, `execution_handler.pl`
line 564 takes the first one:

```prolog
oracle_server:list_available_strategies(Op, [FirstStrategy|_]),
```

This means **every addition crisis learns COBO**, regardless of the problem.

8+5 does not need COBO. COBO (Count On by Bases and Ones) decomposes the second
addend into tens and ones, then counts on by 10s then by 1s. For 8+5, there are no
tens to count by. The strategy that makes developmental sense for 8+5 is either
simple Counting On (count 9, 10, 11, 12, 13) or Rearranging to Make Bases
(move 2 from 5 to 8, get 10+3=13).

Children do not learn COBO before they learn to count on. COBO requires the concept
of a base — a unit larger than one. The insight that ten ones *are* a ten is the
dialectical synthesis of the one-and-the-many problem. A system that offers COBO as
the first strategy for single-digit addition has the developmental sequence backwards.

---

## Why this matters (beyond correctness)

The teacher-wrapping problem and the strategy ordering problem are symptoms of the same
underlying issue: **the system does not model developmental prerequisite structure.**

A genuine synthesis engine would need to know:

1. **What primitives the learner currently has.** A learner who only has `successor/2`
   can synthesize Counting On (iterate successor from the first addend). It cannot
   synthesize COBO because COBO requires `decompose_base10/3`, which presupposes the
   concept of a base.

2. **What problems demand which strategies.** 8+5 is solvable by Counting On (5 steps)
   or RMB (rearrange to make 10). It does not demand COBO. 38+55 demands COBO because
   Counting On would take 55 steps — a resource crisis that specifically calls for
   base-structured counting.

3. **What order concepts become available.** Counting → Counting On → Counting On by
   Groups → Counting On by Bases and Ones. Each step requires the previous one. This
   is the curriculum that CGI research documents and that the system currently lacks.

Without prerequisite structure, the system cannot distinguish between "this crisis
calls for a more efficient counting strategy" and "this crisis calls for a
qualitatively new concept." Both crises look like resource exhaustion. The difference
is in what the learner is ready for — and that requires a developmental model that the
current system does not have.

---

## The three things the system does honestly

Despite the synthesis gap, the system does do three things that are not fake:

1. **Counting All is genuinely expensive.** The Peano enumeration in `object_level.pl`
   really does exhaust inference budgets on problems that a more efficient strategy
   would handle easily. The crisis detection is real.

2. **The teacher's interface is architecturally correct.** The decision to
   return only result + interpretation (hiding the automaton's internal trace) models
   the opacity of what the teacher makes available. A learner who encounters "count on
   by bases then ones" does not receive the teacher's internal procedure — only the
   conditions and the demonstration. This is structurally right even if the system
   doesn't currently exploit it.

3. **The strategy automata are correct.** The files in `Prolog/math/` genuinely
   implement the CGI strategies as finite state machines with execution traces.
   `sar_add_cobo.pl` really does decompose the second addend into bases and ones and
   count on by 10s then 1s. The formal models are sound. The problem is that the
   synthesis engine never builds anything like them — it just calls them through the
   oracle.

---

## What genuine synthesis would require

For the synthesis engine to actually synthesize (rather than wrap), it would need to:

1. **Start from the learner's current primitives**, not from the teacher's strategy
   library. If the learner only has `successor/2`, the search space is: what FSMs
   can I build from iterated succession? Answer: Counting On. Not COBO.

2. **Use the teacher's result as a target, not a procedure.** The teacher makes
   available "13." The learner's job is to find a sequence of primitive operations
   that produces 13 from 8 and 5. The *interpretation* is a hint about the shape
   of the solution, not the solution itself.

3. **Fail when the primitives are insufficient.** If the learner cannot synthesize
   a strategy from its current primitives, that is a *different kind of crisis* — not
   resource exhaustion but conceptual insufficiency. This is where the system would
   need to acquire a new primitive (e.g., `decompose_base10/3`), which is itself a
   learning event that requires its own crisis cycle.

4. **Track developmental state.** The system needs to know what the learner has
   acquired so far — not just which strategies are asserted, but which primitives
   and concepts are available. This is the curriculum structure that Tio described
   as the "on-rails" demonstration.

The 4-argument synthesis path (`synthesize_strategy_from_oracle/4`) gestures toward
this but was never completed and is never called. The gap between that gesture and a
working implementation is the central open problem of this project.

---

## What this document is not

This is not a proposal to fix the synthesis engine. The question of whether the
synthesis engine *should* be fixed — or whether the oracle-wrapping is actually
the honest representation of what formalization can do — is a philosophical question
that belongs to the manuscript's argument, not to the code.

The manuscript's claim is that formalisms break productively. If the synthesis engine
genuinely synthesized strategies from primitives, the system would be a stronger
engineering artifact but a weaker philosophical exhibit. The fact that the system
*cannot* synthesize — that it resorts to memorizing the teacher's phone number — might
be the most honest thing about it. The question is whether that honesty is visible to
the person using the system, or whether it is hidden behind claims of "genuine
accommodation" and "computational hermeneutics."

Right now, it is hidden.

---

## Related concerns

- **SYSTEM_ASSESSMENT.md** (Q4) says "System B provides the functional bones: Crisis
  detection → strategy acquisition → developmental progression. This works." It works
  in the sense that the ORR cycle completes. It does not work in the sense that
  strategy acquisition is genuine synthesis.

- **`fsm_synthesis_engine.pl` module header** claims anti-patterns (no lookup tables,
  no hard-coded templates) that the oracle-wrapping path violates. The header describes
  the 4-argument path that is never called, not the 5-argument path that always runs.

- **The "on-rails" curriculum idea** (Tio, March 2026) would make these problems
  visible by running the system through a developmental sequence. If 8+5 learns COBO
  and 38+55 also learns COBO and the system never discovers Counting On as a distinct
  strategy, the curriculum test would expose the lack of developmental structure.
