# 02 — Projective Validity Tester

## Purpose

Implement the mechanism by which strategies connect to one another. A rule
learned in one strategy context (sentence frame Q) projects into another context
(sentence frame Q') if and only if the rules governing the subsentential elements
extend to the new context. This projection is testable and falsifiable at three
points.

## Philosophical requirements

1. **Projective inferences are not guaranteed to be good.** They inherit
   goodness from three constituent inferences (Savich, 2022, pp. 240-242;
   Brandom, 2000, pp. 124-129). All three must be endorsed for the projection
   to hold. If any fails, the projection fails — and that failure is
   informative.

2. **The sentence frame Q must be declared.** Each strategy automaton IS a
   sentence frame with adicities. `Q(α, β)`: given numbers α, compute using
   the procedure encoded in this FSM, producing output β. The Q frame is NOT
   a free variable — it is the specific algorithmic structure of the strategy.

3. **Substitution inferences must be endorsed independently.** The inference
   from a to a' (original description to new description) and b to b' (original
   output to projected output) must each be validated. These are the "rules
   governing subsentential elements" that must be extendable.

4. **The {I} is located in the projection, not the speech act.** The act of
   projecting — extending rules past their original material contexts — is
   where the subject is at stake. The projected speech act just follows finite
   rules. This is the "missing dimension" (Savich, 2022, p. 217). The
   formalization cannot capture the {I} but can model the projection mechanism
   that the {I} enables.

5. **Projection is normative, not objective.** "Projective validity is
   normative, it has to do with meaning and does not reference any reality
   external to a linguistic reality" (Savich, 2022, p. 218). The test is not
   whether Prolog's native `2` agrees (that's objective validity). The test is
   whether the rules, as the system understands them, extend coherently.
   Objective validation is a separate check.

## Protocol (adapted from Savich, 2022, pp. 242-243)

```
1. Declare Q(α, β) — the strategy as sentence frame
2. Determine a(α, β) — the original computation with known inputs/outputs
3. Generate b(α, β):
   a. Describe the original computation as an algorithm (pseudo-code / FSM)
   b. Choose new inputs based on the algorithm's structure
   c. Run the algorithm on new inputs
   d. Test inference Qa so Qb — is the algorithm's behavior consistent?
   e. Adjust until endorsed
4. Determine a'(α, β) and b'(α, β):
   a. Does a DIFFERENT strategy reproduce a's output? (substitution a → a')
   b. Does that strategy also produce b's output? (substitution b → b')
5. Test Qa' so Qb' — does the second strategy project from the first?
6. If endorsed: register the connection in both strategies' meaning fields
   If rejected: the rejection is informative — deposit as XOR
```

## Adaptation for this system

In the dissertation, projection goes from student work → machine code. Here,
projection goes from **strategy to strategy**. The analogues:

| Dissertation | Formalization |
|---|---|
| Student work sample | Strategy A's computation on inputs α |
| Machine code | Strategy B's computation on same inputs |
| New inputs | Different numbers where projection is tested |
| Q (sentence frame) | The operation type (add, subtract, etc.) |
| a → a' | "Strategy A and Strategy B agree on original inputs" |
| b → b' | "Strategy A and Strategy B agree on new inputs" |
| Qa so Qb | "Strategy A's behavior is internally consistent" |
| Qa' so Qb' | "Strategy B can stand in for Strategy A" |

## Data structure

```prolog
%% projection_test(+StrategyA, +StrategyB, +Operation, +OrigInputs,
%%                  +NewInputs, -Result)
%% Result is one of:
%%   valid(AgreesOriginal, AgreesNew, ConsistentA)
%%   invalid(FailPoint)  where FailPoint is one of:
%%     substitution_a  — strategies disagree on original inputs
%%     substitution_b  — strategies disagree on new inputs
%%     consistency     — Strategy A is internally inconsistent
%%   untestable(Reason) — e.g., Strategy B cannot handle these inputs

:- dynamic projection_record/6.
%% projection_record(StratA, StratB, Op, Inputs, Result, Timestamp)
%% Persistent record of all projection tests for later analysis.
```

## Operations

### test_projection/6
Run the full protocol. Both strategies must be executable on both input sets.
Record the result. If valid, call `meaning_field:endorse/2` to register the
connection. If invalid, call `meaning_field:reject/2` to register the
incompatibility.

### find_projectable/3
```prolog
%% find_projectable(+Strategy, +Operation, -CandidateStrategies)
%% Given a strategy and operation, find other strategies that MIGHT project.
%% "Might" means: they handle the same operation, they have endorsed
%% interpretations in common numerals' meaning fields, and no prior
%% projection test has failed for these inputs.
```

### projection_history/2
```prolog
%% projection_history(+Operation, -History)
%% Retrieve all projection tests for an operation. This is the system's
%% memory of which strategies connect and where they don't.
```

## Constraints for implementers

- The three-point falsifiability is non-negotiable. Every projection test MUST
  check all three inferences and record which (if any) failed.
- Do NOT shortcut by using objective validity (Prolog's native arithmetic) as
  a proxy for projective validity. The point is to test whether the system's
  own rules extend, not whether the answer is "right."
- Projection tests must be **recorded** — the history of successful and failed
  projections is itself a knowledge structure. It tells the system what
  connects and what doesn't.
- Failed projections are **informative**. They should deposit XOR connectives
  in meaning fields. Do not discard them.
- The computational cost of projection testing does not matter. Correctness
  and philosophical coherence take priority over efficiency.

## Open questions

- How does the system decide which projections to test? This is a curriculum
  question. The teacher could prompt: "Try using your counting-on strategy on
  this subtraction problem." Or the system could try projections when a crisis
  occurs and the teacher validates/rejects the attempt.
- Can projective validity be graded rather than binary? A projection might
  be "mostly good" — working for some inputs but not others. This connects to
  the non-monotonicity of material inference.
- How does projection relate to the PML's modal operators? A successful
  projection might move from `exp_poss` (it's possible this strategy works
  here) to `comp_nec` (it necessarily works) through repeated testing.
