# 05 — Counting Engine and Trace Production

## Purpose

The counting engine is the system's only primitive activity. Everything else —
partitions, place value, strategies — is a pattern noticed in counting traces
through reflection. The engine must produce traces that are rich enough to
support reflection but grounded enough that no arithmetic is smuggled in.

## Philosophical requirements

1. **Counting is the primordial practice.** Before naming, before arithmetic,
   there is the act of marking: `|, ||, |||, ...`. The counting engine
   produces tally-sequences through successor operations. This is the
   subjective (s/1) layer — embodied activity that produces traces.

2. **Traces carry history.** A counting trace is not just the final number but
   the sequence of states the engine passed through. Counting from zero to
   five produces the trace `[z, s(z), s(s(z)), s(s(s(z))), s(s(s(s(z)))),
   s(s(s(s(s(z)))))]`. This history is what reflection operates on.

3. **Counting is directional.** Counting forward (successor) and counting
   backward (predecessor) are distinct activities that produce distinct traces.
   The system should not assume they are inverses until it discovers this
   through experience and the teacher endorses it.

4. **Place value emerges from counting, not from decomposition.** The existing
   `decompose_base10/3` treats place value as a utility function. In the
   redesign, place value should emerge from the counting engine's behavior at
   tens boundaries — the experience of 9→10, 19→20, etc. The counting DPDA
   in `counting2.pl` already handles carrying; the trace of that carrying is
   the raw material for discovering place value.

5. **Cost is embodied.** Each counting step has a proprioceptive cost
   (compressive, in the PML's terms). Counting from 0 to 100 is expensive.
   Strategies exist to abbreviate counting. The cost differential between
   strategies is what drives the system to seek more efficient approaches —
   this is the economic pressure that connects to crisis-driven learning.

## Existing code

- `counting2.pl`: DPDA that counts 0→N with automatic place-value carrying.
  Uses Prolog integers internally — needs grounding audit.
- `counting_on_back.pl`: Extended DPDA with tick (forward) and tock (backward)
  events, borrowing. Also uses Prolog integers internally.
- `grounded_arithmetic.pl`: Successor/predecessor on recollection structures.
  Already grounded. Core primitives are sound.
- `sar_add_counting_on.pl`: FSM wrapper for counting-on. Uses successor
  operations.

## What needs to change

### Trace format
Current counting produces final results. It needs to produce **full traces**:

```prolog
%% counting_trace(+From, +To, -Trace)
%% Trace is a list of states visited during counting.
%% Each state includes the tally-sequence and the transition type.
%%
%% Example: counting_trace(s(s(s(0))), s(s(s(s(s(0))))), Trace)
%% Trace = [
%%   state(s(s(s(0))), start),
%%   state(s(s(s(s(0)))), successor),
%%   state(s(s(s(s(s(0))))), successor)
%% ]
```

### Bidirectional traces
```prolog
%% count_forward(+From, +Steps, -To, -Trace)
%% count_backward(+From, +Steps, -To, -Trace)
%% Both produce traces. The system should discover that
%% count_forward(three, two, X, _) and count_backward(five, two, Y, _)
%% reach the same endpoints — but this is a DISCOVERY, not a given.
```

### Trace storage
```prolog
:- dynamic stored_trace/4.
%% stored_trace(From, To, Direction, Trace)
%% Persist all counting traces for later reflection.
%% The reflection mechanism operates on this database.
```

### Carrying traces (place value raw material)
When the counting engine crosses a tens boundary, the trace should record this
specially:

```prolog
%% state(s(s(s(s(s(s(s(s(s(0))))))))), successor),  % 9
%% state(ten_boundary, carry),                        % carrying event
%% state(s(s(s(s(s(s(s(s(s(s(0)))))))))), successor) % 10
```

The `carry` event is the raw material from which place value understanding
can be extracted through reflection. The system doesn't know what it means
yet — that's the reflection mechanism's job.

## Constraints for implementers

- All counting must go through grounded successor/predecessor operations.
  No Prolog `is/2` or `succ/2` built-in.
- Every counting act must produce and store a trace. No traceless computation.
- Traces must be immutable once stored. They are the subjective history — the
  system can reflect on them but not alter them.
- The counting engine should be agnostic about number-words. It operates on
  tally-sequences. The number-word layer translates between tally-sequences
  and names.
- Cost tracking per counting step should integrate with the PML's embodied
  cost model (compressive = cost 2, expansive = cost 1).

## Open questions

- How long should traces be retained? Indefinitely? Or should older traces
  be "compressed" into summary facts (losing detail but retaining structure)?
  The latter is more realistic developmentally.
- Should the counting engine have a maximum range, or should it be
  unbounded? Pragmatically, counting to 100 is sufficient for elementary
  arithmetic. Philosophically, the boundedness is interesting — the system
  has a finite horizon.
- The existing DPDAs (`counting2.pl`, `counting_on_back.pl`) use Prolog
  integers for state representation. Can they be refactored to use
  tally-sequences throughout without unacceptable performance degradation?
  Performance is not a priority but complete infeasibility would be a problem.
