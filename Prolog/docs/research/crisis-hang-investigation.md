# Crisis Hang Investigation

Date: 2026-04-04

## Symptom

`add(38, 55)` with `limit: 20` hangs indefinitely when submitted via
POST /api/compute. Small computations like `add(7, 5)` with `limit: 100`
succeed instantly. The server becomes unresponsive on the hanging request.

## Root Cause: Three Interlocking Bugs

The hang results from three bugs that combine into an infinite loop.

### Bug 1 (Primary): `max_retries` is defined but never checked

`config.pl` (line 48) defines `max_retries(5)`. This predicate is exported
from the module and documented as preventing infinite loops. But
`execution_handler.pl` **never references it**. The retry path at
line 206 is:

```prolog
run_computation(Goal, Limit)
```

This is an unconditional recursive call to `run_computation/2`. There is no
retry counter, no decrement, no guard. The system retries forever as long as
each cycle succeeds in synthesizing a strategy.

### Bug 2 (Enabler): Oracle-backed strategies re-trigger crisis at the same limit

When `run_computation(Goal, 20)` hits crisis on `add(38, 55)`:

1. Crisis is caught, oracle is consulted
2. Oracle recommends e.g. `'Counting On'` strategy
3. `synthesize_strategy_from_oracle/5` asserts an oracle-backed strategy via
   `assert_oracle_backed_strategy/3` (fsm_synthesis_engine.pl line 130)
4. This strategy is asserted as both `object_level:add/3` and
   `more_machine_learner:run_learned_strategy/5`
5. `validate_synthesis/3` is called with the **same limit (20)**
6. The retry calls `run_computation(Goal, 20)` -- same limit

The oracle-backed strategy's body (fsm_synthesis_engine.pl lines 135-159)
calls `oracle_server:query_oracle/4` which returns the correct integer
result. But the **meta-interpreter still needs to process this strategy**
through `solve/6`, which deducts inference budget.

For `add(38, 55)`:

- The meta-interpreter's learned-strategy clause (meta_interpreter.pl
  line 180) calls `run_learned_strategy/5`
- Inside the strategy body, `peano_to_int(A, IntA)` must traverse a
  Peano term of depth 38. `peano_to_int(B, IntB)` traverses depth 55.
  These are recursive predicates called via `fsm_synthesis_engine:peano_to_int/2`.
- But the meta-interpreter also deducts `strategy_runtime_cost` for the
  learned strategy. For `'Counting On'`: `Cost is min(38, 55) + 1 = 39`.
  That exceeds limit 20.
- For `'COBO'`: `Cost is min(38, 55) + 1 = 39`. Also exceeds 20.
- For `'RMB'`: cost depends on distance to 10, but still significant.
- For `'Chunking'`: `Cost is (38 // 10) + (55 // 10) + 3 = 3 + 5 + 3 = 11`.
  This might fit, but only if the meta-interpreter doesn't also deduct
  the base clause-resolution cost.

The critical issue: even after learning a strategy, the system retries with
the **same** limit that was too small for the primordial approach. If the
learned strategy's runtime cost also exceeds that limit, crisis fires again.

The next cycle consults the oracle for a **different** strategy (via
`find_novel_strategy/2` which skips already-learned strategies). The system
learns that strategy, it also exceeds the limit, crisis again. This
continues through all 5 addition strategies: `'Counting On'`, `'RMB'`,
`'COBO'`, `'Chunking'`, `'Rounding'`.

### Bug 3 (Terminal Loop): What happens after all strategies are exhausted

After all 5 strategies are learned, `consult_oracle_for_solution/4` at
execution_handler.pl line 489 does:

```prolog
(   extract_problem_size(object_level:add(A, B, _), SizeA, SizeB)
->  oracle_server:strategy_appropriate_for(add, SizeA+SizeB, StrategyName),
    \+ clause(more_machine_learner:run_learned_strategy(_,_,_,StrategyName,_), _)
;   find_novel_strategy(Strategies, StrategyName)
),
```

The `strategy_appropriate_for/3` call at oracle_server.pl line 394 uses
`findall` + `sort` to pick the cheapest strategy. It finds one (say
`'Rounding'`). But the `\+` guard checks if it's already learned. It is.
So the clause fails. Prolog backtracks.

But there is no alternative clause to backtrack into that handles the
"all strategies already learned" case for the size-sensitive path. The
`strategy_appropriate_for/3` predicate at oracle_server.pl line 403 is a
fallback that returns the first strategy, which is also already learned.

So `consult_oracle_for_solution/4` falls through to:

```prolog
;   emit(oracle_exhausted, _{operation: add, reason: all_strategies_learned, ...}),
    fail
```

This makes `consult_oracle_for_solution` fail. Back in
`handle_perturbation` (execution_handler.pl line 167), the `(... -> ... ; ...)`
branches to the fallback at line 221:

```prolog
;   emit(oracle_exhausted, _{goal: Goal}),
    ...
    reflect_and_learn(Result),
    ...
    run_computation(Goal, Limit)
```

`reflect_and_learn/1` is a **no-op** (more_machine_learner.pl line 138:
`reflect_and_learn(_Result) :- true.`). So the fallback "learns nothing"
and then calls `run_computation(Goal, Limit)` again. Same goal, same limit,
same learned strategies, same failure. **This is the infinite loop.**

## The Loop Sequence

```
run_computation(add(38,55), 20)
  -> solve/4 hits crisis (resource_exhaustion or tension_instability)
  -> handle_perturbation catches it
  -> consult_oracle_for_solution finds 'Counting On' (novel)
  -> synthesize + validate + relax_tension
  -> run_computation(add(38,55), 20)          -- retry #1
    -> solve/4 tries 'Counting On', cost 39 > 20, crisis
    -> consult_oracle_for_solution finds 'RMB' (novel)
    -> synthesize + validate + relax_tension
    -> run_computation(add(38,55), 20)        -- retry #2
      ... (learns 'COBO', 'Chunking', 'Rounding')
      -> run_computation(add(38,55), 20)      -- retry #6
        -> solve/4 tries strategies, all exceed budget, crisis
        -> consult_oracle_for_solution: all learned, fails
        -> fallback: reflect_and_learn (no-op)
        -> run_computation(add(38,55), 20)    -- retry #7 (INFINITE LOOP)
```

After retry #6, the system enters a tight infinite loop: crisis -> oracle
exhausted -> no-op learning -> retry -> crisis -> oracle exhausted -> ...

The loop is made tighter by the fact that tension accumulates across retries
(hysteresis). After several cycles, `check_crisis` may fire even before
`check_viability`, meaning the system barely starts before triggering
crisis again.

## Additional Factor: Tension Accumulation Accelerates the Loop

The tension dynamics module has hysteresis: `relax_tension` only drops
tension to 40% (first crisis), 52% (second), 61% (third), etc. After 5+
crises, the system enters each retry with very high baseline tension.
Once the tension history has 9+ entries, `check_crisis` fires on the
first inference step if stability is negative. This makes each loop
iteration faster (fewer inference steps before crisis), which means
more rapid looping.

## The Fix

Three changes are needed, in order of priority:

### Fix 1: Enforce max_retries (stops the infinite loop)

Add a retry counter to `run_computation/2`. When retries are exhausted,
fail gracefully instead of recursing.

The cleanest approach: add a `run_computation/3` with a retry counter, and
have the public `run_computation/2` call it with `max_retries` from config:

```prolog
run_computation(Goal, Limit) :-
    config:max_retries(MaxRetries),
    run_computation(Goal, Limit, MaxRetries).

run_computation(_Goal, _Limit, 0) :-
    emit(computation_failed, _{reason: max_retries_exhausted}),
    writeln('Maximum retries exhausted. Crisis unresolvable.'),
    fail.

run_computation(Goal, Limit, RetriesLeft) :-
    RetriesLeft > 0,
    tension_dynamics:get_tension_state(TensionBefore),
    emit(computation_start, _{goal: Goal, limit: Limit,
                               tension: TensionBefore,
                               retries_remaining: RetriesLeft}),
    catch(
        call_meta_interpreter(Goal, Limit, Trace),
        Error,
        (   RetriesNext is RetriesLeft - 1,
            handle_perturbation(Error, Goal, Trace, Limit, RetriesNext)
        )
    ).
```

Then update all `run_computation(Goal, Limit)` calls inside
`handle_perturbation` to `run_computation(Goal, Limit, RetriesLeft)`,
threading the counter through.

### Fix 2: Increase limit on retry after learning a strategy

When a new strategy is learned, the retry should use a higher limit that
accounts for the strategy's actual cost. The simplest version:

```prolog
NewLimit is max(Limit, StrategyCost * 2),
run_computation(Goal, NewLimit, RetriesLeft)
```

Or escalate the limit by a fixed factor on each retry:

```prolog
EscalatedLimit is Limit * 2,
run_computation(Goal, EscalatedLimit, RetriesLeft)
```

This is philosophically significant: a learner who has acquired a new tool
should have more patience (higher resource budget) to try it. The original
limit represented the patience of the primordial "Counting All" attempt.
A learner who now knows COBO deserves a budget proportional to COBO's cost,
not to counting-all's.

### Fix 3: Make the oracle-exhausted fallback fail cleanly

When all oracle strategies are learned AND `reflect_and_learn` is a no-op,
the fallback at execution_handler.pl line 221 should not retry. It should
emit an event and fail:

```prolog
;   emit(oracle_exhausted, _{goal: Goal}),
    writeln('  All strategies exhausted. No further learning possible.'),
    emit(computation_failed, _{goal: Goal, reason: all_strategies_exhausted}),
    fail
```

This makes the "all strategies learned but still can't solve" case a
terminal failure rather than an infinite retry.

## Files to Change

| File | Change |
|------|--------|
| `prolog/execution_handler.pl` | Add retry counter to `run_computation`, thread through `handle_perturbation`, fix oracle-exhausted fallback |
| `prolog/config.pl` | No change needed (max_retries already defined) |
| `prolog/meta_interpreter.pl` | No change needed for the hang fix |
| `prolog/server.pl` | No change needed (timeout would help as defense-in-depth but is not the root cause) |

## Impact on Bridge / Fractal Page Behavior

The bridge page (`/bridge`) and fractal page (`/fractal`) both call
POST /api/compute and display the resulting event timeline. The hang means:

1. **Bridge page freezes** waiting for the HTTP response. The fetch call
   in the frontend has no timeout, so the browser connection eventually
   times out (typically 300 seconds). During this time the page appears
   unresponsive.

2. **The Prolog server's single-threaded HTTP handler is blocked.** The
   `config.pl` comment notes "will race under concurrent requests." In
   practice, a hanging computation blocks the entire server because
   SWI-Prolog's `with_output_to/2` wrapper (server.pl line 75) runs
   synchronously. No other API calls can be served.

3. **Tension history grows without bound.** Each loop iteration calls
   `accumulate_tension/2` which appends to `tension_full_log`. During an
   infinite loop, this list grows until memory is exhausted, which would
   eventually crash the server.

4. **Event log grows without bound.** Each iteration emits events
   (`crisis_detected`, `oracle_consulted`, `synthesis_attempted`, etc.).
   The event log held in memory via `event_log.pl` also grows indefinitely.

After the fix, the bridge and fractal pages would receive a proper
failure response (with the event timeline of the crisis cycles that
were attempted) within a bounded number of retries.

## Philosophical Note

The hang is a micro-instance of the manuscript's thesis: a formal system
that lacks a way to recognize its own limitations loops forever. The
`max_retries` counter is the system's admission that sometimes crisis is
not resolvable by the current means. The fallback from infinite retry to
bounded failure is the formal analog of what the manuscript calls the
skeleton boundary -- where the formalization honestly stops.
