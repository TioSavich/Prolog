# learner/

This models a monological learner who acquires arithmetic strategies through crisis.
It is a constructivist module — one possible frame for studying learning. The
project's core commitments are intersubjectivity-first; this module exists to be
tested and extended by researchers who find the crisis model useful.

## The crisis cycle

A computational agent starts with tally marks and nothing else. When it hits a
problem it cannot solve within resource limits, it enters crisis — and must consult
a teacher (an external source of strategies). The teacher creates conditions for
crisis, not delivers answers.

1. The **meta-interpreter** (`meta_interpreter.pl`, `solve/6`) attempts computation
   using known strategies.
2. When computation fails, the **execution handler** (`execution_handler.pl`)
   classifies the crisis and initiates recovery.
3. The **oracle/teacher server** (`oracle_server.pl`) provides strategies the learner
   hasn't discovered yet. Oracle-backed strategies are philosophically hollow: the
   learner memorized a phone number, not a method.
4. **Tension dynamics** (`tension_dynamics.pl`) tracks continuous tension for
   catastrophe geometry — the snap/crisis moment.

## Contents

- `meta_interpreter.pl` — the ORR cycle: Observe, React, Reorganize
- `execution_handler.pl` — crisis classification and recovery
- `oracle_server.pl` — teacher/oracle strategy provider
- `server.pl` — HTTP server with dark-themed timeline visualization (port 8080)
- `config.pl` — system-wide settings and inference limits
- `tension_dynamics.pl` — continuous tension accumulator
- `crisis_processor.pl` — crisis processing pipeline
- `event_log.pl` — event emission for visualization
- `more_machine_learner.pl` — strategy hierarchy and foundational solver
- `knowledge_manager.pl` — strategy knowledge persistence
- `object_level.pl` — dynamic knowledge base
- `reflective_monitor.pl` — reflective analysis
- `reorganization_engine.pl` — system reorganization after crisis
- `fsm_synthesis_engine.pl` — utility shim (synthesis engine archived in Phase 1)

## Running

From the repo root (with `paths.pl` loaded):
```
swipl -l paths.pl -l learner/server.pl
```
Then visit http://localhost:8080

## Cross-module dependencies

Imports `formalization/grounded_arithmetic`, `arche-trace/incompatibility_semantics`,
and all strategy automata from `strategies/math/` (via oracle_server.pl and
hermeneutic_calculator).
