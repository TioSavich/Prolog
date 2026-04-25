# Prolog/ — PML Core (System A)

Polarized Modal Logic framework implementing Brandom's incompatibility semantics
as a sequent calculus prover with modal cost tracking.

## Epistemological commitment

This system enacts **normative inferentialism** (Brandom): meaning is constituted by
incompatibility relations and entailment patterns, not by reference to objects. The
three validity modes — Subjective (s/1), Normative (n/1), Objective (o/1) — are
philosophically load-bearing, not just type tags.

## Modules (load order defined in load.pl)

| File | Purpose |
|------|---------|
| `load.pl` | Entry point — loads all core modules in dependency order |
| `utils.pl` | Shared utilities |
| `pml_operators.pl` | Operator declarations for modal operators (comp_nec, exp_poss, etc.) |
| `incompatibility_semantics.pl` | Core sequent calculus prover (`proves/4`) |
| `semantic_axioms.pl` | Axioms grounding the incompatibility relation |
| `automata.pl` | Counting automaton — transition from tallies to numerals |
| `pragmatic_axioms.pl` | Pragmatic inference rules |
| `intersubjective_praxis.pl` | Multi-agent normative reasoning |
| `critique.pl` | Perturbation handling, stress map, accommodation |
| `dialectical_engine.pl` | ORR cycle entry point (`run_computation/2`, `run_fsm/4`) |

## Key predicates

- `proves(Gamma => Delta, Limit)` — prove sequent within resource limit
- `run_computation(Sequent, Limit)` — run ORR cycle with perturbation handling
- `run_fsm(Module, InitState, History, FinalHistory)` — generic FSM executor

## Loading

```prolog
swipl -l load.pl
```

## Relationship to System B

System B (crisis learning pipeline in parent directory) imports from this system
via `Prolog/load.pl`. The meta-interpreter in System B can invoke `proves/4` from
this system, but the two use different proof strategies and different number
representations. Whether they should merge, layer, or stay separate is an open
question (see TODOS.md P2-1).
