# Phase 4: Test Independence Report

Each module's tests pass in isolation within the current architecture.
Full isolation — tests runnable without `paths.pl` — requires `multifile`
refactoring, marked as future work.

## Results

| Module | Tests | Pass | Notes |
|--------|-------|------|-------|
| formalization | 20 | 20 | Robinson Q harness — fully self-contained |
| strategies | 16 files (153+ subtests) | 16 | Needs `paths.pl` for aliases |
| learner | 10 files | 10 | |
| arche-trace (critique) | 7 | 7 | |
| arche-trace (dialectical engine) | 15 | 15 | |
| pml (core) | 28 | 28 | |
| pml (simple) | 10 | 10 | |

## Per-Module Detail

### formalization/

Run via `swipl -q -l paths.pl -g 'consult("formalization/robinson_q"), run_robinson_tests, halt.'`

Covers: grounded number construction; arithmetic grounding for plus, mult, succ; Robinson axioms Q1-Q7. The test harness lives inside `robinson_q.pl` itself and depends only on `grounded_arithmetic`.

### strategies/

Each test file consults its standards module and runs `run_tests/0`. Imports use the `standards(...)` and `formalization(grounded_arithmetic)` aliases from `paths.pl`. Within that constraint, tests execute independently.

Files:
- `test_oracle_wiring.pl`
- `test_standard_k_ns_1.pl`, `_ns_2.pl`, `_ns_3.pl`, `_ns_4.pl`, `_ns_5_6.pl`, `_ns_7.pl`
- `test_standard_k_ca_1_3.pl`
- `test_standard_1_ca_1.pl`, `_ca_3.pl`, `_ns_1.pl`, `_ns_2.pl`
- `test_standard_2_ca_2.pl`, `_ns.pl`
- `test_standard_3_ca.pl`, `_ns.pl`

### learner/

All ten test files pass:
`test_basic_functionality`, `test_complete_system`, `test_comprehensive`, `test_force_learn_all`, `test_full_curriculum`, `test_full_loop`, `test_native`, `test_oracle_integration`, `test_orr_cycle`, `test_three_wires`.

### arche-trace/

`critique_test.pl` exercises the critique mechanism: pathology detection, stress map, belief revision, and bad-infinite sublation diagnosis. `dialectical_engine_test.pl` exercises the ORR wrapper, perturbation handling, and the generic FSM executor.

### pml/

`core_test.pl` covers module loading, operator declarations, automata utilities, prover basics (identity, explosion, double negation elimination, resource tracking), PML dynamics (dialectical rhythm, Oobleck dynamic, modal context cost), and the trace mechanism (I-Feeling, S-O inversion, proof erasure). `simple_test.pl` covers the same surfaces with smaller assertions.

## Firebreaks

| Firebreak | Status |
|-----------|--------|
| formalization isolated | holds — `robinson_q` self-contained |
| strategies isolated | holds modulo `paths.pl` |
| learner isolated | holds modulo `paths.pl`; imports modal operators, `check_norms/1`, and `proves/1` from arche-trace per `interfaces/arche-trace-to-learner.md` |
| pml tests runnable | holds |
| arche-trace tests runnable | holds |

Cross-module imports are functional (`use_module`), not interfaced. The `include`-based axiom split means the scene-agnostic engine pulls in all axiom sets at load time. True test isolation would require redistributing axiom clauses through `multifile` declarations instead of file-level `include` directives.

## Architecture Context

The arche-trace directory holds two provers. `incompatibility_semantics.pl` is the scene-agnostic engine with `proves/1`. `embodied_prover.pl` is the resource-tracked prover with `proves/4`, a distinct module that coexists via selective import in `load.pl`. The scene-agnostic engine owns `incoherent/1` in the user namespace; the embodied prover's `incoherent/1` is accessible via module qualification. Axiom files in `pml/` extend `embodied_prover:material_inference/3` for PML dynamics while `pragmatic_axioms.pl` extends `incompatibility_semantics:is_incoherent/1` for sequent-level incoherence detection.
