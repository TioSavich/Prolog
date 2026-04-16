# Prolog/tests/ — PML Core Tests (System A)

Tests for the Polarized Modal Logic framework. These test the sequent calculus
prover, dialectical engine, and critique/accommodation mechanisms.

## Test files

| File | Tests | Status |
|------|-------|--------|
| `core_test.pl` | 28 tests — proves/4, modal operators, PML rhythm | 28/28 PASS |
| `dialectical_engine_test.pl` | 15 tests — run_computation/2, run_fsm/4 | 15/15 PASS |
| `critique_test.pl` | Perturbation handling, stress maps | PASS |
| `simple_test.pl` | Basic sequent proofs | PASS |

## Running

```prolog
swipl -l core_test.pl          % from this directory
swipl -l dialectical_engine_test.pl
```

## Other files

- `TEST_SUMMARY.md` — Overview of test results
- `CRITIQUE_IMPLEMENTATION.md` — Notes on critique module implementation
