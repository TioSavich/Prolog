# tests/ — Crisis Learning Tests (System B)

Tests for the crisis-driven learning pipeline: meta-interpreter, teacher intervention,
FSM synthesis, execution handler, and full ORR cycle integration.

## Test inventory

These tests were originally in the prolog/ root directory and moved here during
the March 2026 cleanup. Import paths use `../module_name` to reach the parent.

### Integration tests
| File | What it tests | Status |
|------|---------------|--------|
| `test_complete_system.pl` | Full ORR cycle end-to-end | PASS |
| `test_orr_cycle.pl` | Observe-Reorganize-Reflect cycle | PASS |
| `test_oracle_integration.pl` | Oracle consultation via primordial_start | PASS |
| `test_oracle_wiring.pl` | Oracle server query interface | PASS |

### Phase tests (historical, document refactoring progress)
| File | What it tests |
|------|---------------|
| `test_phase_1_1_complete.pl` | Phase 1.1 — basic oracle loading |
| `test_phase_2_crisis_detection.pl` | Phase 2 — crisis detection and response |
| `test_phase_3_full_curriculum.pl` | Phase 3 — full curriculum processing |
| `test_phase_3_simple.pl` | Phase 3 — simplified curriculum test |
| `test_phase5_synthesis.pl` | Phase 5 — FSM synthesis from oracle |
| `test_phase6_costs.pl` | Phase 6 — cognitive cost tracking |
| `test_phase7_hermeneutics.pl` | Phase 7 — hermeneutic calculator |

### Feature tests
| File | What it tests | Notes |
|------|---------------|-------|
| `test_force_learn_all.pl` | Force-learn all oracle strategies | RMB not_implemented |
| `test_basic_functionality.pl` | Grounded arithmetic basics | PASS |
| `test_comprehensive.pl` | Broad system coverage | PASS |
| `test_full_curriculum.pl` | Curriculum-driven learning | PASS |
| `test_full_loop.pl` | Single execution loop | PASS |
| `test_native.pl` | Native FSM execution | PASS |

## Running

```prolog
cd /path/to/prolog/tests
swipl -l test_complete_system.pl
```

## Known issues

- `test_force_learn_all.pl`: RMB strategy returns `not_implemented`.
