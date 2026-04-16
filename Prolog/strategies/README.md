# strategies/

Children's arithmetic as finite state machines.

These model children's invented arithmetic strategies as executable choreography
for embodied cognition. Each strategy is a script for the temporal unfolding of
thought. The fractal architecture has two layers: an Iterative Core (primitive
counting) and a Strategic Shell (composed operations like chunking, decomposition,
rounding).

## Contents

- `math/` — 27 strategy automata (CGI attribution: Carpenter/Fennema via Hackenberg)
  - Addition (5): counting_on, cobo, chunking, rmb, rounding
  - Subtraction (9): counting_back, cbbo_take_away, cobo_missing_addend, decomposition,
    sliding, chunking (a/b/c), rounding
  - Multiplication (4): c2c, cbo, commutative_reasoning, dr
  - Division (4): cbo, dealing_by_ones, idp, ucr
  - Fractions (3): jason (partitive fractional scheme), jason_fsm, fraction_semantics
  - Base counting DPDAs (2): counting2, counting_on_back
- `standards/` — Indiana K-3 standards mapped to Prolog modules (20 modules, 153 tests)
- `fsm_engine.pl` — shared FSM execution engine with base-10 support
- `hermeneutic_calculator.pl` — strategy dispatcher (routes operations to automata)
- `strategies.pl` — strategy registry
- `composition_engine.pl` — copy-finding for fraction operations
- `normalization.pl` — numeral normalization
- `math_benchmark.pl` — arithmetic benchmarks

## Grounding status

All 27 automata are grounded: zero `is/2` evaluations. Every arithmetic
operation routes through recollection-based counting via
`formalization/grounded_arithmetic` and `formalization/grounded_utils`. The
`mod`/`//` operators were replaced with `base_decompose_grounded/4`, the
counting PDA's stack decomposition. See `GROUNDING_STATUS.md` for per-file
conversion details.

## Cross-module dependencies

All automata import `formalization/grounded_arithmetic` for arithmetic operations
and `arche-trace/incompatibility_semantics` for PML modal operators
(`s/1, comp_nec/1, exp_poss/1`).
