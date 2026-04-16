# Strategies to Formalization: Arithmetic Choreography Raises the Incompleteness Question

## The Claim

The 27 strategy automata collectively generate arithmetic machinery — successor,
addition, multiplication over natural numbers — rich enough to instantiate Robinson
Arithmetic (Q). Robinson Q is the weakest system to which Godel's first
incompleteness theorem applies. The strategies do not prove incompleteness. They
generate the conditions under which incompleteness becomes a question that can be
asked.

The claim is about sufficiency, not proof: children's invented arithmetic, taken
seriously as formal choreography, arrives at a system where self-reference is
possible. That arrival is the meta-mathematical signal the formalization module
tracks.

## What Exists in Code

### Robinson Q Axiomatization

`formalization/robinson_q.pl` contains a self-contained sequent calculus prover
with the seven Robinson axioms (Q1-Q7), verified by 20 passing tests.

The axioms, as `proves_impl` rules in `formalization/axioms_robinson.pl`:

| Axiom | Statement | Encoding |
|-------|-----------|----------|
| Q1 | S(x) != 0 | `is_incoherent` — successor of anything is incompatible with zero |
| Q2 | S(x) = S(y) -> x = y | Sequent conditional: from premise S(x)=S(y), conclude x=y |
| Q3 | x = 0 or x = S(y) | Produces a structural witness, not an opaque proposition |
| Q4 | x + 0 = x | Additive identity via `arith_op` |
| Q5 | x + S(y) = S(x + y) | Addition recursion via `arith_op` |
| Q6 | x * 0 = 0 | Multiplicative zero via `arith_op` |
| Q7 | x * S(y) = (x * y) + x | Multiplication recursion via `arith_op` |

The `arith_op/4` predicate computes arithmetic results using Prolog's built-in
`is/2`. But the `proves_impl` rules that wrap `arith_op/4` require
`is_recollection/2` to succeed for all operands and results — ensuring every
number in a Robinson Q derivation has a constructive counting history:

```prolog
proves_impl(_ => [o(plus(A,B,C))], _) :-
    once(is_recollection(A, _)),
    once(is_recollection(B, _)),
    arith_op(A, B, +, C),
    once(is_recollection(C, _)).
```

`is_recollection/2` itself calls `hermeneutic_calculator:calculate/6` — the
strategy dispatcher — to construct each number's history:

```prolog
is_recollection(0, [axiom(zero)]).
is_recollection(N, History) :-
    integer(N), N > 0, Prev is N - 1,
    is_recollection(Prev, _),
    hermeneutic_calculator:calculate(Prev, +, 1, _Strategy, N, History).
```

This means Robinson Q proofs literally invoke the strategy automata. When
the sequent engine proves `plus(8, 6, 14)`, it calls the hermeneutic
calculator to construct the counting histories for 8, 6, and 14, using
whichever strategy the calculator dispatches. The strategies are not just
analogous to Robinson Q — they are its computational substrate.

The `once/1` wrapper on `is_recollection` calls prevents FSM backtracking loops.

### Grounded Arithmetic Primitives

`formalization/grounded_arithmetic.pl` provides the operations that both Robinson Q
and the strategy automata use:

| Predicate | Signature | What It Does |
|-----------|-----------|--------------|
| `zero/1` | `zero(recollection([]))` | The empty counting history |
| `successor/2` | `(+Rec, -Next)` | Appends one tally |
| `predecessor/2` | `(+Rec, -Prev)` | Removes one tally (fails on zero) |
| `add_grounded/3` | `(+A, +B, -Sum)` | Concatenates counting histories |
| `subtract_grounded/3` | `(+Min, +Sub, -Diff)` | Removes suffix |
| `multiply_grounded/3` | `(+A, +B, -Product)` | Replicates history B times |
| `divide_grounded/4` | `(+Div, +Dsr, -Quot, -Rem)` | Repeated subtraction |

`formalization/grounded_utils.pl` adds place-value decomposition:

| Predicate | Signature | What It Does |
|-----------|-----------|--------------|
| `base_decompose_grounded/4` | `(+Num, +Base, -Quot, -Rem)` | Counting-based division (no `mod` or `//`) |
| `base_recompose_grounded/4` | `(+Quot, +Base, +Rem, -Num)` | Inverse operation |

### The 27 Strategy Automata

All live in `strategies/math/`. All 27 are grounded — zero `is/2` evaluations
remain (`strategies/GROUNDING_STATUS.md`).

Each automaton exports a `run_*/4` predicate:
```prolog
run_counting_on(+A, +B, -Result, -History)
run_cobo(+A, +B, -Result, -History)
run_chunking(+A, +B, -Result, -History)
...
```

The FSM interface (via `strategies/fsm_engine.pl`):
```prolog
transition(+CurrentState, -NextState, -Interpretation)
accept_state(+State)
setup_strategy(+A, +B, -InitialState, -Parameters)
extract_result_from_history(+History, -Result)
```

History is a list of `step(StateName, Data, Interpretation)` terms — the full
execution trace of the strategy.

### Standards Mapping

20 Indiana K-3 standards modules in `strategies/standards/`, with 153 passing
tests. Each standard module maps curriculum expectations to strategy predicates:

```prolog
% standard_1_ca_1.pl
add_counting_on(+A, +B, -Sum)
add_making_ten(+A, +B, -Sum)
sub_decompose_to_ten(+A, +B, -Diff)
```

Tests verify strategy agreement: multiple strategies applied to the same inputs
produce the same result.

### The Hermeneutic Calculator

`strategies/hermeneutic_calculator.pl` dispatches strategy by name:

```prolog
calculate(+Num1, +Op, +Num2, +Strategy, -Result, -History)
list_strategies(+Op, -StrategyNames)
```

This is the user-facing interface: given a problem and a strategy name, execute
the strategy and return both the result and the execution trace.

## Where the Claim Holds

### Shared Primitives

The strategy automata and Robinson Q use the same grounded arithmetic. When
`sar_add_chunking` computes 28 + 35, it calls `add_grounded/3` and
`base_decompose_grounded/4` — the same operations that `axioms_robinson.pl`
axiomatizes through `arith_op/4` and `is_recollection/2`.

The grounding route is:

```
integer (user input)
  -> integer_to_recollection  (conversion)
  -> grounded operations       (zero, successor, add_grounded, etc.)
  -> recollection_to_integer   (conversion back)
integer (result)
```

Both Robinson Q proofs and strategy executions traverse this route. The
difference is that Robinson Q establishes the axiomatic validity of the
operations, while the strategies choreograph their temporal deployment.

### Robinson Q Is Instantiated

The 20 Robinson Q tests verify that the axioms hold for specific instances:

- Zero is recollected: `is_recollection(0, [axiom(zero)])`
- Successor is recollected: `is_recollection(5, [succ(4), succ(3), ...])` 
- Addition: `proves([...] => [o(plus(3, 2, 5))])`
- Multiplication: `proves([...] => [o(mult(3, 4, 12))])`
- Q1-Q7 as sequent derivations

Each of these derivations routes through `arith_op/4`, which calls
`is_recollection/2`, which builds the counting history — the same counting
history that the strategy automata produce.

### Strategies Cover the Robinson Operations

The 27 automata collectively implement:
- **Successor**: every counting strategy applies successor (primitive)
- **Addition**: 5 strategies (counting_on, COBO, chunking, RMB, rounding)
- **Subtraction**: 9 strategies (counting_back, CBBO take-away, COBO missing addend,
  decomposition, sliding, chunking A/B/C, rounding)
- **Multiplication**: 4 strategies (C2C, CBO, commutative reasoning, DR)
- **Division**: 4 strategies (CBO division, dealing by ones, IDP, UCR)
- **Fractions**: 3 (jason, jason_fsm, fraction_semantics)
- **Base counting DPDAs**: 2 (counting2, counting_on_back)

Robinson Q requires successor, addition, and multiplication. The strategies
provide all three, instantiated at every pair of natural number inputs the
automata can handle.

## Where the Claim Exceeds What's Formalized

### No Explicit Encoding Map

There is no function in the codebase that takes a strategy execution trace and
produces a Robinson Q derivation, or vice versa. The shared primitives mean the
operations are the same, but the formal relationship between "the strategy
computed 8 + 6 = 14" and "Robinson Q proves plus(8, 6, 14)" is not made
explicit.

A worked example would look like:

1. Execute `run_chunking(8, 6, 14, History)` — produces a trace of FSM
   transitions through decomposition, base-chunk addition, ones-chunk
   addition, and recombination.
2. The trace invokes `add_grounded/3` three times (base chunk, ones chunk,
   recombination) and `base_decompose_grounded/4` once.
3. Each `add_grounded/3` call is an instance of Q4/Q5 (addition axioms).
4. The `base_decompose_grounded/4` call is an instance of Q6/Q7
   (multiplication axioms, since decomposition uses repeated subtraction
   which is the inverse of multiplication).
5. The composition of these instances constitutes a Robinson Q derivation
   of `plus(8, 6, 14)`.

This example has not been constructed. Steps 3 and 4 require an explicit
mapping from grounded operation calls to axiomatic derivation steps.

### Godel Numbering Not Wired

The prime utilities exist (`arche-trace/automata.pl`: `is_prime/1`,
`nth_prime/2`) but are not connected to syntax encoding. Godel numbering
would assign each formula in the system a unique natural number, enabling
self-reference. Without it, the system cannot formulate sentences about its
own theorems.

### No Godel Sentence

A Godel sentence for this system — a formula that, interpreted, says "I am not
provable in the hermeneutic calculus" — has not been constructed. The Robinson
axioms establish that such a sentence *could* be constructed (given Godel
numbering), but the construction is absent.

### The Meta-Theorem Is Unproved

"Every theorem of Robinson Q is a theorem of the hermeneutic calculus." This
is plausible — the HC's grounded arithmetic implements Robinson's operations
— but the formal proof would require showing that every derivation step in
Robinson Q has a corresponding derivation step in the HC's sequent engine.
The proof has not been attempted.

### Schematic vs. Universal

The Robinson Q tests verify specific instances: `plus(3, 2, 5)`,
`mult(3, 4, 12)`, etc. Robinson Q's axioms are universally quantified
(for ALL x, y: ...). The test harness demonstrates schematic instances, not
universal validity. The `robinson_q.pl` test output's "triumphant framing"
overstates schematic instances as though they establish the universal claim.

## What Would Strengthen This Interface

1. **Construct the worked example.** Take one strategy execution (chunking,
   8 + 6) and manually trace the correspondence between each grounded
   operation call and its Robinson Q axiom instance. Document this as a
   concrete appendix.

2. **Build the encoding map.** Write a predicate
   `strategy_trace_to_derivation(+History, -Derivation)` that takes an FSM
   execution trace and produces the corresponding sequence of Robinson Q
   derivation steps. Even a partial implementation for addition strategies
   would establish the pattern.

3. **Wire Godel numbering.** Connect `automata.pl`'s prime utilities to a
   syntax encoding scheme. The encoding need not be efficient — clarity
   matters more than performance. A `godel_number/2` predicate mapping
   formulas to natural numbers would make self-reference possible.

4. **Prove the meta-theorem for addition.** Restrict to Q4 and Q5 (addition
   axioms). Show that for any A, B: if `add_grounded(A, B, C)` succeeds,
   then `proves([...] => [o(plus(A_int, B_int, C_int))])` succeeds. This
   is the minimal case of the meta-theorem.

5. **Honest framing in test output.** Replace the triumphant test output
   in `robinson_q.pl` with language that distinguishes schematic
   verification from universal proof.
