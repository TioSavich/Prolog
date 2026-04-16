---
module: formalization
sources:
  - path: formalization/axioms_robinson.pl
    sha256: 85fdb82e4876093192b800b14175b83157737dcf0818e4e8f0a9e955e31edecd
  - path: formalization/robinson_q.pl
    sha256: cfb006b8f25f035036d3c3d0c4f319b9c0bdbd411c53e78dfca1ac97e6a811f0
  - path: formalization/axioms_number_theory.pl
    sha256: a40c0523201152c4971741ea810613d8aae36af4434baf2a47c72ce4fa310d8e
  - path: formalization/axioms_geometry.pl
    sha256: e9313b6673fb0bb8c1a09c3da34a2b549957c01fdedab52da8afb1b34cca5c14
  - path: formalization/grounded_arithmetic.pl
    sha256: a8132812c03f6ea9cfce2001e236fa3d2496474fc5f52d1077af8e0b79cabef3
  - path: formalization/grounded_utils.pl
    sha256: 6699300d9bf9ca875ca274a1c92b682c96b08af900ca71cc0b154c59af56feb6
  - path: formalization/grounded_ens_operations.pl
    sha256: b07e0890c46c943a6bdb1b38aab5ddae12d4b461b36b0ec1b34556a22ae6c915
exports:
  # robinson_q.pl module exports
  - proves/1
  - incoherent/1
  - is_recollection/2
  - run_robinson_tests/0
  # grounded_arithmetic.pl module exports
  - add_grounded/3
  - subtract_grounded/3
  - multiply_grounded/3
  - divide_grounded/3
  - smaller_than/2
  - greater_than/2
  - equal_to/2
  - successor/2
  - predecessor/2
  - zero/1
  - leading_place_value/3
  - leading_digit_chunk/3
  - integer_to_recollection/2
  - recollection_to_integer/2
  - incur_cost/1
  # grounded_utils.pl module exports
  - decompose_base10/3
  - decompose_to_peano/3
  - base_decompose_grounded/4
  - base_recompose_grounded/4
  - count_down_by/3
  - count_up_by/3
  - is_zero_grounded/1
  - is_positive_grounded/1
  - peano_to_recollection/2
  - recollection_to_peano/2
  # grounded_ens_operations.pl module exports
  - ens_partition/3
  # axioms_robinson.pl, axioms_number_theory.pl, axioms_geometry.pl
  # assert proves_impl/2 and is_incoherent/1 clauses without :- module declarations;
  # they are included by arche-trace/incompatibility_semantics.pl.
operators:
  - =>/xfy/1050  # sequent arrow, declared in robinson_q.pl
  # No :- op(700, xfx, //) is declared in the stripped formalization sources.
  # Rational terms use SWI-Prolog's built-in rdiv operator (N rdiv D) in axioms_robinson.pl.
research_goal: >
  Robinson Q's seven axioms (Q1–Q7) and the commutativity of addition and
  multiplication are asserted as proves_impl/2 clauses in axioms_robinson.pl and
  robinson_q.pl; grounded arithmetic is defined over tally-list recollections in
  grounded_arithmetic.pl, and Q1–Q7 rules in axioms_robinson.pl route through
  arith_op/4 (the hermeneutic calculator's arithmetic layer). No Gödel numbering
  is wired, no Gödel sentence is constructed, no meta-theorem linking the HC to Q
  is proved in code.
---

# Fact-sheet: formalization

Written from [Stripped_Code/formalization/](../../Stripped_Code/formalization/) without reading any aspirational markdown. Cross-checked against Round 2 after drafting. Voice: SCENE register — hypothetical correspondences, not identities; productive failure is the point of contact.

## What the module defines

Seven .pl files. Three distinct registers: grounded arithmetic (tally-list
recollections with cognitive-cost tracking), standalone Robinson Q (a small
module with its own `proves/1`), and included axiom files (no `:- module`
declaration — consumed by `arche-trace/incompatibility_semantics.pl` via
`:- include(...)`).

- `grounded_arithmetic.pl` — embodied arithmetic. Numbers are
  `recollection(History)` where `History` is a list of `tally` atoms. `zero/1`,
  `successor/2`, `predecessor/2`, `add_grounded/3`, `subtract_grounded/3`,
  `multiply_grounded/3`, `divide_grounded/3` defined over this representation.
  `incur_cost/1` is declared in the module export list. For goals routed
  through `learner/meta_interpreter.pl:solve/6` the meta-interpreter
  intercepts the call, looks up the weight in `learner/config.pl:cognitive_cost/2`,
  applies the modal-context multiplier, and decrements the interpreter's
  inference budget. For direct callers (principally the FSM automata in
  `strategies/math/`, which run outside the meta-interpreter), the body
  now accumulates into a module-local `direct_cost_accumulator/1` using
  a local copy of the cost table; `direct_cost_accumulated/1` and
  `reset_direct_cost_accumulator/0` are exported for readback and reset.
  *(Post-audit fix landed 2026-04-16; pre-fix body was `true`.)*
- `grounded_utils.pl` — base-decomposition helpers. `decompose_base10/3`,
  `base_decompose_grounded/4`, `base_recompose_grounded/4` and conversions
  between Peano `s(N)` form and `recollection(History)` form.
- `grounded_ens_operations.pl` — `ens_partition/3`, which wraps an input unit as
  `unit(partitioned(N_Rec, InputUnit))` and generates N copies.
- `robinson_q.pl` — self-contained module. Declares `=>/xfy/1050` as the sequent
  operator. Defines its own `is_recollection/2` (via direct successor
  recursion, no hermeneutic calculator dependency) and `proves/1`, which
  dispatches through `proves_impl/2`. Includes the `run_robinson_tests/0` test
  harness (20 `test/2` invocations: 2 grounding, 4 arithmetic-grounding, and
  14 arranged in seven Q1–Q7 pairs).
- `axioms_robinson.pl` — included, not a module. Defines `is_recollection/2`
  over integers *and* rationals (`N rdiv D`), calling
  `hermeneutic_calculator:calculate/6` for positive successors and negative
  constructions. Defines `gcd/3`, `normalize/2`, `perform_arith/4`, and
  `arith_op/4`. Asserts `proves_impl/2` clauses for eq, plus, minus, mult,
  succ-eq, Q1–Q7 instances, and the commutativity rules
  `plus(A,B,C) → plus(B,A,C)` and `mult(A,B,C) → mult(B,A,C)` over `n(...)`
  premises. Also asserts `is_incoherent/1` for `o(eq(succ(X), 0))` and for
  `n(minus(A,B,_))` under natural-number domain when `A < B`.
- `axioms_number_theory.pl` — included, not a module. `is_prime/1`,
  `find_prime_factor/2`, `product_of_list/2`. `proves_impl/2` clauses that
  encode the Euclid infinitude argument: given `is_complete(L)`, construct
  `analyze_euclid_number(EF, L)` with `EF = Π(L)+1`, then branch on
  prime-or-composite and derive a contradiction with `is_complete`. A separate
  `is_incoherent/1` clause detects the Euclid contradiction directly.
- `axioms_geometry.pl` — included, not a module. `incompatible_pair/2` facts
  for quadrilateral taxonomy, `entails_via_incompatibility/2` (Brandom's
  material-incompatibility reading: P entails Q iff everything incompatible
  with Q is incompatible with P), and `is_incoherent/1` for a premise pair
  that combines shape predicate and restriction predicate on the same variable.

## What axioms / inferences are asserted

The Robinson Q axiom rules in `axioms_robinson.pl` and `robinson_q.pl` are
aligned in shape but differ in where arithmetic is grounded:

- **Q1 (S(x) ≠ 0)** — expressed as incoherence, not derivation:
  `is_incoherent([o(eq(succ(X), 0))]) :- integer(X), X >= 0, once(is_recollection(X, _)).`
- **Q2 (S injective)** — conditional sequent rule:
  `proves_impl([o(eq(succ(X), succ(Y)))] => [o(eq(X, Y))], _) :- once(is_recollection(X, _)), once(is_recollection(Y, _)).`
  No arithmetic check on X = Y; the conclusion is asserted given the
  antecedent's form. A false antecedent makes the rule vacuously true in
  sequent calculus.
- **Q3 (x = 0 ∨ x = S(y))** — split into two `proves_impl` clauses, both taking
  `integer(X)` and producing a structural witness:
  `eq(X, 0)` when `X =:= 0`, or `eq(X, succ(Y))` where `Y is X - 1` is checked
  as a recollection.
- **Q4 (x + 0 = x)** — `proves_impl(_ => [o(eq(plus(X, 0), X))], _) :- once(is_recollection(X, _)).`
- **Q5 (x + S(y) = S(x + y))** — in `axioms_robinson.pl` the body routes
  through `arith_op/4` four times (compute `Sum = X+Y`, `SY = Y+1`,
  `Sum2 = X+SY`, `SumPlus1 = Sum+1`) and checks `Sum2 =:= SumPlus1`. In
  `robinson_q.pl` the same check uses Prolog's built-in `is/2` directly.
- **Q6 (x × 0 = 0)** — `proves_impl(_ => [o(eq(mult(X, 0), 0))], _) :- once(is_recollection(X, _)).`
- **Q7 (x × S(y) = (x × y) + x)** — in `axioms_robinson.pl` routes through
  `arith_op/4` four times and checks identity; in `robinson_q.pl` uses `is/2`.

Commutativity of addition and multiplication is hardcoded as implication over
`n(...)`-wrapped propositions:

- `proves_impl([n(plus(A,B,C))] => [n(plus(B,A,C))], _).`
- `proves_impl([n(mult(A,B,C))] => [n(mult(B,A,C))], _).`

No body. The rule succeeds structurally whenever the premise is in the sequent.

Arithmetic dispatch in `axioms_robinson.pl` routes through
`arith_op(A, B, Op, C)` which:

1. Requires `Op ∈ {+, -, *}`.
2. Calls `normalize/2` on both operands, producing either a raw integer or
   `N rdiv D`.
3. If both normalized operands are integers, calls `perform_arith/4` directly
   (`C is A + B` etc.).
4. Otherwise decomposes into numerator/denominator, cross-multiplies by the
   other's denominator, and calls `perform_arith/4` on the scaled numerators;
   wraps the result as `N_res rdiv D_res`.
5. Normalizes the result, collapsing to a raw integer when the denominator
   reduces to 1.

`is_recollection/2` in `axioms_robinson.pl` has five clauses: base
`0 → [axiom(zero)]`; a tally-history form
`recollection(History) → [explicit_recollection(History)]` guarded by
`maplist(=(tally), History)`; a positive-integer form that recursively checks
`Prev = N-1` and delegates to
`hermeneutic_calculator:calculate(Prev, +, 1, _Strategy, N, History)`;
a negative-integer form that delegates to
`hermeneutic_calculator:calculate(0, -, |N|, _Strategy, N, History)`;
and a rational form that grounds `N rdiv D → [history(rational, from(N, D))]`
when both numerator and denominator are themselves recollections.

`robinson_q.pl` implements its own forward-chaining meta-rule at
lines 91–98: given a sequent, find any `proves_impl` clause whose antecedents
match premises, run the body, and recurse with the derived conclusion added
to the premise list. This is how the module reaches conclusions from
combined axioms within the same file.

In `axioms_number_theory.pl` the chain of `proves_impl` clauses encodes one
direction of Euclid's argument: from a claimed "complete list of primes" plus
the Euclid number construction, derive `neg(is_complete(L))`. The geometry
module encodes a Brandomian material-inference reading of shape entailments.

## What the module does NOT do

- **No Gödel numbering.** No encoding of this system's own syntax into numbers
  lives in any of the seven files. `axioms_number_theory.pl` has `is_prime/1`
  and `find_prime_factor/2` but these are over ordinary integers; they are not
  wired to anything that mentions the HC's own axiom schemas.
- **No Gödel sentence.** No term `G` is constructed such that `G ↔ ¬proves(G)`
  or an analogue thereof. The words "Gödel" and "incompleteness" appear only
  in `robinson_q.pl`'s `run_robinson_tests/0` banner text ("Goedel's First
  Incompleteness Theorem applies."). That line writes a string; it does not
  prove a meta-theorem.
- **No meta-theorem.** "Every theorem of Q is a theorem of the HC" is not
  proved anywhere in code. The `proves_impl` clauses succeed for concrete
  instances of Q's schemas; that schematic-instance coverage is not lifted to
  a universally quantified meta-claim inside the formalism.
- **No induction schema.** This is appropriate — Q has none. Q5 and Q7 are
  asserted as rules that must hold for arbitrary integer instances, not as
  universally quantified theorems.
- **No proof-theoretic strength results.** No consistency proof, no relative
  consistency argument, no cut-elimination theorem, no conservativity claim.
- **Schematic instances are not universal statements.** The `proves_impl`
  clauses for Q4–Q7 take `once(is_recollection(X, _))` (and `is_recollection(Y, _)`
  where relevant) and then verify the identity by computing both sides. This
  means the rule succeeds for any concrete integer inputs grounded by
  `is_recollection/2`, but a successful run is one instance, not a proof of
  the universally quantified axiom. `robinson_q.pl`'s test harness runs 20
  `test/2` cases (14 of them arranged into Q1–Q7 pairs); the closing banner
  ("All Robinson axioms Q1–Q7 verified.") overstates what two instance-level
  test cases per axiom establish.
- **`incur_cost/1` is light.** Meta-interpreter-routed calls are tracked
  through `learner/meta_interpreter.pl:solve/6`'s interception; direct
  calls accumulate into `direct_cost_accumulator/1` via the local cost
  table introduced in the 2026-04-16 fix. The two paths use different
  accumulators, and no reconciliation wires them together. The local
  cost table duplicates the learner's canonical `config:cognitive_cost/2`;
  drift between the two tables is a known risk.
- **No rational operator declaration.** The stripped files do not contain
  `:- op(700, xfx, //)` or any similar declaration. Rational terms in
  `axioms_robinson.pl` use SWI-Prolog's built-in `rdiv` operator
  (`N rdiv D`); there is no `//` operator active in this module.
- **`axioms_geometry.pl` and `axioms_number_theory.pl` are not re-exports of
  formalization's Robinson work.** They are parallel included-file axiom sets
  for different domains. Their presence in `formalization/` reflects the
  module's role as the axiomatic-claim staging area for the sequent engine
  rather than a claim that number theory or geometry interpret Q.

## Research goal

Robinson Q's seven axioms (Q1–Q7) and the commutativity of addition and
multiplication are asserted as `proves_impl/2` clauses in `axioms_robinson.pl`
and `robinson_q.pl`; grounded arithmetic is defined over tally-list
recollections in `grounded_arithmetic.pl`, and Q1–Q7 rules in
`axioms_robinson.pl` route through `arith_op/4` (the hermeneutic calculator's
arithmetic layer). No Gödel numbering is wired, no Gödel sentence is
constructed, no meta-theorem linking the HC to Q is proved in code.

## Cross-check against Round 2

Read lines 136–162 of
[Prolog Code Analysis And Simulation ROUND 2 After Strip.md](Prolog%20Code%20Analysis%20And%20Simulation%20ROUND%202%20After%20Strip.md).

Lines 136–144 straddle `learner/object_level.pl` and formalization's
boundary: Round 2 describes Peano representation and `recursive_add/3` as
living in `object_level.pl`, with `enumerate/1` as the Peano-term generator.
The stripped formalization files do not contain `recursive_add/3` or
`enumerate/1` — those names belong to the learner module, not here. No
disagreement; lines 136–144 are describing the other side of the boundary.

Lines 146–162 cover Robinson Arithmetic. Triangulating against the stripped
`axioms_robinson.pl`:

- Round 2 line 148: "numbers are verified as valid 'recollections' linked to
  specific axioms" — matches `is_recollection(0, [axiom(zero)])` and the
  history form.
- Round 2 line 150: positive integers built via `hermeneutic_calculator`
  incremental +1, negative integers via subtracting absolute values from
  zero. Matches clauses at lines 13–25 of the stripped source.
- Round 2 line 150: "rational numbers, structured via an infix operator"
  with denominator positive, both numerator and denominator recollections.
  Matches the `N rdiv D` clause at lines 27–31. Round 2 calls this an "infix
  operator" without naming it; the stripped source uses `rdiv` (SWI-Prolog
  built-in), not `//`. No declaration of `//` appears in the stripped files.
- Round 2 line 152: `normalize/2` computes GCD, divides both by GCD, and if
  the denominator becomes 1 the rational is collapsed to a pure integer.
  Matches the stripped `normalize/2` exactly (lines 36–43).
- Round 2 line 154: `arith_op/4` handles scale conversion. Matches the
  cross-multiplication branch (lines 55–62).
- Round 2 line 156: equality proven "exclusively if post-normalization
  values align perfectly (NA \== NB)". Matches the stripped `proves_impl`
  clause at lines 65–68.
- Round 2 line 158: "zero relationships are strictly enforced" with zero
  case and successor-witness case for Q3. Matches the stripped clauses at
  lines 102–106.
- Round 2 line 159: multiplication encoded via Q6/Q7 and commutativity
  "hardcoded via logical implication: plus(A,B,C) explicitly implies
  plus(B,A,C), and mult(A,B,C) implies mult(B,A,C)". Matches the stripped
  commutativity clauses (lines 82 and 89).
- Round 2 line 161: incoherence if `succ(X) = 0` or if natural-domain
  subtraction with minuend less than subtrahend. Matches `is_incoherent/1`
  clauses at lines 96–97 and 132–137.

No mechanical disagreements. Round 2's phrase "infix operator" for rationals
is imprecise (it is `rdiv`, not a declared `//`), but that is a naming
choice in Round 2's prose, not a claim the code contradicts.
