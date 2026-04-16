---
module: arche-trace
sources:
  - path: arche-trace/automata.pl
    sha256: 41564b1d1c844bb283577ec46173dc374f93956cc05fa133a49beb66ba48b647
  - path: arche-trace/critique.pl
    sha256: a6ab3e0dc98a5591e1083f166ed1a8a5bc080a73c0d15071f551cacf5723860f
  - path: arche-trace/dialectical_engine.pl
    sha256: 249b7133b84a4720a532bf894853c73254ca1f2fe02d8225b5feb510bc8d28de
  - path: arche-trace/embodied_prover.pl
    sha256: 05331becbd610ef25e41ef63f40a8cb2e47c484d706b7561ab7914b90aa4c788
  - path: arche-trace/incompatibility_semantics.pl
    sha256: 915fc95e9ab394fc4a6218bc99f977dccf167b8a75f928c761a4b67639768967
  - path: arche-trace/load.pl
    sha256: 06bb0686abe0100b3471da26764b5bce14962e28da953ef5b5d50341314dec1c
  - path: arche-trace/tests/critique_test.pl
    sha256: 757fd58e2779477a17d3ad044a543230efeaaa1fe333c4685875d4607fb334e1
  - path: arche-trace/tests/dialectical_engine_test.pl
    sha256: d73ff0f7ca80943e6d877280183866339b55cc8f38bf7d7c25feb166224da591
exports:
  # automata
  - highlander/2
  - generate_trace/1
  - contains_trace/1
  - nth_prime/2
  - is_prime/1
  - automata:attr_unify_hook/2
  # incompatibility_semantics
  - proves/1
  - is_recollection/2
  - incoherent/1
  - set_domain/1
  - current_domain/1
  - product_of_list/2
  - s/1
  - o/1
  - n/1
  - comp_nec/1
  - exp_nec/1
  - exp_poss/1
  - comp_poss/1
  - neg/1
  - bounded_region/4
  - equality_iterator/3
  # embodied_prover
  - proves/4
  - proves_impl/7
  - is_incoherent/1
  - material_inference/3
  - construct_proof/4
  # critique
  - reflect/2
  - accommodate/1
  - get_stress_map/1
  - reset_stress_map/0
  # dialectical_engine
  - run_computation/2
  - run_fsm/4
operators:
  - comp_nec/fx/500
  - exp_nec/fx/500
  - exp_poss/fx/500
  - comp_poss/fx/500
  - neg/fx/500
  - =>/xfy/1050
  # rdiv is SWI-Prolog's built-in rational operator, not declared here
research_goal: >
  The sequent calculus proof object in embodied_prover.pl collapses from
  proof(...) to erasure(...) at every rule-application site when the sequent
  contains an arche_trace-attributed variable. The mechanism is singular, not
  four-part: the bifurcation lives in construct_proof/4 and propagates by
  inspection of subproofs and term variables. The "four erasure points"
  enumerated in ARCHE_TRACE_ERASURE.md are four demonstrated sequents in a
  catalog, not four structural code sites.
---

# Fact-sheet: arche-trace

Written from [Stripped_Code/arche-trace/](../../Stripped_Code/arche-trace/) without reading any aspirational markdown. Cross-checked against Round 2 after drafting. Voice: SCENE register — hypothetical correspondences, not identities; productive failure is the point of contact.

## What the module defines

Five source files and two test files. Eight predicates carry the module's load.

**`automata.pl`** declares the `arche_trace` attribute. `generate_trace/1` calls
`put_attr(T, automata, arche_trace)` on a fresh variable. `contains_trace/1`
walks `term_variables/2` and checks each for the attribute. The attr_unify_hook
`automata:attr_unify_hook(arche_trace, Value)` fires on every unification
attempt against an attributed variable: if `Value` is another variable, the
attribute propagates; if `Value` is concrete, the hook fails — unification
halts. The module also exports `highlander/2` (singleton-list check) and
`nth_prime/2` / `is_prime/1` (trial division). These last three are unrelated
to trace logic and appear to be unrelated mathematical utilities.

**`incompatibility_semantics.pl`** declares the scene-agnostic sequent calculus
engine. `proves/1` calls `proves_impl/2`. Identity and explosion are scene-
agnostic. Material axioms enter via `:- include(...)` of six files
(`axioms_robinson`, `axioms_geometry`, `axioms_number_theory`, `axioms_eml`,
`axioms_domains`). Structural rules handle forward chaining and arithmetic
evaluation. Reduction schemata (`ln`, `rn`, `l_conj`, `r_conj`, modal variants
`ln_modal`, `rn_modal`, `l_conj_modal`, `r_conj_modal`, modal `nec`-handling)
decompose sequents. Returns only a boolean — there is no proof-object output
in this engine and no trace inspection.

**`embodied_prover.pl`** declares the resource-tracked, proof-object-emitting
prover. `proves/4` wraps `proves_impl/7`, which threads an inference budget
`R_In → R_Out`, a modal context (`compressive` | `expansive` | `neutral`)
determining step cost (2 | 1 | 1), and a history list. Resource exhaustion
throws `perturbation(resource_exhaustion)`. `construct_proof/4` is the
erasure bifurcation: for each completed rule site, it returns
`erasure(propagation)` if any subproof is `erasure(_)`, `erasure(RuleName)` if
the sequent contains a trace variable, else `proof(RuleName, Sequent,
SubProofs)`. The same 12 rule sites that produce proof terms in
`incompatibility_semantics.pl` are re-implemented here and each terminates in
a `construct_proof/4` call.

**`critique.pl`** declares the reflection / accommodation loop.
`reflect(Proof, Trigger)` first calls `detect_pathology/2`
(looks for `find_proof_cycle/2` under a `bad_infinite` signature — a cycle
whose every node is a compressive rule), then extracts commitments from the
proof and checks them with `incoherent/1`. `accommodate/1` dispatches on
`perturbation(resource_exhaustion, Sequent)`, `incoherence(Commitments)`, and
`pathology(bad_infinite, Cycle)`. All three dispatch paths *fail* after
recording stress; belief revision asserts `is_incoherent/1` as a dynamic fact
targeting the most-stressed commitment. `get_stress_map/1` and
`reset_stress_map/0` expose the dynamic stress table.

**`dialectical_engine.pl`** declares the top-level driver. `run_computation/2`
wraps `proves/4` in a catch; `perturbation(Type)` gets routed to
`accommodate/1`, and on successful accommodation the computation is retried
from the top. `run_fsm/4` is a generic FSM traversal — it calls
`Module:accept_state/1`, `Module:transition/3`, and optional
`Module:final_interpretation/2` predicates, building a `step(Name, Data,
Interpretation)` history list. The FSM loop has no arche-trace logic; it is a
reusable state-machine driver that lives in this module for reasons the code
does not explain.

**`load.pl`** loads the stack: `paths.pl` → pml utilities → operators →
`incompatibility_semantics` (the scene-agnostic engine) → pml axiom files →
`automata` → pragmatic axioms → intersubjective praxis → `embodied_prover` →
`critique` → `dialectical_engine`. Both provers load together in this order.

**Tests:** `critique_test.pl` exercises the stress map, commitment extraction,
cycle detection, belief revision, and the accommodate dispatch paths (seven
cases). `dialectical_engine_test.pl` covers happy-path computation (identity,
PML rhythm, Oobleck S→O, explosion), perturbation handling (zero-resource,
stress-accumulation, error catching), and the FSM engine (simple, stuck,
immediate, bare-atom, history ordering).

## What axioms / inferences are asserted

**The erasure bifurcation.** `construct_proof/4` at
`embodied_prover.pl:57–64` is the single site where proof objects become
hollow:

    construct_proof(RuleName, Sequent, SubProofs, Proof) :-
        ( member(erasure(_), SubProofs) -> Proof = erasure(propagation)
        ; ( contains_trace(Sequent) -> Proof = erasure(RuleName)
          ; Proof = proof(RuleName, Sequent, SubProofs)
          )
        ), !.

Every terminating rule in `proves_impl/7` routes through this predicate.
The rule sites observable in the stripped code are:
`identity`, `explosion`, `pml_rhythm(...)`, `pml_possibility_check`,
`mmp(Axiom)`, `ln`, `ln_modal(D)`, `rn`, `rn_modal(D)`, `l_conj`,
`l_conj_modal(D)`, `r_conj`, `r_conj_modal(D)`. That is 13 rule sites; each
produces either `proof(RuleName, ...)` or `erasure(RuleName)` depending on
whether any variable in the sequent carries the `arche_trace` attribute.

**Trace insertion.** `generate_trace/1` is the sole entry point for tagging a
variable with `arche_trace`. The stripped code shows exactly one caller:
`i_feeling/1` in `pml/pragmatic_axioms.pl`, which calls `generate_trace` when
its argument is an unbound variable. Identity claims (`identity_claim/1`) are
the inverse check — they must *not* contain a trace. The Unsatisfiable Desire
axiom (`n(represents(C_Id, I_f))`) is implemented as an
`incompatibility_semantics:is_incoherent/1` clause that requires both an
identity-claim witness and an i-feeling witness in the same sequent.

**The attr_unify_hook contract.** `automata:attr_unify_hook(arche_trace,
Value)` enforces two rules during unification: if `Value` is a variable,
propagate the attribute; if `Value` is concrete, fail. This is the mechanical
sense of "resists stabilization." It operates at the Prolog unification
layer — below the sequent calculus.

**Sequent calculus, embodied variant.** `proves_impl/7` is a left-to-right
sequent reducer over `(Premises => Consequents)`. Modal context determines
per-step cost (compressive = 2, expansive = 1, neutral = 1). Modal rules
(`pml_rhythm_axiom`, triggered on `s(X)` in the antecedent) consult
`material_inference/3` entries keyed to PML modalities; matched antecedents
flip the local context to `compressive` or `expansive` before the recursive
call. The existence-condition for a possibility check requires the possibility
to occur in the consequent before the rule fires.

**Sequent calculus, scene-agnostic variant.** `incompatibility_semantics.pl`
exposes `proves/1` as a boolean over the same sequent format. It does not emit
proof objects and does not inspect `arche_trace`. Its reduction rules are the
same structural rules minus cost tracking. **Downstream caller wire.**
`learner/more_machine_learner.pl:count_loop/4` calls
`proves([] => [o(plus(X, 1, Y))])` to derive the Peano successor — a
cross-module invocation from the learner's bounded interpreter directly into
this scene-agnostic prover. The arche-trace fact-sheet notes this because the
caller lives elsewhere; the callee is the boolean `proves/1` exposed here.
Arithmetic evaluation is inlined (line 79–89) — numeric terms eligible for
Prolog `is/2` evaluation are
rewritten into the premise list.

**Critique dispatch.** `reflect/2` returns a `Trigger` tag —
`pathology(bad_infinite, Cycle)` or `incoherence(Commitments)`.
`accommodate/1` consumes the tag and records stress; belief revision asserts a
dynamic `incompatibility_semantics:is_incoherent/1` clause for the stressed
antecedent set. The accommodate predicate always fails (explicit `fail` at the
end of each clause), forcing the driver's catch-retry loop to report the
failure upward. Bad-infinite cycles are detected by `find_proof_cycle/2` when
every node in the cycle is a compressive rule
(`is_compressive_node(node(pml_rhythm(_), _))` and functor match for wrapped
variants).

## What the module does NOT do

- **No soundness proof.** The sequent calculus has no meta-theorem establishing
  that the rule set is sound with respect to any semantics. The code only
  executes the rules.
- **No completeness proof.** There is no argument that the reduction schemata
  exhaust the derivable sequents of the logic.
- **No proof that the erasure boundary is the only place formal proof halts.**
  `proves_impl/7` can fail silently — resource exhaustion throws a
  perturbation, but a sequent that exceeds the reduction schemata simply
  fails. Nothing in the code guarantees the catalog of four demonstrated
  erasures in `ARCHE_TRACE_ERASURE.md` enumerates *all* places the system
  yields hollow proofs; it enumerates four reproducible sequents.
- **No formal account of why the four demonstrations are privileged.** The
  catalog of four sequents (identity with Trace, S-O Inversion, double
  negation elimination with Trace, Oobleck S→O with Trace) is an empirical
  selection from the reachable proof space. The code does not encode them as
  a distinguished set.
- **No phenomenological claim the mechanism could carry.** The mechanism is
  attribute-propagation and variable-scan. It marks terms that contain a
  specific variable; it does not represent the felt quality of subjective
  experience.
- **No Gödel numbering.** `automata.pl` exposes `nth_prime/2` and `is_prime/1`,
  and `incompatibility_semantics.pl` exposes `product_of_list/2`. These are
  the mechanical pieces a Gödel-numbering scheme would rest on, but they are
  not wired to syntax encoding anywhere in the stripped code.
- **No cycle detection on erased proofs.** `find_cycle_impl(erasure(_), _,
  _) :- fail.` — the cycle detector gives up on erased subtrees. A cycle
  whose nodes are erased is therefore invisible to the bad-infinite check.
- **No sublation mechanism.** The accommodate clause for
  `pathology(bad_infinite, Cycle)` records stress and writes "SUBLATION
  REQUIRED" to stdout, then fails. "System cannot auto-generate new concepts
  yet." is a literal line in the code. The resolution is external.
- **No unified prover.** Two provers ship side-by-side: the scene-agnostic
  boolean engine in `incompatibility_semantics.pl` and the resource-tracked
  proof-object engine in `embodied_prover.pl`. The trace logic lives only in
  the latter.
- **No sustained claim about what a "compressive rule" is.** `is_compressive_
  node/1` matches on the atom `pml_rhythm` as the rule head. The bad-infinite
  detector identifies cycles of *this rule type*, not cycles that are
  compressive in any deeper sense.

## Research goal

The sequent calculus proof object in `embodied_prover.pl` collapses from
`proof(...)` to `erasure(...)` at every rule-application site when the sequent
contains an `arche_trace`-attributed variable. The mechanism is singular, not
four-part: the bifurcation lives in `construct_proof/4` and propagates by
inspection of subproofs and term variables. The "four erasure points"
enumerated in `ARCHE_TRACE_ERASURE.md` are four demonstrated sequents in a
catalog, not four structural code sites.

## Cross-check against Round 2

Round 2 lines 72–88 (dialectical engine) and 90–108 (embodied prover) and
234–244 (automata) describe this module. Mechanical agreement is clean.
Round 2 correctly identifies `run_fsm_loop` with prepend-then-reverse history
(line 76), the tripartite `extract_state_info/3` parser (lines 78–82), the
`perturbation(Type)` dispatch to `accommodate/1` (line 86), the resource cost
table (2/1/1 for compressive/expansive/neutral at lines 98–100), the ln/rn
negation rules and their modal variants (line 102), the
`l_conj` splitter (line 104), the identity and incoherence termination
conditions (lines 106–108), the `put_attr/3` mechanism (line 238), the
`contains_trace` variable-scan (line 240), the `attr_unify_hook(arche_trace,
Value)` and its `var(Value)` branch (line 240), the `highlander/2` singleton
check (line 242), and the `is_prime`/`has_divisor` trial-division loop (line
244). No mechanical disagreement observed. Round 2's language is purple by
default ("violently aborts," "stealth metadata," "the engine is forced to
execute a list reversal operation") — that is register, not mechanics.

Round 2 does not describe `construct_proof/4` — the erasure-emitting
bifurcation. It describes the prover's reduction path as if it always
produced ordinary proof terms. This is a silence, not a contradiction: the
mechanism is present in the code and the `contains_trace` primitive is
described, but Round 2 does not connect the two. That connection is the
research goal of this module and the specific claim the aspirational layer
(README + ARCHE_TRACE_ERASURE) makes about the code.

Lines 118–127 (pml/pragmatic_axioms.pl, the only caller of `generate_trace`)
correctly describe `i_feeling/1` as invoking `generate_trace(I_f)` on a
variable and `contains_trace(I_f)` on a bound term, and `identity_claim/1` as
the negative-check inverse. This cross-reference anchors the erasure mechanism
to the one place in the stack that introduces trace-tagged variables.
