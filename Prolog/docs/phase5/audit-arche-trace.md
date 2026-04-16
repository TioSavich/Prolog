# Audit log: arche-trace

Written after [docs/phase5/fact-sheet-arche-trace.md](fact-sheet-arche-trace.md). Research goal from the fact-sheet is the cut criterion.

This module was pre-classified by the Explore inventory as **realistic** — both `arche-trace/README.md` and `arche-trace/ARCHE_TRACE_ERASURE.md` were expected to match the code. The audit confirms this empirically with one narrow exception: a stale line in README.md's contents section that describes a module-name collision already resolved in Phase 4. No cuts.

## Cut criterion

> The sequent calculus proof object in `embodied_prover.pl` collapses from `proof(...)` to `erasure(...)` at every rule-application site when the sequent contains an `arche_trace`-attributed variable. The mechanism is singular, not four-part: the bifurcation lives in `construct_proof/4` and propagates by inspection of subproofs and term variables. The "four erasure points" enumerated in `ARCHE_TRACE_ERASURE.md` are four demonstrated sequents in a catalog, not four structural code sites.

## Decisions

| File | Section / line range | Claim | Verdict | Rationale |
|---|---|---|---|---|
| `arche-trace/README.md` | L3–L5 (opening) | "This module marks the precise boundary where formalization goes hollow. Derivations succeed structurally but proof objects contain no content." | keep | Code-backed: `construct_proof/4` emits `erasure(...)` while the recursive descent continues to build the tree. |
| `arche-trace/README.md` | L7–L12 (erasure mechanism) | "This happens at exactly the points where human judgment must take over: identity with Trace, S-O Inversion, double negation elimination, Oobleck transfer." | commentary | The mechanism fires at *every* rule site when a trace is present; these four are the demonstrated catalog, not the exhaustive structural list. "Exactly" is slightly tighter than the code guarantees. The four-point list is useful as an experimental catalog but should not be read as a structural enumeration of the code. |
| `arche-trace/README.md` | L14–L15 | "This is not a bug. The erasure marks the skeleton boundary — where the formalism honestly stops being able to say anything." | commentary | Register-appropriate framing, no empirical claim. |
| `arche-trace/README.md` | L17–L26 (sequent engine priority order) | Four-level priority: identity/explosion, material axioms, structural rules, reduction schemata. | keep | Matches the clause order in `incompatibility_semantics.pl` lines 49–103. |
| `arche-trace/README.md` | L27–L34 (axiom set list) | Six `:- include(...)` entries. | keep | Matches `incompatibility_semantics.pl` lines 63–67 (five includes; README lists five — agreement). |
| `arche-trace/README.md` | L36–L37 | "PML operators live canonically in `pml/pml_operators.pl` and are re-exported by the engine." | keep | Matches `:- reexport(pml(pml_operators)).` at line 25. |
| `arche-trace/README.md` | L42–L44 | "`embodied_prover.pl` — alternate prover with `proves/4` (resource-tracked, embodied cost model). Still declares module name `incompatibility_semantics` — cannot be loaded simultaneously with the engine. Needs its own module name (future cleanup)." | substantiate | Stale. Stripped code line 7 shows `:- module(embodied_prover, [...])`. `load.pl` loads both provers simultaneously with no conflict. The module-name collision was resolved in Phase 4 (commit 2a9d498, "test independence verified; resolve module name collision"). Follow-up: replace this sentence with something like "alternate prover with `proves/4` (resource-tracked, embodied cost model). Module name `embodied_prover`; loaded alongside `incompatibility_semantics` by `load.pl`." |
| `arche-trace/README.md` | L45–L50 (rest of contents list) | Descriptions of `critique.pl`, `dialectical_engine.pl`, `automata.pl`, `load.pl`, `ARCHE_TRACE_ERASURE.md`. | keep | Match the stripped code. |
| `arche-trace/README.md` | L52–L56 (cross-module dependencies) | "Imports `formalization/grounded_arithmetic` and `strategies/hermeneutic_calculator`. Re-exports PML operators from `pml/pml_operators`. Imported BY strategy automata and learner." | keep | Matches `:- use_module(...)` lines at the top of `incompatibility_semantics.pl`. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L1–L4 (title + frame) | "Where the skeleton stops and interpretation begins." | commentary | Register-appropriate. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L6–L14 (what the Arche-Trace does) | Two rules: deferral (propagates to other variables) and resistance (fails against concrete terms). | keep | Matches `attr_unify_hook(arche_trace, Value)` at `automata.pl:28–39`. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L16–L18 (construct_proof description) | `construct_proof/4` checks whether the sequent contains a trace; if so, replaces proof with `erasure(RuleName)`; erasure propagates upward. | keep | Exact match to `embodied_prover.pl:57–64`. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L20–L22 (experimental catalog frame) | "All experiments run on System A (`Prolog/load.pl`), resource limit 50." | keep | Reproducible experimental frame. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L24–L37 (first two tables: no-trace and trace) | Tables of sequents and the proof/erasure terms they produce. | keep | Each row is reproducible from the stripped code given the load stack. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L50–L55 (third table: incoherence) | `n(represents(C_Id, I_f))` is rejected outright by `is_incoherent/1`. | keep | Matches the `is_incoherent/1` clause in `pml/pragmatic_axioms.pl:79–82`. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L57–L62 (fourth table: trace mechanics) | Unification `T = hello` fails; `T1 = T2` propagates the trace. | keep | Matches the two branches of `attr_unify_hook`. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L64–L78 (boundary pattern, three-zone map) | "The moment any variable in the sequent carries the `arche_trace` attribute, the proof object becomes hollow." | keep | This is the precise claim the stripped code backs up. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L82–L89 (claim 1: subjective experience) | "Proofs about the learner's subjective experience get erased." | commentary | "Subjective experience" is interpretive. Mechanically, the attribute marks variables generated through `i_feeling/1`; the phenomenological gloss is a reading, not a formalism. The paragraph already frames this as a model ("The system can prove... but the proof of *why* it holds is not available for inspection"), so the register is honest. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L91–L96 (claim 2: recognition) | "Proofs about recognition get erased when grounded in subjective experience." | commentary | The hypothetical ("if the confessing agents are themselves I-Feelings") is not in the demonstrated experimental catalog. Claim is consistent with the mechanism but is a projection, not a catalogued result. Keeping as commentary, not substantiate — the projection is philosophically clear. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L98–L102 (claim 3: Unsatisfiable Desire) | "The claim that any identity (C_Id) fully represents the I-Feeling (I_f) is... logically impossible." | keep | Matches the `is_incoherent/1` clause. The code-backed reading of "logically impossible" here is: rejected by a hand-asserted incoherence clause, not derivable from the general incoherence rule. The claim holds at that level. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L104–L107 (claim 4: abstract structure) | "Proofs about abstract structure are untouched." | keep | Trace-free sequents produce proof terms; this is the negative of the bifurcation rule. |
| `arche-trace/ARCHE_TRACE_ERASURE.md` | L109–L112 (formal signal to LLM-as-oracle) | "These erasure points are the formal signal to an LLM-as-oracle." | commentary | No LLM integration exists in the stripped code. Framing is forward-looking; no claim about current behavior. Keeps as register. |

## Relocations

None.

## Deletions

None.

## One follow-up noted

`arche-trace/README.md` L42–L44 describes a module-name collision that was
resolved in Phase 4. The sentence should be rewritten when the README is next
touched. This is a documentation-sync issue, not an overclaim — the audit
flags it as `substantiate` rather than `cut` because the surrounding paragraph
is correct about what `embodied_prover.pl` does; only the "cannot be loaded
simultaneously" clause is stale.
