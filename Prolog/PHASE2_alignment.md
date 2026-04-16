# Phase 2 Alignment Map

| Synthesized Commitment | Prolog Module(s) | Notes / Follow-up |
| --- | --- | --- |
| Triadic validity modes ($\mathsf{S}$, $\mathsf{O}$, $\mathsf{N}$) and polarized operators | `pml_operators.pl`, `semantic_axioms.pl`, `incompatibility_semantics.pl` | Operators declared in `pml_operators`; modal rhythm (Section 5 of paper) wired through `semantic_axioms`. `incompatibility_semantics` consumes them via structural rule `pml_rhythm_axiom/2`. |
| Dialectical engine schema $U \to \SBoxDown(A) \to \SDiaUp(LG) \to \SBoxUp(U')$ | `semantic_axioms.pl`, `dialectical_engine.pl` | Base material inferences encode the rhythm; `dialectical_engine.pl` hosts macros for multi-step walkthroughs. |
| Zeeman Catastrophe constraint | `critique.pl`, `dialectical_engine.pl`, calculators in `math/` | Parameters appear in crisis modeling predicates (e.g., `critique:zcm_state/4`). |
| Trace deferral/resistance | `automata.pl`, `pragmatic_axioms.pl`, `incompatibility_semantics.pl` | `automata:attr_unify_hook/2` enforces deferral/resistance; `pragmatic_axioms` uses it for `i_feeling/1`; `construct_proof/4` erases proofs contaminated by traces. |
| Normative/Objective coupling | `semantic_axioms.pl`, `intersubjective_praxis.pl` | S-O transfer captured via Oobleck rule; praxis module adds additional commitments for recognition cycles. |
| Embodied exercises / pedagogical scaffolding | `intersubjective_praxis.pl`, `tests/critique_test.pl`, `interactive_ui.pl` | Scripts will ingest text from `synthesized_paper.md` during web build. |
| Calculator / hermeneutic checks | `hermeneutic_calculator.pl`, `math/*` | To be revisited once attr hooks verified. |

Use this sheet as the TODO ledger while pruning redundant clauses.
