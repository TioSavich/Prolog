# Synthesized Paper Outline (Phase 1)

This outline chooses canonical content for the unified paper and flags material to prune. Line numbers follow `tex_analysis_report.md`.

## Core Spine
- **Abstract + Quick Start** (AppendixA_Unified_2.tex:L80-170). Keep entire "Quick Start" narrative; trim duplicate statements from Appendix_Phenomenology.
- **Problem & Philosophical Grounding** (AppendixA_Unified_2.tex:L160-230). Merge Hegel/Carspecken/Brandom/Rödl/Derrida discussions; drop parallel paragraphs flagged by hashes `392f...` through `39ad...` in `tex_analysis_report.md`.
- **Innovation: Polarized Modal Logic** (AppendixA_Unified_2.tex:L221-235). Use this as hinge into formal section; remove redundant framing from AppendixA_Revised sections 146-226 except unique clarifications about "Three Modes" table.
- **Formal Framework** (AppendixA_Unified_2.tex:L236-520 + Appendix_Phenomenology.tex:L187-460). Keep operator definitions, normative mode discussion, and arrow notation. Use Appendix_Phenomenology for clearer descriptions of Objective/Normative modalities and citations to Habermas/Carspecken.
- **Applications to Chapters** (AppendixA_Unified_2.tex:L530-840). Retain summary paragraphs that map modalities to Chapter 1/2/ZCM/Being-Nothing. Collapse verbose duplicates by referencing the chapter table instead of rewriting each derivation.
- **Extended Phenomenology** (Appendix_Phenomenology.tex:L481-1028 + Phenom_Modal.tex:full). Use Phenom_Modal for logical schematics (dialectical engine, consciousness/self-consciousness/spirit) and Appendix_Phenomenology for bridging text. Drop unused repetition from AppendixA_Unified.tex.
- **Exercises & Didactics** (verbose_exercise.tex:L1-223). Keep "Preparation" plus the four parts; edit for brevity and integrate as sidebars in final doc.

## Commitments/Axioms to Preserve
1. **Triadic Validity Modes:** Subjective (S), Objective (O), Normative (N) from AppendixA_Unified_2 Section 3.
2. **Polarized Operators:** Up/Down arrows for expansion/compression applied across all modes (AppendixA_Unified_2:L238-470).
3. **Dialectical Engine Schema:** `Box_S^down -> A -> Diamond_S^up -> Box_S^up` from Phenom_Modal:L5-120.
4. **Zeeman Catastrophe Mapping:** Relationship between finite/infinite tension and catastrophe release (AppendixA_Unified_2:L721ff; Phenom_Modal:L89-150).
5. **Trace/Subject Handling:** Not explicit in LaTeX; include in final synthesis as interpretive bridge to Prolog attr_unify_hook requirements.
6. **Norm/Objective Interaction:** Rödl/Habermas explanation plus Arrow notation for retroactive recognition (Appendix_Phenomenology:L419-463).
7. **Exercise Rhythm:** Sense-certainty through Absolute Knowing narrative to ground readers (verbose_exercise entire structure).

## Redundancies to Drop
- Duplicate literature review paragraphs in AppendixA_Literature_Connection and AppendixA_Revised (hash list in tex_analysis_report lines 224-317).
- Repeated operator definitions across AppendixA_Revised, AppendixA_Unified, Appendix_Phenomenology; keep clearest version (Unified_2) and rephrase rest.
- Extra minipage/longtable scaffolding; convert summary table to markdown.
- Multi-iteration "Formalizing Chapter" subsections; replace with concise bullet list referencing main manuscript.

## Planned Section Order for Markdown Output
1. Abstract & Quick Start vignette
2. Commitment Map (three modes + operator legend)
3. Philosophical Grounding (Hegel → Derrida) with clear citations
4. Formal System (axioms, operator algebra, deferral/resistance axiom referencing Trace)
5. Dialectical Engine walkthrough (Consciousness → Spirit) integrated with Phenom_Modal diagrams
6. Embodied Exercises (condensed from verbose_exercise)
7. Implementation Bridge (summary of how Prolog engine encodes commitments, placeholders for attr_unify_hook + calculator tests)
8. Outlook for interactive interface (Phase 3 hook)

This outline now drives the synthesis draft.
