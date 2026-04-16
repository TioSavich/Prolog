# Audit log: pml

Written after [docs/phase5/fact-sheet-pml.md](fact-sheet-pml.md). The research goal from the fact-sheet is the cut criterion. Voice follows `SCENE_LEVEL_ARCHITECTURE.md`: correspondences are hypotheses; a section that does not serve the goal gets relocated, not reinterpreted.

## Cut criterion

> PML supplies a twelve-operator modal vocabulary (three modes crossed with four polarised modalities) whose structural theorems and dialectical transitions are exercised by the `tests/` suite against the arche-trace embodied prover; the mapping from these operators to observable discourse is proposed in the surrounding prose, not empirically coded.

Files that make the proposal clearly as proposal ("interpretive, not legislative") serve the goal as **commentary**. Files that assert the mapping is empirically secured, or that belong to other modules / the manuscript proper, are **cut**. No file in this directory makes an unsupported code-mechanics claim that could be rescued by further work, so the **substantiate** verdict is unused in this pass.

## Decisions

| File | Section / scope | Claim | Verdict | Rationale |
|---|---|---|---|---|
| `pml/Modal_Logic/README.md` | whole file | Inventory of the `Modal_Logic/` directory: lists the appendix revisions, dictionaries, and diagrams. | keep | Describes what sits in the directory; no mapping claim. Needs a banner noting it is commentary on the directory, not on the pml code. |
| `pml/Modal_Logic/Philosophical_Primer.md` | whole file | The "geometry of the self" story: mathematics as embodied, negation as inversion, numbers as recollections of the "I". | commentary | Makes no claim about what the pml code does. Philosophical framing for the vocabulary the code supplies. Banner needed. |
| `pml/Modal_Logic/Description_Role_of_Formalization.md` | whole file | Reads the Hermeneutic Calculator and Incompatibility Semantics as a liberatory dialectic of alienation and expression. | commentary | Framing essay. Mentions Highlander, Persistence, and the incompatibility axioms, but does not claim the pml module implements these beyond what the code actually carries. Banner needed. |
| `pml/Modal_Logic/Overview_of_UMEDCA_Manuscript.md` | whole file | Executive summary of the UMEDCA manuscript's argument. | commentary | About the manuscript, not the pml code. Stays because Tio reviews the mapping from this summary to the module. Banner needed. |
| `pml/Modal_Logic/Dictionary.md` | whole file | Alphabetical philosophical dictionary (578 lines) grounding Brandom/Carspecken/Hegel/Mead terms used across the project. | commentary | Cross-project reference per memory (`reference_vocabulary_dictionary.md`). Does not describe pml code, but enforces terminological consistency where the code reads these terms (e.g., `identity_claim`, `i_feeling`, `forgiveness`). Banner needed. |
| `pml/Modal_Logic/Dictionary_voice_edit_enhanced.tex` | whole file | LaTeX-formatted version of `Dictionary.md` for book typesetting. | cut | Duplicate content of `Dictionary.md`. The markdown is the source; the `.tex` is a typesetting intermediate that belongs with manuscript build artefacts, not with the pml module. |
| `pml/Modal_Logic/AppendixA_Unified_2.tex` | whole file (the "current" appendix per README) | Polarised modal logic for Hegel's *Phenomenology*; three modes crossed with four polarised modalities; chapter-by-chapter mapping to the manuscript; Geist as the realisation of S=O via N. | commentary | Its own abstract flags the mapping as "formal system... purpose is interpretive, not legislative" — which is what the research goal allows. No code-mechanics claim is asserted that the pml module does not carry. Banner needed. |
| `pml/Modal_Logic/AppendixA_Unified.tex` | whole file | Earlier unification attempt (README notes `Unified_2` supersedes this). | cut | Superseded duplicate. History preserved in git. |
| `pml/Modal_Logic/Appendix_Phenomenology.tex` | whole file | Earlier/intermediate variant covering the same modal framework plus the Hegel *Phenomenology* reconstruction. | cut | Superseded by `Unified_2`. Missing the "Applications to Main Text Chapters" section present in `Unified_2`. History preserved in git. |
| `pml/Modal_Logic/AppendixA_Revised.tex` | whole file | Earlier revision of the appendix (README marks it "earlier revision"). | cut | Superseded. History preserved in git. |
| `pml/Modal_Logic/AppendixA_Literature_Connection.tex` | whole file | Literature connection draft: Hegelian dialectics, inferentialism, Derrida on presence. | cut | Draft; content has been absorbed into the unified appendix. History preserved in git. |
| `pml/Modal_Logic/UMEDCA_Concatenated_fixed.tex` | whole file (5211 lines, 8 chapters) | Full concatenated UMEDCA manuscript. | cut | The manuscript as a whole is not a pml artefact; its presence here predates the module split. Belongs with the manuscript source. |
| `pml/Modal_Logic/ch2_revised.tex` | whole file | Chapter 2 of the manuscript, revised. | cut | Manuscript chapter, not a pml artefact. |
| `pml/Modal_Logic/Jason.tex` | whole file | Article "A Pragmatic Re-Keying of Radical Constructivism": Jason's fractional schemes as nested automata, Steffe's ENS as primitive operations. | cut | Topic belongs to `strategies/` and fractions; the pml module does not touch Jason, ENS, or fractional automata. |
| `pml/Modal_Logic/Jason.pdf` | companion PDF | Compiled Jason.tex. | cut | Follows Jason.tex. |
| `pml/Modal_Logic/counting.tex` | whole file | Pushdown automaton for three-digit base-10 counting. | cut | Topic belongs to `strategies/` (the counting/place-value curriculum). No PML content. |
| `pml/Modal_Logic/counting.pdf` | companion PDF | Compiled counting.tex. | cut | Follows counting.tex. |
| `pml/Modal_Logic/jason_automaton_picture.tex` | whole file | TikZ diagram of Jason's automaton. | cut | Diagram for the Jason material; not PML. |
| `pml/Modal_Logic/jason_automaton_picture.pdf` | companion PDF | Compiled TikZ diagram. | cut | Follows the `.tex`. |
| `pml/Modal_Logic/Carspecken2013aa.pdf` | reading | External Carspecken paper. | cut | Reading material, not a pml artefact; belongs with the user's Readings folder, not in the module tree. |
| `pml/Modal_Logic/Pages from 102924_Savich_Dissertation.pdf` | reading | Extracted pages of Tio's dissertation. | cut | Dissertation excerpt, not a pml artefact. |

## Relocations

All `cut` rows move to `archive/pml-modal-logic/` with the same filenames. The `keep` / `commentary` rows stay in `pml/Modal_Logic/` and receive the commentary banner at the top.

- `pml/Modal_Logic/Dictionary_voice_edit_enhanced.tex` → `archive/pml-modal-logic/` — LaTeX duplicate of Dictionary.md.
- `pml/Modal_Logic/AppendixA_Unified.tex` → `archive/pml-modal-logic/` — superseded by `Unified_2`.
- `pml/Modal_Logic/Appendix_Phenomenology.tex` → `archive/pml-modal-logic/` — superseded.
- `pml/Modal_Logic/AppendixA_Revised.tex` → `archive/pml-modal-logic/` — superseded.
- `pml/Modal_Logic/AppendixA_Literature_Connection.tex` → `archive/pml-modal-logic/` — draft absorbed into unified appendix.
- `pml/Modal_Logic/UMEDCA_Concatenated_fixed.tex` → `archive/pml-modal-logic/` — manuscript, not a pml artefact.
- `pml/Modal_Logic/ch2_revised.tex` → `archive/pml-modal-logic/` — manuscript chapter, not a pml artefact.
- `pml/Modal_Logic/Jason.tex` → `archive/pml-modal-logic/` — strategies/fractions material.
- `pml/Modal_Logic/Jason.pdf` → `archive/pml-modal-logic/` — companion to Jason.tex.
- `pml/Modal_Logic/counting.tex` → `archive/pml-modal-logic/` — strategies material.
- `pml/Modal_Logic/counting.pdf` → `archive/pml-modal-logic/` — companion to counting.tex.
- `pml/Modal_Logic/jason_automaton_picture.tex` → `archive/pml-modal-logic/` — Jason diagram.
- `pml/Modal_Logic/jason_automaton_picture.pdf` → `archive/pml-modal-logic/` — companion.
- `pml/Modal_Logic/Carspecken2013aa.pdf` → `archive/pml-modal-logic/` — reading, not artefact.
- `pml/Modal_Logic/Pages from 102924_Savich_Dissertation.pdf` → `archive/pml-modal-logic/` — dissertation excerpt.

## Deletions

None. Everything cut is relocated rather than deleted.

## Commentary banner

The six files kept in place receive a banner at the top:

> **Commentary — philosophical reflection, not code-report.** This document extends or frames the vocabulary the pml module defines. It does not describe what the Prolog proves. See [fact-sheet-pml.md](../../docs/phase5/fact-sheet-pml.md) for the code-backed claims.

Files receiving the banner: `README.md`, `Philosophical_Primer.md`, `Description_Role_of_Formalization.md`, `Overview_of_UMEDCA_Manuscript.md`, `Dictionary.md`, `AppendixA_Unified_2.tex`.
