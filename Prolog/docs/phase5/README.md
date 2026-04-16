# Phase 5 — Honest Assessment of Academic Products

**Date drafted:** 2026-04-16
**Purpose:** Take inventory of what each module is ready to carry into an academic
product, what it is not ready to carry, and what the next concrete move is —
without closing the modules down around any one product.

Phase 5 in `docs/specs/2026-04-14-modularize-and-consolidate.md` names five
products. Each has a file in this folder. The tone throughout is: the modules
are clean, the tests pass, the interfaces are documented; what remains is
almost entirely work *about* the code rather than work *on* it.

## Why this is a folder of assessments rather than a roadmap

The concern Tio raised when first framing Phase 5 was closure. Collapsing each
module into a single publishable shape would defeat the reason the modules
were separated in the first place. A module is a home for a commitment; a
product is a venue-shaped performance of one commitment. The fit between them
is never tight.

These files respect that. Each one names what the product requires, what the
current module provides, what the gap is, and what would have to be said
honestly in a limitations section if the paper were submitted tomorrow. None
of them prescribes a code change inside the module.

## The five products

| File | Module(s) | Product | Venue |
|---|---|---|---|
| [pml-feasibility-paper.md](pml-feasibility-paper.md) | `pml/` | Feasibility paper | MERJ, ESM, or JMTE |
| [arche-trace-essay.md](arche-trace-essay.md) | `arche-trace/` | Philosophical essay | EPT or Studies in Phil & Ed |
| [strategies-formalization-chapter.md](strategies-formalization-chapter.md) | `strategies/` + `formalization/` | Dissertation chapter / GitHub appendix | Manuscript companion |
| [learner-interactive-demo.md](learner-interactive-demo.md) | `learner/` | Interactive demo | Portfolio, conferences |
| [best-day-grant.md](best-day-grant.md) | Best Day (draws on `pml/`) | Grant proposal | NSF DRK-12 or CAREER |

## What cuts across all five

A few things show up in more than one product file. Collecting them here so
they can be addressed once:

- **The representational layer between Prolog and reader.** Tio cannot read
  the Prolog directly. Every product that points at code will need a
  natural-language or diagrammatic representation that a reviewer can follow
  without opening a `.pl` file. The AutomatonAnalyzer port (from
  LK_RB_Synthesis) would do most of this work once, not product by product.
- **The phenomenological grounding of PML.** PML's operator transitions may
  be insensitive to the reality they are meant to track
  (`.auto-memory/pml_uncertainty.md`). Two products lean on this (feasibility
  paper, grant), one essay sits downstream of it (arche-trace), and one demo
  shows the wire at work (learner). Each product will need its own posture
  toward this uncertainty.
- **The intersubjectivity-first commitment vs. the monological learner.** The
  learner module is a constructivist module by its own README — the project's
  center of gravity is intersubjectivity-first. Any product that centers the
  learner has to say this out loud or get pushed back on.
- **Tio's voice commitments.** Epistemic humility, anti-ocular, anti-schlock,
  anti-AI-tell. These apply to every product.
- **The communication bottleneck.** Independent of any product, nothing in
  this repo is currently legible to non-specialists. Fixing that is prior to
  most of the product work.

## What is NOT in these files

- Prescriptions for how the code should change. The modules are the modules.
- Promises that any of these products can be ready on a specific timeline.
  Each file marks what is needed; it does not claim the needed thing is near.
- Triage of which product to write first. That is a decision Tio makes with
  advisors and collaborators, not one to foreclose here.
