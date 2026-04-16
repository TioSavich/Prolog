# umedcta-formalization — Project Instructions for Claude

## What this repository is

This is the Prolog and manuscript formalization archive for *Understanding Mathematics as
an Emancipatory Discipline: A Critical Theory Approach* (UMEDCTA) — a philosophical
manuscript by Tio Savich.

This repo was split from `TioSavich/UMEDCTA` in March 2026. The formalization work
belongs here; the public-facing portfolio tools belong in `TioSavich/umedcta-portfolio`.
The design doc for that decision is at
`~/.gstack/projects/TioSavich-UMEDCTA/tio-main-design-20260319-114754.md`.

## What the Prolog actually does

The core system models **how a learner acquires arithmetic strategies through crisis**.
A computational agent starts with tally marks and nothing else. When it hits a problem
it cannot solve within resource limits, it enters crisis — and must either consult an
oracle (an external source of strategies) or synthesize a new strategy from primitives.
This is the **ORR cycle** (Observe, React, Reorganize):

1. The **meta-interpreter** (`meta_interpreter.pl`, `solve/6`) attempts computation
   using known strategies. This is the "computing" layer.
2. When computation fails or exhausts resources, the **execution handler**
   (`execution_handler.pl`) classifies the crisis (efficiency, unknown operation,
   normative violation, incoherence) and initiates recovery.
3. The **oracle** (`oracle_server.pl`) provides strategies the learner hasn't discovered
   yet — like asking a teacher. But oracle-backed strategies are philosophically hollow:
   the learner memorized a phone number, not a method.
4. The **FSM synthesis engine** was where the learner would build its own strategies
   from ENS primitives. It never worked as intended (wrapped oracle calls instead of
   genuine synthesis). Archived in Phase 1; `fsm_synthesis_engine.pl` is now a utility
   shim providing only `peano_to_int/2` and `int_to_peano/2`.
5. The **incompatibility semantics** module (`incompatibility_semantics.pl`, `proves/1`)
   is the "justifying" layer — a sequent calculus that tracks inferential commitments.

**Do not merge solve/6 and proves/1.** They model two distinct activities the manuscript
distinguishes: computing and justifying. Merging them conflates what the system can *do*
with what it can *give reasons for*.

## The arche-trace erasure boundary

The sequent calculus has a precise boundary where formal proof goes hollow. When sequent
variables carry the `arche_trace` attribute, the prover returns `erasure(...)` instead
of `proof(...)`. The derivation succeeds structurally but the proof object contains no
content. This happens at exactly the points where human judgment (teacher, LLM, or
interpretive analysis) must take over: identity with Trace, S-O Inversion, double
negation elimination, Oobleck transfer. These erasure points are documented in
`arche-trace/ARCHE_TRACE_ERASURE.md`.

This is not a bug. The erasure marks the skeleton boundary — where the formalism
honestly stops being able to say anything.

## Polarized Modal Logic (PML)

The formal vocabulary is PML: 12 operators (3 modes × □/◇ × ↑/↓).

- Three modes of validity: **Subjective** (S-mode), **Objective** (O-mode),
  **Normative** (N-mode)
- Two polarities: **compressive** (□, necessity, ↓) and **expansive** (◇, possibility, ↑)
- Discourse markers serve as empirical proxies: categorical language → compression,
  hedging/surprise → expansion, self-referential → S-mode, child-focused → O-mode,
  framework references → N-mode

All PML core tests pass (28/28). The LaTeX appendices connecting this to the manuscript
are in `pml/Modal_Logic/`, especially `AppendixA_Unified_2.tex`.

## Where this fits in the three-project pipeline

This repo is the middle layer of a pipeline:

- **Data_Cleaner** (separate repo) — strips curricular template from student TAI
  analyses, tags curriculum, isolates the reflective residue where identity-relevant
  discourse lives. Material subtraction.
- **This repo** — provides the logical grammar (PML) for what the subtraction reveals,
  and proves the vocabulary is internally consistent. The Prolog models reasoning
  strategies; the LaTeX connects them to manuscript claims.
- **umedcta-portfolio** (separate repo) — public-facing interactive tools that enact
  the same structures for an educator audience. The portfolio's More Machine, Hermeneutic
  Calculator, and other tools are experiential companions to what the Prolog formalizes.

The deeper motivation: Tio's students were not talking to each other in ways
commensurate with what they had to offer. AI made it possible to design structured
encounters at scale while protecting students from direct AI interaction. The
contradiction — using AI to foster human connection — is the point.

## What's here (directory map)

Five modules, each with its own directory, tests, and README:

- `formalization/` — Robinson Q and the meta-mathematical signal. `robinson_q.pl`
  (self-contained, 20 tests), grounded arithmetic, ENS operations.
- `strategies/` — children's arithmetic as FSMs. 27 strategy automata in `math/`,
  20 Indiana K-3 standards modules in `standards/` (153 tests), FSM engine,
  hermeneutic calculator.
- `pml/` — PML as discourse analysis instrument. 12 operators, person-position
  markers, axioms, LaTeX appendices and philosophical dictionary in `Modal_Logic/`.
  28 core tests passing.
- `arche-trace/` — where formal proof goes hollow. The sequent calculus engine
  (`incompatibility_semantics.pl`) includes axiom sets from formalization/,
  pml/, and learner/ via `:- include(...)`. Also: embodied prover,
  critique/dialectical engine, erasure boundary documentation.
- `learner/` — crisis cycle constructivist module. Meta-interpreter (`solve/6`),
  execution handler, oracle/teacher server, HTTP server (port 8080), tension
  dynamics, event log.

Cross-module interfaces (Phase 3):

- `interfaces/` — five documents making explicit philosophical claims about how
  modules connect. Each document states its claim, grounds it in code, marks
  where the claim exceeds formalization, and proposes what would strengthen it.
  - `pml-to-discourse.md` — PML operators to observable discourse markers
  - `compression-across-layers.md` — shared compression/expansion polarity
  - `strategies-to-formalization.md` — strategy automata to Robinson Q
  - `arche-trace-to-learner.md` — where the learner's formalization breaks
  - `pml-to-learner.md` — PML modulates inference cost (the one wire)

Supporting directories:

- `paths.pl` — SWI-Prolog `file_search_path` configuration for cross-module imports
- `archive/` — vestigial material from Phase 1 (synthesis engine, old docs, phase tests)
- `design/` — 9 design documents (00_PROJECT_OVERVIEW through 08_PRUNING)
- `docs/` — specs, research investigations, consolidation map, literature bridge
- `more-zeeman/` — 11 HTML pages + `more-machine.js`, `shared.js`. External-facing
  visualization layer. Currently only `bridge.html` and `fractal.html` call the
  learner HTTP server; the other nine are standalone. Potential: a slick
  public-facing deontic scoreboard for the whole project (see stream 3 below).
- `data/learningcommons/` — query scripts for the LearningCommons standards API
- `SCENE_LEVEL_ARCHITECTURE.md` — hypothetical scene-level map (see below)

## Active branches

`main` carries all current work. `standards-elaboration` has been fully folded
in. Other branches (`claude/add-more-machine-SRy5j`,
`claude/review-strategy-connections-TVMYU`) are older Claude-generated
branches that haven't been pruned; check before reusing.

## Robinson Arithmetic and the incompleteness argument

As of April 2026, all seven Robinson axioms (Q1-Q7) are formalized as `proves_impl`
rules in `formalization/axioms_robinson.pl` (included by the sequent engine in
`arche-trace/incompatibility_semantics.pl`). The standalone extraction
`formalization/robinson_q.pl` contains a self-contained proof with a full test harness
(20/20 passing). The full system tests (22/22) also pass.

**What is established:**
- Zero: `is_recollection(0, [axiom(zero)])` — axiomatically grounded
- Successor: `succ(X)` grounded as X+1 via `is_recollection`
- Addition: `plus(X,Y,Z)` derivable via `arith_op` through the HC's grounded layer
- Multiplication: `mult(X,Y,Z)` derivable via `arith_op` (extended for `*` in April 2026)
- Q1 (S(x)≠0): expressed as `is_incoherent`
- Q2 (S injective): correct sequent conditional — from premise S(x)=S(y), conclude x=y
- Q3 (x=0 or x=S(y)): produces a structural witness, not an opaque proposition
- Q4-Q7: recursive definitions of + and * verified through `arith_op`

**What remains open (see `archive/FORMALIZATION_ASSESSMENT.md`):**
- Gödel numbering — the `arche-trace/automata.pl` prime utilities are not wired to syntax encoding
- A Gödel sentence for this specific system has not been constructed
- The meta-theorem ("every theorem of Q is a theorem of the HC") is not formally proved
- The triumphant framing in `robinson_q.pl`'s test output overstates schematic instances

**Do not change the Q2 rule** to add an `X =:= Y` guard. That would make the rule
tautological (encoding identity, not injectivity). A conditional with a false antecedent
is vacuously true in sequent calculus — this is correct.

**`arith_op/4` now handles `*`** (extended April 2026 alongside the Robinson work).
Q5, Q7, and the multiplication grounding route through `arith_op` rather than `is/2`.
All three also use `once/1` on `is_recollection` calls to prevent FSM backtracking loops.

## Current status and active frontiers

Core system stable. 100+ tests pass across all five modules. PML core 28/28,
Robinson Q 20/20, all 27 strategy automata grounded, arche-trace prover and
scene-agnostic engine coexist as distinct modules. HTTP server on port 8080
(`swipl -l paths.pl -l learner/server.pl`). Firebreaks hold at the module
level; cross-module interfaces documented in `interfaces/`.

Phase 1 (archive) through Phase 4 (test independence) are done. Phase 5
began with a code-docs coordination audit (2026-04-16): six fact-sheets
at `docs/phase5/fact-sheet-<module>.md` written directly from stripped
code, six audit logs recording 24 aspirational-layer relocations to
`archive/<topic>/`, and a rollup at `docs/phase5/audit-summary.md`. The
fact-sheets are now the code-truth reference for any Phase 5 product;
the aspirational material was ruthlessly cut against each module's
one-sentence research goal. The six cross-module gaps the audit
surfaced are documented in the summary and are open issues, not bugs
to fix before shipping.

### Three converging streams

Work now faces three simultaneous directions. Any one of them could
flatten the others if chosen carelessly; the sequencing matters.

1. **Import/export with scattered repos.** LK_RB_Synthesis has real
   material (MUA framework, AutomatonAnalyzer with 87 elaboration
   relationships, RegisterMachine, deontic scorekeeper, strategy metadata
   JSON, image schemas) — discriminate per-item rather than porting
   wholesale. The 2525-inference corpus in UMEDCA_2026 is not imported.
   See `docs/consolidation-map.md` for the item-by-item status.

2. **Academic exposure.** PML is the leading candidate for near-term
   review and testing. Getting it in front of readers without locking
   the module into "feasibility paper shape" is the design question.
   Phase 5 as written leans toward closure; exposure without closure
   would preserve room for meaning fields, decentering, and the
   operators beyond the polarity wire.

3. **Intra-module connection + external interface.** The strategies
   module is currently a flat list; the AutomatonAnalyzer idea would
   give it a queryable elaboration graph. The `more-zeeman/` HTML
   suite has 11 pages but only `bridge.html` and `fractal.html` call
   the learner API. The vision is an external-facing deontic
   scoreboard linking PML, Zeeman catastrophe dynamics, and kids'
   strategies as views of one system for non-Prolog readers.

The streams interact. The AutomatonAnalyzer port serves both 1 and 3.
PML HTTP endpoints serve both 2 and 3. An external interface makes
academic exposure (stream 2) more durable because reviewers see PML
acting, not just described.

### Fraction crisis

The system handles whole-number arithmetic through the ORR cycle.
Fractions remain open (`strategies/FRACTION_CRISIS_ASSESSMENT.md`):
variable base, three-level unit coordination, metamorphic accommodation
(recursive strategy composition rather than flat FSMs). The key open
question is philosophical: what does "encountering a fraction" mean for
a system that started with tally marks? Grade 3 `standard_3_ns_2.pl`
models fraction concepts but does not integrate with the ORR cycle
(`fraction/2` is structurally incompatible with `recollection/1`).

## Running the system

- **Interactive server**: `swipl -l paths.pl -l learner/server.pl` → http://localhost:8080
  Dark-themed timeline visualization of ORR events. Three API endpoints:
  POST /api/compute, GET /api/strategies, GET /api/events.
  Must load `paths.pl` first for cross-module imports to resolve.
- **Known issue**: `learner/config.pl` uses global mutable state (`retractall/assertz`) that
  will race under concurrent requests. Fine for single-user local dev.
- **Peano/recollection representations are intentionally slow** for large numbers.
  `add(100, 50)` with high inference limits will be slow. The slowness triggers crisis —
  that is by design.

## Status: research archive

This material is not portfolio-ready and should not be presented as such. The Prolog
formalizations model specific reasoning structures — they do not implement Hegelian
dialectics or post-structural insights in any philosophically overreaching sense. The
interesting thing is where the formalizations fail or oversimplify; that breakdown is the
point of contact with the manuscript's central argument.

## The central argument (context)

The manuscript's claim: formalisms break productively (the Hegelian Infinite) when they
are *consistent* under Brandom's interpretation of Kant's synthetic unity of
apperception. The Prolog work is not a proof of this claim — it is a representation that
can be made to behave like the relevant reasoning strategies under controlled conditions.
The interesting thing is where the representation fails.

## Scene-level architecture (Carspecken 1999)

`SCENE_LEVEL_ARCHITECTURE.md` (root) proposes hypothetical correspondences between
Carspecken's Four Scenes and the codebase. **These are research questions, not claims.**
The ORR cycle is a formal flattening of what Carspecken describes phenomenologically.
The correspondences should be treated as places to look for productive failure — where
the formal model gets Carspecken wrong in ways that teach us something about both the
formalism and the phenomenology.

Do not assert that any code module "is" or "enacts" a Carspecken scene. Frame
connections as questions or hypotheses. Where the mapping breaks is where the research
begins.

The scene-level document also describes a planned **Carspecken Philosophical Concordance**
— a skill that would ground Carspecken's terms in his texts, trace his citation network
(Habermas, Mead, Buber, Hegel, Bhaskar, Nishida), and test the proposed connections
between his vocabulary and the code. Source texts are at
`/Users/tio/Desktop/Readings/Carspecken/`, including retyped machine-readable versions
of *Four Scenes* and *Limits of Knowledge*. The concordance work runs through VS Code
with NotebookLM MCP for RAG against primary sources.

## Voice commitments

When writing documentation, README files, or comments in this repo:

- No puffery: `A powerful framework for...` → describe what it actually does
- No overclaiming: never say the code "implements" or "demonstrates" Hegelian or
  post-structural concepts in a philosophically significant sense
- Epistemic humility: the formalization is a tool for noticing where formalization stops

The full **epistemic-code-voice** skill (`/epistemic-code-voice`) applies to any
user-facing prose (README, docs). It does not apply to Prolog or Python code logic.

## Cruft discipline (Claude provides this; Tio sometimes lacks it)

Ideas outrun reality in this repo. Claude's job is to refuse aspirational
accumulation, not enable it. Apply these rules before writing or accepting
any new markdown:

- **Only describe code that exists.** If the code isn't written yet, the doc
  doesn't describe it in present tense. "Does" is reserved for what the
  stripped code does today. "Will" or "would" belong in a clearly-marked
  proposal file, not in a module README.
- **No aspirational files in module directories.** `pml/`, `arche-trace/`,
  `formalization/`, `learner/`, `strategies/`, `interfaces/` hold only
  docs that describe what those modules do today. Research agendas,
  future designs, and "what we're thinking about" go in `docs/research/`
  or `docs/proposals/` with a banner marking the register.
- **Flush, don't accumulate.** If an old doc is wrong, edit it in place or
  `git mv` it to `archive/<topic>/`. Never write a new doc describing what
  the old doc got wrong. Never add a banner saying "see newer doc X for
  the current view." Either fix it or archive it.
- **Fact-sheets are the code-truth reference.** Before writing any claim
  about what a module does, consult `docs/phase5/fact-sheet-<module>.md`.
  If the claim exceeds what the fact-sheet says, either cut the claim
  or update the fact-sheet with an explicit re-audit note. Drift between
  a paper draft / README / interface doc and the fact-sheet is a flag
  that the drifting doc is overclaiming.
- **No onion layers.** A doc that says "the previous doc got X wrong, here
  is the corrected view" creates the onion. Instead: fix X in the place
  it was wrong, or archive the wrong doc wholesale. The only layered
  doc allowed is the audit log itself, which records decisions by file
  for future reference.
- **If Tio proposes a new doc, check:** Does it describe existing code?
  Does it propose new code? Is it commentary? Each answer routes
  differently. "Describes existing code" is the only register that belongs
  in a module directory. The other two go in `docs/proposals/` or get
  a commentary banner.
- **When in doubt, cut.** The "cut, not defer" policy that ran the Phase 5
  audit is the standing rule. Mark-and-defer grows the onion; archive is
  always recoverable from git history.

These rules apply to Claude's own drafts too. Claude should push back when
asked to generate docs that describe planned-but-unwritten code as if it
exists, or that layer commentary on top of already-stale docs.

## gstack

Use the `/browse` skill from gstack for all web browsing. Never use
`mcp__claude-in-chrome__*` tools.

Available gstack skills:
- `/office-hours` - Office hours discussion
- `/plan-eng-review` - Engineering review planning
- `/review` - Code review
- `/ship` - Ship a change
- `/browse` - Web browsing
