# Strategies + Formalization — Dissertation Chapter / GitHub Appendix

**Modules:** `strategies/` and `formalization/`
**Target:** manuscript companion — dissertation chapter or GitHub appendix
for *Understanding Mathematics as an Emancipatory Discipline: A Critical
Theory Approach*
**Claim the chapter would make:** Children's invented arithmetic strategies,
modeled as finite-state machines over grounded recollection primitives,
generate computational machinery rich enough to interpret Robinson
Arithmetic's axioms Q1–Q7. This places the project in a position to *raise*
Gödel's incompleteness question — not to prove it, but to insist that
children's arithmetic, taken seriously, has already crossed the threshold
where the question becomes meaningful.

---

## What the modules actually provide

### `strategies/`

Twenty-seven strategy automata cover the CGI hierarchy. Addition (5),
subtraction (9), multiplication (4), division (4), fractions (3), and two
counting DPDAs. All 27 are grounded: zero `is/2` in strategy code; every
arithmetic operation routes through recollection-based counting via
`formalization/grounded_arithmetic.pl` and `grounded_utils.pl`. The April
2026 grounding work (commits `854a242`, `746ee56`, `8b6dabe`) closed the
last twenty lazy automata. `GROUNDING_STATUS.md` documents per-file
conversion.

Twenty standards modules cover Indiana K–3. `strategies/standards/` has
153 subtests; all pass. The K.NS, K.CA, 1.CA, 1.NS, 2.CA, 2.NS, 3.CA, and
3.NS modules exist and load cleanly.

The fractal architecture is partially present. The Iterative Core (grounded
arithmetic primitives) and the Strategic Shell (the 27 automata) are
structurally distinct. The LK_RB_Synthesis framing in
`docs/consolidation-map.md` names this as "choreography" — each automaton
as a written script for the temporal unfolding of a strategy. What is
missing is the elaboration graph: the 87 algorithmic elaboration
relationships that the AutomatonAnalyzer (in LK_RB_Synthesis) would
discover if ported. Those edges would make the fractal claim
demonstrable rather than programmatic.

### `formalization/`

Robinson Q is formalized. `formalization/axioms_robinson.pl` encodes Q1–Q7
as `proves_impl` rules in the sequent engine. The standalone harness
`formalization/robinson_q.pl` is self-contained (no external dependencies)
with 20/20 tests passing. The full-system tests also pass.

Arithmetic is grounded. Zero is `is_recollection(0, [axiom(zero)])`.
Successor, addition, and multiplication are derivable via `arith_op` through
the hermeneutic calculator's grounded layer. Multiplication was extended
in April 2026 (also under commits `854a242`/`8b6dabe`) so Q5 and Q7 route
through `arith_op` rather than Prolog's built-in `is/2`.

Q3 produces structural witnesses. Previous versions returned an opaque
proposition; the current version returns `eq(X, 0)` or `eq(X, succ(Y))`
for concrete Y. Q2 (successor injectivity) is correctly encoded as a
conditional sequent — the rule must not be changed to add an `X =:= Y`
guard, since that would collapse injectivity into identity (see CLAUDE.md).

## What a manuscript companion needs that does not yet exist

**A plain-language account of what the Prolog argues.** Tio cannot read
Prolog directly; neither can most readers of a dissertation chapter in
mathematics education. The chapter needs to explain — in prose and
diagrams — what it means for counting-on to "interpret" addition, what the
relationship is between `is_recollection/2` and axiomatic succession, and
why Q1 is a natural consequence of how the system builds numbers rather
than a separate commitment. None of this currently lives in a form a
non-Prolog reader can access.

**An honest status section.** `archive/FORMALIZATION_ASSESSMENT.md` already
drafts this — what is established, what remains open, and why the LaTeX
manuscript's earlier claims ("I proved the system interprets Robinson
Arithmetic," "the Gödel sentence exists") overstate what the code delivers.
The chapter version of that section needs to live in the chapter, not in
`archive/`. The earlier claims have to be retracted in the chapter, clearly.

**The meta-theorem, if it is to be had.** The claim "every theorem of Q is
a theorem of the HC" is not formally proved. It is supported by the
observation that all 20 Robinson tests pass and that the axioms route
through the HC's grounded arithmetic layer. But that is evidence, not
proof. The chapter can either: (a) admit this gap in the limitations
section and carry on, or (b) attempt the meta-theorem as part of the
chapter work. (b) is substantial and may not be within the chapter's
scope. (a) is cheap and honest.

**Gödel numbering and a Gödel sentence.** Not present. `arche-trace/automata.pl`
has prime utilities, but nothing connects them to a syntactic encoding of
the system's own proofs. The chapter must say this plainly: the project is
in a position to raise the incompleteness question, not to instance it.

**The fractal-architecture argument.** The CLAUDE.md and README talk about
Iterative Core + Strategic Shell as a formal demonstration of self-
similarity. The chapter would need to either demonstrate the self-similarity
(via the AutomatonAnalyzer port producing an elaboration graph) or drop the
word "fractal" and describe what is actually there (a layered architecture
with primitives and compositions). The demonstration is substantial work;
the descriptive version is a footnote. Which to take depends on whether
the fractal framing is load-bearing for the dissertation argument.

**The Franzen-respecting frame.** `archive/FORMALIZATION_ASSESSMENT.md`
documents the concern that the LaTeX manuscript inflates beyond what the
code supports. The chapter needs to address Franzen's critique of
non-mathematical applications of Gödel's theorem directly. The move is:
the project applies the theorem to a computational system that *is*
mathematical (the automata, the sequent calculus, the grounded arithmetic),
not to "human thought" in general. The Hegelian reading is structural,
not Gödelian. This needs to be stated in the chapter's opening pages.

## Where the claim would exceed what is formalized

"Interprets Robinson Arithmetic" is doing a lot of work in the claim above.
What is true: the sequent calculus has `proves_impl` rules that, when
queried, succeed on all seven Robinson axioms. What is not proved: that
the sequent calculus's deductive closure contains all theorems of Q.
What is not constructed: a Gödel sentence or numbering for the system.

The chapter cannot use "interprets" without defining it. A defensible
definition would be: for each axiom of Q, there is a derivation in the
sequent calculus that uses the automata's primitive operations to witness
the axiom's instance. That is weaker than the standard metamathematical
sense of "interpretation" but it is what the code actually delivers.

The chapter's central philosophical move — that children's arithmetic,
taken seriously, raises the incompleteness question — does not require
the stronger sense. What it requires is that the computational machinery
be rich enough to express the axioms. It is. The Gödel question is about
what *else* the machinery can express (self-reference, provability
predicates, diagonal construction), and that machinery has not been built.

## The single largest risk

The LaTeX manuscript already makes the inflated claims. The chapter's
difficulty is not getting to the honest claim — it is rolling back from
the inflated claim. A reader who has seen the LaTeX language ("I proved
the system interprets Robinson Arithmetic") will register the chapter's
narrower claim as a retreat. The retreat has to be framed as a tightening,
not a loss: the code is better than the earlier manuscript said, because
it does what it claims rather than more than it can.

The second risk is structural. The chapter needs to stand on its own but
also work as an appendix-with-code for readers who want to see the Prolog.
These are two different documents. A chapter reads as an argument; an
appendix reads as a reference. Trying to be both yields a chapter that is
too technical for the dissertation and too narrative for an appendix. The
cheap fix is to write the chapter as the argument and keep the appendix
as a separate file in the repo that is simply linked from the chapter.

## Concrete next moves (if this product is prioritized)

1. **Lift the honest status section into the chapter.** The content lives
   in `archive/FORMALIZATION_ASSESSMENT.md`. The chapter needs a version
   of §2 ("what the repo actually formalizes") and §3 (the Franzen
   response). Three to four pages.

2. **Write the plain-language account of the grounded arithmetic.** What
   is a recollection? Why is zero an axiom? What does it mean for
   successor to be "grounded" vs. "primitive"? One to two pages, with
   one diagram showing the stack: axiom → grounded arithmetic → automata
   → Robinson rules.

3. **Define "interprets" explicitly.** The chapter's first substantive
   paragraph after the intro. Half a page. Anchor it to what the
   `proves_impl` rules actually do.

4. **Decide on the fractal-architecture framing.** If the AutomatonAnalyzer
   port is happening, do it first and let the chapter reference the
   elaboration graph. If it isn't, drop the word "fractal" from the chapter
   and describe the architecture as a two-layer system with primitives
   and compositions. A footnote can gesture at the self-similarity claim
   without committing to it.

5. **Write the Franzen frame up front.** The chapter's second or third
   paragraph should acknowledge Franzen directly: this project does not
   apply Gödel to human thought, society, or meaning; it applies the
   theorem to a computational system whose arithmetic structure meets
   the theorem's preconditions. The philosophical claims the project does
   make (dialectical, Hegelian) are structural analogues that the system
   makes visible, not consequences of Gödel's theorem.

6. **Separate the appendix.** The code-level reference — module layout,
   test files, how to run the Robinson Q harness — belongs in a separate
   GitHub appendix (a single `docs/appendix.md` or similar), linked from
   the chapter but not embedded in it.

## What this product does not force

The strategies and formalization modules are already serving the rest of
the project — the learner module uses them, the arche-trace module imports
the Robinson axiom set, the pml module shares their grounded arithmetic.
The chapter writes *about* the modules; it does not require them to
change. The AutomatonAnalyzer port is orthogonal to the chapter: if it
happens, the chapter can cite it; if it doesn't, the chapter proceeds
without it.

## What the limitations section of the chapter would say

Honest draft language, for reference:

> This chapter claims that the strategy automata and grounded arithmetic
> together produce a computational system in which the seven Robinson
> axioms Q1–Q7 are derivable as sequent-calculus rules. This is weaker
> than the standard metamathematical sense of "interpretation." I have
> not constructed a Gödel numbering for this system, have not formulated
> a Gödel sentence, and have not proved the meta-theorem that every
> theorem of Q is a theorem of the hermeneutic calculator. The Franzen
> concern — that Gödel's theorem does not license sweeping claims about
> human thought or meaning — is taken seriously here; the theorem is
> invoked to locate a structural boundary in a mathematical system, not
> to ground a philosophical position the theorem cannot support. The
> philosophical claims this chapter does make (that the system's
> architecture exhibits a dialectical structure analogous to what the
> Hegelian tradition describes) are defended on structural grounds, not
> derived from the incompleteness theorem.

That is the chapter's honest frame. Rolling the earlier LaTeX language
back to this frame is most of the work.
