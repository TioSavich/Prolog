# Learner Interactive Demo — Honest Assessment

**Module:** `learner/`
**Target:** portfolio tool, conference demo, classroom audience
**Claim the demo would make:** A computational agent that starts with tally
marks and nothing else encounters problems it cannot solve, enters crisis,
and acquires new strategies from a teacher (oracle). The demo shows the
ORR cycle — Observe, React, Reorganize — with modal-context dynamics, a
catastrophe-surface tension accumulator, and a dark-themed timeline
visualization of what the learner does when formal resources run out.

---

## What the module actually provides

The ORR cycle runs. `learner/meta_interpreter.pl` defines `solve/6`, which
handles PML modal operators, cost accumulation, strategy dispatch, and
perturbation throwing. `execution_handler.pl` classifies five crisis types
(efficiency, unknown operation, normative, incoherence, tension
instability) and dispatches to recovery.

The HTTP server works. `learner/server.pl` runs at port 8080. Three API
endpoints: `POST /api/compute`, `GET /api/strategies`, `GET /api/events`.
The frontend is a dark-themed timeline visualization of ORR events.
Loading requires `paths.pl` first: `swipl -l paths.pl -l learner/server.pl`.

The cost model is live. `learner/config.pl` defines seven cost types
(unit_count, slide_step, inference, fact_retrieval, modal_shift,
recollection_step, norm_check) with defined integer values. Compressive
context multiplies cost by 2; expansive by 1. Tension dynamics multiplies
tension growth by 2.5 (compressive) or 0.3 (expansive) over a rolling
20-value window.

The dialectical rhythm is encoded. Commit `0280031` ("Fix shit: delete
dead NO-OPs, wire the critique UI, sync docs to reality") cleaned up
leftover no-ops and wired `run_critique_interaction` to route through
`execution_handler:run_computation/2` directly. A norm-violating
subtraction like `3-5=R` triggers `perturbation(normative_crisis(...))`
and falls into domain expansion. This is what the UI path's name promised
for months; as of April 16, 2026 it actually does it.

Oracle dispatch covers whole-number arithmetic. Addition, subtraction,
multiplication, and division are all wired; 9/10 test files pass in
`learner/tests/` (the tenth, `test_three_wires`, runs 5/5 after removing
the archived-synthesis-engine test).

Tension instability is computable. The second-derivative check in
`tension_dynamics.pl:181-211` fires when stability goes negative with a
9+-entry history window. Under compressive context tension accelerates
faster, so the catastrophe threshold is reached sooner.

## What an interactive demo needs that does not yet exist

**A one-sentence story for a non-programmer.** The project vision memory
(`.auto-memory/project_vision.md`) is clear: "make visible the epistemic
structure of how people come to understand mathematics, and use that
visibility to design teaching that creates conditions for genuine
recognition between learners." The demo has to open with a concrete
reason a teacher or an audience member would watch it. "Here is an agent
that starts with nothing and learns to count" is probably that reason.
The current server page does not frame itself this way.

**Scenarios a viewer can run without knowing Prolog.** The demo currently
assumes the viewer will POST to `/api/compute` with a well-formed goal.
A demo viewer needs preset scenarios: "Try to subtract 5 from 3 (watch
the normative crisis)," "Add 8 + 5 (watch counting-on emerge),"
"Multiply 4 × 6 (watch it ask for a strategy)." Each scenario needs a
button and a narration panel. None of this is built.

**A readable timeline.** The server logs events with enough structure to
be replayed, but the timeline page (as of the last check) shows raw events
rather than a narrative trace. A teacher-facing version needs to label
what is happening ("Ran out of resources → entered crisis → consulted
oracle → learned counting-on") rather than stream `solve/6` inference
data.

**A way in for fractions.** `strategies/FRACTION_CRISIS_ASSESSMENT.md`
names three gaps: no fraction goals wired into the crisis pipeline, no
representation bridge between the Peano/integer world and the
recollection/unit world, no architectural response to the
three-level-unit-coordination requirement. The demo does not need to
solve fractions — but it probably needs a "this is where the system
stops, and here is why" panel when a fraction is asked, so the boundary
is visible rather than silent.

**A visual story for modal context.** The compressive/expansive polarity
is the part of the system most closely tied to PML. The demo could show,
live, the cost multiplier changing as the agent moves between modal
contexts — a compressive-leaning computation turning red as tension
accelerates, an expansive-leaning computation turning green as tension
settles. The data is there; the visualization is not.

**An honest framing of what the demo is NOT.** This is the critical
piece. The learner is a *monological* module — a single-agent model.
The project's core commitments are intersubjectivity-first. Any viewer
who comes away thinking "this agent learns the way children learn" has
misread the demo. The demo needs a visible label: "This is one
computational model of a single agent's crisis dynamics. The project's
pedagogical commitments — recognition between learners, dyadic
discussion, the Best Day intervention — are not modeled here. Those live
elsewhere in the research program."

## Where the claim would exceed what is formalized

The demo's most attractive claim — that you can *watch* a learner acquire
a strategy — overreaches in three ways.

First, the learner does not synthesize strategies from primitives. The
synthesis engine is archived. The oracle provides pre-computed strategies
and the learner records them; there is no genuine construction. The demo
can honestly show strategy *acquisition* (loading a strategy into the
learner's knowledge base) but not strategy *construction* from the
learner's own primitives.

Second, the crisis dynamics are a model, not a measurement of human
experience. Tension accumulation, modal-context multipliers, and the
second-derivative stability check are computational devices. They are
phenomenologically evocative — compressive pressure accelerating
collapse is exactly the felt quality of working under duress — but the
evocation does not make the numbers measurements of anything
phenomenological.

Third, the oracle/teacher analogy softens under pressure. A human teacher
"creates conditions for crisis" by noticing what a student is ready to
work on, scaffolding a problem just beyond current competence, and
responding to how the student is moving through the problem. The
oracle in this module dispatches to a strategy module. The teacher
creates conditions; the oracle delivers answers. The README tries to
resolve this by saying the teacher creates conditions for crisis
(not delivers answers), but the code is currently a dispatcher. The
demo would need to either frame the oracle as a placeholder for the
real teacher (honest) or develop the oracle into something that
responds to the learner's current state rather than pattern-matching
on the goal type (not yet done).

## The single largest risk

The demo is seductive. A working visualization of crisis-driven
learning, with modal context coloring the screen and tension curves
rising, is exactly the kind of thing a conference audience remembers.
The risk is that the memorable thing is the wrong thing. Viewers will
walk away saying "Tio has built an AI that learns arithmetic the way
kids do." They will have missed the entire intersubjectivity-first
commitment, the arche-trace erasure boundary that marks where the
learner model honestly stops, and the distinction between *this model*
and *the project's center of gravity*.

Every demo surface needs a framing that resists the overclaim. This is
not pessimism; it is consistency with Tio's voice commitments (epistemic
humility, anti-schlock, anti-AI-tell). The demo will be more persuasive,
not less, if it names its own limits on every panel.

## Concrete next moves (if this product is prioritized)

1. **Write the opening narration.** One paragraph that tells a teacher
   or conference audience what the demo is and is not. What they are
   about to see (a single-agent crisis cycle, resource-limited, strategy-
   acquiring) and what the demo is not showing (intersubjectivity,
   recognition, the Best Day intervention). Call it out on the landing
   page.

2. **Build 4–6 preset scenarios.** Each with a button, a description,
   and a narration panel. Suggested set:
   - 8 + 5, with resource budget low (watch counting-all fail, oracle
     teach counting-on)
   - 3 − 5 (watch normative crisis → domain expansion N → Z)
   - 4 × 6 with no multiplication strategies loaded (watch unknown-
     operation crisis)
   - A fractional problem that hits the fraction-crisis boundary
     (watch the system stop and explain why)
   - Same problem as scenario 1 but wrapped in `s(comp_nec(...))` vs.
     `s(exp_poss(...))` (watch modal context change the outcome)
   - A bad-infinite cycle that triggers critique (watch the
     belief-revision path)

3. **Relabel the timeline.** Replace raw event logs with narrative
   labels. Each event gets a plain-language sentence: "Resources low —
   entering crisis," "Oracle provided counting-on strategy — loaded
   into knowledge base," "Tension accelerating under compressive
   context — stability threshold at 3 steps." Keep the raw log as a
   collapsible detail panel.

4. **Add the modal-context visualization.** A simple color strip along
   the timeline — blue for expansive, red for compressive, neutral
   gray — that shows at a glance how the agent's context is shifting.
   Combined with the tension curve, this is the most visually compelling
   story the system can tell.

5. **Build the "where the demo stops" panel.** When fractions, genuine
   synthesis, or recognition between learners are asked for, the demo
   shows a panel: "This is where this model stops. Here is the research
   question. Here is where to look in the research program for what
   happens next." Links to `strategies/FRACTION_CRISIS_ASSESSMENT.md`,
   to the Best Day grant materials when ready, and to the arche-trace
   essay when written.

6. **Decide whether the demo is portfolio or conference.** A portfolio
   demo runs in the browser, expects no one to be present to explain
   it, and must carry its own narration. A conference demo is presented;
   the narration is the talk. Both can share scenarios, but the framing
   text differs. The current server is closer to portfolio-shaped but
   lacks the narration. Choosing one shapes the next decisions.

## What this product does not force

The demo reads the learner module; it does not rewrite it. Preset
scenarios are a frontend concern (the HTML/JS in the `more-zeeman/`
suite, or a new page). Narration is documentation that sits next to the
server code. Modal-context visualization is a frontend addition that
consumes an existing API. The critical-UI wiring that just landed
(commit `0280031`) is structurally load-bearing; everything else the
demo needs lives above the Prolog.

The fraction-crisis boundary in the demo is not an ask to solve fractions
in the learner module. It is an ask to *show the boundary* — which is
itself consistent with the arche-trace module's commitment to honest
limits.

## What the introduction panel would say

Honest draft language, for reference:

> You are about to watch a computational agent try to solve arithmetic
> problems, run out of resources, and acquire new strategies from an
> oracle. Two things this demo is: a live view of the ORR cycle (observe,
> react, reorganize) that the formalization project builds out of
> grounded arithmetic, PML modal context, and catastrophe-surface tension
> dynamics; and a concrete answer to the question "what does it mean for
> a formal system to know it has hit its limits?"
>
> Two things this demo is not: a model of how real children learn (there
> is only one agent here, and children's learning is fundamentally
> intersubjective), and a model of good teaching (the oracle dispatches
> strategies rather than creating conditions for understanding). What
> teaching looks like in this research program lives in the Best Day
> intervention — not here.
>
> The most important parts of the demo may be the places where it stops.
> When you see a "this is where the model stops" panel, you are looking
> at a research question the project is still working on, not a bug.

Anything weaker is demo-marketing; anything stronger overclaims. The
demo's main job is to make the ORR cycle visible without pretending it
is more than it is.
