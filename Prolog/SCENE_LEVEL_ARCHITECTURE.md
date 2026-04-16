# Scene-Level Architecture: umedcta-formalization

## What This Document Does and Does Not Claim

Carspecken's Four Scenes (1999) may provide a useful internal vocabulary for how the
different lines of work in this repository relate. This document proposes hypothetical
correspondences between the scenes and the code — not identities. The ORR cycle is a
formal flattening of what Carspecken describes phenomenologically. Where the flattening
distorts or drops something is at least as interesting as where it holds, and probably
more so.

These are research questions, not answers. The correspondences sketched below should be
treated as places to look for productive failure — where the formal model gets
Carspecken wrong in ways that teach us something about both the formalism and the
phenomenology.

---

## Scene 1: The Sound of Time

**What Carspecken means:** Temporality as the pre-reflective ground of experience.
Rhythm, succession, the felt passage from one moment to the next.

**Where it might connect to the code:**

Could the counting automaton (`standard_k_ns_1.pl`) be understood as a formal
analogue of successor-temporal experience? `count_by_ones/3` iterates forward; the
trace it produces records a sequence. Whether this has anything to do with Carspecken's
temporality — whether flat Prolog iteration can model what he means by "the sound of
time" — is not obvious. Counting is sequential, but sequence is not the same as felt
duration.

The inference limits in `solve/6` enforce a kind of finitude. When the limit is
reached, computation stops. Does this correspond to temporal limitation in any
phenomenologically meaningful way, or is it just a resource ceiling? The ORR cycle's
crisis phase looks temporal from the outside (the system "runs out of time"), but a
resource bound is not the same as lived temporal pressure. What would it mean to take
the difference seriously?

The efficiency gaps in the standards log — counting-all O(A+B), counting-on O(B),
place-value O(1) — compress the number of operations. Whether operational compression
maps onto anything like a reorganization of temporal experience is an open question.
It might. It might also be a case where the formal model flattens out exactly the
dimension Carspecken cares about.

**Questions to investigate:**
- What does "the sound of time" actually mean in Carspecken 1999? Does he develop it
  in ways that connect to or resist formalization?
- If we take the efficiency gaps as temporal compressions, what do we lose in that
  translation? What aspects of the child's experience of mathematical time are absent
  from the cost model?
- Where does the flat sequential iteration of Prolog most obviously fail to capture
  temporal experience?

---

## Scene 2: The Breath — Embodied Limitation

**What Carspecken means:** The body as medium of encounter. Not the body as object but
the body as the condition under which experience becomes possible at all.

**Where it might connect to the code:**

Subitizing (`standard_k_ns_4.pl`) involves something like perceptual apprehension —
O(1) recognition without counting. Whether this captures what Carspecken means by
embodiment, or whether it reduces embodiment to a lookup cost, is worth asking.
The cost gap between subitizing and counting (O(1) vs. O(n)) might correspond to
something about embodied vs. procedural knowing, but it also might just be a
performance characteristic.

The resource limits in `execution_handler.pl` trigger "efficiency crises." This
resembles embodied limitation — the system cannot sustain the computation, like a child
running out of fingers. But Carspecken's body is not a machine that runs out of
resources. His body breathes, feels, is situated. The formal model captures exhaustion
as a threshold, not as a lived condition. What would it mean for the system to be
*tired* rather than *out of inferences*?

**Questions to investigate:**
- Does Carspecken connect "the breath" to mathematical cognition anywhere, or is this
  a projection from the formalism backward onto his work?
- The fraction crisis (3.NS.2) involves a representational mismatch — `fraction/2` is
  structurally different from `recollection/1`. Could this be understood as a crisis of
  embodiment (the body-that-counts encountering something it cannot count), or is that
  a metaphor that obscures more than it reveals?
- What does the formalism's inability to model fatigue, frustration, or spatial
  orientation tell us about the limits of computational models of learning?

---

## Scene 3: The I-Thou — Recognition and the Teacher

**What Carspecken means:** Intersubjectivity. The encounter with the other as someone
who addresses me and whom I address. Not observation but mutual recognition.

**Where it might connect to the code:**

The `review-strategy-connections` branch renamed "oracle" to "teacher" across all
design documents. This terminological shift gestures toward the I-Thou, but renaming
a function does not make it intersubjective. The teacher creates conditions under which
the learner's current approach breaks down, but the interaction is one-directional: the
teacher pulls the control point, the learner snaps. Carspecken's I-Thou involves mutual
recognition — both parties are changed. Does anything in the code model the teacher being
changed by the encounter?

The naming practice in `standard_k_ns_2.pl` is suggestive: "the system cannot derive
that recollection([tally,tally]) is called 'two'; it must be taught." Names arrive
from outside the learner's own activity. Whether normative assignment constitutes an
I-Thou encounter, or whether it's closer to what Carspecken would call cultural power
(the imposition of a norm), is a question the formalism cannot answer on its own.

The `standards-elaboration` branch maps Indiana standards to Prolog modules. Each
standard describes what a teacher makes available at a developmental moment. But
standards are institutional artifacts — they describe what *should* be taught, not what
happens when two people are in a room together. How much of the I-Thou survives when
it is operationalized as curriculum?

**Questions to investigate:**
- Where does Carspecken distinguish the I-Thou from cultural power? The teacher
  in the code creates conditions for crisis. Is this closer to Buber's I-Thou or to what
  Carspecken calls interactive power?
- The erasure boundary in `incompatibility_semantics.pl` marks where formal proof goes
  hollow. Could this be where the I-Thou *must* enter — where the formalism hands off
  to human judgment? Or is that too convenient a reading?
- What would it look like for the learner to *refuse* the teacher's strategy? The
  current model has no mechanism for resistance. Is that a gap worth formalizing, or
  does resistance belong to a domain the formalism should not try to capture?

---

## Scene 4: The Object — What Resists

**What Carspecken means:** The world of things. Not things as inert but things as that
which resists, which has its own structure that the subject must accommodate.

**Where it might connect to the code:**

The incompatibility semantics module (`proves/4`) tracks inferential commitments — what
can be justified about mathematical objects. The arche-trace erasure marks where
derivations succeed structurally but carry no content. Whether this constitutes
"resistance" in Carspecken's sense, or whether it is simply incompleteness in the
formal system, depends on whether you think formal incompleteness has phenomenological
significance. That is not settled.

The FSM synthesis engine attempts to build strategies from ENS primitives (partition,
disembed, iterate). When it fails — when fractions cannot be synthesized from
whole-number operations — something resists. But is it the mathematical *object*
resisting, or the *formalism*? The fraction crisis might say more about the limits of
flat FSM synthesis than about the nature of fractions.

The Zeeman machine (`more-zeeman/`, PR #3) models the topology of strategy
transitions. Catastrophe geometry describes how smooth changes in control parameters
produce discontinuous jumps in behavior. Whether this topology captures the structure
of mathematical objects or the structure of the learner's interaction with them is
ambiguous — and the ambiguity might itself be the point.

**Questions to investigate:**
- Does Carspecken's "object" correspond to the mathematical object, or to the
  situation in which the learner encounters it? These are different things.
- The fraction crisis reveals that `fraction/2` and `recollection/1` are incompatible
  representations. Is this incompatibility a feature of the mathematics, a feature of
  the formalism's design choices, or both? How would we tell?
- Where does the code mistake its own limitations for the object's resistance? The
  CLAUDE.md already warns against this: "the interesting thing is where the
  formalization fails or oversimplifies." Can we be more specific about which failures
  are instructive and which are just bugs?

---

## Branch Map (Hypothetical Scene Associations)

| Branch | Possible Scene Affinity | Status | What it does |
|--------|------------------------|--------|-------------|
| `main` | — | Stable base | Merged work: core Prolog, design docs, More Machine |
| `standards-elaboration` | Scene 3? (I-Thou, curriculum as teacher-learner encounter) | 12 ahead, 20 modules, 153 tests | Maps Indiana K-3 standards to Prolog |
| `claude/review-strategy-connections-TVMYU` | Scenes 3-4? (design architecture, oracle→teacher rename) | Unmerged, 4 days old | Design doc overhaul; how strategies connect |
| `claude/add-more-machine-SRy5j` | Scene 4? (catastrophe topology) | Merged (PR #3) | Zeeman machine, strategy space visualization |

These associations are working hypotheses. The concordance project would test whether
the scenes actually carve the project at its joints, or whether the real articulation
falls along different lines.

---

## How the Scenes Might Connect — or Might Not

The ORR cycle (Observe, React, Reorganize) is a formal loop. The Four Scenes are
phenomenological descriptions of how experience is structured. The ORR cycle flattens
all four scenes into a single crisis-recovery mechanism. This flattening is not
neutral. It makes some things tractable (you can run the cycle, count the operations,
measure efficiency gaps) and makes other things invisible (the felt quality of temporal
experience, the body's fatigue, the mutuality of the I-Thou, the resistance of the
object as distinct from the limits of the model).

One hypothesis worth testing: the ORR cycle's phases (computing → crisis → recovery →
reorganization) might correspond roughly to Scene 1 → Scene 2 or 4 → Scene 3 → Scene 1.
But "might correspond roughly" is doing a lot of work, and the correspondence could be
misleading. The cycle is sequential; the scenes may be concurrent. The cycle is
repeatable; the scenes may be transformative. The cycle resolves crisis; the scenes may
not — Carspecken's temporality does not "resolve."

Where this mapping fails is where the real research begins.

---

## What the Concordance Project Would Add

The vocabulary above is provisional and speculative. It uses Carspecken's terms
without grounding them in his texts. A proper concordance would:

1. **Return to the source.** What does "scene" actually mean in Four Scenes? What does
   Carspecken argue, and how does his argument develop across the essays? The retyped
   versions make this tractable.
2. **Trace the citation network.** Carspecken draws on Habermas, Mead, Buber, Hegel,
   Bhaskar, Nishida, and others. Where he transforms a borrowed concept, the
   transformation matters. The Readings folder has primary sources for most of these.
3. **Test the proposed connections.** Instead of asserting that naming (K.NS.2) "is" the
   I-Thou, the concordance would ask: what does Carspecken say about how names are
   acquired? Does his account involve something the formalism captures, or something it
   drops? Where it drops something, what do we learn from the gap?
4. **Distinguish voice from concepts.** The carspecken-voice-editor handles style. The
   concordance would handle the conceptual apparatus — what "meaning field" means in
   his usage (not Habermas's), what "validity horizon" means, how "scene" differs from
   "context" or "situation."

### Source Texts Available (/Users/tio/Desktop/Readings/Carspecken/):

- Carspecken_1999_Four_Scenes_All_Essays (fixed + Retyped)
- Carspecken_Limits_Of_Knowledge_Series (original + Retyped)
- carspecken_limits_2016, CHAPTER6Limits-2016
- Carspecken 1995, 2009, 2013, 2016, 2018
- Critical Ethnography and Education (Walford)
- Henze and Carspecken — love, forgiveness, ethnographic poetry
- references.bib (125KB) in the Readings root

### Recommended Approach (VS Code + NotebookLM):

Start with the retyped Four Scenes. Extract key terms with Carspecken's own
definitions and the source authors he cites. Note where he transforms a borrowed
concept. Build outward to the Limits of Knowledge series, then later texts. Use
references.bib as the citation backbone. The NotebookLM MCP provides RAG against
primary sources already loaded there — keep the system close to the text.

---

*This document proposes hypothetical correspondences between Carspecken's Four Scenes
and the umedcta-formalization codebase. It is not a claim that these correspondences
hold. It is a research agenda for testing where they hold, where they fail, and what
we learn from the failures. The concordance project would ground this agenda in
Carspecken's actual texts rather than in paraphrases of his terms.*
