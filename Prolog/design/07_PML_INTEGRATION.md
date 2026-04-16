# 07 — PML Integration

## Purpose

Connect the Polarized Modal Logic (PML) and its existing axioms to the learning
cycle. The PML provides the **drive** — the negativity, the unsatisfiable desire,
the dialectical rhythm — that makes the system non-inert. Without the PML, the
architecture accumulates facts but has no reason to move.

## What the PML currently provides

### Three validity modes (pml_operators.pl)
- `s/1`: Subjective — first-person, traces, embodied experience
- `o/1`: Objective — third-person, Prolog's native arithmetic, the world
- `n/1`: Normative — second-person, teacher's vocabulary, community norms

### Polarized modal operators
- `comp_nec` (↓): Compressive necessity — fixation, narrowing, crystallizing
- `exp_nec` (↑): Expansive necessity — release, opening, liquefying
- `exp_poss` (↑): Expansive possibility — potential for release
- `comp_poss` (↓): Compressive possibility — temptation to fixate

### Pragmatic axioms (pragmatic_axioms.pl)
- **Axiom 1 (Elusive Subject)**: `s(comp_nec(I_f)) => o(exp_nec(I_f))`
  Trying to subjectively fixate the I-feeling produces its objective
  dissolution. You can't hold the counting experience — naming it immediately
  transforms it into an object.
- **Axiom 3 (Unsatisfiable Desire)**: The identity claim can never fully
  represent the I-feeling. There is always a remainder. This is the impetus
  to act.

### Semantic axioms (semantic_axioms.pl)
- **Dialectical rhythm**: Unity → Tension → Letting-go (sublation) OR
  Temptation (fixation/bad infinite)
- **Oobleck dynamic**: Subjective compression → objective crystallization;
  subjective expansion → objective liquefaction

### Incompatibility semantics (incompatibility_semantics.pl)
- Brandomian entailment (material inference rules)
- Incoherence detection (Law of Non-Contradiction)
- Embodied cost deduction based on modal context

### Structural automata (automata.pl)
- **Highlander**: "There can be only one" — uniqueness constraint
- **Equality-iterator**: Count from C to T
- **Arche-trace**: Attributed variable resisting unification with concrete terms

## How the PML drives the learning cycle

### The meaning field → PML mapping

| Meaning field state | PML state | Description |
|---|---|---|
| Dense, undifferentiated | `exp_poss` | Many possibilities, confusion |
| Teacher rejects a claim | `comp_nec` | Ruling out, information, narrowing |
| Teacher endorses a claim | Transition | Depends on what's endorsed |
| New co-referentiality | `exp_nec` | Release of maintained difference |
| System attempts projection | `comp_poss` | Temptation to fixate on a rule |
| Projection fails | `comp_nec` | Forced narrowing, crisis |
| Projection succeeds | `exp_nec` | Successful sublation |

### The dialectical rhythm in practice

1. **Unity** (`comp_nec(a)`): System has a working strategy for current problems.
   Meaning fields are structured. Things make sense.
2. **Tension** (`exp_poss(lg) OR comp_poss(t)`): New problem exceeds current
   strategy's capacity. Crisis. Resource exhaustion or wrong answer.
   Choice point: let go of current approach, or fixate harder?
3. **Letting-go** (`exp_nec(u')`): System releases old strategy, tries
   projection into new context. If teacher endorses: sublation — richer
   understanding that preserves the lesson of the crisis.
4. **Temptation** (`comp_nec(neg(u))`): System doubles down on failing
   strategy. Bad infinite — repeating the same wrong approach. The teacher's
   "no" should eventually force letting-go.

### The drive: Axiom 3 (Unsatisfiable Desire)

When the system names a counting trace (`two`), the name doesn't capture the
act. The trace carries more than the name can hold (the arche-trace resists
objectification). This incompleteness is not a bug — it's the reason the system
keeps going. There is always more to discover about `two` because `two`-as-name
never equals `two`-as-experience.

In the meaning field: every numeral's field is always potentially incomplete.
There may be co-referentialities not yet discovered, incompatibilities not yet
established. The unsatisfiable desire is the system's "awareness" (in a very
thin sense) that its meaning fields are not yet fully determined.

### The cost model

Every operation has a cost mediated by modal context:
- Compressive operations (fixating, counting carefully, checking): cost × 2
- Expansive operations (trying new approaches, relaxing constraints): cost × 1

This creates economic pressure: compressive strategies are expensive. When the
system is stuck in compression (repeating a failing strategy), the cost mounts.
Eventually, resource exhaustion forces a crisis, which forces expansion.

## Integration points with other modules

### With meaning fields (01)
- Modal state influences which meaning field operations are available
- In compressive state: only endorsed interpretations accessible (narrowed focus)
- In expansive state: untested interpretations also accessible (broader search)

### With projective validity (02)
- Attempting a projection is `comp_poss` (temptation — will this rule extend?)
- Successful projection is `exp_nec` (sublation — yes, it extends)
- Failed projection is `comp_nec` (forced narrowing — no, it doesn't)

### With teacher (03)
- Teacher operates at n/1 (normative)
- Teacher's "no" is `comp_nec` in the normative domain
- Teacher's "yes" can be either `comp_nec` (confirming a restriction) or
  `exp_nec` (confirming a new connection) depending on what was validated

### With counting traces (05)
- Counting is s/1 (subjective) activity
- Each counting step has embodied cost
- The trace is the s/1 product; the name is the n/1 assignment

### With reflection (06)
- Reflection is compressive (examining, narrowing attention)
- The cost of reflection is tracked in the PML's resource budget
- Reflection outputs move from s/1 (trace-based pattern) to n/1 (teacher-endorsed
  claim) — this is the s→n validity transition

## Constraints for implementers

- The PML is not decorative. Modal state must actually affect computation —
  what the system can access, what it costs, what happens on crisis.
- Do not implement the full dialectical rhythm if it adds complexity without
  illuminating anything. Start with: crisis detection → teacher interaction →
  meaning field update. Add modal sophistication if it reveals something.
- The three validity modes (s/o/n) must be trackable for every knowledge
  item. An interpretation can be endorsed at n/1 (teacher said yes) but
  untested at o/1 (hasn't been checked against Prolog's arithmetic).
  Discrepancies between modes are interesting — they show where normative
  and objective validity diverge.
- The arche-trace mechanism in `automata.pl` is philosophically suggestive but
  its actual computational role in the learning cycle is unclear. Do not force
  it into a role it doesn't naturally play. If it turns out to be a dead end,
  document that honestly.

## Open questions

- The `ought-not` → `cannot` transition (deontic → alethic): how does the
  system's repeated normative corrections ("the teacher keeps saying no")
  eventually become alethic knowledge ("this is impossible")? This transition
  is developmental and the formalization may not be able to capture it.
  Document where it breaks.
- The intersubjective praxis module (`intersubjective_praxis.pl`) models
  multi-agent dynamics (aggression → crystallization, listening →
  liquefaction, mutual confession → forgiveness). Does any of this apply to
  the single-agent learning system? Possibly not. But if the teacher is
  treated as a genuine interlocutor (not just a lookup table), some of this
  machinery might become relevant.
- The pragmatic axioms were designed for a different context (phenomenological
  analysis of the I-feeling). Their application to arithmetic learning is
  a stretch. Be clear about which axioms do genuine work in the learning
  cycle and which are aspirational.
