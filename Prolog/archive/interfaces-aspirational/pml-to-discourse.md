# PML to Discourse: How Operators Map to Observable Talk

## The Claim

PML's 12 operators (3 modes x 2 polarities x 2 directions) can be applied as a
coding framework for classroom discourse. The mapping runs through three channels:
person-position shifts correspond to S/O/N modes, categorical vs. hedging language
corresponds to compressive vs. expansive polarity, and felt tempo of utterance
corresponds to the up/down direction. PML tracks the epistemic stance from which a
teacher speaks — not what they do, but the discursive level at which they operate.

This is a framework claim, not an empirical finding. No transcripts have been coded
with PML. The mapping proposed here is the one that would need to hold for PML to
function as a discourse analysis instrument.

## What Exists in Code

### The 12 Operators

Four modal operators defined as Prolog prefix operators in
`pml/pml_operators.pl:24-33`:

| Operator | Symbol | Meaning |
|----------|--------|---------|
| `comp_nec` | Box-down (necessity, compressive) | Fixation, crystallization |
| `exp_nec` | Box-up (necessity, expansive) | Release, liquefaction |
| `exp_poss` | Diamond-up (possibility, expansive) | Potential for release |
| `comp_poss` | Diamond-down (possibility, compressive) | Temptation to fixate |

Three modes of validity, each a predicate wrapper (`pml/pml_operators.pl:43-73`):

| Mode | Predicate | Person-Position |
|------|-----------|-----------------|
| S-mode | `s/1` | First person (subjective experience) |
| O-mode | `o/1` | Second person (what is known about the other) |
| N-mode | `n/1` | Third person (normative, institutional) |

The 12 operators are compositions: `s(comp_nec(P))`, `o(exp_poss(P))`, etc.
Each combination picks out a distinct discursive act.

### Material Inferences (What Follows from What)

The axiom set `pml/axioms_eml.pl` encodes 8 material inferences that define the
dialectical rhythm:

```
s(u)        =>  s(comp_nec(a))        Unity compresses into awareness
s(a)        =>  s(exp_poss(lg))       Awareness opens possibility of release
s(a)        =>  s(comp_poss(t))       Awareness opens possibility of fixation
s(t)        =>  s(comp_nec(neg(u)))   Temptation compresses into non-unity
s(lg)       =>  s(exp_nec(u_prime))   Release expands into transformed unity
s(t_b)      =>  s(comp_nec(t_n))      Being compresses into Nothing (bad infinite)
s(t_n)      =>  s(comp_nec(t_b))      Nothing compresses into Being (bad infinite)
```

The Oobleck dynamic (`pml/semantic_axioms.pl:55-58`) transfers modality across
person-positions:

```
s(comp_nec(P))  =>  o(comp_nec(P))    Subjective compression becomes objective
```

### Pragmatic Axioms

`pml/pragmatic_axioms.pl` operationalizes two concepts:

- **I-Feeling** (`i_feeling/1`): generates an attributed variable via
  `automata:generate_trace/1`. The I-Feeling resists unification with any
  concrete term — it cannot be stabilized into a representation.
- **Identity Claim** (`identity_claim/1`): must NOT contain the Trace. Any
  identity claim *about* the I-Feeling is incoherent
  (`incoherent([n(represents(C_Id, I_f))])`).

### Intersubjective Recognition

`pml/intersubjective_praxis.pl:58-61` encodes the recognition axiom:

```
n(confession(A)), n(confession(B))  =>  n(exp_nec(forgiveness(A, B)))
```

Two agents confessing (N-mode, normative) produces expansive necessity of
forgiveness. This works with abstract agents but erases when grounded in
subjective experience (I-Feelings).

### Tests

28 core tests pass (`pml/tests/core_test.pl`), verifying:
- Operator syntax and module loading (4 tests)
- Automata: highlander, primes, trace mechanism (7 tests)
- Prover: identity, explosion, double negation, resource tracking (5 tests)
- PML dynamics: dialectical rhythm, oobleck, modal context cost (7 tests)
- Trace mechanism: I-Feeling, identity claim, S-O inversion, erasure (5 tests)

## The Proposed Discourse Mapping

### Channel 1: Person-Position to S/O/N Mode

| Discourse Marker | PML Mode | Example |
|-----------------|----------|---------|
| First-person self-referential ("I think...", "I notice...") | S-mode | Teacher reflecting on their own understanding |
| Second-person child-focused ("You showed...", "What did you...") | O-mode | Teacher attending to the student's reasoning |
| Third-person framework references ("The standard says...", "Mathematicians call this...") | N-mode | Teacher invoking normative authority |

This follows Carspecken's reconstruction: the three modes of validity correspond
to three grammatical persons and three kinds of claim (subjective, objective,
normative). The coding question is whether a given utterance makes a claim about
the speaker's experience, the student's activity, or a shared norm.

### Channel 2: Polarity to Categorical vs. Hedging Language

| Discourse Marker | PML Polarity | Character |
|-----------------|-------------|-----------|
| Categorical assertions ("This IS...", "You MUST...", definitive tone) | Compressive (necessity, box) | Narrowing of possibility space |
| Hedging, surprise, questioning ("Maybe...", "I wonder...", "What if...") | Expansive (possibility, diamond) | Opening of possibility space |

The polarity distinction is not about surface vocabulary but about what the
utterance does to the discursive space. A categorical statement compresses — it
forecloses alternatives. A hedge or question expands — it holds open what might
be the case. Boyd & Markarian (2015) argue that stance matters more than surface
form, which is the same claim: PML codes the stance, not the move.

### Channel 3: Tempo to Up/Down Direction

| Felt Quality | PML Direction | Character |
|-------------|---------------|-----------|
| Quickening, urgency, acceleration | Up (ascending) | Intensification of engagement |
| Settling, deliberateness, deceleration | Down (descending) | Consolidation of understanding |

This is the least formalized channel. The up/down direction distinguishes, for
instance, `comp_nec` (compressive and descending: settling into fixation) from
`exp_nec` (expansive and ascending: urgency of release). Coding tempo requires
attending to prosodic features and pacing, not just lexical content.

## Where the Claim Exceeds What's Formalized

### No Coding Manual

The spec lists a coding manual as needed for `pml/`. It does not exist. Without
a manual specifying decision procedures for each of the 12 operator assignments,
two coders looking at the same transcript would have no shared protocol.

### No Empirical Validation

No classroom transcript has been coded with PML. The mapping above is proposed,
not tested. The literature search (April 2026) identified methodological
templates — Hennessy et al. (2020) for coding scheme validation, Ng et al. (2021)
for instrument development — but the work has not been done.

### No Inter-Rater Reliability

A coding framework requires demonstrable agreement between independent coders.
Without inter-rater reliability, the distinction between PML and ad hoc
interpretation is unclear.

### N-Mode Underdeveloped

The axiom set (`pml/semantic_axioms.pl:64-67`) defines only N-to-N reflexive
patterns. N-to-S transitions (norm internalization) and N-to-O transitions (norm
application to the other) are mentioned in the README but have no axioms.
Normative discourse — the most common register in standards-based teaching — is
the mode with the least formal infrastructure.

### TalkMoves Comparison Missing

The spec mentions 6 divergence points between PML and TalkMoves. These are not
documented. The positioning claim — that PML measures epistemic stance rather
than behavioral moves — needs a systematic comparison showing what PML captures
that TalkMoves misses, and vice versa.

## What Would Strengthen This Interface

1. **Coding manual with worked examples.** Take 3-5 short transcript excerpts
   from published mathematics education research. Code each utterance with all
   three channels. Document the decision procedure for ambiguous cases.

2. **Small-scale pilot.** Two independent coders apply the manual to the same
   10-minute transcript. Compute Cohen's kappa for each channel separately.
   This establishes whether PML's distinctions are reliably observable.

3. **N-mode axiom development.** Add `proves_impl` rules for N-to-S
   (internalization) and N-to-O (application). Test whether these track
   recognizable discourse patterns in standards-based instruction.

4. **TalkMoves comparison document.** For each of the 6 TalkMoves categories,
   specify what PML coding would assign and where the assignment diverges.
   Show at least one case where the divergence matters for understanding
   teaching quality.

5. **Positioning against Hennessy (2020).** The SCHEME framework
   (Hennessy et al. 2020) is the closest methodological comparator. A table
   mapping SCHEME categories to PML operators would clarify whether PML
   extends, refines, or replaces existing coding approaches.

## Literature Positioned for This Work

- Boyd & Markarian (2015) — dialogic stance over interactional form
- Hennessy et al. (2020) — coding scheme methodology
- Ng et al. (2021) — instrument development in math classrooms
- Derry (2017) — inferentialism in math education (theoretical grounding)
- Sert (2013) — epistemic status checks in instructional settings
- Hackenberg et al. (2025) — decentering in responsive teaching
