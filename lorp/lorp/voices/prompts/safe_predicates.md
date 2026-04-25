# Safe Substrate Predicates — Full Catalog

Each predicate below is categorized, given a semantics, and shown with a canonical example. All predicates describe features of *the text itself*, not of the concepts the text refers to. Consult `immanence_rule.md` before adding any new predicate.

## Category 1 — Naming and quotation

### `name(X, Q)`
Q is the quotation of X. Used as a correspondence fact: the author writes X; the reader refers to X by its quotation Q. Use quoted strings for Q.
```prolog
name(sign, "sign").
name(trace, "trace").
```

### `translates_in_source(Author, Q1, Q2)`
Author gives Q1 and Q2 as cross-language equivalents. Preserves the author's own linguistic pairings.
```prolog
translates_in_source("Derrida", "expression", "Ausdruck").
translates_in_source("Derrida", "Verflechtung", "interweaving").
```

## Category 2 — Use/mention

### `quoted_in_passage(P, Q)`
The quoted form of Q appears in passage P (the author wrote Q in quotation marks).
```prolog
quoted_in_passage("vp_ch1_sign_sign", "sign").
```

### `unquoted_in_passage(P, Q)`
Q appears in passage P without quotation marks (the author used Q as if referring to its referent).
```prolog
unquoted_in_passage("vp_ch1_sign_sign", "sign").
```

### `both_quoted_and_unquoted(P, Q)`
Both forms appear in the same passage. This is the textual marker that was previously miscategorized as `self_names/1` (a concept-level claim). The safe form records only what is on the page.
```prolog
both_quoted_and_unquoted("vp_ch1_sign_sign", "sign").
```

## Category 3 — Sentence grammar

### `multi_valued_meaning_predicate(P, Q, Values)`
Passage P's sentence predicates meaning of Q over more than one value. Values is a list of strings.
```prolog
multi_valued_meaning_predicate(
  "vp_ch1_sign_sign",
  "sign",
  ["expression", "indication"]
).
```

### `clause_in_passage(ClauseID, asserts(String), passage(P), position(Pos))`
A specific clause within a passage, with its assertion content and its position (first, second, third, or numeric index).
```prolog
clause_in_passage("og_p65_claim_1",
  asserts("the trace is the absolute origin of sense in general"),
  passage("og_p65"),
  position(first)).
```

### `author_textual_marker(Author, P, marker(String), connects(C1, C2))`
Author uses a specific phrase (like "which amounts to saying") to connect clauses. Records the author's own textual joinery.
```prolog
author_textual_marker("Derrida", "og_p65",
  marker("which amounts to saying"),
  connects("og_p65_claim_1", "og_p65_claim_2")).
```

## Category 4 — Adjacency and denial

### `adjacent_assertion_and_denial(C1, C2, P)`
Two textually-adjacent clauses in passage P assert P and ¬P at the predicate level. This is the textual shape that corresponds to Liar/diagonal, without committing to "the concept has fixed-point structure."

**Precondition — strict P/¬P only.** This predicate fires ONLY when the two clauses contain the same predicate with opposite polarity (e.g. "X is Y" adjacent to "X is not Y" or "not X"). Concessive structure ("but even for him X," "even if we suppose X"), grant-then-withhold, disambiguation, and rhetorical tension are NOT adjacent-assertion-and-denial — they get `author_textual_marker/4` instead. A 2026-04-24 verifier audit on Voice and Phenomenon showed this predicate hallucinating over concessive moves; the precondition closes that gap.

```prolog
adjacent_assertion_and_denial(
  "og_p65_claim_1",
  "og_p65_claim_2",
  "og_p65"
).
```

### `procedurally_licensed_under_erasure(ClauseID)`
The author's procedural directive (recorded separately) licenses the clause being under erasure. Derived from the author's own instruction at e.g. *Of Grammatology* p. 60.

**Precondition — explicit erasure marker required.** This predicate fires ONLY when the source text contains one of: *sous rature*, literal strikethrough, "under erasure," "keep erasing," "strategically nicknamed," "antidote to be abandoned," "pointer-not-claim," or an equivalent author-supplied instruction that the word should be used-while-erased. Paradox, apparent contradiction, or self-undermining structure alone is NOT sufficient. A 2026-04-24 verifier audit showed this predicate hallucinating onto passages that contained only paradox; the precondition closes that gap.

```prolog
procedurally_licensed_under_erasure("og_p65_claim_1").
procedurally_licensed_under_erasure("og_p65_claim_2").
```

## Category 5 — Authorial procedure and self-description

### `author_procedural_directive_to_reader(Author, on(X), sequence(Steps))`
Author instructs the reader to perform a sequence of moves with respect to X. **Fires ONLY for explicit imperatives addressed to the reader** — second-person commands, imperatives, or author statements of the shape "let us X," "read X as Y," "note that X," "pay attention to X," "feel W then let it be erased." A 2026-04-24 verifier audit showed the prior `author_procedural_directive/3` firing on ontological necessities; this predicate replaces it with a stricter scope.
```prolog
author_procedural_directive_to_reader("Derrida",
  on("use of transcendental arche"),
  sequence([
    "make its necessity felt",
    "then let it be erased"
  ])).
```

### `structural_necessity(Source, on(X), claim(String))`
Author asserts that a move or distinction is necessary on structural/phenomenological grounds — NOT as an instruction to the reader. Canonical triggers: "it is necessary to X," "X must be understood as Y," "X is unthinkable starting from Y," "one can only think X from Y," "the distinction cannot hold without Z." This is the predicate that correctly captures Derrida's descriptive necessities without promoting them to reader-directives. Source is the author or the text voice making the claim.
```prolog
structural_necessity("Derrida", on("trace"),
  claim("originary-being must be thought from the trace, not the trace from originary-being")).
structural_necessity("Derrida", on("différance"),
  claim("unthinkable starting from consciousness, presence, or their opposites")).
```

### `author_textual_construction(Author, construction(Shape), with_instruction(String))`
Author marks a specific textual construction (like "arche-X") as requiring a specific treatment.
```prolog
author_textual_construction("Derrida",
  construction("arche-X"),
  with_instruction("must comply with both necessity and erasure")).
```

### `author_self_describes_term(Author, Q, Description)`
Author gives meta-commentary about their own use of Q. This is the safe home for "strategically nicknamed" (Derrida), "pointer-not-claim" (Maharaj), "antidote to be abandoned" (Maharaj), "strategic paleonymy" (Derrida).
```prolog
author_self_describes_term("Derrida", "trace", "strategically nicknamed").
author_self_describes_term("Maharaj", "I am", "pointer, not ontological claim").
```

### `authorial_move(Author, mode, works_inside(Text), draws_consequence(String))`
Author makes a move with respect to another author's text. The **mode** parameter distinguishes the speech-act type:

- `immanent` — author derives the consequence from premises the target author explicitly grants. Canonical case: Derrida reading Husserl on sign iterability from Husserl's own universal-sign requirement.
- `deconstructive_intervention` — author works inside the text but draws a consequence the target author would not grant, exposing a constitutive tension the target's method cannot acknowledge. Canonical case: Derrida deriving the originary supplement from Husserl's own setup while Husserl's phenomenology refuses the conclusion.
- `external_critique` — author introduces a claim or category from outside the target text to criticize it. Canonical case: injecting the grapheme argument into the essential-distinction passage.
- `refuses_commentary` — author explicitly disclaims both commentary and interpretation, marking the reading as something other than immanent or external. Canonical case: Derrida's *VP* p. 87: "a reading that is neither commentary nor interpretation."

The canonical test for mode selection: does the derived claim depend on premises the target author grants (immanent), does it expose a tension the target refuses to acknowledge (deconstructive_intervention), does it inject foreign premises (external_critique), or does the author explicitly disclaim both postures (refuses_commentary)?

A 2026-04-24 verifier audit caught the prior `authorial_immanent_move/3` coding every Derrida gesture as immanent, including passages where Derrida explicitly disclaimed commentary. The mode parameter closes that gap.

```prolog
authorial_move("Derrida", immanent,
  works_inside("Husserl's text"),
  draws_consequence("sign's being-a-sign requires iterability")).

authorial_move("Derrida", deconstructive_intervention,
  works_inside("Husserl's text"),
  draws_consequence("the for-itself of self-presence arises in the movement of supplementarity")).

authorial_move("Derrida", refuses_commentary,
  works_inside("Husserl's text"),
  draws_consequence("a reading that is neither commentary nor interpretation")).
```

## Category 6 — Textual assertion and refusal

### `derrida_text_asserts(P, String)`
Derrida's text at passage P asserts the string content (the assertion is what's on the page). Domain-generalized forms like `author_text_asserts(Author, P, String)` are also safe.
```prolog
derrida_text_asserts("vp_p97",
  "neither expression nor indication is added onto the other as a stratum").
```

### `derrida_text_refuses(P, Claim)`
Derrida's text refuses a specific claim. Useful for capturing negations-at-text-level without committing to what the concept "is."
```prolog
derrida_text_refuses("vp_ch2",
  priority_claim(between("expression", "indication"))).
```

### `derrida_text_uses_term_for_relation(P, term(Q), between(A, B), at_site_of(L))`
Derrida uses term Q to mark a relation between A and B at site L. Records the author's own term usage, not a functional decomposition.
```prolog
derrida_text_uses_term_for_relation(
  "vp_p97",
  term("originary supplement"),
  between("expression", "indication"),
  at_site_of("non-self-presence")).
```

### `husserl_text_asserts(Source, Claim)`
Husserl's text asserts a claim. Husserl's method does license concept-level claims in his own voice, which is why this predicate is more permissive about claim-shape than the Derrida-level predicates. Still records what Husserl says, not an attribution to sign-as-concept outside Husserl's text.
```prolog
husserl_text_asserts("LI_I_first_investigation",
  priority_claim(between("expression", "indication"))).
```

## Category 7 — Procedural / iterability / cross-language

### `derrida_text_asserts_about_husserl_requirement(P, String)`
Derrida draws a consequence from what Husserl's text requires. This captures immanent deconstruction's specific shape.
```prolog
derrida_text_asserts_about_husserl_requirement(
  "vp_ch4_iterability",
  "a sign occurring only once would not be a sign by Husserl's own criteria").
```

## Category 8 — Commentator register

### `commentator_reads(Commentator, Passage, Reading)`
Named commentator's specific reading of a specific passage. Commentator-register is a separate speech-act from the primary text.
```prolog
commentator_reads("Carspecken", "og_p65",
  "adjacent-assertion-and-denial is the textual mark of the Gaifman diagonal").
```

### `commentator_critiques(Commentator, Target, Critique)`
Commentator's explicit critique of a target (author, text, or claim).
```prolog
commentator_critiques("Carspecken", "UMEDCA_ch1",
  "attributes Trace with structure and function, which deconstruction refuses").
```

### `commentator_aligns_with(Commentator, Author1, Author2, Topic)`
Commentator connects two authors on a topic.
```prolog
commentator_aligns_with("Carspecken", "Derrida", "Habermas",
  "deconstruction of presence and dialogical-model truth").
```

### `commentator_self_describes(Commentator, Topic, Description)`
Commentator's meta-commentary on their own method.
```prolog
commentator_self_describes("Carspecken",
  method,
  "recasts Derridean insight from solitary-scene to intersubjective-scene").
```

## Category 9 — Passage citation

### `passage_source(P, Citation)`
Bibliographic citation for a passage ID.
```prolog
passage_source("vp_ch1_sign_sign",
  "Derrida, Voice and Phenomenon, ch. 1").
```

## Predicate naming conventions

- Passage IDs are short strings like `"vp_ch1_sign_sign"`, `"og_p65"`, `"vp_p97"`. Keep them stable across files.
- Quotations use double-quoted strings.
- Multi-word descriptions use double-quoted strings.
- Atoms that contain special characters (periods, apostrophes, spaces) must be single-quoted: `'Verflechtung'`, `'différance'`.
- Avoid CamelCase for atoms (Prolog reserves capitalized initials for variables).

## When to add a new predicate

Before adding, apply the test in `immanence_rule.md`:

> Does this predicate describe a sentence (something the author wrote on the page) or does it describe a concept (something the author's sentences refer to)?

If the answer is "a concept," do not add. If the answer is "a sentence," draft the predicate, add it to this catalog with semantics and example, and flag the addition in the gloss for Tio's review.

The test has a second form when the passage seems to require something this catalog does not cover:

> Is the thing I need to record a feature of the text's composition, or a feature of what the text refers to?

Composition-level predicates are safe and belong in this catalog. Reference-level predicates belong to the PML rhythm layer or to a different skill altogether.
