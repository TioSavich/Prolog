# Immanence Rule (Policy D17.1)

## The rule

**The substrate layer describes what the sentences DO, not what the concepts ARE.**

This is a durable guarding rule, not a stylistic preference. Violation of this rule re-introduces exactly the metaphysics Derrida's text refuses, and was explicitly flagged by Phil Carspecken in his commentary on the UMEDCA Chapter One draft:

> First of all, the Trace shouldn't be a formal structure with functions, for Derrida. Derrida sought to have the name, Trace, to emerge from a deconstructive reading of, initially, Husserl's philosophy. It emerges as a result of Husserl's own logic and vocabulary. Derrida sought to stay within Husserl's texts and make no claims about anything else. All of the claims, assertions, in the deconstruction only have their meaning and support from within the text that is being deconstructed. Thus, in Speech and Phenomena Derrida has quite a few statements to the effect that he remains within Husserl's logic and uses Husserl's texts to take them, internally, to deconstructive ends. He brings nothing from the outside, deconstruction is necessarily immanent. (Carspecken, note on UMEDCA ch. 1 and 2)

The Reader's immanence constraint is the same constraint Derrida observes toward Husserl: stay inside the text, describe what the text does with its own vocabulary, do not impose a functional apparatus from outside.

## Why the rule is load-bearing

The slip is easy. It has a specific shape:

1. We observe something about a sentence: "the word 'sign' appears both in quotation marks and unquoted in this passage."
2. We want to capture that observation in a Prolog predicate.
3. We reach for a predicate name that names a concept-level feature: `self_names("sign")`, `fixed_point("sign", sign_function)`.
4. The predicate now attributes a concept-level property (self-reference, fixed-point-hood, function) to the Derridean concept of sign.
5. We have re-introduced structure that deconstruction refuses — while believing ourselves to be working at the meta-level.

The slide between (1) and (3) is the core problem. The fix is to use a predicate whose name is explicitly about the sentence, not about the concept. Step (3) becomes `both_quoted_and_unquoted(passage, "sign")` — a fact about the passage's composition, not about what the category of sign is.

## Safe predicates (describe the text)

- `name(X, Q)` — Q is the quotation of X; a fact about how the text writes X
- `quoted_in_passage(P, Q)` — the quoted form Q appears in passage P
- `unquoted_in_passage(P, Q)` — Q appears without quotes in P
- `both_quoted_and_unquoted(P, Q)` — both forms appear in the same passage
- `translates_in_source(Author, Q1, Q2)` — the author supplies Q1 and Q2 as equivalents
- `multi_valued_meaning_predicate(P, Q, Values)` — the passage's grammar predicates meaning of Q over multiple values
- `clause_in_passage(ClauseID, asserts(String), passage(P), position(Pos))` — a clause's textual content and position
- `adjacent_assertion_and_denial(C1, C2, P)` — two textually-adjacent clauses assert P and ¬P **at the predicate level** (not concessive or hedged structure)
- `author_textual_marker(Author, P, marker(String), connects(C1, C2))` — the author uses a phrase connecting clauses
- `author_procedural_directive_to_reader(Author, on(X), sequence(Steps))` — the author instructs the reader to do a sequence (imperative mood, second-person address, or "let us X" required)
- `structural_necessity(Source, on(X), claim(String))` — the text asserts a structural necessity ("it is necessary to," "X is unthinkable starting from Y") without addressing the reader
- `author_textual_construction(Author, construction(Shape), with_instruction(String))` — shapes like "arche-X"
- `author_self_describes_term(Author, Q, Description)` — the author gives meta-commentary on their own term
- `authorial_move(Author, mode, works_inside(Text), draws_consequence(String))` — the author makes a move with respect to another text; **mode ∈ {immanent, deconstructive_intervention, external_critique, refuses_commentary}** distinguishes the speech-act type
- `derrida_text_asserts(P, String)` — Derrida's text at P asserts the string content
- `derrida_text_refuses(P, Claim)` — Derrida's text at P refuses a specific claim
- `derrida_text_uses_term_for_relation(P, term(Q), between(A, B), at_site_of(L))` — Derrida's text uses term Q for a relation between A and B at site L
- `husserl_text_asserts(Source, Claim)` — Husserl's text asserts a claim (Husserl's method licenses concept-level claims in his own voice)
- `passage_source(P, Citation)` — bibliographic citation
- `procedurally_licensed_under_erasure(ClauseID)` — author-supplied explicit erasure marker required (*sous rature*, strikethrough, "keep erasing," "strategically nicknamed")

The test for adding a new safe predicate: does it describe a feature of *how the text is composed* or a feature of *what the text refers to*? Composition-level is safe; reference-level is the slip.

## Unsafe predicates (attribute function or structure to concepts)

- `fixed_point/2` — attributes fixed-point status to a concept
- `self_names/1` — slides into concept-level self-reference
- `is_X_function/1` for any term where X is a Derridean concept — attributes function
- `gaifman_structure/N` — imposes Gaifman's functional apparatus on a concept
- `essence/1,2` — classical metaphysics
- `structure/1,2` — when applied to a Derridean concept
- `function/1,2` — when applied to a Derridean concept
- `located_in/2` — when locating a concept's identity in some feature
- Any predicate asserting that a Derridean concept has a determinate property, role, or relation at the concept-level

## The test

When drafting a substrate predicate, ask:

> Does this predicate describe a **sentence** (something the author wrote on the page) or does it describe a **concept** (something the author's sentences refer to)?

Sentence-level is safe. Concept-level is the slip.

The test generalizes beyond Derrida to any author whose method refuses concept-level functional attribution: Maharaj (whose speech is pointer rather than assertion), Nagarjuna (whose *tathata* refuses predicative attribution), and arguably Brandom in certain registers (scorekeeping attributions are commitments, not properties).

## The Gaifman apparatus at meta-level

The naming apparatus from Gaifman's *Naming and Diagonalization* (2006) stays at the meta-level. We can describe the *logical shape* of Derrida's sentences using Gaifman's vocabulary:

- diagonal shape (φ ↔ ψ(⌜φ⌝) as a descriptor of an adjacent-assertion-and-denial passage)
- name (Quine-corner as a description of quotation-use)
- quotation as a predicate over the textual form

We cannot apply Gaifman's vocabulary to Derrida's *concepts*. We cannot say "trace is a fixed point of the trace function" without re-introducing the metaphysics of presence the concept of trace was coined to refuse.

## When the passage refuses decomposition

If a rhythm-layer axiom (or in the immanent-reader's case, a substrate predicate) would require a premise shape that forces a concept-level attribution, that is a signal. The passage is doing something the Reader cannot axiomatize without slipping. Options:

1. Leave the passage un-axiomatized and flag it in the gloss.
2. Axiomatize only the text-level facts and allow downstream (rhythm-layer) axioms to fire without a concept-level premise.
3. Attribute the claim to the commentator register (Husserl, Brandom, Habermas, Carspecken) when the commentator's method does license the attribution.

Option 3 is often the right answer for alignment-style readings where two frames are in play. A commentator whose method is Habermasian communicative-action theory (Carspecken) has earned entitlement to cash-out attributions that Derrida himself refused.

## The durable rule, stated in one line

> We describe what the text does. We do not say what the concepts are.
