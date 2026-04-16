# 04 — Number-Word Layer and Numeral Semantics

## Purpose

Implement a layer of Prolog atoms (`one`, `two`, `three`, ...) that serve as
normatively assigned names for tally-sequences. These atoms carry no built-in
arithmetic — they are symbols whose meaning is entirely constituted by their
inferential roles within the system's meaning fields.

## Philosophical requirements

1. **Numerals are anaphoric terms, not singular terms.** This is the central
   claim from Savich (2022, Ch. 3, pp. 234-236) and Ch. 2 (revised). If
   numerals were singular terms, they would participate in symmetric
   substitution inferences and the partitioning predicate would be stable
   under substitution. Hybridized models show this is not the case. Numerals
   recollect an act of thought ("I think") — each invocation refers to a
   different act. "Anaphoric terms have syntactic sameness without semantic
   sameness" (Savich, 2022, p. 235).

   Practical consequence: `two` in one context (counting trace) and `two` in
   another context (partition of ten) are syntactically the same atom but may
   not be semantically identical. The meaning field tracks the different
   interpretations.

2. **Names are normatively assigned, not derived.** The teacher teaches that
   `s(s(0))` is called `two`. The system cannot derive this — it must be
   taught. The mapping from tally-sequence to number-word is conventional,
   not natural. This is the normative (n/1) layer.

3. **Number-words must not participate in Prolog's native arithmetic.** You
   cannot write `X is two + three` because `two` and `three` are atoms, not
   integers. This is a feature. Any arithmetic on number-words must go through
   the system's own grounded operations and meaning fields. The philosophical
   point: the system earns its arithmetic rather than inheriting it from the
   implementation language.

4. **Co-referentiality is earned, not given.** `two` and `one-plus-one` are
   NOT co-referential at system start. They become co-referential when the
   system discovers (through counting) that adding one to one produces the
   same tally-sequence that `two` names, and the teacher endorses this. The
   relaxation from "two separate things" to "the same thing" is the experience
   of symmetric intersubstitutability described in Ch. 2 (revised) — the
   "aha!" moment as differentiation relaxes.

5. **The name defers to the trace.** When you use `two` in a computation, you
   use the name. The trace (the counting history that produced it) is
   elsewhere — in the meaning field, in the grounded arithmetic module. The
   name is always a shorthand that doesn't fully capture the web of inferences
   that produced it. This separation between name and history is built into
   the Prolog architecture (atoms vs. dynamic database).

## Data structure

```prolog
%% Number-word atoms: one, two, three, four, five, six, seven, eight, nine, ten
%% (extend as needed for benchmark numbers: twenty, fifty, hundred)

%% The teacher's naming table (asserted during Level 0 curriculum):
:- dynamic numeral_name/2.
%% numeral_name(+TallySequence, +NumberWord)
%% numeral_name(s(0), one).
%% numeral_name(s(s(0)), two).
%% etc.

%% Conversion utilities (for interfacing with grounded arithmetic):
tally_to_word(Tally, Word) :-
    numeral_name(Tally, Word).

word_to_tally(Word, Tally) :-
    numeral_name(Tally, Word).
```

## Operations

### learn_name/2
```prolog
%% learn_name(+TallySequence, +NumberWord)
%% Called by the teacher during Level 0. Asserts the naming convention.
%% Also creates initial meaning field entry for this numeral:
%%   deposit(NumberWord, interp(named_count(TallySequence), teacher, endorsed))
```

### resolve/2
```prolog
%% resolve(+NumberWord, -TallySequence)
%% Look up the tally-sequence for a number-word.
%% Fails if the name hasn't been taught yet.
```

### co_referential/2
```prolog
%% co_referential(+ExprA, +ExprB)
%% True if ExprA and ExprB have been endorsed as co-referential in the
%% meaning field. NOT the same as structural equality.
%% e.g., co_referential(two, add(one, one)) succeeds only after the
%% system has discovered and the teacher has endorsed this connection.
```

## Constraints for implementers

- **Never** use Prolog integers (0, 1, 2, ...) as number-words. The whole point
  is that `two` is an atom with no arithmetic properties.
- **Never** pre-populate co-referentiality. It must be earned.
- The `numeral_name/2` table should be populated incrementally through the
  curriculum, not all at once. The system learns `one` before `two` before
  `three`.
- Number-words beyond `ten` may use compound forms: `twenty`, `thirty`, etc.
  Place-value structure (twenty = two tens) is NOT built into the naming —
  it must be discovered through the system's own activity.
- The "more/less" vocabulary (`more(five, three, two)` meaning "five is
  two-more-than-three") is a relational predicate over number-words, NOT a
  new naming layer. It belongs in the meaning field as an asymmetric
  predicate. See `01_MEANING_FIELDS.md`.

## Relationship to existing code

- `grounded_arithmetic.pl` already uses `recollection([tally, tally, ...])` —
  these are the tally-sequences that number-words name.
- `grounded_utils.pl` has `decompose_base10/3` — this should work with
  number-words via the conversion utilities, not directly on integers.
- The Peano notation (`s(s(s(0)))`) used elsewhere in the codebase is a
  tally-sequence representation. Number-words are the normative names for
  these sequences.

## Open questions

- Should number-words be taught by the teacher for all numbers up to 100, or
  only for benchmarks (1-10, 20, 25, 50, 100)? Teaching all 100 is
  philosophically honest (children do learn to name all of them) but tedious
  to implement. Benchmark-only is pragmatic.
- How do compound number-words work? `thirteen` is not compositional in English
  the way `twenty-three` is. Should the system learn `thirteen` as a primitive
  name, or discover it as `ten-and-three`? Both happen developmentally.
- The anaphoric claim is strong — each use of `two` refers to a different act
  of thought. In the implementation, `two` is a single Prolog atom that
  unifies with itself everywhere. This is a known gap between the philosophy
  and the formalization. Document it honestly.
