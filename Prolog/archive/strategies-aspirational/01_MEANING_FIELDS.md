# 01 — Meaning Fields Module

## Purpose

Implement per-numeral meaning fields that represent the set of compossible
interpretations a numeral participates in. The meaning field is the connective
substrate between strategies — it is what strategies deposit into and draw from.

## Philosophical requirements

1. **Meaning fields are not databases of facts.** A meaning field is the holistic
   experience of meaning for a numeral within a particular normative horizon
   (Carspecken, 1995). The module reconstructs this through analysis — it
   produces an artifact, not the field itself. Documentation must reflect this
   distinction.

2. **Density represents confusion.** A meaning field with many undifferentiated
   elements (connected by inclusive OR) represents a state of confusion.
   Learning is the depopulation of the field through information — the teacher's
   "no" rules out elements. The combinatorics of confusion grow as 2^n where n
   is the number of vocabularies in play (Savich, 2022, Ch. 3).

3. **Internal structure matters.** Elements in a meaning field are connected by:
   - **AND** (conjunction/compossibility): both interpretations co-occur in the
     same normative horizon. `two` is `one-plus-one` AND `successor-of-one`.
   - **OR** (inclusive disjunction/boundary): interpretations may or may not
     share a universe of thought. At the boundary between compossible regions.
   - **XOR** (exclusive disjunction/incompossibility): interpretations are
     claimed as inhabiting distinct compossible regions. `two` is NOT `three`.

   The acquisition of XOR for a domain is developmental — young children may
   not yet distinguish OR from XOR for certain mathematical vocabularies
   (Savich, 2022, pp. 226-228). Before XOR, the learner experiences inclusive
   disjunction, resulting in hybridized models.

4. **Relaxation enriches, depopulation clarifies.** When a new co-referentiality
   is accepted (e.g., `two` = `one-plus-one`), the meaning field gains a new
   AND-connected element. When an incompatibility is established (e.g., `two`
   XOR `three`), the field gains structure by excluding. Both are learning.
   Neither alone is sufficient.

5. **Meaning fields are non-monotonic.** New information can change the status
   of existing elements — what was OR can become AND or XOR. What was
   compossible can become incompossible given new context. This non-monotonicity
   is a feature, not a bug (Brandom, 2000, p. 88).

## Data structure

```prolog
%% meaning_field(+Numeral, -Field)
%% Field is a list of interpretations with connective structure.
%%
%% Each interpretation is a term of the form:
%%   interp(Content, Origin, Validity)
%%
%% Content: the inferential content, e.g., add(one, one)
%% Origin:  the trace that produced it (strategy name + counting history)
%% Validity: one of {endorsed, rejected, untested}
%%
%% Connectives between interpretations are stored separately:
%%   connective(Numeral, InterpA, InterpB, Type)
%%   Type: one of {and, or, xor}

:- dynamic meaning_field_entry/3.   % meaning_field_entry(Numeral, Interp, Validity)
:- dynamic connective/4.            % connective(Numeral, InterpA, InterpB, Type)
```

## Operations

### deposit/3
```prolog
%% deposit(+Numeral, +Interp, +Origin)
%% Add a new interpretation to a numeral's meaning field.
%% Initial validity: untested.
%% Initial connective to all existing entries: or (undifferentiated).
```

Philosophical constraint: depositing an interpretation does NOT make it
endorsed. It enters the field as untested — the teacher must validate it. This
prevents the system from "learning" by mere accumulation.

### endorse/2
```prolog
%% endorse(+Numeral, +Interp)
%% Mark an interpretation as endorsed. Called by teacher after validation.
%% Changes connective from 'or' to 'and' with other endorsed interpretations
%% of the same numeral (they are now compossible).
```

### reject/2
```prolog
%% reject(+Numeral, +Interp)
%% Mark as rejected. Changes connective to 'xor' with endorsed entries.
%% This is the teacher's "no" — the informative signal.
```

### query_field/3
```prolog
%% query_field(+Numeral, +RequiredConnective, -Interps)
%% Retrieve all endorsed interpretations connected by the given connective.
%% Strategies call this to check what they can use.
```

### project/4
```prolog
%% project(+Numeral, +SourceStrategy, +TargetStrategy, -Result)
%% Test whether an endorsed interpretation from SourceStrategy's deposits
%% can be used by TargetStrategy. This is the projective validity test.
%% Returns: valid, invalid, or untested.
```

## Constraints for implementers

- Do NOT use Prolog's native arithmetic (`is/2`, `=:=`, etc.) anywhere in this
  module. All numeric content must come from grounded representations or
  number-word atoms.
- Do NOT pre-populate meaning fields. They must be empty at system start and
  grow only through counting activity and teacher interaction.
- Do NOT assume AND/OR/XOR structure is fixed. It must be revisable through
  new teacher interactions (non-monotonicity).
- Document honestly where the implementation simplifies the philosophical
  concept. The meaning field as experienced is holistic and embodied; the
  module is a reconstruction.

## Open questions

- How should meaning fields interact with the PML's three validity modes?
  An interpretation endorsed at the normative level (teacher says yes) may not
  yet be endorsed at the objective level (Prolog's `2` hasn't confirmed it).
  The subjective level (the trace) is always present as Origin.
- Should meaning fields have a maximum density before forcing depopulation?
  This could model the phenomenology of confusion — too many OR-connected
  elements triggers a crisis.
- How does the meaning field relate to Carspecken's (2013) work on recursion
  over meaning? The diagonalization move — where the system reflects on its
  own meaning field — is the hard problem. See `06_REFLECTION.md`.
