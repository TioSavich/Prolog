# 06 — Reflection Mechanism (The Hard Problem)

## Purpose

The reflection mechanism is how the system looks at its own counting traces and
meaning fields and notices structural patterns — partitions, commutativity, place
value, more/less relations. This is the **algorithmic elaboration** step in
Brandom's meaning-use analysis: the PP (practice-to-practice) move that
transforms counting practices into arithmetic practices.

This is the hardest component to design. We do not yet have a satisfactory
answer. This document describes the problem, surveys possible approaches, and
sets constraints. The point of the formalization is partly to discover where
reflection resists formalization. That breakdown is itself a finding.

## The problem

The system counts from zero to ten. The trace exists:
`[z, s(z), s(s(z)), ..., s^10(z)]`

The system also counts from three to ten. That trace exists too.
And from zero to three.

The **structural fact** that `ten = three + seven` (a partition) is implicit in
these three traces — the first trace can be decomposed into the second and third.
But the system doesn't know this until it reflects on the traces and notices the
pattern.

What does "notice" mean computationally? This is the hard problem. Options:

## Approach 1: Teacher-prompted reflection

The teacher asks questions that force the system to examine its traces:

```
Teacher: "You counted from zero to ten. You also counted from zero to three
         and from three to ten. What do you notice?"
System: [examines traces, finds shared endpoints]
System: "The trace from zero to ten passes through three."
Teacher: "Yes. What does that tell you about ten, three, and seven?"
System: "ten is three-and-seven?"
Teacher: "Yes." [endorses partition(ten, three, seven)]
```

The conversation IS the reflection. The teacher's questions structure what the
system examines. The system does the pattern-matching; the teacher directs
attention.

**Advantages**: Implementable. Philosophically coherent — the teacher-as-teacher
prompts reflection without doing the reflecting. The Socratic method.

**Disadvantages**: Shifts intelligence to question-selection. The teacher must know
which questions to ask, which means it implicitly knows the answers. This is
less problematic than the current teacher (which gives answers directly) but still
concentrates knowledge in the teacher.

**Implementation sketch**:
```prolog
%% teacher_reflection_prompt(+Level, +Context, -Prompt)
%% Level 2 prompts (partition discovery):
teacher_reflection_prompt(2, traces_available(From, Via, To), Prompt) :-
    Prompt = check_shared_endpoints(From, Via, To).

%% The system responds by querying its own trace database:
respond_to_prompt(check_shared_endpoints(From, Via, To), Response) :-
    stored_trace(From, Via, forward, _TraceA),
    stored_trace(Via, To, forward, _TraceB),
    stored_trace(From, To, forward, _TraceC),
    Response = partition_candidate(To, From_to_Via_steps, Via_to_To_steps).
```

## Approach 2: Hard-coded reflection templates

Define a set of "reflection queries" per curriculum level:

```prolog
%% Level 1 reflection: successor/predecessor relations
reflect_level_1 :-
    stored_trace(A, B, forward, Trace),
    length(Trace, 2),  % single step
    deposit(B, interp(successor_of(A), counting, untested)).

%% Level 2 reflection: partition discovery
reflect_level_2(Target) :-
    stored_trace(zero, A, forward, _),
    stored_trace(A, Target, forward, _),
    stored_trace(zero, Target, forward, _),
    count_steps(zero, A, StepsA),
    count_steps(A, Target, StepsB),
    deposit(Target, interp(partition(A, StepsB_word), counting, untested)).
```

**Advantages**: Simple, transparent, testable.

**Disadvantages**: The templates encode the patterns the system is supposed to
discover. The system isn't really discovering anything — the programmer is
discovering and encoding the discovery as a template. This is honest but limited.

**When to use**: As a first implementation. The templates make explicit what
reflection is supposed to produce. A more sophisticated mechanism can replace
them later.

## Approach 3: Diagonalization over the meaning field

This is the most philosophically interesting and hardest approach. The idea
(drawing on Carspecken, 2013) is that the system reflects by applying its own
inference mechanisms to its own meaning field — a kind of self-application or
diagonalization.

The system's counting practice produces traces. Those traces are stored. The
reflection mechanism treats the trace database as INPUT to the same counting/
pattern-matching mechanisms. This is the recursion: the system counts its own
counting.

```prolog
%% The system has counting traces for 0→3, 0→7, 0→10, 3→10
%% Reflection: count how many traces share an endpoint with 10
%% This is counting over traces, not over numbers
reflect_diagonal(Target) :-
    findall(Via,
        (stored_trace(_, Via, _, _), stored_trace(Via, Target, _, _)),
        Vias),
    %% For each Via, check if 0→Via and Via→Target traces exist
    %% If so, this is a partition candidate
    ...
```

**Advantages**: Genuine self-reference. The system uses its own capacities to
examine its own products. This is the closest to what Hegel means by
self-consciousness — Spirit examining itself.

**Disadvantages**: Risks infinite regress (reflecting on reflections on
reflections...). Hard to implement without smuggling in the very patterns we
want the system to discover. The `findall/3` in the sketch above is already
a meta-level operation that presupposes the pattern structure.

**The productive failure**: This approach will likely fail to produce fully
autonomous reflection. That failure is itself the point of contact with the
manuscript's central argument — formalization breaks productively when it tries
to capture genuine self-reference. The system can count, but it cannot fully
capture what counting means without stepping outside counting. This is the
Hegelian Infinite that the manuscript is about.

## Approach 4: Recognition-triggered reflection (recommended starting point)

Combine approaches 1 and 2 under the recognition-trigger framing:
- The student acts (counts, computes, makes errors)
- The teacher monitors the student's trace database for recognizable patterns
  (hard-coded recognition templates represent the teacher's expertise)
- When a pattern is recognized, the teacher prompts the student to look
  again at its own work
- The student applies its own operations where the teacher pointed
- Results are deposited into meaning fields as untested, then validated
  by teacher

The intelligence lives in the **relationship** — the teacher's capacity to
recognize patterns in the student's work, and the student's capacity to
look again when prompted. Neither alone produces learning. The recognition
templates represent what a teacher would notice, not what the system
discovers autonomously.

Later work can explore Approach 3 as an extension, with the explicit goal of
finding where it breaks.

## Constraints for implementers

- **Never claim the system "discovers" patterns autonomously.** The reflection
  templates encode the programmer's knowledge of what patterns exist. The
  system applies those templates to its own traces, which is non-trivial,
  but it is not discovery in the philosophical sense.
- **All reflection outputs must be deposited as untested.** The teacher validates
  them. The system proposes; the community endorses.
- **Reflection templates must operate only on stored traces.** They cannot
  access Prolog's native arithmetic or any knowledge not earned through
  counting.
- **Document where reflection fails.** When a template cannot extract a pattern
  that a human would notice, document that gap. Those gaps are findings.
- **The cost of reflection should be tracked.** Reflecting on traces is itself
  an activity with proprioceptive cost. In the PML, reflection is compressive
  (examining, narrowing attention). This cost should be part of the resource
  budget.

## What reflection should produce (by level)

### Level 1: Successor/predecessor relations
- `successor_of(one, two)`, `predecessor_of(three, two)`

### Level 2: Partition discovery
- `partition(ten, three, seven)`, `partition(ten, five, five)`
- All partitions of benchmark numbers (5, 10, 20)

### Level 3: More/less relations
- `more(five, three, two)` — five is two-more-than-three
- `less(three, five, two)` — three is two-less-than-five

### Level 4: Commutativity (if discoverable)
- Counting from A to A+B takes the same steps as counting from B to B+A
- This requires comparing traces, which is a harder reflection

### Level 5: Place value structure
- The carry events in counting traces recur every 10 steps
- This requires pattern-matching over trace structure, not just endpoints

## Open questions

- Can Prolog's homoiconicity (code = data) be used for genuine self-reference
  in Approach 3? The system's traces are Prolog terms. Its reflection
  mechanism is Prolog code. In principle, the code can examine the code. But
  does this constitute the kind of self-reference that matters philosophically?
- Is there a connection between Gödel diagonalization (mentioned in ch2_revised
  re: incompleteness) and the reflection mechanism? The system encoding its
  own traces as data and operating on them is structurally similar to Gödel
  numbering. This might be worth exploring as a formal analogy.
- The "attributed variable" mechanism (arche-trace in `automata.pl`) resists
  unification with concrete terms. Could this be used in the reflection
  mechanism to model the irreducibility of subjective experience? The trace
  can be examined but never fully captured as a concrete term. Whether this
  constitutes a philosophically serious engagement with irreducibility or just
  a technical trick is an open question. Do not claim more than the mechanism
  delivers.
