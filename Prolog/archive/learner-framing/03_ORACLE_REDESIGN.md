# 03 — Teacher Module (formerly "Oracle")

## Purpose

Replace the oracle with a **teacher** — an agent that monitors the student's
activity for recognizable patterns and prompts the student to look again at
its own work. The teacher does not compute, does not give answers, and does
not follow a fixed curriculum script. The teacher recognizes what's
interesting in what the student already did, says "look at this," and
validates or rejects the student's response.

This framing comes from a basic observation about teaching: the student often
has no idea what they're doing or what's interesting about it. A good teacher
recognizes patterns in the student's work before the student does, and says
"hey, this is interesting — think about it a little bit more." Reflection
is not mysterious. It is recognition by a second person, followed by the
student looking again.

## What the current oracle does (and why it's wrong)

The current `oracle_server.pl`:
- Receives an operation and strategy name
- Internally runs the strategy FSM
- Returns the result AND an interpretation string
- The synthesis engine wraps this in a clause and asserts it

This makes the oracle an omniscient answer-giver. The name "oracle" implies
one-way transmission from an all-knowing source. The student does nothing;
the oracle does everything. This is philosophically wrong and pedagogically
backwards.

## What the teacher does

### 1. Monitors the student's traces and meaning fields

The teacher watches what the student does. When the student counts, the trace
goes into the database. The teacher looks at the database and recognizes
patterns — shared endpoints, repeated structures, potential partitions,
errors that reveal interesting reasoning.

```prolog
%% teacher_recognize(+TraceDB, +MeaningFields, -Recognition)
%% Scans the student's traces and meaning fields for recognizable patterns.
%% Returns a recognition: a pattern the teacher noticed in the student's work.
%%
%% Example recognitions:
%%   shared_endpoints(zero, three, ten)
%%     — "You counted 0→3 and 3→10 and 0→10"
%%   repeated_structure(carry_at_9, [trace1, trace2, trace3])
%%     — "Something happens every time you cross 9"
%%   potential_partition(ten, three, seven)
%%     — "These traces might tell you something about ten"
%%   interesting_error(claimed_partition(ten, four, seven))
%%     — "You said ten = four + seven. Look again."
```

The teacher's recognition is **triggered by the student's activity**. The
teacher doesn't invent questions from a curriculum table. The student's
prior activity creates the conditions for the teacher's recognition. This
is the second-person position — recognizing something in another's work
that they can't yet see themselves.

### 2. Prompts the student to look again

When the teacher recognizes something, it prompts the student:

```prolog
%% teacher_prompt(+Recognition, -Prompt)
%% Translates a recognition into a prompt directed at the student's own work.
%%
%% teacher_prompt(shared_endpoints(zero, three, ten),
%%                count_steps_between(three, ten)).
%%   — "Count the steps from three to ten."
%%
%% teacher_prompt(potential_partition(ten, three, seven),
%%                check_partition(ten, three, seven)).
%%   — "Is ten the same as three and seven together?"
%%
%% teacher_prompt(interesting_error(claimed_partition(ten, four, seven)),
%%                recount(four, ten)).
%%   — "Count from four to ten again."
```

The prompt is always about the student's own work. It directs the student to
apply its own operations (counting, checking) to its own traces. The teacher
doesn't do the counting. The student does.

### 3. Validates or rejects the student's response

After the student looks again and produces a claim:

```prolog
%% teacher_validate(+Claim, -Response)
%% Response: yes | no
%%
%% The teacher does NOT explain why. It does not provide the correct answer
%% on rejection. It says yes or no. The student must figure out what went
%% wrong through further activity.
```

The "no" is the informative signal — it depopulates the meaning field. The
"yes" endorses a new connection. Both are recognition acts.

### 4. Names things (normatively)

```prolog
%% teacher_name(+TallySequence, -NumberWord)
%% teacher_name(s(s(0)), two).
```

Naming is teaching vocabulary. The teacher says "that's called two." This
is normative — conventional, not derivable. The student cannot learn names
without being taught.

## The recognition-trigger architecture

The key insight: **the student's action triggers the teacher's recognition,
and the teacher's recognition enables the student's growth.** Neither alone
produces learning.

```
Student counts 0→3           → trace stored
Student counts 3→10          → trace stored
Student counts 0→10          → trace stored
                              ↓
Teacher notices shared endpoints (0→3→10 = 0→10)
                              ↓
Teacher prompts: "Count steps from 0 to 3, then from 3 to 10"
                              ↓
Student counts: 3 steps, then 7 steps
Student claims: partition(ten, three, seven)
                              ↓
Teacher validates: "yes"
                              ↓
Meaning field updated: ten AND three-plus-seven (endorsed)
```

The teacher's monitoring function runs after each student action (or batch
of actions). It pattern-matches on the trace database for recognizable
structures. The patterns the teacher can recognize are, honestly, hard-coded
by the programmer — this is the same limitation as the "reflection templates"
in 06_REFLECTION.md. But the framing is better: the templates represent what
a teacher would notice, not what the system "discovers autonomously."

## What the teacher must NOT do

- **Must NOT compute results.** The teacher recognizes patterns and prompts.
  It does not solve problems.
- **Must NOT provide strategy descriptions.** No "count on from 8 by 5."
  Strategies are the student's activity. The teacher points at interesting
  aspects of that activity.
- **Must NOT follow a rigid curriculum sequence.** The teacher's prompts are
  triggered by what the student has actually done, not by a predetermined
  lesson plan. (The teacher does have background knowledge about which
  patterns are worth recognizing — this is the teacher's expertise.)
- **Must NOT use Prolog's native arithmetic for validation.** Validation
  should be against a table of facts established through prior student
  activity and teacher endorsement. An outer "objective check" layer (o/1)
  may use native arithmetic but should be clearly separated.

## The "no" as care

The teacher's rejection is not punishment. It is the informative signal that
depopulates meaning fields. "Others may not understand what you are trying
to say. I get it, and it's good thinking, but think a little bit more about
how well it fits into the larger system" (Savich, 2022, p. 221, paraphrasing
Carspecken). The recognition desire impels the student to work to be
understood. Education is an enabling condition for recognition, not a
dictatorial imposition of correctness.

In the formalization:
- Rejected claims are recorded, not discarded
- The teacher may re-prompt with a different angle if the same wrong claim
  recurs
- The cost of rejection is low — the student is not penalized, just
  redirected

## Migration path from current oracle

1. Rename `oracle_server.pl` to `teacher.pl`
2. Replace `query_oracle/4` with `teacher_recognize/3` + `teacher_prompt/2` +
   `teacher_validate/2`
3. Add `teacher_name/2` for vocabulary teaching
4. Remove all direct FSM execution — strategies run in the student system,
   not in the teacher
5. The teacher's recognition patterns are loaded from a knowledge base that
   represents the teacher's expertise (what patterns are worth noticing)

## Philosophical grounding

The teacher operates at the **normative** (n/1) level. It represents the
second-person position — the community's capacity to recognize what the
student is doing and respond appropriately.

The teacher's authority is **revisable**. The teacher can be wrong. The
student can eventually outgrow the teacher's recognition patterns (this is
where the formalization breaks productively — a finite set of recognition
templates cannot capture all possible interesting patterns in a student's
work).

The relationship between student and teacher is **mutual recognition**, not
transmission. The teacher needs the student's activity to have something to
recognize. The student needs the teacher's recognition to see what they
couldn't see alone. This is Hegel's Anerkennung applied to education.

## Open questions

- How rich does the teacher's pattern-recognition need to be? A handful of
  templates (shared endpoints, carry events, repeated structures) might
  suffice for Level 1-3. More sophisticated recognition (commutativity,
  distributivity) would require more templates. Where does this stop being
  tractable?
- Can the teacher learn to recognize new patterns from the student's
  activity? This would be a teacher that develops alongside the student.
  Almost certainly too ambitious for now, but worth noting as an aspiration.
- The teacher's "look at this" is a speech act — an illocutionary force that
  changes the student's attention. How does this interact with the PML's
  modal operators? The prompt is a normative compression — "focus here."
  The student's response may be expansion (new connection discovered) or
  further compression (recount, recheck).
