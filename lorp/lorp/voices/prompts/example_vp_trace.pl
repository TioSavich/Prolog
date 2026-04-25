:- module(list_vp_trace, []).

% =========================================================================
% Immanent-reader substrate for Voice and Phenomenon, Chapters 6-7
% (the "trace" passage, instance 8 in the load-bearing context).
%
% Output under Policy D17.1 (immanence rule). Phil Carspecken flagged
% "trace" as the concept most prone to attribution-of-structure
% (Gaifman-like fixed-point attribution). This substrate is extra-careful:
% no predicate emits is_trace_function, trace_structure, or any
% fixed_point-shaped claim. Where the passage uses the word "trace" to
% mark a relation, the usage is recorded with
% derrida_text_uses_term_for_relation/4, which describes the author's
% term-usage without attributing function to the concept.
%
% The passage contains Derrida's own explicit anti-attribution move
% ("The trace is not an attribute about which we could say..."). That
% refusal is captured at the text level with derrida_text_refuses/2 and
% author_self_describes_term/3, not lifted into a concept-level claim.
%
% Citation kept short per the cheat-proofing constraint. Verbatim
% excerpts live only in the gloss.
% =========================================================================

% -------------------------------------------------------------------------
% Passage citation
% -------------------------------------------------------------------------

passage_source("vp_trace", "VP Ch 6-7 (trace passage)").

% -------------------------------------------------------------------------
% Category 1 -- Naming
% -------------------------------------------------------------------------

name(trace, "trace").
name(archi_writing, "archi-writing").

% -------------------------------------------------------------------------
% Category 2 -- Use/mention in the passage
% -------------------------------------------------------------------------

quoted_in_passage("vp_trace", "movement").
quoted_in_passage("vp_trace", "signification").
quoted_in_passage("vp_trace", "expressive stratum").
quoted_in_passage("vp_trace", "spacing").
quoted_in_passage("vp_trace", "interval").
quoted_in_passage("vp_trace", "time").
quoted_in_passage("vp_trace", "outside").
quoted_in_passage("vp_trace", "in").
quoted_in_passage("vp_trace", "presents").
quoted_in_passage("vp_trace", "is originarily").

unquoted_in_passage("vp_trace", "trace").
unquoted_in_passage("vp_trace", "living present").
unquoted_in_passage("vp_trace", "self-presence").
unquoted_in_passage("vp_trace", "archi-writing").

% -------------------------------------------------------------------------
% Category 3 -- Multi-valued meaning predicate on "spacing"
% The sentence: "spacing at once as 'interval' or difference and as
% openness to the outside" predicates meaning of "spacing" over two values.
% -------------------------------------------------------------------------

multi_valued_meaning_predicate(
  "vp_trace",
  "spacing",
  ["interval or difference", "openness to the outside"]
).

% -------------------------------------------------------------------------
% Category 4 -- Textual marker ("at once") joining the two values of spacing
% -------------------------------------------------------------------------

clause_in_passage("vp_trace_spacing_interval",
  asserts("spacing is interval or difference"),
  passage("vp_trace"),
  position(first)).

clause_in_passage("vp_trace_spacing_openness",
  asserts("spacing is openness to the outside"),
  passage("vp_trace"),
  position(second)).

author_textual_marker("Derrida",
  "vp_trace",
  marker("at once"),
  connects("vp_trace_spacing_interval", "vp_trace_spacing_openness")).

% -------------------------------------------------------------------------
% Category 5 -- Authorial procedural directives and self-description
% -------------------------------------------------------------------------

% Derrida's explicit priority-of-thinking directive:
% "It is necessary to think originary-being from the trace and not
%  the trace from originary-being."
author_procedural_directive("Derrida",
  on("thinking the trace and originary-being"),
  sequence([
    "think originary-being from the trace",
    "do not think the trace from originary-being"
  ])
).

% The "unthinkable if we start from" refusal pattern.
author_procedural_directive("Derrida",
  on("thinking the trace"),
  sequence([
    "do not start from the simplicity of a present whose life would be interior to itself"
  ])
).

% Derrida's explicit anti-attribution move. Recorded in the self-describes
% register because it is meta-commentary on how the term "trace" must not
% be grammatically positioned.
author_self_describes_term("Derrida",
  "trace",
  "not an attribute of the self of the living present").

author_self_describes_term("Derrida",
  "trace",
  "that from which originary-being is to be thought").

% -------------------------------------------------------------------------
% Category 6 -- Textual construction (the "archi-X" shape)
% -------------------------------------------------------------------------

author_textual_construction("Derrida",
  construction("archi-writing"),
  with_instruction("at work in the origin of sense")).

% -------------------------------------------------------------------------
% Category 7 -- Text-level assertions
% -------------------------------------------------------------------------

derrida_text_asserts("vp_trace",
  "pure difference reintroduces originarily all the impurity into the self-presence of the living present").

derrida_text_asserts("vp_trace",
  "the living present arises on the basis of its non-self-identity").

derrida_text_asserts("vp_trace",
  "the living present arises on the basis of the retentional trace").

derrida_text_asserts("vp_trace",
  "the living present is always already a trace").

derrida_text_asserts("vp_trace",
  "the self of the living present is originarily a trace").

derrida_text_asserts("vp_trace",
  "archi-writing is at work in the origin of sense").

derrida_text_asserts("vp_trace",
  "sense has a temporal nature and is never simply present").

derrida_text_asserts("vp_trace",
  "sense has always already exited from itself into the expressive stratum of lived-experience").

derrida_text_asserts("vp_trace",
  "the temporalization of sense is from the very beginning spacing").

derrida_text_asserts("vp_trace",
  "there is no absolute interiority").

derrida_text_asserts("vp_trace",
  "space is in time as the pure exiting of time to the outside of itself").

% -------------------------------------------------------------------------
% Category 7 -- Text-level refusals
% -------------------------------------------------------------------------

derrida_text_refuses("vp_trace",
  thinkable_starting_from("the simplicity of a present whose life would be interior to itself")).

derrida_text_refuses("vp_trace",
  attribute_claim("the self of the living present is originarily the trace")).

derrida_text_refuses("vp_trace",
  priority_claim(think_trace_from("originary-being"))).

derrida_text_refuses("vp_trace",
  absolute_interiority).

% -------------------------------------------------------------------------
% Category 7 -- Relation-term usage
% Derrida uses "trace" to mark relations. Recorded as term-usage, not
% as functional attribution. This is the predicate that replaces the
% tempting (but forbidden) is_trace_function / trace_structure /
% fixed_point("trace", ...) moves.
% -------------------------------------------------------------------------

derrida_text_uses_term_for_relation(
  "vp_trace",
  term("trace"),
  between("living present", "its outside"),
  at_site_of("openness to exteriority in general")
).

derrida_text_uses_term_for_relation(
  "vp_trace",
  term("retentional trace"),
  between("living present", "its non-self-identity"),
  at_site_of("origin of self-presence")
).

derrida_text_uses_term_for_relation(
  "vp_trace",
  term("spacing"),
  between("time", "its outside"),
  at_site_of("self-relation of time")
).

% -------------------------------------------------------------------------
% Category 7 -- Authorial immanent move (working inside Husserl's text)
% -------------------------------------------------------------------------

authorial_immanent_move("Derrida",
  works_inside("Husserl's recognition of the temporal nature of sense"),
  draws_consequence("sense is never simply present and is always already engaged in the movement of the trace")).
