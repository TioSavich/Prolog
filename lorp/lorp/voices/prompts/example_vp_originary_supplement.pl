:- module(list_vp_originary_supplement_v2, []).

:- discontiguous author_self_describes_term/3.
:- discontiguous derrida_text_asserts/2.
:- discontiguous derrida_text_asserts_about_husserl_requirement/2.
:- discontiguous authorial_move/4.
:- discontiguous author_procedural_directive_to_reader/3.
:- discontiguous structural_necessity/3.
:- discontiguous derrida_text_uses_term_for_relation/4.

% =========================================================================
% Immanent-reader substrate for Derrida, Voice and Phenomenon, Ch 7, p. 87.
% Passage anchor: "originary supplement" (instance 0).
%
% v2 (2026-04-24): second pass under the updated predicate catalog. Three
% catalog fixes applied:
%
%   (1) author_procedural_directive/3 split into
%       author_procedural_directive_to_reader/3 (imperative / second-person
%       / "let us X" only) and structural_necessity/3 (descriptive
%       constraint).
%   (2) procedurally_licensed_under_erasure/1 now requires an explicit
%       author-supplied erasure marker (sous rature, strikethrough,
%       "strategically nicknamed," etc.). This passage contains no such
%       marker; the predicate is NOT emitted. Likewise
%       adjacent_assertion_and_denial/3 now requires strict P / not-P at
%       the same predicate; the closing paradox is concessive, so it goes
%       to author_textual_marker/4 instead.
%   (3) authorial_immanent_move/3 replaced by authorial_move/4 with a mode
%       in {immanent, deconstructive_intervention, external_critique,
%       refuses_commentary}. The "neither commentary nor interpretation"
%       clause is the paradigm case for refuses_commentary.
%
% Citation is short ("VP p. 87 (Ch 7)") per cheat-proofing: no
% multi-sentence excerpts in this file. Verbatim text lives in the gloss.
% =========================================================================

% -------------------------------------------------------------------------
% Cross-language pairings that Derrida supplies inside the passage
% -------------------------------------------------------------------------

translates_in_source("Derrida", "in the place of", "fuer etwas").
translates_in_source("Derrida", "for-itself",      "fuer-sich").

% -------------------------------------------------------------------------
% Use/mention and quotation facts
% -------------------------------------------------------------------------

quoted_in_passage("vp_p87_supp", "in the place of").
quoted_in_passage("vp_p87_supp", "fuer etwas").
quoted_in_passage("vp_p87_supp", "fuer-sich").
quoted_in_passage("vp_p87_supp", "in-the-place-of-itself").

unquoted_in_passage("vp_p87_supp", "supplement").
unquoted_in_passage("vp_p87_supp", "supplementarity").
unquoted_in_passage("vp_p87_supp", "differance").
unquoted_in_passage("vp_p87_supp", "for-itself").
unquoted_in_passage("vp_p87_supp", "presence").

% -------------------------------------------------------------------------
% Authorial moves with respect to Husserl's text.
%
% Mode selection:
%
%   refuses_commentary  -- Derrida's own self-description of the reading's
%                          register. He writes that the reading "can be
%                          simply neither that of commentary nor that of
%                          interpretation." Paradigm case per the
%                          2026-04-24 catalog update.
%
%   deconstructive_intervention -- Derrida stays inside Husserl's setup
%                          (the in-the-place-of structure of every sign
%                          that Husserl's LI presupposes) but draws the
%                          consequence that the for-itself of
%                          self-presence itself arises in this movement.
%                          Husserl's phenomenology would not grant this
%                          conclusion; the move exposes a tension
%                          Husserl's method refuses to acknowledge.
% -------------------------------------------------------------------------

authorial_move("Derrida", refuses_commentary,
  works_inside("Husserl's First Logical Investigation"),
  draws_consequence("a reading that is simply neither that of commentary nor that of interpretation")).

authorial_move("Derrida", deconstructive_intervention,
  works_inside("Husserl's text"),
  draws_consequence("the for-itself of self-presence arises in the movement of supplementarity as originary substitution")).

authorial_move("Derrida", deconstructive_intervention,
  works_inside("Husserl's First Logical Investigation"),
  draws_consequence("Husserl's distinction between indicative and expressive signs presupposes an uninterrogated in-the-place-of")).

% -------------------------------------------------------------------------
% Reader-directed procedural directives.
%
% Three triggers in the passage: (a) "We must now verify, going through
% the First Logical Investigation," (b) "Let us first note that...," and
% (c) "What we would like finally to start thinking about is...". All three
% use first-person-plural inclusive address, which the updated catalog
% treats as equivalent to "let us X."
% -------------------------------------------------------------------------

author_procedural_directive_to_reader("Derrida",
  on("relation between sign in general and presence in general"),
  sequence([
    "go through the First Logical Investigation",
    "verify in what way these concepts respect the relation"
  ])).

author_procedural_directive_to_reader("Derrida",
  on("concept of originary supplementarity"),
  sequence([
    "first note that it implies non-fullness of presence",
    "note also that it designates substitutive supplementing in general"
  ])).

author_procedural_directive_to_reader("Derrida",
  on("for-itself of self-presence"),
  sequence([
    "start thinking the for-itself",
    "place it in the movement of supplementarity",
    "read it as originary substitution in the form of fuer etwas"
  ])).

% -------------------------------------------------------------------------
% Structural necessities asserted by Derrida's text (descriptive, NOT
% addressed to the reader). Each one is of the form "X arises / belongs /
% implies Y" without imperative or second-person framing.
% -------------------------------------------------------------------------

structural_necessity("Derrida", on("supplementary differance"),
  claim("vicariates for presence in presence's originary lack in regard to itself")).

structural_necessity("Derrida", on("originary supplementarity"),
  claim("implies the non-fullness of presence")).

structural_necessity("Derrida", on("in-the-place-of (fuer etwas)"),
  claim("belongs to every sign in general")).

structural_necessity("Derrida", on("for-itself of self-presence"),
  claim("arises in the movement of supplementarity as originary substitution")).

structural_necessity("Derrida", on("for-itself"),
  claim("would be an in-the-place-of-itself: put for itself, in the place of itself")).

structural_necessity("Derrida", on("supplement"),
  claim("a possibility that produces by delay that to which it is said to be added")).

% -------------------------------------------------------------------------
% Derrida's text asserts (at passage level).
% Flat assertions the sentence makes without an author-directive frame.
% -------------------------------------------------------------------------

derrida_text_asserts("vp_p87_supp",
  "Husserl took the possibility of the in-the-place-of for granted when distinguishing indicative sign from expressive sign").

derrida_text_asserts("vp_p87_supp",
  "at the beginning we were astonished that Husserl subjected the possibility of this structure to no critical question").

% -------------------------------------------------------------------------
% Derrida renames Husserl's non-fulfillment under Derrida's vocabulary
% -------------------------------------------------------------------------

derrida_text_asserts_about_husserl_requirement(
  "vp_p87_supp",
  "non-fullness of presence is, in Husserl's language, non-fulfillment of an intuition").

% -------------------------------------------------------------------------
% The closing paradox.
%
% Two clauses: "a possibility is said to be added to a prior presence" /
% "the possibility produces by delay that to which it is said to be added."
% These are NOT a strict P / not-P pair at the predicate level -- the
% second clause inverts the temporal priority presupposed by the first
% rather than negating its predicate. Per the 2026-04-24 catalog update,
% concessive / paradoxical structure goes to author_textual_marker/4 and
% NOT to adjacent_assertion_and_denial/3. No sous rature or strikethrough
% marker is present, so procedurally_licensed_under_erasure/1 also does
% NOT fire.
% -------------------------------------------------------------------------

clause_in_passage("vp_p87_supp_c1",
  asserts("the supplement is said to be added to a prior presence"),
  passage("vp_p87_supp"),
  position(first)).

clause_in_passage("vp_p87_supp_c2",
  asserts("a possibility produces by delay that to which it is said to be added"),
  passage("vp_p87_supp"),
  position(second)).

author_textual_marker("Derrida",
  "vp_p87_supp",
  marker("The strange structure of the supplement appears here"),
  connects("vp_p87_supp_c1", "vp_p87_supp_c2")).

% -------------------------------------------------------------------------
% Derrida's text uses terms for relations at specific sites
% -------------------------------------------------------------------------

derrida_text_uses_term_for_relation(
  "vp_p87_supp",
  term("originary supplement"),
  between("presence", "sign in general"),
  at_site_of("originary lack of presence in regard to itself")
).

derrida_text_uses_term_for_relation(
  "vp_p87_supp",
  term("in-the-place-of-itself"),
  between("for-itself", "for-itself"),
  at_site_of("auto-donation of self-presence")
).

% -------------------------------------------------------------------------
% Passage citations
% -------------------------------------------------------------------------

passage_source("vp_p87_supp",    "VP p. 87 (Ch 7)").
passage_source("vp_p87_supp_c1", "VP p. 87 (Ch 7), first clause of closing sentence").
passage_source("vp_p87_supp_c2", "VP p. 87 (Ch 7), second clause of closing sentence").
