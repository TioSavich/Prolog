:- module(example_list_derrida, []).

% =========================================================================
% Canonical example of immanent-reader output.
%
% Source: five passages from Derrida (Voice and Phenomenon, Of Grammatology)
% chosen for triggering the substrate markers: use/mention, adjacent
% assertion/denial, author procedural directive, author self-description,
% and textual construction ("arche-X").
%
% This file is a reference for what the immanent-reader skill produces
% on a dense Derridean passage set. All predicates are safe per
% references/immanence_rule.md. No predicate attributes function,
% structure, or essence to a Derridean concept.
% =========================================================================

% -------------------------------------------------------------------------
% Passage 1 -- Voice and Phenomenon ch. 1
% "The sign 'sign' can mean 'expression' (Ausdruck) or 'indication'
%  (Anzeichen)."
% -------------------------------------------------------------------------

both_quoted_and_unquoted("vp_ch1_sign_sign", "sign").
quoted_in_passage("vp_ch1_sign_sign", "expression").
quoted_in_passage("vp_ch1_sign_sign", "indication").

translates_in_source("Derrida", "expression", "Ausdruck").
translates_in_source("Derrida", "indication", "Anzeichen").

multi_valued_meaning_predicate(
  "vp_ch1_sign_sign",
  "sign",
  ["expression", "indication"]
).

husserl_text_asserts("LI_I_first_investigation",
  priority_claim(between("expression", "indication"))).

derrida_text_refuses("vp_ch2",
  priority_claim(between("expression", "indication"))).

% -------------------------------------------------------------------------
% Passage 2 -- Of Grammatology p. 65 (adjacent assertion/denial)
% -------------------------------------------------------------------------

clause_in_passage("og_p65_claim_1",
  asserts("the trace is the absolute origin of sense in general"),
  passage("og_p65"),
  position(first)).

clause_in_passage("og_p65_claim_2",
  asserts("there is no absolute origin of sense in general"),
  passage("og_p65"),
  position(second)).

author_textual_marker("Derrida",
  "og_p65",
  marker("which amounts to saying"),
  connects("og_p65_claim_1", "og_p65_claim_2")).

adjacent_assertion_and_denial(
  "og_p65_claim_1",
  "og_p65_claim_2",
  "og_p65"
).

procedurally_licensed_under_erasure("og_p65_claim_1").
procedurally_licensed_under_erasure("og_p65_claim_2").

% -------------------------------------------------------------------------
% Passage 3 -- Voice and Phenomenon ch. 4 (iterability as immanent move)
% -------------------------------------------------------------------------

authorial_immanent_move("Derrida",
  works_inside("Husserl's text"),
  draws_consequence("sign's being-a-sign requires iterability")).

derrida_text_asserts_about_husserl_requirement(
  "vp_ch4_iterability",
  "a sign occurring only once would not be a sign by Husserl's own criteria"
).

derrida_text_asserts_about_husserl_requirement(
  "vp_ch4_iterability",
  "formal recognizability is required by Husserl's concept of the signifier"
).

derrida_text_asserts_about_husserl_requirement(
  "vp_ch4_iterability",
  "Husserl's concept of signifier-identity requires ideality"
).

% -------------------------------------------------------------------------
% Passage 4 -- Of Grammatology p. 60 (procedural directive on arche-X)
% -------------------------------------------------------------------------

author_procedural_directive("Derrida",
  on("use of transcendental arche"),
  sequence([
    "make its necessity felt",
    "then let it be erased"
  ])
).

author_textual_construction("Derrida",
  construction("arche-X"),
  with_instruction("must comply with both necessity and erasure")).

% -------------------------------------------------------------------------
% Passage 5 -- Voice and Phenomenon p. 97 (Verflechtung as originary)
% -------------------------------------------------------------------------

quoted_in_passage("vp_p97", "Verflechtung").
translates_in_source("Derrida", "Verflechtung", "interweaving").

derrida_text_asserts("vp_p97",
  "neither expression nor indication is added onto the other as a stratum"
).

derrida_text_asserts("vp_p97",
  "the interweaving is not a contingent association that methodical care could undo"
).

derrida_text_uses_term_for_relation(
  "vp_p97",
  term("originary supplement"),
  between("expression", "indication"),
  at_site_of("non-self-presence")
).

% -------------------------------------------------------------------------
% Author self-descriptions (paleonymy)
% -------------------------------------------------------------------------

author_self_describes_term("Derrida", "trace",      "strategically nicknamed").
author_self_describes_term("Derrida", "reserve",    "strategically nicknamed").
author_self_describes_term("Derrida", "differance", "strategically nicknamed").

% -------------------------------------------------------------------------
% Passage citations
% -------------------------------------------------------------------------

passage_source("vp_ch1_sign_sign",   "Derrida, Voice and Phenomenon, ch. 1").
passage_source("vp_ch2",             "Derrida, Voice and Phenomenon, ch. 2 (p. 28)").
passage_source("vp_ch4_iterability", "Derrida, Voice and Phenomenon, ch. 4").
passage_source("vp_p97",             "Derrida, Voice and Phenomenon, p. 97").
passage_source("og_p60",             "Derrida, Of Grammatology, p. 60").
passage_source("og_p65",             "Derrida, Of Grammatology, p. 65").
passage_source("og_p65_claim_1",     "Derrida, Of Grammatology, p. 65, first clause").
passage_source("og_p65_claim_2",     "Derrida, Of Grammatology, p. 65, second clause").
