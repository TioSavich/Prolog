% geometry_runner.pl — JSON-emitting CLI dispatcher for the geometry KB.
%
% Loaded by Python via subprocess:
%   swipl -q -g geom_main src/geometry_runner.pl -- <predicate> <json_args>
%
% Where <predicate> is one of the eight query predicates from
% /Users/tio/Documents/GitHub/umedcta-formalization/geometry/query.pl
% and <json_args> is a JSON-encoded list of the predicate arguments.
%
% On success, prints a single JSON document to stdout and halts 0.
% On failure (no matching results), prints "null" to stdout and halts 0.
% On error, writes a message to stderr and halts non-zero.

:- use_module(library(http/json)).

% ── CLI entrypoint ───────────────────────────────────────────────────

geom_main :-
    catch(geom_main_, E,
          ( format(user_error, "geometry_runner error: ~q~n", [E]),
            halt(4)
          )),
    halt(0).

geom_main_ :-
    consult('/Users/tio/Documents/GitHub/Prolog/geometry_bridge.pl'),
    load_geometry_kb,
    current_prolog_flag(argv, Argv),
    maplist(arg_to_string, Argv, [PredStr, ArgsJson]),
    atom_string(PredAtom, PredStr),
    atom_to_term_args(ArgsJson, ArgList),
    dispatch(PredAtom, ArgList).

arg_to_string(X, S) :-
    ( string(X) -> S = X
    ; atom(X) -> atom_string(X, S)
    ; S = X
    ).

% Decode a JSON list of arguments. Each element is an atom/string/list/number.
atom_to_term_args(JsonStr, ArgList) :-
    atom_string(A, JsonStr),
    atom_json_term(A, ArgList, []).

% ── Predicate dispatch ──────────────────────────────────────────────

dispatch(matching_concepts, [Tokens, GradeBand]) :-
    norm_grade_band(GradeBand, GB),
    user:matching_concepts(Tokens, GB, Concepts),
    emit_list(Concepts).

dispatch(applicable_misconceptions, [UserText, ConceptIds]) :-
    norm_concept_ids(ConceptIds, CIds),
    user:applicable_misconceptions(UserText, CIds, Miscs),
    emit_list(Miscs).

dispatch(linked_misconceptions, [ConceptIds]) :-
    dispatch(linked_misconceptions, [ConceptIds, 2]).
dispatch(linked_misconceptions, [ConceptIds, MaxTier]) :-
    norm_concept_ids(ConceptIds, CIds),
    user:linked_misconceptions(CIds, MaxTier, Miscs),
    emit_list(Miscs).

dispatch(vh_markers_for, [ConceptId, LevelOpt]) :-
    norm_atom(ConceptId, CId),
    norm_level(LevelOpt, Level),
    user:vh_markers_for(CId, Level, Markers),
    emit_list(Markers).

dispatch(bootstraps_for, [ConceptId, Transition, Kind]) :-
    norm_atom(ConceptId, CId),
    norm_transition(Transition, T),
    norm_kind(Kind, K),
    user:bootstraps_for(CId, T, K, Bs),
    emit_list(Bs).

dispatch(developmental_arc_for, [Id]) :-
    norm_atom(Id, AId),
    user:developmental_arc_for(AId, Arc),
    emit_term(Arc).

dispatch(pck_synthesis_for, [ConceptId]) :-
    norm_atom(ConceptId, CId),
    user:pck_synthesis_for(CId, Pck),
    emit_term(Pck).

dispatch(standards_bundle_for, [Framework, Code]) :-
    norm_atom(Framework, FW),
    % Code is stored as a string in standard_anchor/4; preserve it.
    ( atom(Code) -> atom_string(Code, CD) ; CD = Code ),
    user:standards_bundle_for(FW, CD, Bundle),
    emit_term(Bundle).

dispatch(concepts_in_neighborhood, [ConceptIds, Depth]) :-
    norm_concept_ids(ConceptIds, CIds),
    user:concepts_in_neighborhood(CIds, Depth, Neighborhood),
    % Emit as a JSON array of strings (concept IDs).
    maplist([A, S]>>(atom_string(A, S)), Neighborhood, Strings),
    atom_json_term(Json, Strings, []),
    write(Json), nl.

% ── Argument normalization ──────────────────────────────────────────
% JSON null decodes to @(null); JSON strings decode to atoms in this
% library configuration. Each normalizer accepts both.

is_null(@(null)).
is_null(null).

norm_grade_band(X, any) :- is_null(X), !.
norm_grade_band(any, any) :- !.
norm_grade_band([], any) :- !.
norm_grade_band(L, L) :- is_list(L).

norm_concept_ids(X, any) :- is_null(X), !.
norm_concept_ids(any, any) :- !.
norm_concept_ids(L, AtomIds) :-
    is_list(L),
    maplist(norm_atom, L, AtomIds).

norm_atom(X, A) :-
    ( atom(X) -> A = X
    ; string(X) -> atom_string(A, X)
    ; A = X
    ).

norm_level(X, any) :- is_null(X), !.
norm_level(any, any) :- !.
norm_level(N, N) :- integer(N), !.
norm_level(S, N) :- string(S), number_string(N, S), !.
norm_level(A, N) :- atom(A), atom_number(A, N).

norm_transition(X, any) :- is_null(X), !.
norm_transition(any, any) :- !.
norm_transition(T, T).  % pass through compound/atom unchanged

norm_kind(X, any) :- is_null(X), !.
norm_kind(any, any) :- !.
norm_kind(S, K) :- string(S), atom_string(K, S), !.
norm_kind(A, A) :- atom(A).

% ── Term-to-JSON emitters ───────────────────────────────────────────

emit_list(Terms) :-
    maplist(term_to_dict, Terms, Dicts),
    json_write_dict(current_output, Dicts, [width(0)]),
    nl.

emit_term(none) :-
    write('null'), nl.
emit_term(not_found) :-
    write('null'), nl.
emit_term(Term) :-
    term_to_dict(Term, Dict),
    json_write_dict(current_output, Dict, [width(0)]),
    nl.

% concept(Id, Name, Topic, Score|Bands)
term_to_dict(concept(Id, Name, Topic, Score), D) :-
    s(Id, IdS), s(Name, NameS), s(Topic, TopicS),
    score_field(Score, ScoreV),
    D = _{kind: "concept", id: IdS, name: NameS, topic: TopicS, score: ScoreV}.

% misconception(Id, ConceptId, Name, MatchedTrigger, Repair, Tier)
term_to_dict(misconception(Id, Cid, Name, Trigger, Repair, Tier), D) :-
    s(Id, IdS), s(Cid, CidS), s(Name, NameS),
    trigger_field(Trigger, TriggerV),
    s(Repair, RepairS),
    D = _{kind: "misconception",
          id: IdS, concept_id: CidS, name: NameS,
          matched_trigger: TriggerV, repair: RepairS, tier: Tier}.

% marker(Level, Phrases, Citation, Tier)
term_to_dict(marker(Level, Phrases, Citation, Tier), D) :-
    string_list(Phrases, PhraseStrs),
    citation_list(Citation, CiteStrs),
    D = _{kind: "vh_marker", level: Level, phrases: PhraseStrs,
          citation: CiteStrs, tier: Tier}.

% bs(Id, Kind, Prompt, Tools, Citation, Tier)
term_to_dict(bs(Id, Kind, Prompt, Tools, Citation, Tier), D) :-
    s(Id, IdS), s(Kind, KindS), s(Prompt, PromptS),
    string_list(Tools, ToolStrs),
    citation_list(Citation, CiteStrs),
    D = _{kind: "bootstrap", id: IdS, bs_kind: KindS,
          prompt: PromptS, tools: ToolStrs,
          citation: CiteStrs, tier: Tier}.

% arc(ArcConceptId, FromStance, ToStance, TransitionEvidence)
term_to_dict(arc(ArcId, From, To, Evidence), D) :-
    s(ArcId, ArcIdS),
    stance_to_string(From, FromS),
    stance_to_string(To, ToS),
    string_list(Evidence, EvidenceStrs),
    D = _{kind: "arc", arc_concept_id: ArcIdS,
          from: FromS, to: ToS, evidence: EvidenceStrs}.

% pck(KeyKidThinking, KeyTeacherMoves, DevelopmentalArc, Citation)
term_to_dict(pck(KKT, KTM, DA, Citation), D) :-
    ref_list(KKT, KKTList),
    ref_list(KTM, KTMList),
    s(DA, DAS),
    citation_list(Citation, CiteStrs),
    D = _{kind: "pck",
          key_kid_thinking: KKTList,
          key_teacher_moves: KTMList,
          developmental_arc: DAS,
          citation: CiteStrs}.

% bundle(ConceptId, Concept, Statement, Misconceptions, Markers, Bootstraps, Arc, Pck)
term_to_dict(bundle(ConceptId, Concept, Statement, Miscs, Markers, Bs, Arc, Pck), D) :-
    s(ConceptId, CidS),
    term_to_dict(Concept, ConceptD),
    s(Statement, StmtS),
    maplist(term_to_dict, Miscs, MiscDs),
    maplist(term_to_dict, Markers, MarkerDs),
    maplist(term_to_dict, Bs, BsDs),
    arc_to_dict(Arc, ArcD),
    pck_to_dict(Pck, PckD),
    D = _{kind: "bundle",
          concept_id: CidS,
          concept: ConceptD,
          statement: StmtS,
          misconceptions: MiscDs,
          vh_markers: MarkerDs,
          bootstraps: BsDs,
          arc: ArcD,
          pck: PckD}.

arc_to_dict(none, null) :- !.
arc_to_dict(Arc, D) :- term_to_dict(Arc, D).

pck_to_dict(none, null) :- !.
pck_to_dict(Pck, D) :- term_to_dict(Pck, D).

% Score field can be an integer (concept hit score) or a list (grade bands).
score_field(N, N) :- integer(N), !.
score_field(L, L) :- is_list(L), !.
score_field(X, S) :- s(X, S).

trigger_field(none, null) :- !.
trigger_field(all, "all") :- !.
trigger_field(X, S) :- s(X, S).

% ref(N103|L&N|corpus, Anchor, Note) → string
ref_list(L, Out) :-
    is_list(L), !,
    maplist(ref_to_string, L, Out).
ref_list(X, [S]) :- s(X, S).

ref_to_string(ref(Source, Anchor, Note), S) :-
    !,
    s(Source, Sa), s(Anchor, Aa), s(Note, Na),
    format(string(S), "~w/~w: ~w", [Sa, Aa, Na]).
ref_to_string(X, S) :- s(X, S).

string_list(L, Out) :-
    is_list(L), !,
    maplist(s, L, Out).
string_list(X, [S]) :- s(X, S).

citation_list(L, Out) :-
    is_list(L), !,
    maplist(s, L, Out).
citation_list(X, [S]) :- s(X, S).

% Render any compound stance as a string (preserve the developmental term).
stance_to_string(S, Str) :-
    ( atom(S) -> atom_string(S, Str)
    ; string(S) -> Str = S
    ; format(string(Str), "~w", [S])
    ).

% Generic atom/string/number → string
s(X, S) :-
    ( string(X) -> S = X
    ; atom(X)   -> atom_string(X, S)
    ; number(X) -> number_string(X, S)
    ; format(string(S), "~w", [X])
    ).
