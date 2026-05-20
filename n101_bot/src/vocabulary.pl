:- module(vocabulary, [
    load_vocabulary/1,
    validate_response/3,
    system_prompt/1,
    system_prompt/2,
    export_json/1,
    emit_lql/1,
    term_modules/1,
    detect_terms/2,
    term_card/2,
    focused_system_prompt/2,
    commitments_from_text/2,
    material_consequences/2,
    term_requires/2,
    entitled_to_use/3,
    missing_requirements/3,
    cli_main/0
]).

:- use_module(library(lists)).
:- use_module(library(strings)).
:- use_module(library(http/json)).
:- use_module(library(apply)).

% Load every per-term module with empty import list; we access via
% module-qualified calls to avoid term_def/1 collisions.
:- use_module('./move_grammar', []).
:- use_module('./dialogue_state', []).
:- use_module('../vocabularies/n101/quantity', []).
:- use_module('../vocabularies/n101/measurement_unit', []).
:- use_module('../vocabularies/n101/measurement_process', []).
:- use_module('../vocabularies/n101/measure', []).
:- use_module('../vocabularies/n101/counting', []).
:- use_module('../vocabularies/n101/base', []).
:- use_module('../vocabularies/n101/base_five', []).
:- use_module('../vocabularies/n101/explanation', []).
:- use_module('../vocabularies/n101/strategy', []).
:- use_module('../vocabularies/n101/creative_activity', []).

term_modules([
    term_quantity,
    term_measurement_unit,
    term_measurement_process,
    term_measure,
    term_counting,
    term_base,
    term_base_five,
    term_explanation,
    term_strategy,
    term_creative_activity
]).

% load_vocabulary(-Defs) unifies Defs with the list of all term definitions
% from the loaded per-term modules.
load_vocabulary(Defs) :-
    term_modules(Modules),
    findall(D,
        ( member(M, Modules),
          M:term_def(D) ),
        Defs).

% validate_response(+Text, -Violations, -HitCount)
%
% Scan Text (string or atom) for any incompatibility trigger pattern
% from any loaded term. A hit is a violation(Term, Rule, Trigger, Correction).
validate_response(Text, Violations, HitCount) :-
    to_lower_string(Text, Lower),
    load_vocabulary(Defs),
    findall(
        violation(TermName, Rule, Trigger, Correction),
        ( member(term(TermName, _Aliases, _Defs, _Req, Incompats, _Src), Defs),
          member(incompat(Rule, Triggers, Correction), Incompats),
          member(Trigger, Triggers),
          to_lower_string(Trigger, LowerTrigger),
          sub_string(Lower, _, _, _, LowerTrigger)
        ),
        Violations
    ),
    length(Violations, HitCount).

to_lower_string(X, Lower) :-
    ( string(X) -> S = X
    ; atom(X) -> atom_string(X, S)
    ; S = X
    ),
    string_lower(S, Lower).

% system_prompt(-Prompt)
%   Compose the system prompt from the full vocabulary.
% system_prompt(+Options, -Prompt)
%   Options: include_sources(bool), style(amy|terse).
system_prompt(Prompt) :-
    system_prompt([include_sources(false), style(amy)], Prompt).

system_prompt(Options, Prompt) :-
    load_vocabulary(Defs),
    preamble(Options, Pre),
    maplist(term_block(Options), Defs, Blocks),
    postamble(Options, Post),
    atomic_list_concat([Pre | Blocks], "\n\n", Middle),
    atomic_list_concat([Middle, Post], "\n\n", Prompt).

preamble(Options, Pre) :-
    ( member(style(amy), Options)
    -> Pre = "You are a mathematics methods tutor for pre-service elementary teachers in Amy Hackenberg's N101 course at Indiana University. Respond in the spirit of Amy Hackenberg, Les Steffe, and CGI (Cognitively Guided Instruction): listen to how students think, build on their strategies, and never say 'because it's easier' as a reason.\n\nYou MUST use the following terms in the following ways. Never use them in ways that contradict the INCOMPATIBILITY rules. When a student asks about a term, give Amy's definition."
    ; Pre = "Use these definitions. Respect the incompatibilities."
    ).

term_block(Options, term(Name, _Aliases, PositiveDefs, Requires, Incompats, Sources), Block) :-
    format(string(Header), "TERM: ~w", [Name]),
    maplist(bullet, PositiveDefs, DefLines),
    ( Requires = [] -> ReqLine = ""
    ; atomic_list_concat(Requires, ", ", ReqStr),
      format(string(ReqLine), "Requires: ~w", [ReqStr])
    ),
    maplist(incompat_line, Incompats, IncompLines),
    ( member(include_sources(true), Options)
    -> maplist(source_line, Sources, SrcLines)
    ;  SrcLines = []
    ),
    append([[Header|DefLines], [ReqLine], IncompLines, SrcLines], All0),
    exclude([S]>>(S==""), All0, All),
    atomic_list_concat(All, "\n", Block).

bullet(S, Line) :- format(string(Line), "- ~w", [S]).

incompat_line(incompat(Rule, _Triggers, Correction), Line) :-
    format(string(Line), "INCOMPATIBILITY: ~w — ~w", [Rule, Correction]).

source_line(source(File, Lines, Note), Line) :-
    format(string(Line), "[source: ~w lines ~w — ~w]", [File, Lines, Note]).

postamble(_, "End of vocabulary. If a student asks about a term not listed, answer in Amy's spirit: ask them what they are thinking, don't hand them the answer first.").

% export_json(+Path)
%   Write the full vocabulary as a JSON array to Path.
export_json(Path) :-
    load_vocabulary(Defs),
    maplist(term_to_dict, Defs, Dicts),
    setup_call_cleanup(
        open(Path, write, Stream),
        json_write_dict(Stream, Dicts, [width(120)]),
        close(Stream)
    ).

term_to_dict(term(Name, Aliases, Defs, Requires, Incompats, Sources),
             _{
                 name: Name,
                 aliases: Aliases,
                 positive_defs: Defs,
                 requires: Requires,
                 incompatibilities: IncompatDicts,
                 sources: SourceDicts
             }) :-
    maplist(incompat_to_dict, Incompats, IncompatDicts),
    maplist(source_to_dict, Sources, SourceDicts).

incompat_to_dict(incompat(Rule, Triggers, Correction),
                 _{rule: Rule, triggers: Triggers, correction: Correction}).

source_to_dict(source(File, Lines, Note),
               _{file: File, lines: Lines, note: Note}).

% emit_lql(+Path)
%   Write an LQL patch script that, when fed to `larql lql` against a
%   Gemma vindex, would insert N101 vocabulary edges. Text format.
emit_lql(Path) :-
    load_vocabulary(Defs),
    setup_call_cleanup(
        open(Path, write, Stream),
        write_lql(Stream, Defs),
        close(Stream)
    ).

write_lql(Stream, Defs) :-
    format(Stream, "-- N101 vocabulary patch (LQL source)~n", []),
    format(Stream, "-- Generated by n101_bot/src/vocabulary.pl~n", []),
    format(Stream, "-- Source: Amy Hackenberg, N101 course notes (Fall 2024)~n", []),
    format(Stream, "-- Target runtime: larql + Gemma vindex (NOT YET APPLIED)~n~n", []),
    format(Stream, "BEGIN PATCH \"n101-vocabulary.vlp\";~n~n", []),
    forall(member(D, Defs), write_term_lql(Stream, D)),
    format(Stream, "SAVE PATCH;~n", []).

write_term_lql(Stream, term(Name, Aliases, PositiveDefs, Requires, Incompats, _Sources)) :-
    % ALPHA blends the edge into the FFN down_weights, making the patch
    % affect generation (not just DESCRIBE retrieval). 0.30 per larql README example.
    format(Stream, "-- ~w~n", [Name]),
    forall(member(Alias, Aliases),
        format(Stream,
            "INSERT INTO EDGES (entity, relation, target) VALUES (\"~w\", \"alias_of\", \"~w\") CONFIDENCE 0.90 ALPHA 0.30;~n",
            [Alias, Name])),
    forall(member(Def, PositiveDefs),
        ( escape_for_lql(Def, Esc),
          format(Stream,
            "INSERT INTO EDGES (entity, relation, target) VALUES (\"~w\", \"has_definition\", \"~w\") CONFIDENCE 0.95 ALPHA 0.30;~n",
            [Name, Esc]) )),
    forall(member(Req, Requires),
        format(Stream,
            "INSERT INTO EDGES (entity, relation, target) VALUES (\"~w\", \"requires\", \"~w\") CONFIDENCE 0.90 ALPHA 0.30;~n",
            [Name, Req])),
    forall(member(incompat(Rule, _Triggers, _Correction), Incompats),
        ( escape_for_lql(Rule, EscRule),
          format(Stream,
            "INSERT INTO EDGES (entity, relation, target) VALUES (\"~w\", \"incompatible_with\", \"~w\") CONFIDENCE 0.95 ALPHA 0.30;~n",
            [Name, EscRule]) )),
    format(Stream, "~n", []).

escape_for_lql(S, Escaped) :-
    string_to_atom(S, A),
    atom_string(A, S1),
    re_replace("\""/g, "\\\"", S1, Escaped0),
    ( string(Escaped0) -> Escaped = Escaped0 ; atom_string(Escaped0, Escaped) ).

% ═══════════════════════════════════════════════════════════════
% Material-inference layer: detection + focused cards + commitments
% ═══════════════════════════════════════════════════════════════
%
% Brandomian sketch: every incompatibility trigger that matches a text
% becomes a "commitment" the speaker has (implicitly) taken on. Each
% commitment blocks Amy's entitled move (the correction). Non-monotonic
% because a later turn can add commitments that revoke earlier
% entitlements — e.g., student first says "I know it because of making
% ten" (entitled), then says "because it's easier" (retracts the
% entitlement under Amy's strategy-incompatibility rule).

% detect_terms(+Text, -TermNames)
%   Find the subset of vocabulary terms (by name or alias) that are
%   mentioned in Text. Used to focus the system prompt on just the
%   terms the student brought up, rather than dumping all 10.
detect_terms(Text, TermNames) :-
    to_lower_string(Text, Lower),
    load_vocabulary(Defs),
    findall(Name,
        ( member(term(Name, Aliases, _, _, _, _), Defs),
          term_or_alias_mentioned(Name, Aliases, Lower)
        ),
        Raw),
    list_to_set(Raw, TermNames).

term_or_alias_mentioned(Name, Aliases, Lower) :-
    ( mentioned(Name, Lower)
    ; member(A, Aliases), mentioned(A, Lower)
    ).

mentioned(Thing, Lower) :-
    ( atom(Thing) -> atom_string(Thing, S0)
    ; string(Thing) -> S0 = Thing
    ; S0 = Thing
    ),
    % normalise underscores to spaces so "measurement_process" matches
    % natural-language "measurement process"
    re_replace("_"/g, " ", S0, S),
    string_lower(S, LowerThing),
    string_length(LowerThing, L),
    L > 0,
    sub_string(Lower, _, _, _, LowerThing).

% term_card(+TermName, -CardString)
%   Compose a focused card for one term: positive defs + requires +
%   incompatibilities with explicit corrections. Shorter and more
%   salient than dumping the full vocabulary.
term_card(TermName, Card) :-
    load_vocabulary(Defs),
    member(term(TermName, _Aliases, PosDefs, Requires, Incompats, _), Defs),
    term_block([style(amy)],
        term(TermName, [], PosDefs, Requires, Incompats, []),
        Card).

% focused_system_prompt(+HitTerms, -Prompt)
%   Build a system prompt scoped to just the detected terms. Falls back
%   to the full system prompt when no term was detected, so the bot
%   still has vocabulary context for open-ended questions.
focused_system_prompt([], Prompt) :-
    system_prompt(Prompt).
focused_system_prompt(HitTerms, Prompt) :-
    HitTerms \= [],
    preamble([style(amy)], Pre),
    maplist(term_card, HitTerms, Cards),
    atomic_list_concat(Cards, "\n\n", Middle),
    Postamble = "Stay inside the incompatibilities above. If the student's question touches a term not listed, answer in Amy's spirit: ask them what they are thinking first.",
    atomic_list_concat([Pre, Middle, Postamble], "\n\n", Prompt).

% commitments_from_text(+Text, -Commitments)
%   Every incompat trigger that fires in Text becomes a commitment.
%   Commitment is a term: commitment(Term, Rule, Trigger, Correction).
commitments_from_text(Text, Commitments) :-
    to_lower_string(Text, Lower),
    load_vocabulary(Defs),
    findall(
        commitment(TermName, Rule, Trigger, Correction),
        ( member(term(TermName, _, _, _, Incompats, _), Defs),
          member(incompat(Rule, Triggers, Correction), Incompats),
          member(Trigger, Triggers),
          to_lower_string(Trigger, LowerTrigger),
          sub_string(Lower, _, _, _, LowerTrigger)
        ),
        Commitments).

% material_consequences(+Commitments, -Consequences)
%   For each commitment, the blocked-move + Amy's entitled correction.
%   Non-monotonic: given more commitments, the set of blocked moves
%   grows; a correction that was entitled becomes blocked if a later
%   commitment contradicts it (not modeled tonight — that would need an
%   incompatibility graph between the corrections themselves).
material_consequences(Commitments, Consequences) :-
    maplist(commitment_to_consequence, Commitments, Consequences).

commitment_to_consequence(
    commitment(Term, Rule, Trigger, Correction),
    consequence(Term, Rule, Trigger, blocked, Correction)
).

% ═══════════════════════════════════════════════════════════════
% Entitlement graph
% ═══════════════════════════════════════════════════════════════
%
% Brandom: a commitment is entitled when its supporting commitments
% are in place AND no incompatible commitment is in play. For N101
% vocabulary, a term's "supporting commitments" are its declared
% `requires` list (e.g., `quantity` requires `measurement_unit`,
% `measurement_process`, `measure`). To be entitled to use `quantity`
% correctly, a speaker must have engaged those related terms AND not
% have fired any incompatibility on `quantity` itself.
%
% This is the non-monotonic piece: an earlier entitlement can be
% revoked by a later turn's commitment. The ledger carries both sides
% (engagements and incompatibility-fires) across turns; entitlement is
% recomputed on each consultation.

% term_requires(+TermName, -RequiredRelata)
term_requires(TermName, Requires) :-
    load_vocabulary(Defs),
    member(term(TermName, _, _, Requires, _, _), Defs).

% entitled_to_use(+TermName, +EngagedTerms, +IncompatibilityFires)
%
% Succeeds iff:
%   - the speaker has engaged (mentioned or probed) every term in
%     TermName's requires list
%   - no incompatibility-fire on TermName has been recorded
%
% EngagedTerms: list of atom term names the speaker has engaged this
%   session (typically from accumulated `detect_terms/2` results).
% IncompatibilityFires: list of commitment(Term, Rule, Trigger, Corr)
%   tuples from the session ledger.
entitled_to_use(TermName, EngagedTerms, IncompatibilityFires) :-
    term_requires(TermName, Requires),
    forall(member(R, Requires), member(R, EngagedTerms)),
    \+ ( member(commitment(TermName, _Rule, _Trig, _Corr), IncompatibilityFires) ).

% missing_requirements(+TermName, +EngagedTerms, -Missing)
%   List of required relata not yet engaged by the speaker. An empty
%   list means the speaker has chattered with all the supporting
%   machinery. A nonempty list points at what the teacher should
%   probe next before handing over Amy's full definition of TermName.
missing_requirements(TermName, EngagedTerms, Missing) :-
    term_requires(TermName, Requires),
    findall(R, (member(R, Requires), \+ member(R, EngagedTerms)), Missing).

% ═══════════════════════════════════════════════════════════════
% CLI entrypoints for the Python bridge
% ═══════════════════════════════════════════════════════════════
%
% Usage: swipl -q -g main src/vocabulary.pl -- <command> <arg>
% Commands:
%   detect <text>         — print JSON list of detected term names
%   card <term>           — print JSON term card string (positive defs, incompats)
%   system <terms_csv>    — print focused system prompt for comma-separated terms
%   commitments <text>    — print JSON list of commitments fired by text
%   reason <text>         — print JSON {detected, commitments, consequences}
%   dump                  — regenerate logs/vocabulary.json

cli_main :-
    current_prolog_flag(argv, Argv),
    maplist(arg_to_string, Argv, ArgvStrs),
    ( ArgvStrs = [Cmd|Rest] -> dispatch(Cmd, Rest)
    ; format(user_error, "usage: -- <command> <arg>~n", []), halt(2)
    ),
    halt(0).

arg_to_string(X, S) :-
    ( string(X) -> S = X
    ; atom(X) -> atom_string(X, S)
    ; S = X
    ).

dispatch("detect", [Text]) :-
    detect_terms(Text, Terms),
    maplist(atom_string, Terms, TermStrs),
    json_write_dict(current_output, TermStrs, []),
    nl.
dispatch("card", [Name]) :-
    atom_string(NameAtom, Name),
    ( term_card(NameAtom, Card)
    -> json_write_dict(current_output, _{name: Name, card: Card}, []),
       nl
    ; format(user_error, "no such term: ~w~n", [Name]), halt(3)
    ).
dispatch("system", [TermsCsv]) :-
    split_string(TermsCsv, ",", " ", Parts),
    include([S]>>(S \= ""), Parts, Clean),
    maplist(atom_string, Names, Clean),
    focused_system_prompt(Names, Prompt),
    write(Prompt).
dispatch("commitments", [Text]) :-
    commitments_from_text(Text, Commitments),
    maplist(commitment_to_dict, Commitments, Dicts),
    json_write_dict(current_output, Dicts, []),
    nl.
dispatch("reason", [Text]) :-
    detect_terms(Text, Terms),
    commitments_from_text(Text, Commitments),
    material_consequences(Commitments, Consequences),
    maplist(atom_string, Terms, TermStrs),
    maplist(commitment_to_dict, Commitments, CommitDicts),
    maplist(consequence_to_dict, Consequences, ConsDicts),
    json_write_dict(current_output,
        _{detected: TermStrs, commitments: CommitDicts, consequences: ConsDicts},
        []),
    nl.
dispatch("entitlement", [TermName, EngagedCsv, FiresCsv]) :-
    atom_string(TermAtom, TermName),
    split_string(EngagedCsv, ",", " ", EngagedStrsRaw),
    include([S]>>(S \= ""), EngagedStrsRaw, EngagedStrs),
    maplist(atom_string, EngagedAtoms, EngagedStrs),
    % FiresCsv is a semicolon-separated list of "term:rule" pairs
    split_string(FiresCsv, ";", " ", FireEntries),
    include([S]>>(S \= ""), FireEntries, FireClean),
    maplist(parse_fire, FireClean, Fires),
    ( entitled_to_use(TermAtom, EngagedAtoms, Fires) -> Entitled = true ; Entitled = false ),
    ( missing_requirements(TermAtom, EngagedAtoms, Missing)
    -> maplist(atom_string, Missing, MissingStrs)
    ;  MissingStrs = [] ),
    json_write_dict(current_output,
        _{entitled: Entitled, missing_requirements: MissingStrs},
        []),
    nl.
dispatch("classify", [Text]) :-
    move_grammar:classify_prompt(Text, Class),
    class_to_dict(Class, Dict),
    json_write_dict(current_output, Dict, []),
    nl.
dispatch("move", [Text]) :-
    move_grammar:classify_prompt(Text, Class),
    ( move_grammar:select_move(Class, MoveTag, Template)
    -> template_to_dict(Template, TmplDict),
       atom_string(MoveTag, MoveStr),
       class_to_dict(Class, ClassDict),
       json_write_dict(current_output,
           _{class: ClassDict, move_tag: MoveStr, template: TmplDict},
           [])
    ; json_write_dict(current_output,
           _{class: _{kind: "unclear"}, move_tag: "FMST", template: _{prompt: "Ask a gentle clarifying question.", slots: _{}}},
           [])
    ),
    nl.
dispatch("state_init", []) :-
    dialogue_state:initial_state(S),
    state_to_dict(S, Dict),
    json_write_dict(current_output, Dict, []),
    nl.
dispatch("state_step", [StateJson, MoveTagStr, CommitCountStr]) :-
    atom_json_dict(StateJson, Dict, []),
    dict_to_state(Dict, S0),
    atom_string(MoveTag, MoveTagStr),
    number_string(Count, CommitCountStr),
    dialogue_state:update_state(S0, MoveTag, Count, S1),
    dialogue_state:render_state(S1, Rendered),
    ( dialogue_state:near_catastrophe(S1) -> Cusp = true ; Cusp = false ),
    state_to_dict(S1, NewDict),
    put_dict(rendered, NewDict, Rendered, WithRender),
    put_dict(near_cusp, WithRender, Cusp, Out),
    json_write_dict(current_output, Out, []),
    nl.
dispatch("dump", []) :-
    export_json("logs/vocabulary.json").
dispatch(Cmd, _) :-
    format(user_error, "unknown command: ~w~n", [Cmd]),
    halt(2).

commitment_to_dict(
    commitment(Term, Rule, Trigger, Correction),
    _{term: TermStr, rule: Rule, trigger: Trigger, correction: Correction}
) :- atom_string(Term, TermStr).

consequence_to_dict(
    consequence(Term, Rule, Trigger, Status, Correction),
    _{term: TermStr, rule: Rule, trigger: Trigger, status: StatusStr, correction: Correction}
) :-
    atom_string(Term, TermStr),
    atom_string(Status, StatusStr).

% class_to_dict/2 — turn a classify_prompt/2 result into JSON-friendly dict.
class_to_dict(arithmetic_computation(Op, Operands, strategy_unspoken),
              _{kind: "arithmetic_computation", op: OpStr, operands: Operands, strategy_stated: false}) :-
    atom_string(Op, OpStr).
class_to_dict(arithmetic_computation(Op, Operands, strategy_stated(What)),
              _{kind: "arithmetic_computation", op: OpStr, operands: Operands, strategy_stated: true, strategy: WhatStr}) :-
    atom_string(Op, OpStr),
    atom_string(What, WhatStr).
class_to_dict(vocabulary_question(Term),
              _{kind: "vocabulary_question", term: TermStr}) :-
    ( atom(Term) -> atom_string(Term, TermStr) ; TermStr = Term ).
class_to_dict(strategy_report(Text),
              _{kind: "strategy_report", text: Text}).
class_to_dict(strategy_declaration(What, Text),
              _{kind: "strategy_declaration", strategy: WhatStr, text: Text}) :-
    atom_string(What, WhatStr).
class_to_dict(unclear, _{kind: "unclear"}).

% template_to_dict/2 — turn a move-grammar template into dict. Renders the
% slot bindings as concrete strings; the Python layer passes these to the LLM.
template_to_dict(template(Prompt, Slots), _{prompt: Prompt, slots: SlotsDict}) :-
    slots_to_dict(Slots, SlotsDict).

slots_to_dict([], _{}).
slots_to_dict(Slots, Dict) :-
    Slots \= [],
    maplist(slot_pair, Slots, Pairs),
    dict_create(Dict, _, Pairs).

slot_pair(Key=Value, KeyAtom-ValueStr) :-
    ( atom(Key) -> KeyAtom = Key ; atom_string(KeyAtom, Key) ),
    ( string(Value) -> ValueStr = Value
    ; atom(Value) -> atom_string(Value, ValueStr)
    ; format(string(ValueStr), "~w", [Value])
    ).

parse_fire(Str, commitment(TermAtom, Rule, "", "")) :-
    split_string(Str, ":", " ", [T, R | _]),
    atom_string(TermAtom, T),
    ( string(R) -> Rule = R ; atom_string(R, Rule) ).

% ── dialogue-state dict round-trip ──
state_to_dict(state(A, V, T, Hist), _{a: A, v: V, t: T, history: HistDicts}) :-
    maplist(hist_entry_to_dict, Hist, HistDicts).

hist_entry_to_dict((Move, Count), _{move_tag: MoveStr, commits: Count}) :-
    atom_string(Move, MoveStr).

dict_to_state(Dict, state(A, V, T, Hist)) :-
    A = Dict.get(a, 0.0),
    V = Dict.get(v, 0.0),
    T = Dict.get(t, 0.0),
    HistDicts = Dict.get(history, []),
    maplist(dict_to_hist_entry, HistDicts, Hist).

dict_to_hist_entry(D, (Move, Count)) :-
    MoveStr = D.get(move_tag, "NONE"),
    atom_string(Move, MoveStr),
    Count = D.get(commits, 0).

% Python invokes this with:
%   swipl -q -g cli_main src/vocabulary.pl -- <command> <arg>
% `cli_main` is not an initialization directive so the module still
% loads cleanly from tests without halting the interpreter.
