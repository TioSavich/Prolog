% hermes_worker.pl — local JSONL worker for Hermes.
%
% Protocol:
%   {"id":"req_1","op":"health"}
%   {"id":"req_2","op":"event_score","event":{...}}
%   {"id":"req_3","op":"batch_event_score","events":[...]}
%   {"id":"req_4","op":"pair_score","events":[...]}
%   {"id":"req_5","op":"pair_graph","events":[...]}
%
% One JSON object in, one JSON object out. Human-readable diagnostics go to
% stderr only.

:- use_module(library(http/json)).
:- use_module(library(readutil)).

worker_main :-
    catch(load_runtime, E, worker_fatal(E)),
    worker_loop.

load_runtime :-
    (   getenv('UMEDCTA_ROOT', Root0)
    ->  atom_string(Root, Root0)
    ;   throw(error(missing_environment('UMEDCTA_ROOT'), load_runtime/0))
    ),
    directory_file_path(Root, 'hermes/event_scoring.pl', EventScoring),
    use_module(EventScoring),
    directory_file_path(Root, 'hermes/pair_scoring.pl', PairScoring),
    use_module(PairScoring),
    load_geometry_runtime(Root).

load_geometry_runtime(Root) :-
    directory_file_path(Root, 'geometry/schema.pl', Schema),
    consult(Schema),
    load_geometry_files(Root, 'geometry/concepts/*.pl'),
    load_geometry_files(Root, 'geometry/metaphors/*.pl'),
    load_geometry_files(Root, 'geometry/van_hiele/*.pl'),
    load_geometry_files(Root, 'geometry/bootstrap/*.pl'),
    load_geometry_files(Root, 'geometry/standards/*.pl'),
    load_geometry_files(Root, 'geometry/pck/*.pl'),
    directory_file_path(Root, 'geometry/query.pl', Query),
    consult(Query).

load_geometry_files(Root, Pattern) :-
    directory_file_path(Root, Pattern, AbsolutePattern),
    expand_file_name(AbsolutePattern, Files),
    maplist(consult, Files).

worker_loop :-
    read_line_to_string(user_input, Line),
    (   Line == end_of_file
    ->  true
    ;   handle_line(Line),
        worker_loop
    ).

handle_line(Line) :-
    catch(
        ( atom_json_dict(Line, Request, []),
          handle_request(Request, Response)
        ),
        E,
        error_response("unknown", malformed_request, E, Response)
    ),
    json_write_dict(current_output, Response, [width(0)]),
    nl,
    flush_output(current_output).

handle_request(Request, Response) :-
    request_id(Request, Id),
    (   get_dict(op, Request, Op0)
    ->  atom_string(Op, Op0),
        dispatch_request(Op, Id, Request, Response)
    ;   error_response(Id, missing_op, "request has no op", Response)
    ).

dispatch_request(health, Id, _Request, Response) :-
    ok_response(Id, _{
        worker: "hermes_swi",
        loaded: ["event_scoring", "pair_scoring", "geometry"],
        mode: "persistent"
    }, Response).

dispatch_request(event_score, Id, Request, Response) :-
    (   get_dict(event, Request, Event)
    ->  hermes_event_scoring:score_event(Event, Score),
        json_safe(Score, Safe),
        ok_response(Id, Safe, Response)
    ;   error_response(Id, missing_event, "event_score requires event", Response)
    ).

dispatch_request(batch_event_score, Id, Request, Response) :-
    (   get_dict(events, Request, Events),
        is_list(Events)
    ->  maplist(hermes_event_scoring:score_event, Events, Scores),
        json_safe(Scores, Safe),
        ok_response(Id, Safe, Response)
    ;   error_response(Id, missing_events, "batch_event_score requires events list", Response)
    ).

dispatch_request(pair_score, Id, Request, Response) :-
    (   get_dict(events, Request, Events),
        is_list(Events)
    ->  hermes_pair_scoring:score_pair_candidates(Events, Pairs),
        json_safe(Pairs, Safe),
        ok_response(Id, Safe, Response)
    ;   error_response(Id, missing_events, "pair_score requires events list", Response)
    ).

dispatch_request(pair_graph, Id, Request, Response) :-
    (   get_dict(events, Request, Events),
        is_list(Events)
    ->  hermes_pair_scoring:score_pair_candidates(Events, Pairs),
        hermes_pair_scoring:pair_graph(Pairs, Graph),
        json_safe(Graph, Safe),
        ok_response(Id, Safe, Response)
    ;   error_response(Id, missing_events, "pair_graph requires events list", Response)
    ).

dispatch_request(geometry, Id, Request, Response) :-
    (   get_dict(predicate, Request, Predicate0),
        get_dict(args, Request, Args)
    ->  atom_string(Predicate, Predicate0),
        dispatch_geometry(Predicate, Args, Id, Response)
    ;   error_response(Id, malformed_geometry_request,
            "geometry requires predicate and args", Response)
    ).

dispatch_request(Op, Id, _Request, Response) :-
    format(string(Message), "Unsupported op: ~w", [Op]),
    error_response(Id, unknown_op, Message, Response).

dispatch_geometry(matching_concepts, [Tokens, GradeBand], Id, Response) :-
    !,
    norm_grade_band(GradeBand, GB),
    matching_concepts(Tokens, GB, Concepts),
    maplist(concept_dict, Concepts, Dicts),
    ok_response(Id, Dicts, Response).

dispatch_geometry(concepts_in_neighborhood, [ConceptIds, Depth], Id, Response) :-
    !,
    norm_concept_ids(ConceptIds, CIds),
    concepts_in_neighborhood(CIds, Depth, Neighborhood),
    maplist(atom_to_string_value, Neighborhood, Strings),
    ok_response(Id, Strings, Response).

dispatch_geometry(Predicate, _Args, Id, Response) :-
    format(string(Message), "Unsupported geometry predicate: ~w", [Predicate]),
    error_response(Id, unknown_geometry_predicate, Message, Response).

request_id(Request, Id) :-
    (   get_dict(id, Request, Id)
    ->  true
    ;   Id = "unknown"
    ).

ok_response(Id, Result, _{id: Id, ok: true, result: Result}).

error_response(Id, Type, Error, _{id: Id, ok: false, error: _{type: TypeString, message: Message}}) :-
    atom_string(Type, TypeString),
    message_string(Error, Message).

message_string(Error, Message) :-
    (   string(Error)
    ->  Message = Error
    ;   atom(Error)
    ->  atom_string(Error, Message)
    ;   message_to_string(Error, Message)
    ).

norm_grade_band(any, any) :- !.
norm_grade_band([], any) :- !.
norm_grade_band(L, L) :- is_list(L), !.
norm_grade_band(_, any).

norm_concept_ids(any, any) :- !.
norm_concept_ids(L, AtomIds) :-
    is_list(L),
    !,
    maplist(string_or_atom_to_atom, L, AtomIds).
norm_concept_ids(_, any).

string_or_atom_to_atom(Value, Atom) :-
    (   atom(Value)
    ->  Atom = Value
    ;   string(Value)
    ->  atom_string(Atom, Value)
    ;   Atom = Value
    ).

atom_to_string_value(Atom, String) :-
    atom_string(Atom, String).

concept_dict(concept(Id, Name, Topic, Score), _{
    kind: "concept",
    id: IdString,
    name: NameString,
    topic: TopicString,
    score: ScoreValue
}) :-
    atom_to_string_value(Id, IdString),
    atom_to_string_value(Name, NameString),
    atom_to_string_value(Topic, TopicString),
    score_value(Score, ScoreValue).

score_value(score(Value), Value) :- !.
score_value(Value, Value).

worker_fatal(Error) :-
    message_string(Error, Message),
    format(user_error, "hermes_worker fatal: ~s~n", [Message]),
    halt(2).

json_safe(Value, Safe) :-
    is_dict(Value),
    !,
    dict_pairs(Value, Tag, Pairs),
    maplist(json_safe_pair, Pairs, SafePairs),
    dict_pairs(Safe, Tag, SafePairs).
json_safe(Value, Safe) :-
    is_list(Value),
    !,
    maplist(json_safe, Value, Safe).
json_safe(Value, Safe) :-
    safe_reason_term(Value, Safe),
    !.
json_safe(Value, Value) :-
    atomic(Value),
    !.
json_safe(Value, Safe) :-
    term_string(Value, Safe).

json_safe_pair(Key-Value, Key-Safe) :-
    json_safe(Value, Safe).

safe_reason_term(shared_domain(Value), Safe) :-
    reason_value_string(Value, String),
    format(string(Safe), "shared_domain(~s)", [String]).
safe_reason_term(shared_topic(Value), Safe) :-
    reason_value_string(Value, String),
    format(string(Safe), "shared_topic(~s)", [String]).
safe_reason_term(shared_validity_register(Value), Safe) :-
    reason_value_string(Value, String),
    format(string(Safe), "shared_validity_register(~s)", [String]).

reason_value_string(Value, String) :-
    (   string(Value)
    ->  String = Value
    ;   atom(Value)
    ->  atom_string(Value, String)
    ;   term_string(Value, String)
    ).
