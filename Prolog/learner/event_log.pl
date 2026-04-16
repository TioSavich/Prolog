/** <module> Structured Event Logger

    Emits JSON events for each ORR cycle step, making the system's
    developmental progression visible without reading Prolog traces.

    Events are accumulated in a thread-local list and can be retrieved
    as a JSON array. This is the prerequisite for any visualization.

    Usage:
        reset_events,
        ... (run ORR cycle) ...
        get_events(Events).   % Events is a list of dicts
*/
:- module(event_log, [
    emit/2,           % emit(+Type, +Data) — log a structured event
    reset_events/0,   % clear the event log
    get_events/1,     % get_events(-Events) — retrieve all events as list
    events_to_json/1  % events_to_json(-JSONAtom) — serialize events to JSON string
]).

:- use_module(library(lists)).

%% Event storage — uses global assert for simplicity.
%% P3-3 will convert to thread_local when multi-user is needed.
:- dynamic stored_event/2.  % stored_event(Timestamp, EventDict)

%!  emit(+Type:atom, +Data:dict) is det.
%
%   Log a structured event. Type is one of:
%     computation_start, computation_success,
%     crisis_detected, crisis_classified,
%     oracle_consulted, oracle_exhausted,
%     synthesis_attempted, synthesis_succeeded, synthesis_failed,
%     validation_passed, validation_failed,
%     retry, computation_failed
%
emit(Type, Data) :-
    get_time(T),
    Event = event{type: Type, time: T}.put(Data),
    assert(stored_event(T, Event)).

%!  reset_events is det.
%
%   Clear all stored events. Call before starting a new computation.
%
reset_events :-
    retractall(stored_event(_, _)).

%!  get_events(-Events:list) is det.
%
%   Retrieve all stored events in chronological order.
%
get_events(Events) :-
    findall(E, stored_event(_, E), Events).

%!  events_to_json(-JSON:atom) is det.
%
%   Serialize the event log to a JSON string suitable for HTTP response
%   or file output.
%
events_to_json(JSON) :-
    get_events(Events),
    events_list_to_json(Events, JSON).

% Convert event list to JSON array string
events_list_to_json([], '[]').
events_list_to_json(Events, JSON) :-
    maplist(event_to_json_string, Events, Strings),
    atomic_list_concat(Strings, ',', Joined),
    format(atom(JSON), '[~w]', [Joined]).

% Convert a single event dict to a JSON string
event_to_json_string(Event, JSONStr) :-
    dict_pairs(Event, _, Pairs),
    maplist(pair_to_json, Pairs, PairStrings),
    atomic_list_concat(PairStrings, ',', Joined),
    format(atom(JSONStr), '{~w}', [Joined]).

pair_to_json(Key-Value, Str) :-
    (   number(Value)
    ->  format(atom(Str), '"~w":~w', [Key, Value])
    ;   is_dict(Value)
    ->  event_to_json_string(Value, SubJSON),
        format(atom(Str), '"~w":~w', [Key, SubJSON])
    ;   is_list(Value)
    ->  maplist(value_to_json, Value, VStrs),
        atomic_list_concat(VStrs, ',', VJoined),
        format(atom(Str), '"~w":[~w]', [Key, VJoined])
    ;   escape_json_string(Value, Escaped),
        format(atom(Str), '"~w":"~w"', [Key, Escaped])
    ).

value_to_json(V, S) :-
    (   number(V) -> format(atom(S), '~w', [V])
    ;   is_dict(V) -> event_to_json_string(V, S)
    ;   escape_json_string(V, Escaped),
        format(atom(S), '"~w"', [Escaped])
    ).

escape_json_string(Value, Escaped) :-
    term_to_atom(Value, Atom),
    atom_string(Atom, Str),
    split_string(Str, "\"", "", Parts),
    atomics_to_text(Parts, "\\\"", Escaped).

atomics_to_text([], _, '').
atomics_to_text([H], _, H) :- !.
atomics_to_text([H|T], Sep, Result) :-
    atomics_to_text(T, Sep, Rest),
    format(atom(Result), '~w~w~w', [H, Sep, Rest]).
