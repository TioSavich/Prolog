:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).

% Load the core application logic
:- use_module(execution_handler).
:- use_module(reorganization_log).
:- use_module(reflective_monitor).
:- use_module(object_level).

% Define the REST API endpoints
:- http_handler(root(solve), solve_handler, [method(post)]).
:- http_handler(root(log), log_handler, [method(get)]).
:- http_handler(root(knowledge), knowledge_handler, [method(get)]).

% Main predicate to start the server
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% --- Endpoint Handlers ---

% POST /solve
% Accepts a JSON object like {"goal": "add(s(0),s(0),X)"}
% Runs the ORR cycle and returns the final result.
solve_handler(Request) :-
    http_read_json_dict(Request, In),
    term_string(Goal, In.goal),
    % Run the query, which performs the full ORR cycle
    run_query(Goal),
    % After the cycle, find the result
    (   clause(object_level:Goal, true) ->
        Result = Goal,
        Status = 'success'
    ;   Result = 'failed to find solution',
        Status = 'failure'
    ),
    term_string(ResultString, Result),
    reply_json_dict(_{status: Status, result: ResultString}).

% GET /log
% Returns the full reorganization log as a JSON object.
log_handler(_Request) :-
    generate_report(Report),
    reply_json_dict(_{report: Report}).

% GET /knowledge
% Returns the current knowledge base and conceptual stress map.
knowledge_handler(_Request) :-
    findall(
        Clause,
        (clause(object_level:Head, Body), Clause = (Head :- Body)),
        Clauses
    ),
    get_stress_map(StressMap),
    prolog_to_json(_{clauses: Clauses, stress_map: StressMap}, JSON_Object),
    reply_json(JSON_Object).


% --- Helper for JSON conversion ---
% This is needed because clause bodies can be complex terms.
:- multifile json_convert:prolog_to_json/2.
json_convert:prolog_to_json(Term, JSON) :-
    is_list(Term), !,
    maplist(json_convert:prolog_to_json, Term, JSON).
json_convert:prolog_to_json(Term, JSON) :-
    compound(Term),
    Term =.. [Functor | Args],
    maplist(json_convert:prolog_to_json, Args, JSONArgs),
    JSON = _{functor: Functor, args: JSONArgs}.
json_convert:prolog_to_json(Term, JSON) :-
    \+ compound(Term),
    term_string(Term, JSON).

% To run the server from the command line:
% swipl api_server.pl -g "server(8000)"
:- initialization(server(8000), main).