/** <module> Full-featured API Server for Cognitive Modeling
 *
 * This module provides a comprehensive HTTP server that exposes the full
 * capabilities of the cognitive modeling system. It integrates various
 * components, including the core execution handler for the ORR (Observe,
 * Reorganize, Reflect) cycle, logging, semantic analysis, and student
 * strategy analysis.
 *
 * This server is intended for development and provides a richer set of
 * endpoints compared to `working_server.pl`.
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_header)).

% Load the core application logic
:- use_module(execution_handler).
:- use_module(reorganization_log).
:- use_module(reflective_monitor).
:- use_module(object_level).
:- use_module(incompatibility_semantics).
:- use_module(hermeneutic_calculator).

% Define the REST API endpoints
:- http_handler(root(solve), solve_handler, [method(post)]).
:- http_handler(root(log), log_handler, [method(get)]).
:- http_handler(root(knowledge), knowledge_handler, [method(get)]).
:- http_handler(root(analyze_semantics), analyze_semantics_handler, [method(post)]).
:- http_handler(root(analyze_strategy), analyze_strategy_handler, [method(post)]).

% Enable CORS for all endpoints
:- set_setting(http:cors, [*]).

%!      server(+Port:integer) is det.
%
%       Starts the HTTP server on the specified Port.
%
%       @param Port The port number for the server to listen on.
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% --- Endpoint Handlers ---

%!      solve_handler(+Request:list) is det.
%
%       Handles POST requests to the `/solve` endpoint.
%       It expects a JSON object with a `goal` key, e.g., `{"goal": "add(s(0),s(0),X)"}`.
%       It runs the full ORR (Observe, Reorganize, Reflect) cycle for the given
%       goal and returns the final result.
%
%       @param Request The incoming HTTP request.
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


%!      log_handler(+Request:list) is det.
%
%       Handles GET requests to the `/log` endpoint.
%       It generates and returns the full reorganization log as a JSON object,
%       detailing the cognitive steps taken by the system.
%
%       @param _Request The incoming HTTP request (unused).
log_handler(_Request) :-
    generate_report(Report),
    reply_json_dict(_{report: Report}).


%!      knowledge_handler(+Request:list) is det.
%
%       Handles GET requests to the `/knowledge` endpoint.
%       It returns the current state of the system's knowledge base, including
%       all clauses in the `object_level` module and the current conceptual
%       stress map.
%
%       @param _Request The incoming HTTP request (unused).
knowledge_handler(_Request) :-
    findall(
        Clause,
        (clause(object_level:Head, Body), Clause = (Head :- Body)),
        Clauses
    ),
    get_stress_map(StressMap),
    prolog_to_json(_{clauses: Clauses, stress_map: StressMap}, JSON_Object),
    reply_json(JSON_Object).


%!      analyze_semantics_handler(+Request:list) is det.
%
%       Handles POST requests to the `/analyze_semantics` endpoint.
%       It expects a JSON object with a `statement` key, e.g., `{"statement": "The object is red"}`.
%       It performs a semantic analysis of the statement based on incompatibility semantics.
%
%       @param Request The incoming HTTP request.
analyze_semantics_handler(Request) :-
    cors_enable(Request, [methods([post, options])]),
    (   http_read_json_dict(Request, In) ->
        Statement = In.statement,
        analyze_statement_semantics(Statement, Analysis),
        reply_json_dict(Analysis)
    ;   reply_json_dict(_{error: "Invalid JSON input"})
    ).


%!      analyze_strategy_handler(+Request:list) is det.
%
%       Handles POST requests to the `/analyze_strategy` endpoint.
%       It expects a JSON object with `problemContext` and `strategy` keys,
%       e.g., `{"problemContext": "Math-JRU", "strategy": "student counted all"}`.
%       It returns a CGI/Piagetian analysis of the described student strategy.
%
%       @param Request The incoming HTTP request.
analyze_strategy_handler(Request) :-
    cors_enable(Request, [methods([post, options])]),
    (   http_read_json_dict(Request, In) ->
        ProblemContext = In.problemContext,
        StrategyDescription = In.strategy,
        analyze_cgi_strategy(ProblemContext, StrategyDescription, Analysis),
        reply_json_dict(Analysis)
    ;   reply_json_dict(_{error: "Invalid JSON input"})
    ).


% --- Helper for JSON conversion ---

%!      json_convert:prolog_to_json(+Term, -JSON) is multi.
%
%       A multifile predicate that extends the default JSON conversion library.
%       This implementation is needed to handle the conversion of complex Prolog
%       terms (like rule bodies) into a structured JSON format.
%
%       @param Term The Prolog term to convert.
%       @param JSON The resulting JSON object.
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

% --- Helper Predicates for Analysis ---

%!      analyze_statement_semantics(+Statement:string, -Analysis:dict) is det.
%
%       Performs semantic analysis on a given statement.
%       It finds all implications and incompatibilities for the normalized
%       (lowercase) statement.
%
%       @param Statement The input string to analyze.
%       @param Analysis A dict containing the original statement, a list of
%       implications, and a list of incompatibilities.
analyze_statement_semantics(Statement, Analysis) :-
    atom_string(StatementAtom, Statement),
    downcase_atom(StatementAtom, Normalized),
    
    % Basic semantic analysis based on statement content
    findall(Implication, get_implications(Normalized, Implication), Implies),
    findall(Incompatibility, get_incompatibilities(Normalized, Incompatibility), IncompatibleWith),
    
    Analysis = _{
        statement: Statement,
        implies: Implies,
        incompatibleWith: IncompatibleWith
    }.

%!      get_implications(+Statement:atom, -Implication:string) is nondet.
%
%       Generates implications for a given statement.
%       This predicate defines the semantic entailments based on keywords
%       found in the statement. It is a multi-clause predicate where each
%       clause represents a different implication rule.
%
%       @param Statement The normalized (lowercase) input atom.
%       @param Implication A string describing what the statement implies.
get_implications(Statement, 'The object is colored') :-
    sub_atom(Statement, _, _, _, red).
get_implications(Statement, 'The shape is a rectangle') :-
    sub_atom(Statement, _, _, _, square).
get_implications(Statement, 'The shape is a polygon') :-
    sub_atom(Statement, _, _, _, square).
get_implications(Statement, 'The shape has 4 sides of equal length') :-
    sub_atom(Statement, _, _, _, square).
get_implications(Statement, 'This statement has semantic content') :-
    Statement \= ''.

%!      get_incompatibilities(+Statement:atom, -Incompatibility:string) is nondet.
%
%       Generates incompatibilities for a given statement.
%       This predicate defines what a statement semantically rules out based
%       on keywords. It is a multi-clause predicate where each clause
%       represents a different incompatibility rule.
%
%       @param Statement The normalized (lowercase) input atom.
%       @param Incompatibility A string describing what the statement is incompatible with.
get_incompatibilities(Statement, 'The object is entirely blue') :-
    sub_atom(Statement, _, _, _, red).
get_incompatibilities(Statement, 'The object is monochromatic and green') :-
    sub_atom(Statement, _, _, _, red).
get_incompatibilities(Statement, 'The shape is a circle') :-
    sub_atom(Statement, _, _, _, square).
get_incompatibilities(Statement, 'The shape has exactly 3 sides') :-
    sub_atom(Statement, _, _, _, square).
get_incompatibilities(Statement, 'The negation of this statement') :-
    Statement \= ''.

%!      analyze_cgi_strategy(+ProblemContext:string, +StrategyDescription:string, -Analysis:dict) is det.
%
%       Analyzes a student's problem-solving strategy within a given context.
%       It normalizes the strategy description and uses `classify_strategy/7`
%       to get a detailed analysis.
%
%       @param ProblemContext The context of the problem (e.g., "Math-Addition").
%       @param StrategyDescription A text description of the student's strategy.
%       @param Analysis A dict containing the classification, developmental stage,
%       implications, incompatibilities, and pedagogical recommendations.
analyze_cgi_strategy(ProblemContext, StrategyDescription, Analysis) :-
    atom_string(StrategyAtom, StrategyDescription),
    downcase_atom(StrategyAtom, Normalized),
    
    classify_strategy(ProblemContext, Normalized, Classification, Stage, Implications, Incompatibility, Recommendations),
    
    Analysis = _{
        classification: Classification,
        stage: Stage,
        implications: Implications,
        incompatibility: Incompatibility,
        recommendations: Recommendations
    }.

%!      classify_strategy(+Context:string, +Strategy:atom, -Classification:string, -Stage:string, -Implications:string, -Incompatibility:string, -Recommendations:string) is det.
%
%       Classifies a student's strategy based on context and description.
%       This multi-clause predicate uses keyword matching on the strategy
%       description to determine the CGI classification, Piagetian stage,
%       and associated pedagogical insights for various domains (Math, Science).
%
%       @param Context The problem context (e.g., "Math-Addition", "Science-Float").
%       @param Strategy The normalized student strategy description.
%       @param Classification The CGI classification of the strategy.
%       @param Stage The associated Piagetian developmental stage.
%       @param Implications What the strategy implies about the student's understanding.
%       @param Incompatibility The conceptual conflict this strategy might lead to.
%       @param Recommendations Pedagogical suggestions to advance the student's understanding.
classify_strategy(Context, Strategy, Classification, Stage, Implications, Incompatibility, Recommendations) :-
    atom_string(Context, ContextStr),
    sub_atom(ContextStr, 0, 4, _, "Math"),
    (   (sub_atom(Strategy, _, _, _, 'count all') ; 
         sub_atom(Strategy, _, _, _, 'starting from one') ; 
         sub_atom(Strategy, _, _, _, '1, 2, 3')) ->
        Classification = "Direct Modeling: Counting All",
        Stage = "Preoperational (Piaget)",
        Implications = "The student needs to represent the quantities concretely and cannot treat the initial number as an abstract unit.",
        Incompatibility = "A commitment to 'Counting All' is incompatible with the concept of 'Cardinality' (understanding the first set can be counted abstractly).",
        Recommendations = "Encourage 'Counting On'. Ask: 'You know there are 5 here. Can you start counting from 5 instead of 1?' This induces disequilibrium regarding their reliance on concrete modeling."
    ;   (sub_atom(Strategy, _, _, _, 'count on') ; 
         sub_atom(Strategy, _, _, _, 'started at 5')) ->
        Classification = "Counting Strategy: Counting On",
        Stage = "Concrete Operational (Early)",
        Implications = "The student understands the cardinality of the first number. This is a significant accommodation from Direct Modeling.",
        Incompatibility = "Reliance on 'Counting On' is incompatible with the immediate retrieval required for 'Fluency/Known Facts'.",
        Recommendations = "Work on derived facts. Ask: 'If you know 5 + 5 = 10, how can that help you solve 5 + 6?'"
    ;   (sub_atom(Strategy, _, _, _, 'known fact') ; 
         sub_atom(Strategy, _, _, _, 'just knew')) ->
        Classification = "Known Fact / Fluency",
        Stage = "Concrete Operational",
        Implications = "The student has internalized the number relationship.",
        Incompatibility = "",
        Recommendations = "Introduce more complex problem structures (e.g., Join Change Unknown or multi-step problems) to generalize this understanding."
    ;   
        Classification = "Unclassified",
        Stage = "Unknown",
        Implications = "Could not clearly identify the strategy based on the description. Please provide more detail about the student's actions and reasoning.",
        Incompatibility = "",
        Recommendations = ""
    ).

classify_strategy("Science-Float", Strategy, Classification, Stage, Implications, Incompatibility, Recommendations) :-
    (   (sub_atom(Strategy, _, _, _, heavy) ; sub_atom(Strategy, _, _, _, big)) ->
        Classification = "Perceptual Reasoning: Weight/Size as defining factor",
        Stage = "Preoperational",
        Implications = "The student is focusing on salient perceptual features (size, weight) rather than the underlying principle (density).",
        Incompatibility = "The concept that 'heavy things sink' is incompatible with observations of 'large, heavy objects floating' (e.g., a boat).",
        Recommendations = "Introduce an incompatible observation (disequilibrium). Show a very large object that floats (e.g., log) and a very small object that sinks (e.g., pebble). Ask them to revise their rule."
    ;   
        Classification = "Unclassified",
        Stage = "Unknown", 
        Implications = "Could not clearly identify the strategy based on the description. Please provide more detail about the student's actions and reasoning.",
        Incompatibility = "",
        Recommendations = ""
    ).

% Default case for unmatched contexts
classify_strategy(_, _, "Unclassified", "Unknown", "Could not clearly identify the strategy based on the description. Please provide more detail about the student's actions and reasoning.", "", "").

% To run the server from the command line:
% swipl api_server.pl -g "server(8000)"
:- initialization(server(8000), main).