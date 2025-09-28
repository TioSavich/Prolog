/** <module> Simple, Self-Contained API Server
 *
 * This module provides a lightweight, self-contained HTTP server that offers
 * semantic and strategy analysis endpoints. Unlike `api_server.pl` or
 * `working_server.pl`, this file includes the analysis logic directly within it,
 * making it independent of other modules like `incompatibility_semantics.pl`.
 *
 * It is likely intended for testing, demonstration, or as a simplified
 * alternative to the more complex, modularized servers.
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_cors)).

% Define the REST API endpoints
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

%!      analyze_semantics_handler(+Request:list) is det.
%
%       Handles POST requests to the `/analyze_semantics` endpoint.
%       It expects a JSON object with a `statement` key, e.g., `{"statement": "The object is red"}`.
%       It performs a semantic analysis of the statement using its internal helper predicates.
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

% --- Helper Predicates for Analysis ---

% analyze_statement_semantics(+Statement, -Analysis)
% Analyzes a statement using incompatibility semantics
analyze_statement_semantics(Statement, Analysis) :-
    atom_string(StatementAtom, Statement),
    downcase_atom(StatementAtom, Normalized),
    
    findall(Implication, get_implications(Normalized, Implication), Implies),
    findall(Incompatibility, get_incompatibilities(Normalized, Incompatibility), IncompatibleWith),
    
    Analysis = _{
        statement: Statement,
        implies: Implies,
        incompatibleWith: IncompatibleWith
    }.

% get_implications(+NormalizedStatement, -Implication)
% Determines what a statement implies
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

% get_incompatibilities(+NormalizedStatement, -Incompatibility)  
% Determines what a statement is incompatible with
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

% analyze_cgi_strategy(+ProblemContext, +StrategyDescription, -Analysis)
% Analyzes a student strategy using CGI and Piagetian frameworks
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

% classify_strategy(+Context, +NormalizedStrategy, -Classification, -Stage, -Implications, -Incompatibility, -Recommendations)
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
% swipl -g "server(8080)" simple_api_server.pl
:- initialization(server(8080), main).