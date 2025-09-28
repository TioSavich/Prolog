% Minimal working Prolog API server
% This server provides the semantic analysis and CGI strategy analysis endpoints
% without depending on complex modules that may have loading issues.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

% Define API endpoints
:- http_handler(root(analyze_semantics), analyze_semantics_handler, [method(post)]).
:- http_handler(root(analyze_strategy), analyze_strategy_handler, [method(post)]).
:- http_handler(root(test), test_handler, [method(get)]).

% Start the server
start_server(Port) :-
    format('Starting Prolog API server on port ~w~n', [Port]),
    http_server(http_dispatch, [port(Port)]),
    format('Server started successfully at http://localhost:~w~n', [Port]),
    format('Test with: curl http://localhost:~w/test~n', [Port]).

% Simple test endpoint
test_handler(_Request) :-
    format('Content-type: application/json~n~n'),
    format('{"status": "ok", "message": "Prolog server is running"}~n').

% POST /analyze_semantics
analyze_semantics_handler(Request) :-
    % Add CORS headers
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: POST, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type~n'),
    
    (   http_read_json_dict(Request, In) ->
        Statement = In.statement,
        analyze_statement_semantics(Statement, Analysis),
        reply_json_dict(Analysis)
    ;   reply_json_dict(_{error: "Invalid JSON input"})
    ).

% POST /analyze_strategy
analyze_strategy_handler(Request) :-
    % Add CORS headers
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: POST, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type~n'),
    
    (   http_read_json_dict(Request, In) ->
        ProblemContext = In.problemContext,
        StrategyDescription = In.strategy,
        analyze_cgi_strategy(ProblemContext, StrategyDescription, Analysis),
        reply_json_dict(Analysis)
    ;   reply_json_dict(_{error: "Invalid JSON input"})
    ).

% Semantic analysis implementation
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

% What statements imply
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

% What statements are incompatible with
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

% CGI strategy analysis
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

% Strategy classification for math problems
classify_strategy(Context, Strategy, Classification, Stage, Implications, Incompatibility, Recommendations) :-
    atom_string(Context, ContextStr),
    sub_string(ContextStr, 0, 4, _, "Math"),
    !,
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

% Strategy classification for science problems  
classify_strategy("Science-Float", Strategy, Classification, Stage, Implications, Incompatibility, Recommendations) :-
    !,
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

% Default case
classify_strategy(_, _, "Unclassified", "Unknown", "Could not clearly identify the strategy based on the description. Please provide more detail about the student's actions and reasoning.", "", "").

% Entry point
main :-
    start_server(8083),
    % Block the main thread to keep the server alive.
    thread_get_message(_).