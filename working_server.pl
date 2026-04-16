/** <module> Minimal working Prolog API server
 *
 * This server provides the semantic analysis and CGI strategy analysis endpoints
 * without depending on complex modules that may have loading issues.
 * It is the main entry point for the web application.
 *
 * 
 * 
 */

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).

% Define API endpoints
:- http_handler(root(analyze_semantics), analyze_semantics_handler, [method(post)]).
:- http_handler(root(analyze_strategy), analyze_strategy_handler, [method(post)]).
:- http_handler(root(test), test_handler, [method(get)]).
:- http_handler(root(calculator/strategies), calculator_strategies_handler, [method(get)]).
:- http_handler(root(calculator/calculate), calculator_calculate_handler, [method(post)]).

:- use_module(hermeneutic_calculator).

%!      start_server(+Port:integer) is det.
%
%       Starts the Prolog HTTP server on the specified Port.
%       It registers the API handlers and prints a startup message.
%
%       @param Port The port number to listen on.

start_server(Port) :-
    format('Starting Prolog API server on port ~w~n', [Port]),
    http_server(http_dispatch, [port(Port)]),
    format('Server started successfully at http://localhost:~w~n', [Port]),
    format('Test with: curl http://localhost:~w/test~n', [Port]).


%!      test_handler(+Request:list) is det.
%
%       Handles GET requests to the /test endpoint.
%       Responds with a simple JSON object to confirm the server is running.
%
%       @param _Request The incoming HTTP request (unused).

test_handler(_Request) :-
    format('Content-type: application/json~n~n'),
    format('{"status": "ok", "message": "Prolog server is running"}~n').

add_cors_headers :-
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: GET, POST, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type~n').


%!      analyze_semantics_handler(+Request:list) is det.
%
%       Handles POST requests to the /analyze_semantics endpoint.
%       It reads a JSON object with a "statement" key, analyzes it using
%       incompatibility semantics, and returns the analysis as a JSON object.
%
%       @param Request The incoming HTTP request.
%       @error reply_json_dict(_{error: "Invalid JSON input"}) if the request body is not valid JSON.

analyze_semantics_handler(Request) :-
    add_cors_headers,
    
    (   http_read_json_dict(Request, In) ->
        Statement = In.statement,
        analyze_statement_semantics(Statement, Analysis),
        reply_json_dict(Analysis)
    ;   reply_json_dict(_{error: "Invalid JSON input"})
    ).


%!      analyze_strategy_handler(+Request:list) is det.
%
%       Handles POST requests to the /analyze_strategy endpoint.
%       It reads a JSON object with "problemContext" and "strategy" keys,
%       analyzes the student's strategy, and returns the analysis as a JSON object.
%
%       @param Request The incoming HTTP request.
%       @error reply_json_dict(_{error: "Invalid JSON input"}) if the request body is not valid JSON.

analyze_strategy_handler(Request) :-
    add_cors_headers,
    
    (   http_read_json_dict(Request, In) ->
        ProblemContext = In.problemContext,
        StrategyDescription = In.strategy,
        analyze_cgi_strategy(ProblemContext, StrategyDescription, Analysis),
        reply_json_dict(Analysis)
    ;   reply_json_dict(_{error: "Invalid JSON input"})
    ).

calculator_strategies_handler(Request) :-
    add_cors_headers,
    (   catch(http_parameters(Request, [op(OpParam, [default("+")])]), _, fail)
    ->  true
    ;   reply_json_dict(_{error: "Missing op parameter"}, [status(400)]), !
    ),
    normalize_op(OpParam, Op),
    (   valid_op(Op),
        hermeneutic_calculator:list_strategies(Op, Strategies)
    ->  maplist(atom_string, Strategies, StrategyStrings),
        atom_string(Op, OpString),
        reply_json_dict(_{op: OpString, strategies: StrategyStrings})
    ;   reply_json_dict(_{error: "Unsupported operator"}, [status(400)])
    ).

calculator_calculate_handler(Request) :-
    add_cors_headers,
    (   http_read_json_dict(Request, In) ->
        (   get_dict(op, In, OpParam),
            get_dict(num1, In, Num1Val),
            get_dict(num2, In, Num2Val),
            get_dict(strategy, In, StrategyParam)
        ->  true
        ;   reply_json_dict(_{error: "Missing required fields"}, [status(400)]), !
        )
    ;   reply_json_dict(_{error: "Invalid JSON input"}, [status(400)]), !
    ),
    normalize_op(OpParam, Op),
    (   valid_op(Op) -> true ; reply_json_dict(_{error: "Unsupported operator"}, [status(400)]), !),
    (   integer(Num1Val) -> Num1 = Num1Val ; number(Num1Val) -> Num1 is round(Num1Val) ; reply_json_dict(_{error: "num1 must be numeric"}, [status(400)]), !),
    (   integer(Num2Val) -> Num2 = Num2Val ; number(Num2Val) -> Num2 is round(Num2Val) ; reply_json_dict(_{error: "num2 must be numeric"}, [status(400)]), !),
    normalize_strategy(StrategyParam, StrategyAtom),
    (   hermeneutic_calculator:list_strategies(Op, StrategyList),
        member(StrategyAtom, StrategyList)
    ->  true
    ;   reply_json_dict(_{error: "Strategy not available for operator"}, [status(400)]), !
    ),
    (   hermeneutic_calculator:calculate(Num1, Op, Num2, StrategyAtom, Result, History)
    ->  maplist(term_string, History, HistoryStrings),
        atom_string(Op, OpString),
        atom_string(StrategyAtom, StrategyString),
        Reply = _{
            op: OpString,
            strategy: StrategyString,
            result: Result,
            history: HistoryStrings
        },
        reply_json_dict(Reply)
    ;   reply_json_dict(_{error: "Calculation failed for the requested strategy"}, [status(422)])
    ).


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

normalize_op(Value, Atom) :-
    (   string(Value)
    ->  atom_string(Atom0, Value)
    ;   Atom0 = Value
    ),
    Atom = Atom0.

normalize_strategy(Value, Atom) :-
    (   string(Value)
    ->  atom_string(Atom, Value)
    ;   Atom = Value
    ).

valid_op(Op) :- member(Op, [+, -, *, /]).


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
%       Classifies a student's strategy for a math problem.
%       This predicate uses keyword matching on the strategy description to
%       determine the CGI classification (e.g., "Direct Modeling", "Counting On"),
%       the Piagetian stage, and associated pedagogical insights. This is the
%       primary clause for handling math-related strategies.
%
%       @param Context The problem context (must contain "Math").
%       @param Strategy The normalized student strategy description.
%       @param Classification The CGI classification of the strategy.
%       @param Stage The associated Piagetian developmental stage.
%       @param Implications What the strategy implies about the student's understanding.
%       @param Incompatibility The conceptual conflict this strategy might lead to.
%       @param Recommendations Pedagogical suggestions to advance the student's understanding.

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


%!      classify_strategy(+Context:string, +Strategy:atom, -Classification:string, -Stage:string, -Implications:string, -Incompatibility:string, -Recommendations:string) is det.
%
%       Classifies a student's strategy for a science (floating) problem.
%       This clause handles strategies related to why objects float or sink.
%       It identifies common misconceptions (e.g., heavy things sink) and
%       provides recommendations for inducing cognitive conflict.
%
%       @param Context The problem context (must be "Science-Float").
%       @param Strategy The normalized student strategy description.
%       @param Classification The classification of the student's reasoning.
%       @param Stage The associated Piagetian developmental stage.
%       @param Implications What the strategy implies about the student's understanding.
%       @param Incompatibility The conceptual conflict this strategy might lead to.
%       @param Recommendations Pedagogical suggestions to advance the student's understanding.

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

%!      classify_strategy(?, ?, -Classification, -Stage, -Implications, -Incompatibility, -Recommendations) is det.
%
%       Default catch-all for `classify_strategy/7`.
%       This clause is used when the context does not match any of the more
%       specific `classify_strategy` predicates. It returns a generic
%       "Unclassified" result.
%
%       @param _Context Unused context argument.
%       @param _Strategy Unused strategy argument.
%       @param Classification Set to "Unclassified".
%       @param Stage Set to "Unknown".
%       @param Implications A message indicating the strategy could not be identified.
%       @param Incompatibility Set to an empty string.
%       @param Recommendations Set to an empty string.

classify_strategy(_, _, "Unclassified", "Unknown", "Could not clearly identify the strategy based on the description. Please provide more detail about the student's actions and reasoning.", "", "").

%!      main is det.
%
%       The main entry point for the server.
%       It starts the server on port 8083 and then blocks, waiting for
%       messages, to keep the server process alive. This is the predicate
%       to run to launch the application.

main :-
    start_server(8083),
    % Block the main thread to keep the server alive.
    thread_get_message(_).
