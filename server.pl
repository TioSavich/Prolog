:- module(web_server, [start_server/1, stop_server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_cors)).

% Load system components
:- use_module(object_level).
:- use_module(execution_handler).
:- use_module(knowledge_manager).
:- use_module(more_machine_learner).
:- use_module(fsm_synthesis_engine).
:- use_module(config).

% Enable CORS
:- set_setting(http:cors, [*]).

% HTTP Handlers
:- http_handler(root(api/solve), handle_solve, [method(post), time_limit(20)]).
:- http_handler(root(api/reset), handle_reset, [method(post)]).
:- http_handler(root(api/state), handle_state, [method(get)]).
:- http_handler(root(.), http_reply_from_files('public', []), [prefix]).

%!  start_server(+Port)
%   Start the HTTP server on the given port.
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('Server started on http://localhost:~w/~n', [Port]).

%!  stop_server(+Port)
%   Stop the HTTP server.
stop_server(Port) :-
    http_stop_server(Port, []).

% --- API Endpoints ---

handle_solve(Request) :-
    cors_enable,
    http_read_json_dict(Request, JSON),
    % Expecting {"op": "add", "a": 8, "b": 5, "limit": 20}
    OpName = JSON.get(op),
    A_Int = JSON.get(a),
    B_Int = JSON.get(b),
    (   Limit = JSON.get(limit) -> true ; Limit = 20 ),
    
    % Update global limit dynamically
    retractall(config:max_inferences(_)),
    assertz(config:max_inferences(Limit)),
    
    % Convert integers to Peano numbers
    fsm_synthesis_engine:int_to_peano(A_Int, A_Peano),
    fsm_synthesis_engine:int_to_peano(B_Int, B_Peano),
    
    atom_string(OpAtom, OpName),
    Goal =.. [OpAtom, A_Peano, B_Peano, _ResultPeano],
    
    % Capture stdout to send to frontend
    with_output_to(string(OutputString),
        (   catch(execution_handler:run_computation(object_level:Goal, Limit), Error,
                (   writeln(Error),
                    writeln('Computation failed or ended in unresolved crisis.')
                )
            )
        )
    ),
    
    get_learned_strategies(Strategies),
    
    Reply = _{
        output: OutputString,
        learned_strategies: Strategies
    },
    reply_json_dict(Reply).


handle_reset(Request) :-
    cors_enable,
    _JSON = Request, % ignore body
    with_output_to(string(OutputString),
        knowledge_manager:reset_learned_knowledge
    ),
    Reply = _{
        status: "success",
        output: OutputString,
        learned_strategies: []
    },
    reply_json_dict(Reply).


handle_state(_Request) :-
    cors_enable,
    get_learned_strategies(Strategies),
    Reply = _{
        learned_strategies: Strategies
    },
    reply_json_dict(Reply).


get_learned_strategies(Strategies) :-
    findall(N, clause(more_machine_learner:run_learned_strategy(_,_,_,N,_), _), StrategyList),
    % Deduplicate and ensure JSON compatible output
    sort(StrategyList, Strategies).
