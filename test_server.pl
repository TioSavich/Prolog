:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

% Define a simple test endpoint
:- http_handler(root(test), test_handler, [method(get)]).

% Main predicate to start the server
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Simple test handler
test_handler(_Request) :-
    reply_json_dict(_{message: "Hello from Prolog!"}).

% To run the server from the command line:
% swipl -g "server(8082)" test_server.pl
:- initialization(server(8082), main).