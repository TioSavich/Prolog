/** <module> Basic Test HTTP Server
 *
 * This module provides a minimal HTTP server with a single endpoint (`/test`).
 * Its purpose is to serve as a basic test to confirm that the SWI-Prolog
 * HTTP libraries are working correctly and that a server can be started.
 *
 * It is not part of the main application logic but can be useful for
 * debugging or initial environment setup verification.
 *
 * @author Tilo Wiedera
 * @license MIT
 */
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

% Define a simple test endpoint
:- http_handler(root(test), test_handler, [method(get)]).

%!      server(+Port:integer) is det.
%
%       Starts the HTTP server on the specified Port.
%
%       @param Port The port number for the server to listen on.
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

%!      test_handler(+Request:list) is det.
%
%       Handles GET requests to the `/test` endpoint.
%
%       It responds with a simple, fixed JSON object `_{message: "Hello from Prolog!"}`
%       to confirm that the server is running and able to handle requests.
%
%       @param _Request The incoming HTTP request (unused).
test_handler(_Request) :-
    reply_json_dict(_{message: "Hello from Prolog!"}).

% To run the server from the command line:
% swipl -g "server(8082)" test_server.pl
:- initialization(server(8082), main).