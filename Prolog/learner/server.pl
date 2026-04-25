/** <module> ORR Cycle HTTP Server

    Minimal HTTP server exposing the ORR cycle as a JSON API.
    Serves the frontend and handles computation requests.

    Usage:
        swipl server.pl
        % Server starts on http://localhost:8080
*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).

:- use_module(execution_handler).
:- use_module(oracle_server).
:- use_module(event_log).
:- use_module(more_machine_learner, []).
:- use_module(tension_dynamics).
:- use_module(strategies(hermeneutic_calculator)).
:- use_module(strategies(visualization)).

% Route definitions
:- http_handler(root(api/compute), handle_compute, []).
:- http_handler(root(api/strategies), handle_strategies, []).
:- http_handler(root(api/strategy/run), handle_strategy_run, []).
:- http_handler(root(api/knowledge), handle_knowledge, []).
:- http_handler(root(api/tension), handle_tension, []).
:- http_handler(root(api/reset), handle_reset, []).
:- http_handler(root(bridge), serve_bridge, []).
:- http_handler(root(fractal), serve_fractal, []).
:- http_handler(root(landing), serve_landing, []).
:- http_handler(root(strategies), serve_strategy_page, [prefix]).
:- http_handler(root(assets), serve_zeeman_asset, [prefix]).
:- http_handler(root(.), serve_frontend, [prefix]).

% CORS for local dev
:- set_setting(http:cors, [*]).

%!  server_port(-Port) is det.
%   Default port for the ORR server.
server_port(8080).

%!  start_server is det.
%   Start the HTTP server on the default port.
start_server :-
    server_port(Port),
    http_server(http_dispatch, [port(Port)]),
    format('ORR Cycle Explorer running at http://localhost:~w~n', [Port]).

% ═══════════════════════════════════════════════════════════════════════
% API Handlers
% ═══════════════════════════════════════════════════════════════════════

%!  handle_compute(+Request) is det.
%
%   POST /api/compute
%   Body: {"operation": "add", "a": 3, "b": 2, "limit": 20}
%   Returns: {"success": bool, "problem": {...}, "events": [...]}
%
handle_compute(Request) :-
    cors_enable(Request, [methods([post])]),
    http_read_json_dict(Request, Input),
    atom_string(Op, Input.operation),
    A = Input.a,
    B = Input.b,
    Limit = Input.get(limit, 20),

    % Reset event log, run computation, collect events.
    % Redirect stdout to a string — run_computation uses writeln
    % which would otherwise corrupt the HTTP response stream.
    reset_events,
    build_goal(Op, A, B, Goal),
    (   catch(
            with_output_to(string(_Stdout),
                run_computation(Goal, Limit)),
            Error,
            (   emit(computation_failed, _{goal: Goal, error: Error}),
                fail
            )
        )
    ->  Success = true
    ;   Success = false
    ),
    get_events(Events),
    maplist(event_to_dict, Events, EventDicts),
    get_learned_strategies(Knowledge),
    tension_dynamics:get_tension_state(TensionState),
    tension_dynamics:get_tension_history(TensionHistory),
    reply_json_dict(_{
        success: Success,
        problem: _{operation: Op, a: A, b: B},
        budget: Limit,
        events: EventDicts,
        knowledge: Knowledge,
        tension: TensionState,
        tension_history: TensionHistory
    }).

%!  handle_strategies(+Request) is det.
handle_strategies(Request) :-
    cors_enable(Request, [methods([get])]),
    http_parameters(Request, [operation(OpStr, [])]),
    atom_string(Op, OpStr),
    (   oracle_server:list_available_strategies(Op, Strategies)
    ->  reply_json_dict(_{operation: Op, strategies: Strategies})
    ;   reply_json_dict(_{operation: Op, strategies: []})
    ).

%!  handle_strategy_run(+Request) is det.
%
%   POST /api/strategy/run
%   Body: {"strategy": "Chunking", "op": "+", "a": 46, "b": 37}
%   Returns: {"success": bool, "result": Int, "jumps": [...], "history": [...]}
%
%   Directly executes a named strategy via hermeneutic_calculator:calculate/6
%   without going through the ORR cycle. This is the authoritative path for
%   strategy-specific visualization.
handle_strategy_run(Request) :-
    cors_enable(Request, [methods([post])]),
    http_read_json_dict(Request, Input),
    atom_string(Strategy, Input.strategy),
    atom_string(OpAtom, Input.op),
    Op = OpAtom,
    A = Input.a,
    B = Input.b,
    (   catch(
            hermeneutic_calculator:calculate(A, Op, B, Strategy, Result, History),
            Error,
            (   format(user_error,
                       'strategy_run error: ~w~n', [Error]),
                fail
            )
        )
    ->  ( is_list(History)
        -> visualization:strategy_jumps(Strategy, History, Jumps),
           visualization:history_to_dicts(Strategy, History, HistoryDicts),
           reply_json_dict(_{
               success: true,
               strategy: Strategy,
               op: Op,
               a: A, b: B,
               result: Result,
               jumps: Jumps,
               history: HistoryDicts
           })
        ;  % Pre-existing dispatcher bug: hermeneutic_calculator binds
           % History to a non-list (e.g., Remainder for CBO Division and IDP).
           % Surface honestly rather than crash.
           term_to_atom(History, HAtom),
           reply_json_dict(_{
               success: false,
               strategy: Strategy,
               op: Op,
               a: A, b: B,
               result: Result,
               jumps: [],
               history: [],
               error: 'dispatcher does not return a step history for this strategy',
               raw_history_value: HAtom
           })
        )
    ;   reply_json_dict(_{
            success: false,
            strategy: Strategy,
            op: Op,
            a: A, b: B,
            error: 'strategy execution failed'
        }, [status(400)])
    ).

%!  serve_strategy_page(+Request) is det.
%
%   GET /strategies/<name>.html — serves files from more-zeeman/strategies/
%   with a <base> tag pointing at /assets/strategies/ so relative CSS loads.
serve_strategy_page(Request) :-
    cors_enable(Request, [methods([get])]),
    memberchk(path(Path), Request),
    ( Path == '/strategies' ; Path == '/strategies/' ),
    !,
    source_file(serve_bridge(_), ThisFile),
    file_directory_name(ThisFile, PrologDir),
    file_directory_name(PrologDir, RepoRoot),
    atom_concat(RepoRoot, '/more-zeeman/strategies/index.html', IndexPath),
    ( exists_file(IndexPath) ->
        read_file_to_string(IndexPath, HTML, []),
        inject_strategies_base_tag(HTML, Patched),
        serve_html_string(Patched)
    ;
        reply_json_dict(_{error: 'no index.html'}, [status(404)])
    ).
serve_strategy_page(Request) :-
    cors_enable(Request, [methods([get])]),
    memberchk(path(Path), Request),
    atom_concat('/strategies/', RelPath, Path),
    \+ sub_atom(RelPath, _, _, _, '..'),
    source_file(serve_bridge(_), ThisFile),
    file_directory_name(ThisFile, PrologDir),
    file_directory_name(PrologDir, RepoRoot),
    atomic_list_concat([RepoRoot, '/more-zeeman/strategies/', RelPath], FullPath),
    ( exists_file(FullPath) ->
        ( atom_concat(_, '.html', RelPath) ->
            read_file_to_string(FullPath, HTML, []),
            inject_strategies_base_tag(HTML, Patched),
            serve_html_string(Patched)
        ;
            % Non-HTML assets served directly from same folder
            asset_content_type(RelPath, CT),
            format('Content-type: ~w~n~n', [CT]),
            setup_call_cleanup(
                open(FullPath, read, In, [type(binary)]),
                copy_stream_data(In, current_output),
                close(In))
        )
    ;
        reply_json_dict(_{error: 'strategy page not found', path: RelPath},
                        [status(404)])
    ).

inject_strategies_base_tag(HTML, Patched) :-
    ( sub_string(HTML, Before, _, _, "<head>") ->
        HeadEnd is Before + 6,
        sub_string(HTML, 0, HeadEnd, _, Prefix),
        sub_string(HTML, HeadEnd, _, 0, Suffix),
        atomic_list_concat([Prefix,
            '\n<base href="/strategies/">\n', Suffix], Patched)
    ;
        Patched = HTML
    ).

%!  handle_knowledge(+Request) is det.
%
%   GET /api/knowledge
%   Returns learned strategies per operation.
%
handle_knowledge(Request) :-
    cors_enable(Request, [methods([get])]),
    get_learned_strategies(Knowledge),
    reply_json_dict(Knowledge).

%!  handle_tension(+Request) is det.
%
%   GET /api/tension
%   Returns current tension state and full history for visualization.
%   The tension history is the data that drives the More Machine bridge.
%
handle_tension(Request) :-
    cors_enable(Request, [methods([get])]),
    tension_dynamics:get_tension_state(State),
    tension_dynamics:get_tension_history(History),
    reply_json_dict(_{state: State, history: History}).

%!  handle_reset(+Request) is det.
%
%   POST /api/reset
%   Resets the machine to primordial state (forgets all learned strategies).
%   Also resets tension to zero — a full clean slate.
%
handle_reset(Request) :-
    cors_enable(Request, [methods([post])]),
    retractall(more_machine_learner:run_learned_strategy(_,_,_,_,_)),
    reset_events,
    tension_dynamics:reset_tension,
    reply_json_dict(_{status: reset}).

% ═══════════════════════════════════════════════════════════════════════
% Knowledge Tracking
% ═══════════════════════════════════════════════════════════════════════

get_learned_strategies(Knowledge) :-
    findall(
        _{operation: Op, learned: Learned},
        (   member(Op, [add, subtract, multiply, divide]),
            oracle_server:list_available_strategies(Op, Available),
            findall(S, (
                member(S, Available),
                clause(more_machine_learner:run_learned_strategy(_,_,_,S,_), _)
            ), Learned)
        ),
        Knowledge
    ).

% ═══════════════════════════════════════════════════════════════════════
% Goal Construction
% ═══════════════════════════════════════════════════════════════════════

build_goal(add, A, B, object_level:add(PA, PB, _)) :-
    int_to_peano(A, PA), int_to_peano(B, PB).
build_goal(subtract, A, B, object_level:subtract(PA, PB, _)) :-
    int_to_peano(A, PA), int_to_peano(B, PB).
build_goal(multiply, A, B, object_level:multiply(PA, PB, _)) :-
    int_to_peano(A, PA), int_to_peano(B, PB).
build_goal(divide, A, B, object_level:divide(PA, PB, _)) :-
    int_to_peano(A, PA), int_to_peano(B, PB).

int_to_peano(0, 0) :- !.
int_to_peano(N, s(P)) :-
    N > 0,
    N1 is N - 1,
    int_to_peano(N1, P).

% ═══════════════════════════════════════════════════════════════════════
% Event Serialization
% ═══════════════════════════════════════════════════════════════════════

event_to_dict(Event, SafeDict) :-
    dict_pairs(Event, Tag, Pairs),
    maplist(safe_pair, Pairs, SafePairs),
    dict_pairs(SafeDict, Tag, SafePairs).

safe_pair(Key-Value, Key-SafeValue) :-
    safe_value(Value, SafeValue).

safe_value(V, V) :- number(V), !.
safe_value(V, V) :- atom(V), !.
safe_value(V, V) :- string(V), !.
safe_value(V, S) :- is_dict(V), !, event_to_dict(V, S).
safe_value(V, S) :- is_list(V), !, maplist(safe_value, V, S).
safe_value(V, S) :- term_to_atom(V, S).

% ═══════════════════════════════════════════════════════════════════════
% Frontend
% ═══════════════════════════════════════════════════════════════════════

%!  serve_bridge(+Request) is det.
%   GET /bridge — serves bridge.html with a <base> tag so relative
%   asset paths (shared.js, more-machine.js) resolve to /assets/.
serve_bridge(Request) :-
    cors_enable(Request, [methods([get])]),
    source_file(serve_bridge(_), ThisFile),
    file_directory_name(ThisFile, PrologDir),
    file_directory_name(PrologDir, RepoRoot),
    atom_concat(RepoRoot, '/more-zeeman/bridge.html', BridgePath),
    (   exists_file(BridgePath)
    ->  read_file_to_string(BridgePath, HTML, []),
        inject_base_tag(HTML, Patched),
        serve_html_string(Patched)
    ;   reply_json_dict(_{error: 'bridge.html not found'}, [status(404)])
    ).

%!  serve_fractal(+Request) is det.
%   GET /fractal — serves fractal.html with base tag for asset resolution.
serve_fractal(Request) :-
    cors_enable(Request, [methods([get])]),
    source_file(serve_bridge(_), ThisFile),
    file_directory_name(ThisFile, PrologDir),
    file_directory_name(PrologDir, RepoRoot),
    atom_concat(RepoRoot, '/more-zeeman/fractal.html', FractalPath),
    (   exists_file(FractalPath)
    ->  read_file_to_string(FractalPath, HTML, []),
        inject_base_tag(HTML, Patched),
        serve_html_string(Patched)
    ;   reply_json_dict(_{error: 'fractal.html not found'}, [status(404)])
    ).

%!  serve_landing(+Request) is det.
%   GET /landing — serves the unified landing page with base tag.
serve_landing(Request) :-
    cors_enable(Request, [methods([get])]),
    source_file(serve_bridge(_), ThisFile),
    file_directory_name(ThisFile, PrologDir),
    file_directory_name(PrologDir, RepoRoot),
    atom_concat(RepoRoot, '/more-zeeman/landing.html', LandingPath),
    (   exists_file(LandingPath)
    ->  read_file_to_string(LandingPath, HTML, []),
        inject_base_tag(HTML, Patched),
        serve_html_string(Patched)
    ;   reply_json_dict(_{error: 'landing.html not found'}, [status(404)])
    ).

%!  inject_base_tag(+HTML, -Patched) is det.
%   Inserts <base href="/assets/"> after <head> so relative script/link
%   paths resolve to /assets/ when served via the Prolog server.
inject_base_tag(HTML, Patched) :-
    (   sub_string(HTML, Before, _, _, "<head>")
    ->  HeadEnd is Before + 6,
        sub_string(HTML, 0, HeadEnd, _, Prefix),
        sub_string(HTML, HeadEnd, _, 0, Suffix),
        atomic_list_concat([Prefix, '\n<base href="/assets/">\n', Suffix], Patched)
    ;   Patched = HTML
    ).

%!  serve_html_string(+HTML) is det.
%   Write an HTML string to the HTTP response with correct UTF-8 encoding.
%   Temporarily sets UTF-8 encoding on the output stream, then restores it.
serve_html_string(HTML) :-
    format('Content-type: text/html; charset=utf-8~n~n'),
    current_output(Out),
    stream_property(Out, encoding(OldEnc)),
    set_stream(Out, encoding(utf8)),
    write(Out, HTML),
    set_stream(Out, encoding(OldEnc)).

%!  serve_zeeman_asset(+Request) is det.
%   GET /assets/* — serves static files from more-zeeman/ (shared.js, etc.)
%   so that pages served via the Prolog server can load their JS dependencies.
serve_zeeman_asset(Request) :-
    cors_enable(Request, [methods([get])]),
    memberchk(path(Path), Request),
    atom_concat('/assets/', RelPath, Path),
    \+ sub_atom(RelPath, _, _, _, '..'),
    source_file(serve_bridge(_), ThisFile),
    file_directory_name(ThisFile, PrologDir),
    file_directory_name(PrologDir, RepoRoot),
    atomic_list_concat([RepoRoot, '/more-zeeman/', RelPath], AssetPath),
    (   exists_file(AssetPath)
    ->  asset_content_type(RelPath, ContentType),
        format('Content-type: ~w~n~n', [ContentType]),
        setup_call_cleanup(
            open(AssetPath, read, In, [type(binary)]),
            copy_stream_data(In, current_output),
            close(In))
    ;   reply_json_dict(_{error: 'asset not found'}, [status(404)])
    ).

asset_content_type(Path, 'application/javascript') :- atom_concat(_, '.js', Path), !.
asset_content_type(Path, 'text/css') :- atom_concat(_, '.css', Path), !.
asset_content_type(Path, 'text/html') :- atom_concat(_, '.html', Path), !.
asset_content_type(Path, 'image/png') :- atom_concat(_, '.png', Path), !.
asset_content_type(_, 'application/octet-stream').

serve_frontend(Request) :-
    memberchk(path(Path), Request),
    (   Path == '/'
    ->  serve_index(Request)
    ;   \+ sub_atom(Path, _, _, _, '..'),
        atom_concat('public', Path, FilePath),
        exists_file(FilePath)
    ->  http_reply_file(FilePath, [], Request)
    ;   serve_index(Request)
    ).

serve_index(_Request) :-
    inline_frontend(HTML),
    format('Content-type: text/html~n~n'),
    format('~w', [HTML]).

inline_frontend(HTML) :-
    HTML = '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>ORR Cycle Explorer</title>
<style>
:root {
  --bg: #0f0f1a;
  --surface: #1a1a2e;
  --surface2: #16213e;
  --border: #2a2a4a;
  --text: #d4d4e0;
  --text-dim: #7a7a9a;
  --accent: #e94560;
  --success: #4ecca3;
  --warn: #f0a050;
  --oracle: #9b7aed;
  --mono: "SF Mono", "Fira Code", "Cascadia Code", "Consolas", monospace;
  --sans: -apple-system, "Segoe UI", sans-serif;
}
* { box-sizing: border-box; margin: 0; padding: 0; }
body {
  font-family: var(--sans);
  background: var(--bg);
  color: var(--text);
  min-height: 100vh;
  line-height: 1.6;
}

.container { max-width: 720px; margin: 0 auto; padding: 2rem 1.5rem; }

header { margin-bottom: 2rem; }
h1 { font-size: 1.5rem; font-weight: 600; margin-bottom: 0.25rem; }
.subtitle { color: var(--text-dim); font-size: 0.9rem; }

/* Controls */
.controls {
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 1.25rem;
  margin-bottom: 1.5rem;
}
.controls-row {
  display: flex; gap: 0.75rem; align-items: end; flex-wrap: wrap;
}
.field label {
  display: block; font-size: 0.7rem; color: var(--text-dim);
  text-transform: uppercase; letter-spacing: 0.05em; margin-bottom: 0.25rem;
}
.field select, .field input {
  background: var(--bg); border: 1px solid var(--border);
  color: var(--text); padding: 0.45rem 0.6rem; border-radius: 4px;
  font-family: var(--mono); font-size: 0.85rem; width: 100%;
}
.field select { width: 130px; }
.field input[type=number] { width: 65px; }
.controls-row .spacer { flex: 1; }
button.run {
  background: var(--accent); color: white; border: none;
  padding: 0.45rem 1.5rem; border-radius: 4px; cursor: pointer;
  font-family: var(--sans); font-size: 0.85rem; font-weight: 600;
  white-space: nowrap;
}
button.run:hover { filter: brightness(1.1); }
button.run:disabled { opacity: 0.4; cursor: not-allowed; }
button.reset {
  background: none; border: 1px solid var(--border); color: var(--text-dim);
  padding: 0.45rem 0.75rem; border-radius: 4px; cursor: pointer;
  font-size: 0.8rem;
}
button.reset:hover { border-color: var(--text-dim); }
.limit-warning {
  font-size: 0.75rem; color: var(--warn); margin-top: 0.5rem;
  display: none;
}

/* Narrative cards */
.narrative { display: flex; flex-direction: column; gap: 0; }
.card {
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 1.25rem 1.5rem;
  margin-bottom: 0;
  position: relative;
  animation: fadeIn 0.3s ease both;
}
.card + .connector {
  width: 2px; height: 24px; background: var(--border);
  margin: 0 auto;
}
.card + .connector + .card { }
@keyframes fadeIn { from { opacity: 0; transform: translateY(8px); } to { opacity: 1; } }
.card:nth-child(1)  { animation-delay: 0s; }
.card:nth-child(3)  { animation-delay: 0.15s; }
.card:nth-child(5)  { animation-delay: 0.3s; }
.card:nth-child(7)  { animation-delay: 0.45s; }
.card:nth-child(9)  { animation-delay: 0.6s; }

.card-phase {
  font-size: 0.65rem; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.12em; margin-bottom: 0.5rem; display: flex;
  align-items: center; gap: 0.5rem;
}
.card-phase .dot {
  width: 8px; height: 8px; border-radius: 50%; display: inline-block;
}
.card p { margin-bottom: 0.5rem; font-size: 0.9rem; }
.card p:last-child { margin-bottom: 0; }
.card .dim { color: var(--text-dim); }
.card .emph { font-weight: 600; }

/* Phase colors */
.card.observe .card-phase { color: var(--text-dim); }
.card.observe .dot { background: var(--text-dim); }
.card.crisis .card-phase { color: var(--warn); }
.card.crisis .dot { background: var(--warn); }
.card.crisis { border-color: rgba(240,160,80,0.3); }
.card.reorganize .card-phase { color: var(--oracle); }
.card.reorganize .dot { background: var(--oracle); }
.card.reorganize { border-color: rgba(155,122,237,0.2); }
.card.resolve .card-phase { color: var(--success); }
.card.resolve .dot { background: var(--success); }
.card.resolve { border-color: rgba(78,204,163,0.3); }
.card.direct-success .card-phase { color: var(--success); }
.card.direct-success .dot { background: var(--success); }
.card.failure .card-phase { color: var(--accent); }
.card.failure .dot { background: var(--accent); }
.card.failure { border-color: rgba(233,69,96,0.3); }

/* Tallies */
.tallies {
  font-family: var(--mono); font-size: 1rem;
  letter-spacing: 0.15em; margin: 0.5rem 0;
  line-height: 1.8;
}
.tallies .group { display: inline; margin-right: 0.4em; }
.tallies .mark { color: var(--text); }
.tallies .mark.counted { color: var(--success); }
.tallies .mark.uncounted { color: var(--border); opacity: 0.5; }
.tallies .op { color: var(--text-dim); margin: 0 0.3em; }
.tallies .bracket { color: var(--oracle); font-weight: bold; }

/* Resource bar */
.resource-bar {
  margin: 0.75rem 0 0.25rem;
  display: flex; align-items: center; gap: 0.5rem;
  font-family: var(--mono); font-size: 0.75rem; color: var(--text-dim);
}
.bar-track {
  flex: 1; height: 6px; background: var(--bg);
  border-radius: 3px; overflow: hidden; max-width: 200px;
}
.bar-fill {
  height: 100%; border-radius: 3px;
  transition: width 0.5s ease;
}
.bar-fill.ok { background: var(--success); }
.bar-fill.warn { background: var(--warn); }
.bar-fill.exhausted { background: var(--accent); }

/* Oracle quote */
.oracle-quote {
  background: rgba(155,122,237,0.08);
  border-left: 3px solid var(--oracle);
  padding: 0.5rem 0.75rem;
  margin: 0.5rem 0;
  font-size: 0.85rem;
  font-style: italic;
  color: var(--text);
}

/* Synthesis checklist */
.checklist { list-style: none; margin: 0.5rem 0; }
.checklist li {
  font-size: 0.85rem; padding: 0.15rem 0;
  display: flex; align-items: center; gap: 0.4rem;
}
.checklist .ok { color: var(--success); }
.checklist .fail { color: var(--accent); }

/* Knowledge panel */
.knowledge-panel {
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 1.25rem 1.5rem;
  margin-top: 2rem;
}
.knowledge-panel h2 {
  font-size: 0.9rem; font-weight: 600; margin-bottom: 0.75rem;
}
.knowledge-op {
  display: flex; align-items: baseline; gap: 0.5rem;
  margin-bottom: 0.4rem; font-size: 0.85rem;
}
.knowledge-op .op-name {
  font-family: var(--mono); font-weight: 600;
  min-width: 70px; color: var(--text-dim);
}
.knowledge-op .strategies { color: var(--text); }
.knowledge-op .primordial {
  font-style: italic; color: var(--text-dim); font-size: 0.8rem;
}
.knowledge-op .arrow { color: var(--success); margin: 0 0.2rem; }

/* Empty state */
.empty-state {
  text-align: center; padding: 3rem 1rem; color: var(--text-dim);
}
.empty-state p { font-size: 0.9rem; margin-bottom: 0.5rem; }
.empty-state .suggestion {
  font-size: 0.8rem; font-family: var(--mono);
  background: var(--surface); display: inline-block;
  padding: 0.3rem 0.6rem; border-radius: 4px; margin-top: 0.5rem;
}

/* Tension readout */
.tension-readout {
  margin: 0.5rem 0 0.25rem;
  display: flex; align-items: center; gap: 0.5rem;
  font-family: var(--mono); font-size: 0.75rem; color: var(--text-dim);
}
.tension-bar-track {
  flex: 0 0 80px; height: 6px; background: var(--bg);
  border-radius: 3px; overflow: hidden;
}
.tension-bar-fill {
  height: 100%; border-radius: 3px;
  transition: width 0.5s ease;
}
.tension-bar-fill.stable { background: var(--success); }
.tension-bar-fill.inflection { background: var(--warn); }
.tension-bar-fill.unstable { background: var(--accent); }
.tension-label {
  font-size: 0.7rem; color: var(--text-dim);
}
.tension-label .stability-tag {
  font-weight: 600; margin-left: 0.25rem;
}
.tension-label .stability-tag.stable { color: var(--success); }
.tension-label .stability-tag.inflection { color: var(--warn); }
.tension-label .stability-tag.unstable { color: var(--accent); }
.tension-relaxation {
  font-size: 0.72rem; color: var(--warn); font-style: italic;
  margin-top: 0.2rem;
}

/* Bridge link */
.bridge-link {
  display: inline-flex; align-items: center; gap: 0.4rem;
  color: var(--warn); text-decoration: none;
  font-family: var(--mono); font-size: 0.8rem;
  padding: 0.35rem 0.75rem;
  border: 1px solid rgba(240,160,80,0.3);
  border-radius: 4px;
  transition: border-color 0.2s, color 0.2s;
}
.bridge-link:hover {
  border-color: var(--warn); color: #f5c070;
}
.bridge-link .arrow { font-size: 0.9rem; }
.bridge-subtitle {
  font-size: 0.65rem; color: var(--text-dim);
  margin-top: 0.15rem;
}

/* About */
.about {
  margin-top: 2rem; border-top: 1px solid var(--border);
  padding-top: 1rem;
}
.about summary {
  font-size: 0.8rem; color: var(--text-dim); cursor: pointer;
  list-style: none;
}
.about summary::before { content: "+ "; font-family: var(--mono); }
.about[open] summary::before { content: "- "; }
.about .about-body {
  font-size: 0.8rem; color: var(--text-dim); line-height: 1.7;
  margin-top: 0.75rem;
}
.about .about-body h3 {
  font-size: 0.8rem; color: var(--text); margin: 1rem 0 0.25rem;
}
.about .about-body p { margin-bottom: 0.5rem; }
.about .about-body a {
  color: var(--warn); text-decoration: none;
}
.about .about-body a:hover { text-decoration: underline; }
.about .about-body .limitation {
  border-left: 2px solid var(--border);
  padding-left: 0.75rem;
  margin: 0.4rem 0;
}
</style>
</head>
<body>
<div class="container">

<header>
  <div style="display:flex;justify-content:space-between;align-items:flex-start;flex-wrap:wrap;gap:0.75rem">
    <div>
      <h1>ORR Cycle Explorer</h1>
      <p class="subtitle">Watch a machine learn arithmetic through crisis</p>
    </div>
    <div style="text-align:right">
      <a href="/bridge" class="bridge-link" title="See the formalization''s tension dynamics as catastrophe geometry">
        Watch in Bridge <span class="arrow">&#8594;</span>
      </a>
      <div class="bridge-subtitle">Tension dynamics as catastrophe geometry</div>
    </div>
  </div>
</header>

<div class="controls">
  <div class="controls-row">
    <div class="field">
      <label>Operation</label>
      <select id="op" onchange="checkWarnings()">
        <option value="add">add</option>
        <option value="subtract">subtract</option>
        <option value="multiply">multiply</option>
        <option value="divide">divide</option>
      </select>
    </div>
    <div class="field">
      <label>A</label>
      <input type="number" id="a" value="8" min="0" max="99" onchange="checkWarnings()">
    </div>
    <div class="field">
      <label>B</label>
      <input type="number" id="b" value="5" min="0" max="99" onchange="checkWarnings()">
    </div>
    <div class="field">
      <label>Limit</label>
      <input type="number" id="limit" value="20" min="5" max="500">
    </div>
    <div class="spacer"></div>
    <button class="run" id="run" onclick="compute()">Run</button>
    <button class="reset" onclick="resetMachine()">Reset</button>
  </div>
  <div class="limit-warning" id="warning">
    Numbers above ~15 use Peano representation (tally marks) and will be slow by design.
    This slowness triggers crisis and learning.
  </div>
</div>

<div id="narrative" class="narrative">
  <div class="empty-state">
    <p>The machine starts knowing only one thing: how to count.</p>
    <p>Give it a problem it cannot solve by counting alone.</p>
    <div class="suggestion">Try: 8 + 5 with limit 20</div>
  </div>
</div>

<div id="knowledge-panel" class="knowledge-panel">
  <h2>What the machine knows</h2>
  <div id="knowledge"></div>
</div>

<details class="about">
  <summary>About this system</summary>
  <div class="about-body">
    <h3>The ORR Cycle</h3>
    <p>Observe, React, Reorganize. The machine attempts a computation using what it knows.
    When its approach fails (resource exhaustion or unknown operation), it enters crisis.
    Crisis triggers teacher intervention, strategy synthesis, and retry.</p>

    <h3>Counting All</h3>
    <p>The machine starts with one strategy: build both numbers as tallies (successor applications),
    then count the total from 1. This is how young children add before learning shortcuts.
    It works, but it is expensive: adding 8 + 5 requires constructing 13 tally marks and counting
    each one, which exceeds a tight inference budget.</p>

    <h3>The Teacher</h3>
    <p>The teacher creates conditions under which the learner''s current approach breaks down.
    What the system receives is a result and a description of the method, but not
    its internal workings. The machine must reconstruct the strategy from its own primitives
    (successor, predecessor, decompose). The strategies come from Carpenter and Fennema''s
    Cognitively Guided Instruction research.</p>

    <h3>Tension Dynamics</h3>
    <p>Each inference step accumulates tension. The tension system tracks not just a level but
    its acceleration: when the second derivative goes negative, the system has entered an
    unstable zone where any perturbation can trigger a snap. After crisis, tension partially
    relaxes but does not reset to zero. The system remembers it was stressed. This
    hysteresis means successive crises hit differently.</p>

    <h3>The Bridge</h3>
    <p>The <a href="/bridge">Bridge</a> connects this explorer to a Zeeman catastrophe machine
    visualization. Each inference step accumulates tension in the Prolog meta-interpreter.
    When stability drops below zero, the learner has entered the catastrophe zone &mdash;
    any perturbation triggers a discontinuous snap. The Bridge visualizes this geometry in
    real time, mapping the formalization''s internal dynamics onto catastrophe surface
    coordinates.</p>

    <h3>Where This Stops Working</h3>
    <p>These limitations are not bugs. They mark the boundary where the formalism honestly
    stops being able to say anything.</p>
    <div class="limitation">
      <p><strong>The fraction crisis.</strong> The system handles whole-number arithmetic
      through the ORR cycle. Fractions need three-level unit coordination (unit, fractional
      part, whole); the current system only manages two. The fraction representation
      (<code>fraction/2</code>) is structurally incompatible with
      <code>recollection/1</code>. What does &ldquo;encountering a fraction&rdquo; mean
      for a system that started with tally marks?</p>
    </div>
    <div class="limitation">
      <p><strong>The synthesis gap.</strong> Current synthesis wraps teacher-provided
      results rather than building genuine FSM strategies from ENS primitives. The machine
      says it synthesized a strategy, but it memorized a phone number, not a method.
      Strategy ordering also ignores developmental prerequisites.</p>
    </div>
    <div class="limitation">
      <p><strong>The erasure boundary.</strong> The sequent calculus (incompatibility
      semantics) has a precise point where formal proof goes hollow. When sequent
      variables carry the arche-trace attribute, the prover returns an erasure instead
      of a proof. The derivation succeeds structurally but the proof object contains
      no content. These are exactly the points where human judgment must enter.</p>
    </div>

    <h3>Why this matters</h3>
    <p>This is not a calculator. It is a formal model of crisis-driven learning from the
    manuscript <em>Understanding Mathematics as an Emancipatory Discipline: A Critical Theory
    Approach</em>. The interesting thing is not the arithmetic but the structure of the
    developmental crisis and the limits of what formal systems can capture about learning.
    The <a href="/bridge">Bridge visualization</a> and the
    <a href="https://github.com/TioSavich/umedcta-portfolio" target="_blank">More Machine</a>
    in the portfolio are experiential companions to what the Prolog formalizes here.</p>
  </div>
</details>

</div>

<script>
const OP_SYMBOLS = { add: "+", subtract: "\\u2212", multiply: "\\u00d7", divide: "\\u00f7" };

const STRATEGY_NAMES = {
  "COBO": "Count On by Bases and Ones",
  "RMB": "Rearranging to Make Bases",
  "Chunking": "Chunking",
  "Rounding": "Rounding to Nearest Ten",
  "COBO (Missing Addend)": "Count On (Missing Addend)",
  "CBBO (Take Away)": "Count Back (Take Away)",
  "Decomposition": "Decomposition",
  "Sliding": "Sliding",
  "Chunking A": "Chunking (variant A)",
  "Chunking B": "Chunking (variant B)",
  "Chunking C": "Chunking (variant C)",
  "C2C": "Count to Count",
  "CBO": "Count By Ones to Base",
  "Commutative Reasoning": "Commutative Reasoning",
  "DR": "Doubling and Redistribution",
  "Dealing by Ones": "Dealing By Ones",
  "CBO (Division)": "Count By Ones to Base",
  "IDP": "Inverse Division by Product",
  "UCR": "Unit Coordination and Remainders"
};

const CRISIS_EXPLANATIONS = {
  efficiency_crisis: (a, b, op, limit) =>
    `The machine can ${op} by counting, but ${a} ${OP_SYMBOLS[op]} ${b} requires more ` +
    `counting steps than its ${limit}-inference budget allows. ` +
    `Counting All touches every unit one at a time \\u2014 it works, but at a cost ` +
    `proportional to the size of the numbers.`,
  unknown_operation: (a, b, op) =>
    `The machine has never encountered ${op}. It has no concept of this operation ` +
    `and cannot even begin to attempt it. This is not an efficiency problem \\u2014 ` +
    `the operation is entirely absent from the machine''s repertoire.`
};

function strategyDisplay(abbrev) {
  const full = STRATEGY_NAMES[abbrev];
  return full ? `${full} (${abbrev})` : abbrev;
}

function tallies(n) {
  if (n > 25) return `[${n}]`;
  let html = "";
  for (let i = 0; i < n; i++) {
    if (i > 0 && i % 5 === 0) html += " ";
    html += "\\u2758";
  }
  return html;
}

function resourceBar(used, total) {
  const pct = Math.min(100, Math.round((used / total) * 100));
  const cls = pct >= 100 ? "exhausted" : pct > 70 ? "warn" : "ok";
  return `<div class="resource-bar">
    <div class="bar-track"><div class="bar-fill ${cls}" style="width:${pct}%"></div></div>
    <span>${used}/${total} inferences${pct >= 100 ? " (exhausted)" : ""}</span>
  </div>`;
}

function tensionReadout(tensionState, tensionHistory) {
  if (!tensionState) return "";
  const level = tensionState.level || 0;
  const stability = tensionState.stability || 0;
  const stClass = stability > 0.5 ? "stable" : stability > -0.5 ? "inflection" : "unstable";
  const stLabel = stability > 0.5 ? "stable" : stability > -0.5 ? "inflection" : "unstable";
  // Tension bar: normalize level to 0-100 range (cap at 50 for display)
  const barPct = Math.min(100, Math.round((level / 50) * 100));
  let html = `<div class="tension-readout">
    <span>Tension</span>
    <div class="tension-bar-track"><div class="tension-bar-fill ${stClass}" style="width:${barPct}%"></div></div>
    <span class="tension-label">${level.toFixed(1)}
      <span class="stability-tag ${stClass}">${stLabel}</span>
    </span>
  </div>`;
  // Check for relaxation events in the history
  if (tensionHistory && tensionHistory.length > 0) {
    const relaxations = tensionHistory.filter(h => h.context === "relaxation");
    if (relaxations.length > 0) {
      const last = relaxations[relaxations.length - 1];
      // Find the tension level just before the relaxation
      const idx = tensionHistory.indexOf(last);
      const before = idx > 0 ? tensionHistory[idx - 1] : null;
      if (before) {
        html += `<div class="tension-relaxation">Crisis resolved \\u2014 tension relaxed from ${before.level.toFixed(1)} to ${last.level.toFixed(1)}</div>`;
      }
    }
  }
  return html;
}

function makeCard(phase, cls, content) {
  return `<div class="card ${cls}">
    <div class="card-phase"><span class="dot"></span>${phase}</div>
    ${content}
  </div>`;
}

function connector() { return \'<div class="connector"></div>\'; }

function checkWarnings() {
  const a = parseInt(document.getElementById("a").value) || 0;
  const b = parseInt(document.getElementById("b").value) || 0;
  const warn = document.getElementById("warning");
  warn.style.display = (a > 15 || b > 15) ? "block" : "none";
}

async function compute() {
  const btn = document.getElementById("run");
  const narr = document.getElementById("narrative");
  btn.disabled = true;
  narr.innerHTML = "<p class=\\"dim\\" style=\\"text-align:center;padding:2rem\\">Computing...</p>";

  const problem = {
    operation: document.getElementById("op").value,
    a: parseInt(document.getElementById("a").value),
    b: parseInt(document.getElementById("b").value),
    limit: parseInt(document.getElementById("limit").value)
  };

  try {
    const res = await fetch("/api/compute", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify(problem)
    });
    const data = await res.json();
    renderNarrative(data, problem);
    renderKnowledge(data.knowledge);
  } catch (e) {
    narr.innerHTML = `<div class="card failure">
      <div class="card-phase"><span class="dot"></span>Error</div>
      <p>${e.message}</p>
    </div>`;
  } finally {
    btn.disabled = false;
  }
}

function renderNarrative(data, problem) {
  const narr = document.getElementById("narrative");
  const events = data.events;
  const op = problem.operation;
  const a = problem.a, b = problem.b;
  const sym = OP_SYMBOLS[op];
  const limit = problem.budget || problem.limit;
  const tState = data.tension || null;
  const tHistory = data.tension_history || [];

  const cards = [];
  let resolveEvent = null;

  // Process events into cards
  let i = 0;
  while (i < events.length) {
    const e = events[i];

    if (e.type === "computation_start" && i + 1 < events.length) {
      const next = events[i + 1];

      if (next.type === "computation_success") {
        // Direct success
        const used = next.inferences_used || "?";
        const result = next.result != null ? next.result : "?";
        const isRetry = cards.length > 0;

        if (isRetry) {
          // This is the resolution after learning
          const strategy = resolveEvent ? resolveEvent.strategy : null;
          cards.push(makeCard("Resolve", "resolve", `
            <p class="emph" style="font-size:1.3rem">${a} ${sym} ${b} = ${result}</p>
            ${strategy ? `<p>Using ${strategyDisplay(strategy)}</p>` : ""}
            ${strategy ? renderStrategyVisual(op, a, b, result, strategy) : ""}
            ${resourceBar(used, limit)}
            ${tensionReadout(tState, tHistory)}
          `));
        } else {
          cards.push(makeCard("Observe", "direct-success", `
            <p>The machine computes <span class="emph">${a} ${sym} ${b} = ${result}</span></p>
            <p class="dim">Solved directly with current knowledge.</p>
            ${resourceBar(used, limit)}
            ${tensionReadout(tState, tHistory)}
          `));
        }
        i += 2;
        continue;
      }

      if (next.type === "crisis_detected") {
        // Failed attempt
        cards.push(makeCard("Observe", "observe", `
          <p>The machine attempts <span class="emph">${a} ${sym} ${b}</span>
          using its only approach: <span class="emph">Counting All</span>.</p>
          <p class="dim">Build both numbers as tallies, then count the total from 1.</p>
          <div class="tallies">${tallies(a)} <span class="op">${sym}</span> ${tallies(b)}</div>
          <p class="dim">Each tally mark costs an inference. The meta-interpreter adds overhead
          for each step of the computation.</p>
          ${resourceBar(limit, limit)}
        `));
        i += 2;

        // Crisis classification
        if (i < events.length && events[i].type === "crisis_classified") {
          const cls = events[i];
          const crisisType = cls.classification || "unclassified";
          const explanation = CRISIS_EXPLANATIONS[crisisType]
            ? CRISIS_EXPLANATIONS[crisisType](a, b, op, limit)
            : cls.signal || "The machine''s current approach has failed.";
          cards.push(makeCard("Crisis", "crisis", `
            <p class="emph">${crisisType.replace(/_/g, " ")}</p>
            <p>${explanation}</p>
            <p class="dim">The machine''s current way of being is inadequate.
            It must learn or fail.</p>
            ${tensionReadout(tState, tHistory)}
          `));
          i++;
        }

        // Reorganize: collect oracle + synthesis events
        const reorgParts = [];
        let synthesisOk = false;
        let validationOk = false;
        let oracleStrategy = null;
        let oracleResult = null;
        let oracleInterp = null;

        while (i < events.length && events[i].type !== "computation_start"
               && events[i].type !== "computation_failed") {
          const re = events[i];
          if (re.type === "oracle_consulted") {
            oracleStrategy = re.strategy;
            oracleResult = re.result;
            oracleInterp = re.interpretation;
          }
          if (re.type === "oracle_exhausted") {
            reorgParts.push(`<p class="dim">The teacher has no further interventions for this operation.</p>`);
          }
          if (re.type === "synthesis_succeeded") synthesisOk = true;
          if (re.type === "synthesis_failed") synthesisOk = false;
          if (re.type === "validation_passed") validationOk = true;
          if (re.type === "validation_failed") validationOk = false;
          if (re.type === "retry") resolveEvent = { strategy: oracleStrategy };
          i++;
        }

        if (oracleStrategy) {
          cards.push(makeCard("Reorganize", "reorganize", `
            <p class="emph">Teacher intervention</p>
            <p>Strategy: ${strategyDisplay(oracleStrategy)}</p>
            <div class="oracle-quote">${oracleInterp || ""}</div>
            <p class="dim">The teacher created conditions for this strategy to emerge.
            The system receives a result (${oracleResult}) and a description of
            the method, but not its internal workings. It must reconstruct the practice from
            its own primitives (successor, predecessor, decompose).</p>
            <ul class="checklist">
              <li><span class="${synthesisOk ? "ok" : "fail"}">${synthesisOk ? "\\u2713" : "\\u2717"}</span>
                Strategy synthesized from primitives</li>
              <li><span class="${validationOk ? "ok" : "fail"}">${validationOk ? "\\u2713" : "\\u2717"}</span>
                Validation: result matches oracle</li>
            </ul>
            ${reorgParts.join("")}
          `));
        } else {
          cards.push(makeCard("Reorganize", "failure", `
            <p class="emph">No intervention available</p>
            ${reorgParts.join("")}
            <p class="dim">No strategy available. The crisis remains unresolved.</p>
          `));
        }
        continue;
      }
    }

    // Fallback for computation_failed at top level
    if (e.type === "computation_failed") {
      cards.push(makeCard("Failed", "failure", `
        <p>The computation failed. The machine could not solve
        ${a} ${sym} ${b} within its constraints.</p>
      `));
      i++;
      continue;
    }

    i++;
  }

  // Join cards with connectors
  narr.innerHTML = cards.join(connector());
}

function renderStrategyVisual(op, a, b, result, strategy) {
  if (!strategy) return "";

  if (strategy === "COBO" && op === "add") {
    // COBO decomposes B into tens (bases) and ones, then counts on
    const bases = Math.floor(b / 10);
    const ones = b % 10;
    const baseSteps = [];
    let cur = a;
    for (let s = 0; s < bases; s++) { cur += 10; baseSteps.push(cur); }
    const oneSteps = [];
    for (let s = 0; s < ones; s++) { cur += 1; oneSteps.push(cur); }
    let viz = `<div class="tallies"><span class="bracket">[${a}]</span>`;
    if (bases > 0) viz += `<span class="dim"> +${bases} tens: ${baseSteps.join(", ")}</span>`;
    if (ones > 0) viz += `<span class="dim"> +${ones} ones: ${oneSteps.join(", ")}</span>`;
    viz += `<span class="dim"> \\u2192 ${result}</span></div>`;
    return viz;
  }

  if (strategy === "COBO (Missing Addend)" && op === "subtract") {
    const steps = [];
    for (let s = b + 1; s <= a; s++) steps.push(s);
    return `<div class="tallies">
      <span class="bracket">[${b}]</span>
      <span class="dim"> count up to ${a}: ${steps.join(", ")} \\u2192 gap = ${result}</span>
    </div>`;
  }

  return "";
}

function renderKnowledge(knowledge) {
  const el = document.getElementById("knowledge");
  if (!knowledge) { el.innerHTML = "<p class=\\"dim\\">Loading...</p>"; return; }

  let html = "";
  for (const k of knowledge) {
    const learned = k.learned || [];
    const display = learned.length > 0
      ? "Counting All <span class=\\"arrow\\">\\u2192</span> " +
        learned.map(s => strategyDisplay(s)).join(", ")
      : "<span class=\\"primordial\\">Counting All only</span>";
    html += `<div class="knowledge-op">
      <span class="op-name">${k.operation}</span>
      <span class="strategies">${display}</span>
    </div>`;
  }
  el.innerHTML = html;
}

async function resetMachine() {
  if (!confirm("Reset the machine to primordial state? All learned strategies will be forgotten.")) return;
  await fetch("/api/reset", { method: "POST" });
  document.getElementById("narrative").innerHTML = `
    <div class="empty-state">
      <p>Machine reset to primordial state.</p>
      <p>It knows only Counting All.</p>
    </div>`;
  // Refresh knowledge
  const res = await fetch("/api/knowledge");
  const knowledge = await res.json();
  renderKnowledge(knowledge);
}

// Load initial knowledge state
(async function() {
  try {
    const res = await fetch("/api/knowledge");
    const knowledge = await res.json();
    renderKnowledge(knowledge);
  } catch(e) {}
})();

checkWarnings();
</script>
</body>
</html>'.

% ═══════════════════════════════════════════════════════════════════════
% Auto-start
% ═══════════════════════════════════════════════════════════════════════

:- initialization((start_server, thread_get_message(_))).
