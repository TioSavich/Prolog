/** <module> System Configuration
 *
 * This module defines configuration parameters for the ORR (Observe,
 * Reorganize, Reflect) system. These parameters control the behavior of the
 * cognitive cycle, such as resource limits.
 *
 * 
 * 
 */
:- module(config, [
    max_inferences/1,
    max_retries/1,
    cognitive_cost/2,
    server_mode/1,
    server_endpoint_enabled/1
    ]).

%!      max_inferences(?Limit:integer) is nondet.
%
%       Defines the maximum number of inference steps the meta-interpreter
%       is allowed to take before a `resource_exhaustion` perturbation is
%       triggered.
%
%       This is a key parameter for learning. It is intentionally set to a
%       low value to make inefficient strategies (like the initial `add/3`
%       implementation) fail, thus creating a "disequilibrium" that the
%       system must resolve through reorganization.
%
%       This predicate is dynamic, so it can be changed at runtime if needed.
:- dynamic max_inferences/1.
max_inferences(1).

%!      max_retries(?Limit:integer) is nondet.
%
%       Defines the maximum number of times the system will attempt to
%       reorganize and retry a goal after a failure. This prevents infinite
%       loops if the system is unable to find a stable, coherent solution.
%
%       This predicate is dynamic.
:- dynamic max_retries/1.
max_retries(5).

% --- Cognitive Cost Configuration ---

%!      cognitive_cost(?Action:atom, ?Cost:number) is nondet.
%
%       Defines the fundamental unit costs of cognitive operations for the
%       embodied mathematics system. This implements the "measuring stick"
%       metaphor where computational effort represents embodied distance.
%
%       Different actions have different cognitive costs based on their
%       embodied nature:
%       - unit_count: The effort of counting one item (high effort, temporal)
%       - slide_step: Moving one step on a mental number line (spatial, lower effort)
%       - fact_retrieval: Accessing a known fact (compressed, minimal effort)
%       - inference: Standard logical inference (abstract reasoning)
%
%       This predicate is dynamic to allow learning-based cost adjustments.
:- dynamic cognitive_cost/2.

% Default cost for a standard logical inference (abstract reasoning)
cognitive_cost(inference, 1).

% Cost for an atomic, embodied counting action (temporally extended)
cognitive_cost(unit_count, 5).

% Cost for moving one unit on a mental number line (spatialized action)
cognitive_cost(slide_step, 2).

% Cost of retrieving a known fact (highly compressed, minimal effort)
cognitive_cost(fact_retrieval, 1).

% Cost for modal state transitions (embodied cognitive shifts)
cognitive_cost(modal_shift, 3).

% Cost for normative checking (validating against mathematical context)
cognitive_cost(norm_check, 2).

% --- Server Configuration ---

%!      server_mode(?Mode:atom) is nondet.
%
%       Defines the current server mode which controls which endpoints
%       and features are available.
%       - development: Full debugging and analysis endpoints
%       - production: Full-featured production server with all core endpoints
%       - testing: Limited endpoints for automated testing  
%       - simple: Self-contained endpoints without module dependencies
%
%       This predicate is dynamic to allow runtime reconfiguration.
:- dynamic server_mode/1.
server_mode(development).

%!      server_endpoint_enabled(?Endpoint:atom) is nondet.
%
%       Defines which endpoints are enabled based on the current server mode.
%       This allows fine-grained control over API availability.
:- dynamic server_endpoint_enabled/1.

% Production mode: Core endpoints for deployment
server_endpoint_enabled(solve) :- server_mode(production).
server_endpoint_enabled(analyze_semantics) :- server_mode(production).
server_endpoint_enabled(analyze_strategy) :- server_mode(production).
server_endpoint_enabled(execute_orr) :- server_mode(production).
server_endpoint_enabled(get_reorganization_log) :- server_mode(production).
server_endpoint_enabled(cognitive_cost) :- server_mode(production).

% Development mode: All endpoints enabled
server_endpoint_enabled(solve) :- server_mode(development).
server_endpoint_enabled(analyze_semantics) :- server_mode(development).
server_endpoint_enabled(analyze_strategy) :- server_mode(development).
server_endpoint_enabled(execute_orr) :- server_mode(development).
server_endpoint_enabled(get_reorganization_log) :- server_mode(development).
server_endpoint_enabled(cognitive_cost) :- server_mode(development).
server_endpoint_enabled(debug_trace) :- server_mode(development).
server_endpoint_enabled(modal_analysis) :- server_mode(development).
server_endpoint_enabled(stress_analysis) :- server_mode(development).
server_endpoint_enabled(test_grounded_arithmetic) :- server_mode(development).

% Testing mode: Minimal endpoints for validation
server_endpoint_enabled(test) :- server_mode(testing).
server_endpoint_enabled(health) :- server_mode(testing).

% Simple mode: Self-contained endpoints
server_endpoint_enabled(analyze_semantics) :- server_mode(simple).
server_endpoint_enabled(analyze_strategy) :- server_mode(simple).
server_endpoint_enabled(test) :- server_mode(simple).

% Production mode: Minimal endpoints
server_endpoint_enabled(solve) :- server_mode(production).