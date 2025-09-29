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
    max_retries/1
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
max_inferences(15).

%!      max_retries(?Limit:integer) is nondet.
%
%       Defines the maximum number of times the system will attempt to
%       reorganize and retry a goal after a failure. This prevents infinite
%       loops if the system is unable to find a stable, coherent solution.
%
%       This predicate is dynamic.
:- dynamic max_retries/1.
max_retries(5).