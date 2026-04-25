/** <module> Module search paths for modularized codebase
 *
 * Load this file before any module to set up file_search_path so
 * use_module directives resolve across module boundaries.
 *
 * Usage: swipl -l paths.pl -l learner/server.pl
 *    or: :- [paths].  at the top of an entry point
 */

:- multifile file_search_path/2.

% Module directories (at repo root)
file_search_path(pml,           'pml').
file_search_path(arche_trace,   'arche-trace').
file_search_path(strategies,    'strategies').
file_search_path(learner,       'learner').
file_search_path(formalization,  'formalization').
file_search_path(misconceptions, 'misconceptions').

% Sub-directories within modules
file_search_path(math,          strategies('math')).
file_search_path(standards,     strategies('standards')).
file_search_path(meta,          strategies('meta')).
