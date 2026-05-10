/** <module> Module search paths for modularized codebase
 *
 * Load this file before any module to set up file_search_path so
 * use_module directives resolve across module boundaries.
 *
 * Usage: swipl -l paths.pl -l learner/server.pl
 *    or: :- [paths].  at the top of an entry point
 */

:- multifile user:file_search_path/2.
:- dynamic user:file_search_path/2.

:- prolog_load_context(directory, PrologRoot),
   forall(member(Alias-Relative,
                 [ pml-'pml',
                   arche_trace-'arche-trace',
                   strategies-'strategies',
                   learner-'learner',
                   formalization-'formalization',
                   misconceptions-'misconceptions'
                 ]),
          ( directory_file_path(PrologRoot, Relative, Absolute),
            ( user:file_search_path(Alias, Absolute)
            -> true
            ;  asserta(user:file_search_path(Alias, Absolute))
            )
          )).

% Sub-directories within modules
file_search_path(math,          strategies('math')).
file_search_path(standards,     strategies('standards')).
file_search_path(meta,          strategies('meta')).
