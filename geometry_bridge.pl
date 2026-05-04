% geometry_bridge.pl — thin loader of the geometry KB into Hermes.
%
% The geometry knowledge base is authored in
% /Users/tio/Documents/GitHub/umedcta-formalization/geometry/.
% This file is the *only* point at which the Hermes anchor repo
% pulls geometry facts. Authoring lives across the repo line; consumption
% does not.
%
% Loaded via:  ?- consult('/Users/tio/Documents/GitHub/Prolog/geometry_bridge.pl'), load_geometry_kb.
%
% After load, all KB predicates are available in the user namespace:
%   geom_concept/4, van_hiele_marker/4, metaphor_source/4,
%   geom_misconception/6, material_inference/4, bootstrap/6,
%   construction/5, standard_anchor/4, tier/4, triangulation/2,
%   pck_synthesis/5, developmental_marker/4,
%   validate_geom_kb/0, coverage_report/1.
%
% See docs/superpowers/specs/2026-05-03-hermes-geometry-overnight-design.md
% for the full design.

geometry_kb_root('/Users/tio/Documents/GitHub/umedcta-formalization/geometry/').

% Loader — consults schema.pl and every .pl in the tagging-module subdirs.
load_geometry_kb :-
    geometry_kb_root(Root),
    atom_concat(Root, 'schema.pl', SchemaPath),
    consult(SchemaPath),
    load_subdir(Root, concepts),
    load_subdir(Root, metaphors),
    load_subdir(Root, van_hiele),
    load_subdir(Root, bootstrap),
    load_subdir(Root, standards),
    load_subdir(Root, pck).

load_subdir(Root, Sub) :-
    atom_concat(Root, Sub, SubPath),
    atom_concat(SubPath, '/*.pl', Glob),
    expand_file_name(Glob, Files),
    forall(member(F, Files), consult(F)).
