:- module(config, [max_inferences/1]).

% config.pl
:- dynamic max_inferences/1.
max_inferences(15). % Set a low threshold to easily trigger reorganization.