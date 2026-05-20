:- module(n101_index, [term_file/1, all_terms/1]).

% One entry per term file. Add new terms by creating the file and appending here.

term_file('vocabularies/n101/quantity.pl').
term_file('vocabularies/n101/measurement_unit.pl').
term_file('vocabularies/n101/measurement_process.pl').
term_file('vocabularies/n101/measure.pl').
term_file('vocabularies/n101/counting.pl').
term_file('vocabularies/n101/base.pl').
term_file('vocabularies/n101/base_five.pl').
term_file('vocabularies/n101/explanation.pl').
term_file('vocabularies/n101/strategy.pl').
term_file('vocabularies/n101/creative_activity.pl').

all_terms(Terms) :-
    findall(F, term_file(F), Terms).
