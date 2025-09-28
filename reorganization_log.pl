:- module(reorganization_log, [
    log_event/1,
    get_log/1,
    clear_log/0,
    generate_report/1
]).

% The log is stored as a series of dynamic facts.
% log_entry(Timestamp, Event).
:- dynamic log_entry/2.

% log_event(+Event)
%
% Records a structured event with a timestamp.
log_event(Event) :-
    get_time(Timestamp),
    assertz(log_entry(Timestamp, Event)).

% get_log(-Log)
%
% Retrieves the entire log as a list of entries.
get_log(Log) :-
    findall(log_entry(T, E), log_entry(T, E), Log).

% clear_log
%
% Clears all entries from the log.
clear_log :-
    retractall(log_entry(_, _)).

% generate_report(-Report)
%
% Translates the log into a human-readable narrative string.
generate_report(Report) :-
    get_log(Log),
    phrase(narrative(Log), Tokens),
    atomics_to_string(Tokens, Report).

% --- DCG for Narrative Generation ---

narrative([]) --> [].
narrative([log_entry(_, Event)|Rest]) -->
    event_narrative(Event),
    narrative(Rest).

event_narrative(orr_cycle_start(Goal)) -->
    ["- System started observing goal: ", Goal, ".\n"].

event_narrative(disequilibrium(Trigger)) -->
    ["- Reflection detected disequilibrium. Trigger: ", Trigger, ".\n"].

event_narrative(reorganization_start(Signature)) -->
    ["- Reorganization started, targeting predicate: ", Signature, ".\n"].

event_narrative(retracted(Clause)) -->
    ["  - The old clause was retracted: ", Clause, ".\n"].

event_narrative(asserted(Clause)) -->
    ["  - A new clause was asserted: ", Clause, ".\n"].

event_narrative(reorganization_success) -->
    ["- Reorganization was successful. System is retrying the goal to seek a new equilibrium.\n"].

event_narrative(reorganization_failure) -->
    ["- Reorganization failed. The system could not find a way to accommodate the issue.\n"].

event_narrative(equilibrium) -->
    ["- Equilibrium reached. The goal succeeded and was found to be coherent.\n"].

event_narrative(Unknown) -->
    ["- An unknown event was logged: ", Unknown, ".\n"].