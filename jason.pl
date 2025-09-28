:- module(jason, [run_tests/0]).
:- use_module(library(rat)).

% =============================================================================
% I. Cognitive Material Representation (ContinuousUnit)
% =============================================================================
%
% We represent a ContinuousUnit as a compound term: unit(Value, History).
% - Value: A rational number (e.g., 1, 3 rdiv 7).
% - History: A string representing the operational history.

% =============================================================================
% II. Iterative Core: Explicitly Nested Number Sequence (ENS) Operations
% =============================================================================

% [CORE::Partitioning]
% ens_partition(+UnitIn, +N, -PartitionedWhole)
% Divides a continuous unit into N equal parts.
ens_partition(unit(Value, History), N, PartitionedWhole) :-
    N > 0,
    NewValue is Value / N,
    format(string(NewHistory), '1/~w part of (~w)', [N, History]),
    length(PartitionedWhole, N),
    maplist(=(unit(NewValue, NewHistory)), PartitionedWhole).

% [CORE::Disembedding]
% ens_disembed(+PartitionedWhole, -UnitFraction)
% Isolates a single unit part from the partitioned whole.
ens_disembed([UnitFraction | _], UnitFraction) :- !.
ens_disembed([], _) :- throw(error(cannot_disembed_from_empty_list, _)).

% [CORE::Iterating]
% ens_iterate(+UnitIn, +M, -ResultUnit)
% Repeats a unit M times.
ens_iterate(unit(Value, History), M, unit(NewValue, NewHistory)) :-
    NewValue is Value * M,
    format(string(NewHistory), '~w iterations of [~w]', [M, History]).

% =============================================================================
% III. Strategic Shell: The Partitive Fractional Scheme (PFS)
% =============================================================================

% run_pfs(+Whole, +Numerator, +Denominator, -Result, -Trace)
run_pfs(Whole, Num, Den, Result, Trace) :-
    % Initialize V (variables) in a dict
    V0 = v{whole: Whole, n: Den, m: Num},
    format(string(Log0), 'PFS Initialized: Find ~w/~w of ~w', [Num, Den, Whole.value]),

    % Start the state machine loop
    pfs_loop(q_start, V0, Result, [Log0|Trace]).

% pfs_loop is the recursive state machine driver
pfs_loop(q_accept, V, V.result, Trace) :-
    reverse(Trace, RevTrace), % Keep chronological order
    append(RevTrace, ["PFS Complete."], Trace).
pfs_loop(CurrentState, V_in, Result, Trace) :-
    pfs_transition(CurrentState, V_in, NextState, V_out, Log),
    pfs_loop(NextState, V_out, Result, [Log|Trace]).

% pfs_transition(+State, +V_in, -NextState, -V_out, -Log)
% Defines the state transitions (delta function)
pfs_transition(q_start, V, q_partition, V, "Transition to partition state") :- !.

pfs_transition(q_partition, V_in, q_disembed, V_out, Log) :-
    format(string(Log), '[State: q_partition] Action: Partitioning Whole into ~w parts.', [V_in.n]),
    ens_partition(V_in.whole, V_in.n, Partitioned),
    V_out = V_in.put(partitioned_whole, Partitioned),
    !.

pfs_transition(q_disembed, V_in, q_iterate, V_out, Log) :-
    ens_disembed(V_in.partitioned_whole, UnitFraction),
    format(string(Log), '[State: q_disembed] Action: Disembedded Unit Fraction (~w).', [UnitFraction.value]),
    V_out = V_in.put(unit_fraction, UnitFraction),
    !.

pfs_transition(q_iterate, V_in, q_accept, V_out, Log) :-
    format(string(Log), '[State: q_iterate] Action: Iterating Unit Fraction ~w times.', [V_in.m]),
    ens_iterate(V_in.unit_fraction, V_in.m, Result),
    V_out = V_in.put(result, Result),
    !.

% =============================================================================
% IV. Strategic Shell: The Fractional Composition Scheme (FCS)
% =============================================================================

% run_fcs(+Whole, +OuterFrac, +InnerFrac, -Result, -Trace)
% OuterFrac and InnerFrac are pairs: Num-Den
run_fcs(Whole, A-B, C-D, Result, Trace) :-
    V0 = v{whole: Whole, a:A, b:B, c:C, d:D},
    format(string(Log0), 'FCS Initialized: Find ~w/~w of ~w/~w of ~w', [A, B, C, D, Whole.value]),
    fcs_loop(q_start, V0, Result, [log(q_start, Log0, [])|Trace]).

% fcs_loop is the recursive state machine driver for FCS
fcs_loop(q_accept, V, V.final_result, Trace) :-
    reverse(Trace, RevTrace),
    append(RevTrace, [log(q_accept, "FCS Complete.", [])], Trace).
fcs_loop(CurrentState, V_in, Result, Trace) :-
    fcs_transition(CurrentState, V_in, NextState, V_out, Log, NestedTrace),
    fcs_loop(NextState, V_out, Result, [log(NextState, Log, NestedTrace)|Trace]).

% fcs_transition(+State, +V_in, -NextState, -V_out, -Log, -NestedTrace)
fcs_transition(q_start, V, q_inner_PFS, V, "Transition to inner_PFS state", []) :- !.

fcs_transition(q_inner_PFS, V_in, q_accommodate, V_out, Log, NestedTrace) :-
    format(string(LogAction), 'Calculating inner fraction (~w/~w).', [V_in.c, V_in.d]),
    % Invoke PFS for the inner fraction
    run_pfs(V_in.whole, V_in.c, V_in.d, IntermediateResult, NestedTrace),
    V_out = V_in.put(intermediate_result, IntermediateResult),
    format(string(Log), '-> Intermediate Result: ~w', [IntermediateResult.value]),
    % Log contains the action, NestedTrace contains the trace from run_pfs
    !.

fcs_transition(q_accommodate, V_in, q_outer_PFS, V_out, Log, []) :-
    Log = "[State: q_accommodate] METAMORPHIC ACCOMMODATION: Using IntermediateResult as new Whole.",
    % The output of the last step is the input for the next
    V_out = V_in.put(new_whole, V_in.intermediate_result),
    !.

fcs_transition(q_outer_PFS, V_in, q_accept, V_out, Log, NestedTrace) :-
    format(string(LogAction), 'Calculating outer fraction (~w/~w) on new Whole.', [V_in.a, V_in.b]),
    % Invoke PFS for the outer fraction on the new whole
    run_pfs(V_in.new_whole, V_in.a, V_in.b, FinalResult, NestedTrace),
    V_out = V_in.put(final_result, FinalResult),
    format(string(Log), '-> Final Result: ~w', [FinalResult.value]),
    !.

% =============================================================================
% V. Demonstration and Testing
% =============================================================================

run_tests :-
    writeln('=== JASON AUTOMATON MODEL TESTING ==='),

    % Define the initial Whole
    TheWhole = unit(1, "Reference Unit"),

    % --- Test 1: Partitive Fractional Scheme (PFS) ---
    writeln('\n' + '============================================================'),
    writeln('TEST 1: Construct 3/7 of the Whole (PFS)'),
    writeln('============================================================'),
    run_pfs(TheWhole, 3, 7, ResultPFS, TracePFS),
    writeln('\nExecution Trace (Cognitive Choreography):'),
    print_pfs_trace(TracePFS),
    format('~nRESULT (PFS): ~w~n', [ResultPFS]),

    % --- Test 2: Fractional Composition Scheme (FCS) ---
    writeln('\n' + '============================================================'),
    writeln('TEST 2: Construct 3/4 of 1/4 of the Whole (FCS)'),
    writeln('Modeling Metamorphic Accommodation (Recursive Partitioning)'),
    writeln('============================================================'),
    run_fcs(TheWhole, 3-4, 1-4, ResultFCS, TraceFCS),
    writeln('\nExecution Trace (Cognitive Choreography):'),
    print_fcs_trace(TraceFCS, ""),
    format('~nRESULT (FCS): ~w~n', [ResultFCS]).

% Helper to print the flat trace from PFS
print_pfs_trace(Trace) :-
    forall(member(Line, Trace), writeln(Line)).

% Helper to print the potentially nested trace from FCS
print_fcs_trace([], _).
print_fcs_trace([log(State, Action, NestedTrace)|Rest], Indent) :-
    format('~wState: ~w, Action: ~w~n', [Indent, State, Action]),
    ( NestedTrace \= [] ->
        format('~w  [Begin Nested PFS Execution]~n', [Indent]),
        atom_concat(Indent, '    ', NewIndent),
        % Since PFS trace is flat list of strings
        forall(member(Line, NestedTrace), format('~w~w~n', [NewIndent, Line])),
        format('~w  [End Nested PFS Execution]~n', [Indent])
    ; true
    ),
    print_fcs_trace(Rest, Indent).
