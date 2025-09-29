### Implementation Guide: Grounding Fractional Arithmetic

This guide involves creating several new supporting modules and then rewriting `jason.pl`.

#### Phase 0: Prerequisites

**Grounded Multiplication:** Ensure `grounded_arithmetic.pl` robustly implements `multiply_grounded/3` (via repeated addition/list concatenation) and `incur_cost/1`. This is required for calculating composite denominators during the integration phase.

#### Phase 1: The Grounded Architecture - Representation and Equivalence

**1.1. The Nested Unit Representation**

We must adopt a convention where quantities are represented as lists of recursively defined units, capturing the history of partitioning.

```prolog
% Representation Conventions:

% A Quantity is a list of Units.

% The fundamental unit:
% unit(whole)

% A unit derived from partitioning a ParentUnit into D parts (1/D of Parent):
% unit(partitioned(D_Rec, ParentUnit))
% D_Rec MUST be a recollection structure.

% Example: 1/4 of 1/3 of the Whole (R3, R4 are recollections for 3 and 4)
% unit(partitioned(R4, unit(partitioned(R3, unit(whole)))))
```

**1.2. The Generalized Composition Engine**

This engine implements the embodied act of grouping.

**Action:** Create `composition_engine.pl`.

```prolog
% File: composition_engine.pl
:- module(composition_engine, [find_and_extract_copies/4]).
:- use_module(grounded_arithmetic, [incur_cost/1]).

% find_and_extract_copies(+CountRec, +UnitType, +InputQty, -Remainder) is semidet.
find_and_extract_copies(recollection(Tallies), UnitType, InputQty, Remainder) :-
    extract_recursive(Tallies, UnitType, InputQty, Remainder).

extract_recursive([], _UnitType, CurrentQty, CurrentQty).
extract_recursive([t|Ts], UnitType, InputQty, Remainder) :-
    % select/3 finds and removes one instance.
    select(UnitType, InputQty, TempQty),
    incur_cost(unit_grouping),
    extract_recursive(Ts, UnitType, TempQty, Remainder).
```

**1.3. Fractional Semantics (Equivalence Rules)**

This module defines the rules of equivalence for the nested representation.

**Action:** Create `fraction_semantics.pl`.

```prolog
% File: fraction_semantics.pl
:- module(fraction_semantics, [apply_equivalence_rule/3]).
:- use_module(composition_engine, [find_and_extract_copies/4]).
:- use_module(grounded_arithmetic, [incur_cost/1, multiply_grounded/3]).

% apply_equivalence_rule(+RuleName, +QtyIn, -QtyOut) is semidet.

% Rule 1: Grouping (Reconstitution)
% D copies of (1/D of P) equals P.
apply_equivalence_rule(grouping, QtyIn, QtyOut) :-
    % Identify a unit fraction type (D_Rec and ParentUnit) present in the list.
    UnitToGroup = unit(partitioned(D_Rec, ParentUnit)),
    member(UnitToGroup, QtyIn),

    % Try to find D copies of this specific unit.
    find_and_extract_copies(D_Rec, UnitToGroup, QtyIn, Remainder),

    % If successful, they are replaced by the ParentUnit.
    QtyOut = [ParentUnit|Remainder],
    incur_cost(equivalence_grouping).

% Rule 2: Composition (Integration/Coordination of Units)
% (1/A of (1/B of P)) equals (1/(A*B) of P).
% This handles the coordination of three levels of units.
apply_equivalence_rule(composition, QtyIn, QtyOut) :-
    % Look for a nested partition structure.
    NestedUnit = unit(partitioned(A_Rec, unit(partitioned(B_Rec, ParentUnit)))),
    member(NestedUnit, QtyIn),

    % Calculate the new denominator A*B (Fully grounded).
    multiply_grounded(A_Rec, B_Rec, AB_Rec),

    % Define the equivalent simple unit fraction.
    SimpleUnit = unit(partitioned(AB_Rec, ParentUnit)),

    % Replace the nested unit with the simple unit.
    select(NestedUnit, QtyIn, TempQty),
    QtyOut = [SimpleUnit|TempQty],
    incur_cost(equivalence_composition).
```

**1.4. Normalization Engine**

This engine repeatedly applies the equivalence rules until the quantity is simplified.

**Action:** Create `normalization.pl`.

```prolog
% File: normalization.pl
:- module(normalization, [normalize/2]).
:- use_module(fraction_semantics, [apply_equivalence_rule/3]).

% normalize(+QtyIn, -QtyOut) is det.
normalize(QtyIn, QtyOut) :-
    (   apply_normalization_step(QtyIn, QtyTemp)
    ->  normalize(QtyTemp, QtyOut)
    ;   % Sort for a canonical representation
        sort(QtyIn, QtyOut)
    ).

% Tries to apply one rule. Use once/1 to commit to the first success.
apply_normalization_step(QtyIn, QtyOut) :-
    % 1. Try Grouping (e.g., 3/3 -> 1)
    once(apply_equivalence_rule(grouping, QtyIn, QtyOut)).
apply_normalization_step(QtyIn, QtyOut) :-
    % 2. Try Composition (e.g., 1/4 of 1/3 -> 1/12)
    once(apply_equivalence_rule(composition, QtyIn, QtyOut)).
```

#### Phase 2: Refactoring Jason's Schemes (`jason.pl`)

We now rewrite `jason.pl` to implement the Partitive Fractional Scheme (PFS) using the new grounded architecture.

**2.1. Grounded ENS Operations (Helper)**

We need a module for the core action of partitioning a unit, which generates the nested structure.

**Action:** Create `grounded_ens_operations.pl`.

```prolog
% File: grounded_ens_operations.pl
:- module(grounded_ens_operations, [ens_partition/3]).
:- use_module(grounded_arithmetic, [incur_cost/1]).

% ens_partition(+InputUnit, +N_Rec, -PartitionedParts) is det.
% Partitions a single InputUnit into N parts.
ens_partition(InputUnit, N_Rec, PartitionedParts) :-
    % The new unit is defined structurally as 1/N of the InputUnit.
    % This naturally handles recursive partitioning by creating nested structures.
    NewUnit = unit(partitioned(N_Rec, InputUnit)),

    % The result is N copies of this new unit.
    generate_copies(N_Rec, NewUnit, PartitionedParts),
    incur_cost(ens_partition).

% Helper to generate copies based on recollection structure.
generate_copies(recollection(Tallies), Unit, Copies) :-
    generate_recursive(Tallies, Unit, [], Copies).
generate_recursive([], _Unit, Acc, Acc).
generate_recursive([t|Ts], Unit, Acc, Copies) :-
    generate_recursive(Ts, Unit, [Unit|Acc], Copies).
```

**2.2. Implementing the Partitive Fractional Scheme**

**Action:** Replace the contents of `jason.pl`. This implementation correctly handles input quantities as lists of units.

```prolog
% File: jason.pl (Refactored)
:- module(jason, [partitive_fractional_scheme/4]).
:- use_module(grounded_ens_operations, [ens_partition/3]).
:- use_module(normalization, [normalize/2]).
:- use_module(grounded_arithmetic, [incur_cost/1]).

% partitive_fractional_scheme(+M_Rec, +D_Rec, +InputQty, -ResultQty)
% Calculates M/D of InputQty.

partitive_fractional_scheme(M_Rec, D_Rec, InputQty, ResultQty) :-
    % --- 1. Partitioning Stage ---
    % Partition *each* unit in InputQty into D parts.
    pfs_partition_quantity(D_Rec, InputQty, PartitionedParts),
    incur_cost(pfs_partitioning_stage),

    % PartitionedParts is a list of lists.

    % --- 2. Disembedding and 3. Iteration Stage (Combined as Selection) ---
    % For each sublist, select M parts.
    pfs_select_parts(M_Rec, PartitionedParts, SelectedPartsFlat),
    incur_cost(pfs_selection_stage),

    % --- 4. Normalization Stage ---
    % Apply equivalence rules (Grouping and Composition).
    normalize(SelectedPartsFlat, ResultQty).


% pfs_partition_quantity(+D_Rec, +InputQty, -PartitionedParts)
pfs_partition_quantity(_D_Rec, [], []).
pfs_partition_quantity(D_Rec, [Unit|RestUnits], [Parts|RestParts]) :-
    ens_partition(Unit, D_Rec, Parts),
    pfs_partition_quantity(D_Rec, RestUnits, RestParts).

% pfs_select_parts(+M_Rec, +PartitionedParts, -SelectedPartsFlat)
pfs_select_parts(_M_Rec, [], []).
pfs_select_parts(M_Rec, [Parts|RestParts], SelectedPartsFlat) :-
    % Take the first M elements from the list 'Parts'.
    take_m(M_Rec, Parts, Selection),
    pfs_select_parts(M_Rec, RestParts, RestSelection),
    append(Selection, RestSelection, SelectedPartsFlat).

% take_m(+M_Rec, +List, -Selection)
% Grounded selection based on the recollection structure.
take_m(recollection([]), _List, []).
take_m(recollection([t|Ts]), [H|T], [H|RestSelection]) :-
    !,
    take_m(recollection(Ts), T, RestSelection).
take_m(recollection(_), [], []). % Handle case where List is shorter than M_Rec.
```