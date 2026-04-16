/** <module> Standards 3.CA.3-4 — Multiplication and division models
 *
 * Indiana: 3.CA.3 — "Model the concept of multiplication of whole
 *          numbers using equal-sized groups, arrays, area models,
 *          and equal intervals on a number line." (E)
 *          3.CA.4 — "Model the concept of division of whole numbers
 *          with partitioning, sharing, and an inverse of multiplication."
 * CCSS:    3.OA.A.1 (mult as equal groups), 3.OA.A.2 (div as partition)
 *
 * VPV MAPPING:
 *   V  (target vocabulary): "times", "groups of", "rows and columns",
 *      "divided into", "shared equally", "how many in each group"
 *   P  (practices): equal-group formation; array construction;
 *      repeated addition for multiplication; partitioning for
 *      division; inverse relationship (mult↔div)
 *   V' (metavocabulary): "how many groups of ___?", "how many in
 *      each group?", "what times what equals?", "___ divided by"
 *
 * CONNECTION TO EXISTING AUTOMATA:
 *   Multiplication models connect to:
 *   - smr_mult_c2c.pl — coordinating two counts (most primitive)
 *   - smr_mult_cbo.pl — count by ones (repeated addition)
 *   Division models connect to:
 *   - smr_div_dealing_by_ones.pl — dealing (partitioning model)
 *   - smr_div_cbo.pl — repeated subtraction
 *
 * BRANDOM CONNECTION: Multiplication introduces a genuinely new
 *   vocabulary that cannot be reduced to addition. "3 groups of 4"
 *   and "3 + 3 + 3 + 3" produce the same result (12) but express
 *   different inferential commitments. The multiplication vocabulary
 *   makes the GROUP STRUCTURE visible. Division makes the INVERSE
 *   STRUCTURE visible. These vocabularies are algorithmically
 *   elaborated from addition/subtraction but are not eliminable
 *   into them.
 *
 * CRISIS CONNECTION: Multiplication is where the system's first
 *   efficiency crisis in a new domain appears. Repeated addition
 *   of 7×8 costs 8 additions (O(n)). Skip counting costs O(n)
 *   steps. Derived facts (7×8 = 7×7+7 = 49+7 = 56) are O(1)
 *   if the component facts are known. This parallels the
 *   counting-on → place-value transition in addition.
 */

:- module(standard_3_ca_3_4, [
    multiply_equal_groups/3,   % +NumGroups, +GroupSize, -Product
    multiply_array/3,          % +Rows, +Cols, -Product
    multiply_repeated_add/3,   % +Factor, +Times, -Product
    divide_partition/3,        % +Total, +NumGroups, -GroupSize
    divide_repeated_sub/3,     % +Total, +Divisor, -Quotient
    mult_div_family/4          % +A, +B, -Product, -Facts
]).

:- use_module(formalization(grounded_arithmetic), [
    zero/1,
    successor/2,
    predecessor/2,
    equal_to/2,
    smaller_than/2,
    add_grounded/3,
    subtract_grounded/3,
    multiply_grounded/3,
    divide_grounded/3,
    integer_to_recollection/2,
    recollection_to_integer/2,
    incur_cost/1
]).

% ============================================================
% Multiplication models
% ============================================================

%!  multiply_equal_groups(+NumGroups, +GroupSize, -Product) is det.
%
%   Multiplication as equal-sized groups: NumGroups groups,
%   each with GroupSize items. Uses grounded multiplication
%   (which IS repeated addition internally).

multiply_equal_groups(NumGroups, GroupSize, Product) :-
    incur_cost(inference),
    multiply_grounded(GroupSize, NumGroups, Product).

%!  multiply_array(+Rows, +Cols, -Product) is det.
%
%   Multiplication as array: Rows × Cols. Structurally
%   identical to equal groups but conceptually different —
%   arrays make commutativity visible (rotate the array).

multiply_array(Rows, Cols, Product) :-
    incur_cost(inference),
    multiply_grounded(Rows, Cols, Product).

%!  multiply_repeated_add(+Factor, +Times, -Product) is det.
%
%   Multiplication as explicit repeated addition.
%   Product = Factor + Factor + ... (Times times).
%   This makes the connection to addition visible.

multiply_repeated_add(Factor, Times, Product) :-
    incur_cost(inference),
    zero(Zero),
    repeated_add_(Factor, Times, Zero, Product).

repeated_add_(_Factor, Times, Acc, Acc) :-
    zero(Zero), equal_to(Times, Zero), !.
repeated_add_(Factor, Times, Acc, Product) :-
    add_grounded(Acc, Factor, NewAcc),
    predecessor(Times, NewTimes),
    repeated_add_(Factor, NewTimes, NewAcc, Product).


% ============================================================
% Division models
% ============================================================

%!  divide_partition(+Total, +NumGroups, -GroupSize) is semidet.
%
%   Division as partitioning: divide Total into NumGroups
%   equal groups, find how many in each group.
%   This is the "sharing" model of division.

divide_partition(Total, NumGroups, GroupSize) :-
    incur_cost(inference),
    divide_grounded(Total, NumGroups, GroupSize).

%!  divide_repeated_sub(+Total, +Divisor, -Quotient) is semidet.
%
%   Division as repeated subtraction: how many times can
%   Divisor be subtracted from Total?
%   This is the "measurement" model of division.

divide_repeated_sub(Total, Divisor, Quotient) :-
    incur_cost(inference),
    zero(Zero),
    repeated_sub_(Total, Divisor, Zero, Quotient).

repeated_sub_(Remainder, Divisor, Acc, Acc) :-
    smaller_than(Remainder, Divisor), !.
repeated_sub_(Remainder, _Divisor, Acc, Acc) :-
    zero(Zero), equal_to(Remainder, Zero), !.
repeated_sub_(Remainder, Divisor, Acc, Quotient) :-
    subtract_grounded(Remainder, Divisor, NewRemainder),
    successor(Acc, NewAcc),
    repeated_sub_(NewRemainder, Divisor, NewAcc, Quotient).


% ============================================================
% Multiplication/division family (inverse relationship)
% ============================================================

%!  mult_div_family(+A, +B, -Product, -Facts) is det.
%
%   Given two factors, produce the complete family:
%   A×B=P, B×A=P, P÷A=B, P÷B=A

mult_div_family(A, B, Product, Facts) :-
    incur_cost(inference),
    multiply_grounded(A, B, Product),
    Facts = [
        mult(A, B, Product),
        mult(B, A, Product),
        div(Product, A, B),
        div(Product, B, A)
    ].
