% Axiom pack audit and hierarchy witnesses.
%
% Run from the repository root:
%   swipl -q -s Prolog/tools/axiom_pack_audit.pl -g run_audit -t halt

:- ensure_loaded('../paths').
:- ensure_loaded(arche_trace(load)).

:- use_module(library(lists), [subtract/3]).

:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, comp_poss).
:- op(500, fx, neg).
:- op(1050, xfy, =>).
:- op(550, xfy, rdiv).

:- dynamic audit_failure/2.

run_audit :-
    retractall(audit_failure(_, _)),
    nl,
    writeln('=== Axiom Pack Audit ==='),
    print_enabled_packs,
    forall(audit_obligation(Name, Description, Goal),
           run_obligation(Name, Description, Goal)),
    print_hierarchy_witnesses,
    findall(Name-Reason, audit_failure(Name, Reason), Failures),
    (   Failures = []
    ->  writeln(''),
        writeln('Axiom pack audit passed.')
    ;   writeln(''),
        writeln('Axiom pack audit failed:'),
        forall(member(Failure, Failures),
               format('  ~q~n', [Failure])),
        fail
    ).

print_enabled_packs :-
    findall(Pack, incompatibility_semantics:enabled_axiom_pack(Pack), Packs0),
    sort(Packs0, Packs),
    format('enabled_packs(~q).~n', [Packs]).

run_obligation(Name, Description, Goal) :-
    (   catch(call(Goal),
              Error,
              ( assertz(audit_failure(Name, error(Error))),
                fail ))
    ->  format('ok(~w).  % ~w~n', [Name, Description])
    ;   ( audit_failure(Name, _)
        -> true
        ;  assertz(audit_failure(Name, failed(Description)))
        ),
        format('fail(~w).  % ~w~n', [Name, Description])
    ).

safe(Sequent, Packs) :-
    incompatibility_semantics:safe_proves(Sequent,
                                          [time_limit(2), packs(Packs)]).

not_safe(Sequent, Packs) :-
    \+ safe(Sequent, Packs).

with_domain(Domain, Goal) :-
    (   incompatibility_semantics:current_domain(Saved)
    ->  true
    ;   Saved = n
    ),
    setup_call_cleanup(
        incompatibility_semantics:set_domain(Domain),
        call(Goal),
        incompatibility_semantics:set_domain(Saved)
    ).

throws_normative_crisis(Goal, Context) :-
    catch((incompatibility_semantics:check_norms(Goal), fail),
          normative_crisis(_, Context),
          true).

all_default_packs_enabled :-
    findall(Pack, incompatibility_semantics:enabled_axiom_pack(Pack), Packs0),
    sort(Packs0, Packs),
    Packs == [domains, eml, geometry, number_theory, robinson].

audit_obligation(default_packs_enabled,
                 'all five default axiom packs are enabled after load',
                 all_default_packs_enabled).

audit_obligation(geometry_strength_square_rectangle,
                 'geometry proves square as inferentially stronger than rectangle',
                 safe([n(square(x))] => [n(rectangle(x))], [geometry])).

audit_obligation(geometry_rejects_rectangle_square_converse,
                 'geometry rejects the invalid rectangle-to-square converse',
                 not_safe([n(rectangle(x))] => [n(square(x))], [geometry])).

audit_obligation(geometry_hard_no_incoherence,
                 'geometry detects a hard-no restriction as incoherent',
                 incompatibility_semantics:incoherent([n(square(x)), n(r6(x))])).

audit_obligation(robinson_arithmetic_grounding,
                 'Robinson arithmetic proves a grounded addition fact',
                 safe([] => [o(plus(3, 2, 5))], [robinson, domains])).

audit_obligation(robinson_rejects_wrong_sum,
                 'Robinson arithmetic rejects a false addition fact',
                 not_safe([] => [o(plus(3, 2, 6))], [robinson, domains])).

audit_obligation(robinson_q1_incoherence,
                 'Robinson Q1 marks successor-zero identity as incoherent',
                 incompatibility_semantics:incoherent([o(eq(succ(0), 0))])).

audit_obligation(domain_n_to_z_expansion,
                 'natural-number subtraction crisis is resolved by integer expansion',
                 ( with_domain(n, throws_normative_crisis(subtract(2, 3, _),
                                                          natural_numbers)),
                   with_domain(z, safe([] => [o(minus(2, 3, -1))],
                                       [robinson, domains]))
                 )).

audit_obligation(domain_n_to_q_expansion,
                 'natural-number division crisis is resolved by rational partitioning',
                 ( with_domain(n, throws_normative_crisis(divide(1, 2, _),
                                                          natural_numbers)),
                   with_domain(q, incompatibility_semantics:check_norms(divide(1, 2, _))),
                   safe([] => [o(partition(1, 2, 1 rdiv 2))],
                        [robinson, domains])
                 )).

audit_obligation(number_theory_prime_grounding,
                 'number theory proves primality for Euclid witness 31',
                 safe([] => [n(prime(31))], [number_theory])).

audit_obligation(number_theory_completeness_self_defeat,
                 'finite prime-list completeness derives its own negation',
                 safe([n(is_complete([2, 3, 5]))] =>
                      [n(neg(is_complete([2, 3, 5])))],
                      [number_theory])).

audit_obligation(eml_direct_modal_commitment,
                 'EML proves the direct letting-go modal commitment',
                 safe([s(lg)] => [s(exp_nec(u_prime))], [eml])).

audit_obligation(eml_necessity_cashout,
                 'EML necessity cashes out into a material state transition',
                 safe([s(lg)] => [s(u_prime)], [eml])).

audit_obligation(pack_isolation,
                 'a geometry-only horizon does not prove arithmetic facts',
                 not_safe([] => [o(plus(1, 2, 3))], [geometry])).

shape(square).
shape(rectangle).
shape(rhombus).
shape(parallelogram).
shape(kite).
shape(trapezoid).
shape(quadrilateral).

restriction_set(Shape, Restrictions) :-
    findall(R, incompatibility_semantics:incompatible_pair(Shape, R), Raw),
    sort(Raw, Restrictions).

strength_value(Shape, Strength) :-
    restriction_set(Shape, Restrictions),
    length(Restrictions, Strength).

proper_entails(Strong, Weak) :-
    Strong \== Weak,
    incompatibility_semantics:entails_via_incompatibility(Strong, Weak).

shape_term(Shape, X, Term) :-
    Term =.. [Shape, X].

proved_geometry_edge(Strong, Weak) :-
    shape_term(Strong, x, StrongTerm),
    shape_term(Weak, x, WeakTerm),
    safe([n(StrongTerm)] => [n(WeakTerm)], [geometry]).

geometry_cover_edge(Strong, Weak, ExtraRejections) :-
    shape(Strong),
    shape(Weak),
    proper_entails(Strong, Weak),
    proved_geometry_edge(Strong, Weak),
    \+ ( shape(Middle),
         Middle \== Strong,
         Middle \== Weak,
         proper_entails(Strong, Middle),
         proper_entails(Middle, Weak)
       ),
    restriction_set(Strong, StrongRestrictions),
    restriction_set(Weak, WeakRestrictions),
    subtract(StrongRestrictions, WeakRestrictions, ExtraRejections).

print_hierarchy_witnesses :-
    writeln(''),
    writeln('=== Hierarchy Witnesses ==='),
    print_geometry_hierarchy,
    print_domain_hierarchy,
    print_eml_hierarchy,
    print_number_theory_hierarchy.

print_geometry_hierarchy :-
    writeln('geometry_incompatibility_strength:'),
    forall(shape(Shape),
           ( strength_value(Shape, Strength),
             restriction_set(Shape, Restrictions),
             format('  strength(~w, ~w, rejects(~q)).~n',
                    [Shape, Strength, Restrictions])
           )),
    findall(edge(Strong, Weak, extra_rejections(Extra)),
            geometry_cover_edge(Strong, Weak, Extra),
            Edges),
    sort(Edges, Sorted),
    forall(member(Edge, Sorted),
           format('  cover(~q).~n', [Edge])).

print_domain_hierarchy :-
    writeln('domain_expansion_witnesses:'),
    format('  n_to_z(subtract(2,3,-1), blocked_in(n), proved_in(z)).~n', []),
    format('  n_to_q(divide(1,2,_), blocked_in(n), partitioned_in(q, 1 rdiv 2)).~n', []).

print_eml_hierarchy :-
    writeln('eml_necessity_witnesses:'),
    format('  necessity_cashout(s(u), s(comp_nec(a)), s(a)).~n', []),
    format('  necessity_cashout(s(lg), s(exp_nec(u_prime)), s(u_prime)).~n', []).

print_number_theory_hierarchy :-
    writeln('number_theory_incoherence_witnesses:'),
    format('  completeness_assumption([2,3,5]) entails neg(completeness_assumption([2,3,5])).~n', []).
