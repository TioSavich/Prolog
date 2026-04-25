% ===================================================================
% Embodied Modal Logic (EML) Axioms — Dialectical Rhythm
% ===================================================================
%
% These material inferences encode the dialectical rhythm of
% embodied reasoning: awareness (a), temptation (t), letting-go (lg),
% and the return to a transformed unawareness (u_prime).
%
% The compression/expansion polarity (comp_nec, exp_poss, etc.)
% tracks the felt quality of each transition — whether consciousness
% is narrowing (compressive necessity) or opening (expansive
% possibility).
%
% Interpretive correspondence: this material may be read alongside
% Carspecken's Scene Two (The Feeling-Body), where meaning is
% located in internal body awareness and the rhythm of desire
% and letting-go. The modal operators track something like what
% Carspecken calls the "I-feeling mode" prior to the
% subject-object split. The compression/expansion polarity
% maps onto the felt tempo of proprioceptive experience.
% ===================================================================

% --- EML Material Inferences ---

% Commitment 2: Emergence of Awareness (Temporal Compression)
proves_impl([s(u)] => [s(comp_nec a)], _) :-
    axiom_pack_enabled(eml).
proves_impl([s(u_prime)] => [s(comp_nec a)], _) :-
    axiom_pack_enabled(eml).

% Commitment 3: The Tension of Awareness (Choice Point)
proves_impl([s(a)] => [s(exp_poss lg)], _) :-  % Possibility of Release
    axiom_pack_enabled(eml).
proves_impl([s(a)] => [s(comp_poss t)], _) :-   % Possibility of Fixation
    axiom_pack_enabled(eml).

% Commitment 4: Dynamics of the Choice
% 4a: Fixation (Deepened Contraction)
proves_impl([s(t)] => [s(comp_nec neg(u))], _) :-
    axiom_pack_enabled(eml).
% 4b: Release (Sublation)
proves_impl([s(lg)] => [s(exp_nec u_prime)], _) :-
    axiom_pack_enabled(eml).

% Hegel's Triad Oscillation:
proves_impl([s(t_b)] => [s(comp_nec t_n)], _) :-
    axiom_pack_enabled(eml).
proves_impl([s(t_n)] => [s(comp_nec t_b)], _) :-
    axiom_pack_enabled(eml).

% --- EML Dynamics Structural Rule ---
proves_impl((Premises => Conclusions), History) :-
    axiom_pack_enabled(eml),
    select(s(P), Premises, RestPremises), \+ member(s(P), History),
    eml_axiom(s(P), s(M_Q)),
    ( (M_Q = comp_nec Q ; M_Q = exp_nec Q) -> proves_impl(([s(Q)|RestPremises] => Conclusions), [s(P)|History])
    ; ((M_Q = exp_poss _ ; M_Q = comp_poss _), (member(s(M_Q), Conclusions) ; member(M_Q, Conclusions)))
    ).

% --- EML Helpers ---
eml_axiom(A, C) :-
    clause(incompatibility_semantics:proves_impl(([A] => [C]), _), true),
    is_eml_modality(C).

is_eml_modality(s(comp_nec _)).
is_eml_modality(s(exp_nec _)).
is_eml_modality(s(exp_poss _)).
is_eml_modality(s(comp_poss _)).
