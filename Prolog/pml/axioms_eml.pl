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

eml_transition(s(u), s(comp_nec(a))).        % Emergence of awareness
eml_transition(s(u_prime), s(comp_nec(a))).  % Re-entry into the next cycle
eml_transition(s(a), s(exp_poss(lg))).       % Possibility of release
eml_transition(s(a), s(comp_poss(t))).       % Possibility of fixation
eml_transition(s(t), s(comp_nec(neg(u)))).   % Deepened contraction
eml_transition(s(lg), s(exp_nec(u_prime))).  % Sublation / release
eml_transition(s(t_b), s(comp_nec(t_n))).    % Bad infinite (Being -> Nothing)
eml_transition(s(t_n), s(comp_nec(t_b))).    % Bad infinite (Nothing -> Being)

% Commitment 2: Emergence of Awareness (Temporal Compression)
proves_impl([A] => [C], _) :-
    eml_transition(A, C),
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
    axiom_pack_enabled(eml),
    eml_transition(A, C),
    is_eml_modality(C).

is_eml_modality(s(comp_nec _)).
is_eml_modality(s(exp_nec _)).
is_eml_modality(s(exp_poss _)).
is_eml_modality(s(comp_poss _)).
