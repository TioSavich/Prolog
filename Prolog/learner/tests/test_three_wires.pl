/** <module> Test: Three Wires — Tension Instability Triggers Crisis

    Verifies that the tension surface can independently trigger crisis,
    even when the inference counter has budget remaining. This is the
    "third wire" connecting catastrophe geometry to the ORR cycle:

    Wire 1: Inference counter (check_viability) — already wired
    Wire 2: Tension accumulation (accumulate_tension) — already wired
    Wire 3: Tension instability triggers crisis (check_crisis) — THIS

    The test pumps 30 compressive tension steps with ACCELERATING cost
    to drive stability negative, then checks that check_crisis/0 detects
    the instability and throws perturbation(tension_instability).

    Why accelerating cost? With constant cost, damping makes tension
    increase sub-linearly (decelerating), which reads as stable. In a
    real compressive spiral, each inference step costs MORE than the last
    because the system is straining harder — this is the catastrophe
    geometry's core idea. Accelerating cost makes the second derivative
    of tension positive, which the stability check reads as negative
    stability (unstable zone).
*/

:- use_module('../tension_dynamics').
:- use_module('../event_log').
:- use_module('../oracle_server').
:- use_module(math(smr_div_idp)).
:- use_module('../more_machine_learner').
:- use_module('../fsm_synthesis_engine').
:- use_module('../execution_handler').

:- initialization(run_tests).

run_tests :-
    writeln(''),
    writeln('========================================================'),
    writeln('  Test: Three Wires — Tension + Strategy + Elaboration'),
    writeln('========================================================'),
    writeln(''),
    test_tension_instability_triggers_crisis(R1),
    test_check_crisis_throws(R2),
    test_strategy_selection_by_size(R3),
    test_idp_requires_multiplication(R4),
    test_idp_after_learning_multiplication(R5),
    % Test 6 (full elaboration chain) removed 2026-04-16 along with its
    % dependency on the archived synthesis engine. The chain's symbolic
    % core — IDP requiring learned multiplication — is covered by
    % tests 4 and 5. Integration via ORR cycle would re-introduce the
    % infinite crisis-loop bug documented in prior versions of this file.
    writeln(''),
    writeln('========================================================'),
    summarize([R1, R2, R3, R4, R5]),
    writeln('========================================================'),
    (   R1 == pass, R2 == pass, R3 == pass, R4 == pass, R5 == pass
    ->  halt(0)
    ;   halt(1)
    ).

%% ─────────────────────────────────────────────────────────────────
%% Test 1: Pumping 30 compressive steps drives stability negative
%% ─────────────────────────────────────────────────────────────────

test_tension_instability_triggers_crisis(Result) :-
    writeln('Test 1: Accelerating compressive steps drive stability negative'),
    tension_dynamics:reset_tension,
    event_log:reset_events,
    pump_accelerating_compressive(30),
    tension_dynamics:check_stability(Stab),
    format('  Stability after 30 accelerating compressive steps: ~4f~n', [Stab]),
    (   Stab < 0
    ->  writeln('  PASS — stability is negative (unstable zone)'),
        Result = pass
    ;   writeln('  FAIL — stability should be negative after accelerating compression'),
        Result = fail
    ).

%% ─────────────────────────────────────────────────────────────────
%% Test 2: check_crisis/0 throws perturbation(tension_instability)
%% ─────────────────────────────────────────────────────────────────

test_check_crisis_throws(Result) :-
    writeln(''),
    writeln('Test 2: check_crisis/0 throws perturbation(tension_instability)'),
    tension_dynamics:reset_tension,
    event_log:reset_events,
    pump_accelerating_compressive(30),
    (   catch(
            ( tension_dynamics:check_crisis, fail ),
            perturbation(tension_instability),
            true
        )
    ->  writeln('  PASS — check_crisis threw perturbation(tension_instability)'),
        Result = pass
    ;   writeln('  FAIL — check_crisis did not throw the expected perturbation'),
        Result = fail
    ).

%% ─────────────────────────────────────────────────────────────────
%% Test 3: Strategy selection is sensitive to problem size
%% ─────────────────────────────────────────────────────────────────

test_strategy_selection_by_size :-
    test_strategy_selection_by_size(_).

test_strategy_selection_by_size(Result) :-
    writeln(''),
    writeln('Test 3: Strategy selection is sensitive to problem size'),
    oracle_server:strategy_appropriate_for(add, 8+5, Strategy1),
    oracle_server:strategy_appropriate_for(add, 38+55, Strategy2),
    (   Strategy1 \= Strategy2
    ->  format('  PASS: 8+5 gets ~w, 38+55 gets ~w~n', [Strategy1, Strategy2]),
        Result = pass
    ;   format('  FAIL: both get ~w — selection not size-sensitive~n', [Strategy1]),
        Result = fail
    ).

%% ─────────────────────────────────────────────────────────────────
%% Test 4: IDP fails when multiplication hasn't been learned
%% ─────────────────────────────────────────────────────────────────

test_idp_requires_multiplication(Result) :-
    writeln(''),
    writeln('Test 4: IDP fails without learned multiplication'),
    retractall(more_machine_learner:run_learned_strategy(_,_,_,_,_)),
    ( catch(
        smr_div_idp:run_idp(12, 3, [], _Res, _History),
        _Error, fail
      )
    -> writeln('  FAIL: IDP succeeded without learned multiplication'),
       Result = fail
    ;  writeln('  PASS: IDP fails without learned multiplication'),
       Result = pass
    ).

%% ─────────────────────────────────────────────────────────────────
%% Test 5: IDP succeeds AFTER multiplication is learned
%% ─────────────────────────────────────────────────────────────────

test_idp_after_learning_multiplication(Result) :-
    writeln(''),
    writeln('Test 5: IDP succeeds after learning multiplication'),
    retractall(more_machine_learner:run_learned_strategy(_,_,_,_,_)),
    assertz((more_machine_learner:run_learned_strategy(A, B, Res, 'C2C', []) :-
        fsm_synthesis_engine:peano_to_int(A, IntA),
        fsm_synthesis_engine:peano_to_int(B, IntB),
        IntResult is IntA * IntB,
        fsm_synthesis_engine:int_to_peano(IntResult, Res)
    )),
    ( smr_div_idp:run_idp(12, 3, [], Quotient, _Remainder)
    -> ( Quotient =:= 4
       -> writeln('  PASS: IDP returns 4 using learned multiplication'),
          Result = pass
       ;  format('  FAIL: IDP returned ~w, expected 4~n', [Quotient]),
          Result = fail
       )
    ;  writeln('  FAIL: IDP failed even with learned multiplication'),
       Result = fail
    ),
    retractall(more_machine_learner:run_learned_strategy(_,_,_,_,_)).

%% ─────────────────────────────────────────────────────────────────
%% Helpers
%% ─────────────────────────────────────────────────────────────────

%% Pump with accelerating cost: step K costs K.
%% This simulates a compressive spiral where each inference is
%% harder than the last — the hallmark of approaching catastrophe.
pump_accelerating_compressive(Total) :-
    pump_accelerating_compressive(1, Total).

pump_accelerating_compressive(Step, Total) :-
    Step > Total, !.
pump_accelerating_compressive(Step, Total) :-
    tension_dynamics:accumulate_tension(Step, compressive),
    Step1 is Step + 1,
    pump_accelerating_compressive(Step1, Total).

summarize(Results) :-
    include(==(pass), Results, Passes),
    length(Passes, NPass),
    length(Results, NTotal),
    NFail is NTotal - NPass,
    format('  ~w/~w passed, ~w failed~n', [NPass, NTotal, NFail]).
