/** <module> FSM Synthesis Engine — Utility Shim
 *
 * The synthesis engine was archived in Phase 1 (2026-04-14 modularization).
 * See archive/fsm_synthesis_engine.pl for the original.
 *
 * This shim preserves the module interface so existing imports don't break.
 * Only peano_to_int/2 and int_to_peano/2 are live. The synthesis predicates
 * are stubs that log and fail — they were never called in practice (the
 * 5-argument path wrapped oracle calls rather than synthesizing from
 * primitives; see archive/SYNTHESIS_HONESTY.md).
 */
:- module(fsm_synthesis_engine,
          [ synthesize_strategy_from_oracle/4,
            synthesize_strategy_from_oracle/5,
            peano_to_int/2,
            int_to_peano/2
          ]).

% ===================================================================
% Live utilities: Peano ↔ Integer conversion
% ===================================================================

%!      peano_to_int(+Peano, -Int) is det.
peano_to_int(0, 0) :- !.
peano_to_int(s(N), Int) :-
    peano_to_int(N, SubInt),
    Int is SubInt + 1.

%!      int_to_peano(+Int, -Peano) is det.
int_to_peano(0, 0) :- !.
int_to_peano(N, s(Peano)) :-
    N > 0,
    N1 is N - 1,
    int_to_peano(N1, Peano).

% ===================================================================
% Stubs: synthesis predicates (archived, not live)
% ===================================================================

synthesize_strategy_from_oracle(_Goal, _FailedTrace, _TargetResult, _TargetInterpretation) :-
    writeln('[FSM Synthesis] Archived. See archive/fsm_synthesis_engine.pl'),
    fail.

synthesize_strategy_from_oracle(_Goal, _FailedTrace, _TargetResult, _TargetInterpretation, _StrategyName) :-
    writeln('[FSM Synthesis] Archived. See archive/fsm_synthesis_engine.pl'),
    fail.
