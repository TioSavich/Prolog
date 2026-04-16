---
module: learner
sources:
  - path: learner/axioms_domains.pl
    sha256: f095ee836a2f7ae1c87ea5f91ac42ea55f6db14c411b5d980deeb8fb41d9f484
  - path: learner/config.pl
    sha256: 6e972692b2a9c6f3001a186f809d896efe1db1d65014d54b54a4b39ff05da3e3
  - path: learner/crisis_processor.pl
    sha256: e471dcef9bc4b53f124e92c2008e7696712e8ff0cdfbaa4527abca6c99d51396
  - path: learner/curriculum_processor.pl
    sha256: 9b8dfc11868774506a59eff30549c7fabb3843d48b87d36a08ee72f98f415a9e
  - path: learner/event_log.pl
    sha256: c07ff07874c9f06f9469574a4b3b3140d4d684e09168ff16d1d7ab96e66e2d4a
  - path: learner/execution_handler.pl
    sha256: 73d1c98272782c2a70d82382aa4d7cd6a9fe6433296f0d95238d619db62137e5
  - path: learner/fsm_synthesis_engine.pl
    sha256: d20291b8bf83a97cfc7c76188eb6044dae288d8d04dbe542ae4eadb752af3561
  - path: learner/interactive_ui.pl
    sha256: 578832f3bac7fe720e5f93c161533a45642654b27b72b858a78ad11bceec3fdb
  - path: learner/knowledge_manager.pl
    sha256: cf46da1e945fd1f8ae6a238dcbd69b343d77b838c8655584a6463533e26385ed
  - path: learner/learned_knowledge_v2.pl
    sha256: 75dbabd223380596f221820eba73da867d71e5e38e8ea1d9a604cbb457345c87
  - path: learner/main.pl
    sha256: 4fa68197c0bd36cef6d72c2a0ec6fa46468719f7527b384b30baed7d23b6d872
  - path: learner/meta_interpreter.pl
    sha256: 309f69c22fe20a1e0265fcc05951ee19f4f89195828e599ce6c0f9443fbef2e3
  - path: learner/more_machine_learner.pl
    sha256: e82b9e3bf2e7c429185e2b1deeeaa160e7127a7c1ef4ecb2a258cef975d6fc1c
  - path: learner/object_level.pl
    sha256: 51cbb5eecaa0f973c7c056b1f9fd9eb24ad588a4b4e53b4eac0416facf5f46c7
  - path: learner/oracle_server.pl
    sha256: 60e9893cc5b5721cd4b8c854c8edc16019a3c9f940be2cef307ac59bd68a7319
  - path: learner/primordial_start.pl
    sha256: 6f08d02876a0441394d93041ba4f98b0ee23d5c751e985008014723c05e82c49
  - path: learner/reflective_monitor.pl
    sha256: f1197b7f6dd61ee5d572430ffc378e34205b2a3bcb851385648a27e8ee9a9b3c
  - path: learner/reorganization_engine.pl
    sha256: 47f6be3137be438493b564d60bcbbcb9e36ab55064933093592de238ec97f292
  - path: learner/reorganization_log.pl
    sha256: a07fd8e52cf1946158bf7b57fdc0b4dafd602ce1b00a8dbb1420ad967244114c
  - path: learner/server.pl
    sha256: 442d04bd7f86973a51d5db82aa4c011c1476620cb048ab6456e7e0b29ebc555d
  - path: learner/tension_dynamics.pl
    sha256: 6ef946affedf23f770716dd070920d94bb5f9ccbdd42009d3265e420f6b6d5dd
  - path: learner/tests/test_basic_functionality.pl
    sha256: 0376fbc831cc8638d0ccb116201a3ac5fd6aa620d68843bc67aeb12299f109ef
  - path: learner/tests/test_complete_system.pl
    sha256: 9271ad5c64a4f5b945b5376a4421b3e4fa9b306b3e3924342c0a780eccb34663
  - path: learner/tests/test_comprehensive.pl
    sha256: 6d27e52b1759235217bf8832f7dc1cab9eade8c2fddc013dfb919437c6dc8ade
  - path: learner/tests/test_force_learn_all.pl
    sha256: 0075ab530f52d463b509444cb219777c44530574396f81d2433872fca747e0bd
  - path: learner/tests/test_full_curriculum.pl
    sha256: bae7fab5d1a3cbd4d624a83bbc99c80574be6f18380fed8cb5537f7017a760b7
  - path: learner/tests/test_full_loop.pl
    sha256: 4b4b6eef8414c23446ebcf71f68b637dfe7bbbb7382a995889ac36189402b75c
  - path: learner/tests/test_native.pl
    sha256: 79dba4c6ad13b80e040a1d58067d9ff2fed07f8f0111d0693b46dd43f0b7d92e
  - path: learner/tests/test_oracle_integration.pl
    sha256: 914352f2173e16ed9e0b348962a6357d28c2a7ee3f1ac7dd685a9a0cade35a82
  - path: learner/tests/test_orr_cycle.pl
    sha256: fa861dfb8e3d8fe940976c5b51b353d423a6d3035e7a7ed60fc23e878176af68
  - path: learner/tests/test_three_wires.pl
    sha256: 0a87215f1e1935d96566bfa7b845fa66f044f44edb68e49ead58953083b02a6a
exports:
  # config.pl
  - max_inferences/1
  - max_retries/1
  - cognitive_cost/2
  - calculate_recollection_cost/2
  - server_mode/1
  - server_endpoint_enabled/1
  # meta_interpreter.pl (module exports solve/4; solve/6 is called internally)
  - solve/4
  # tension_dynamics.pl
  - reset_tension/0
  - accumulate_tension/2
  - get_tension_state/1
  - check_stability/1
  - check_crisis/0
  - relax_tension/0
  - get_tension_history/1
  # execution_handler.pl
  - run_computation/2
  # oracle_server.pl
  - query_oracle/4
  - list_available_strategies/2
  - strategy_appropriate_for/3
  - estimate_strategy_cost/5
  # more_machine_learner.pl
  - run_learned_strategy/5
  - solve/4
  - save_knowledge/0
  # knowledge_manager.pl
  - reset_learned_knowledge/0
  - backup_learned_knowledge/1
  - restore_learned_knowledge/1
  - inspect_learned_knowledge/0
  - count_learned_strategies/1
  # reflective_monitor.pl
  - reflect/2
  - get_stress_map/1
  - reset_stress_map/0
  # reorganization_engine.pl
  - accommodate/1
  - handle_normative_crisis/2
  - handle_incoherence/1
  - reorganize_system/2
  # crisis_processor.pl
  - process_crisis_curriculum/1
  - run_crisis_demo/0
  # curriculum_processor.pl
  - process_curriculum/1
  - process_curriculum_file/1
  - run_progressive_learning/0
  - process_task/1
  # event_log.pl
  - emit/2
  - reset_events/0
  - get_events/1
  - events_to_json/1
  # reorganization_log.pl
  - log_event/1
  - get_log/1
  - clear_log/0
  - generate_report/1
  # fsm_synthesis_engine.pl (shim only)
  - synthesize_strategy_from_oracle/4
  - synthesize_strategy_from_oracle/5
  - peano_to_int/2
  - int_to_peano/2
  # object_level.pl
  - add/3
  # interactive_ui.pl
  - start/0
  # server.pl — HTTP endpoints (not Prolog exports, but the public surface)
  # POST /api/compute, GET /api/strategies, GET /api/knowledge,
  # GET /api/tension, POST /api/reset, GET /bridge, GET /fractal,
  # GET /landing, GET /assets/* (prefix), GET / (prefix frontend)
operators:
  # more_machine_learner.pl
  - =>/xfy/1050
  - neg/fx/500
  - rdiv/xfy/550
research_goal: >
  The ORR cycle (Observe, React, Reorganize) is implemented as `solve/6` +
  `execution_handler:run_computation/2` + `oracle_server:query_oracle/4` +
  `reorganization_engine`. Crisis classification is mechanical (resource
  exhaustion, tension instability, unknown operation, normative crisis,
  incoherence); the learning trajectory is not empirically validated against
  student behavior. Genuine FSM synthesis is archived; the "synthesize from
  oracle" path reaches a shim that fails.
---

# Fact-sheet: learner

Written from [Stripped_Code/learner/](../../Stripped_Code/learner/) without reading any aspirational markdown. Cross-checked against Round 2 after drafting. Voice: SCENE register — hypothetical correspondences, not identities; productive failure is the point of contact.

## What the module defines

The learner is a resource-bounded meta-interpreter wrapped in a crisis-recovery loop, with a persistent cache of learned strategies and an HTTP dashboard over both.

**Configuration (`config.pl`).** `max_inferences(20)`, `max_retries(5)`, and a `cognitive_cost/2` table: `inference=1`, `unit_count=5`, `slide_step=2`, `fact_retrieval=1`, `modal_shift=3`, `recollection_step=1`, `norm_check=2`. `calculate_recollection_cost/2` scales cost linearly by tally length. `server_mode/1` and `server_endpoint_enabled/1` gate which HTTP endpoints are live per mode (`production`, `development`, `testing`, `simple`).

**Meta-interpreter (`meta_interpreter.pl`).** Module exports `solve/4` but internally dispatches `solve/6` with explicit modal context threading (`Goal, CtxIn, CtxOut, I_In, I_Out, Trace`). Clauses cover: `true`, `incur_cost(Action)`, `s(ModalGoal)` (modal shift into compressive/expansive context), conjunction `(A,B)`, built-in calls, learned-strategy dispatch via `more_machine_learner:run_learned_strategy/5`, norm-checked object-level clauses, unknown-operation detection for `subtract/multiply/divide` (throws `perturbation(unknown_operation(Op, Goal))`), and a terminal `fail(Goal)` trace. `check_viability/2` throws `perturbation(resource_exhaustion)` when cost exceeds remaining budget. Context multipliers: `compressive=2`, `expansive=1`, `neutral=1` (inference-cost table, not tension multipliers). `is_modal_operator/2` maps `comp_nec/comp_poss` to `compressive` and `exp_nec/exp_poss` to `expansive`. `strategy_runtime_cost/4` gives per-strategy cost estimates (COBO, RMB, Chunking, Rounding, default).

**Tension dynamics (`tension_dynamics.pl`).** Separate accumulator running alongside the inference counter. `accumulate_tension/2` applies `natural_damping(0.02)` to the running level, then adds `BaseCost * context_tension_multiplier(Ctx)` where multipliers are `compressive=2.5`, `neutral=1.0`, `expansive=0.3`. Keeps a rolling `tension_history` of window `history_window(20)` and a `tension_full_log` of every update. `check_stability/1` computes a second-difference measure over three windows of three when history ≥ 9 (degraded forms for ≥ 6 or less). `check_crisis/0` throws `perturbation(tension_instability)` when history ≥ 9 and stability is negative. `relax_tension/0` multiplies tension by an escalating rate `relaxation_rate(0.4) + (1 - 0.4) * (1 - 1 / (1 + N/3))` where N is `crisis_count`. Events emitted every 3 steps via `event_log:emit/2`.

**Execution handler (`execution_handler.pl`).** `run_computation/2` wraps meta-interpreter invocation in a `catch/3`, decrements `max_retries` on each perturbation, and hands off to `handle_perturbation/5`. Dispatch is per-perturbation:
- `tension_instability` → classified as `tension_instability`, falls through to resource-exhaustion path.
- `resource_exhaustion` → classified as `efficiency_crisis`. Consults oracle (`consult_oracle_for_solution/4`), attempts `synthesize_from_oracle/1` (shim-failing), validates via `validate_synthesis/3` against oracle's expected integer result, and on success doubles `Limit` and retries. On any failure, fails.
- `normative_crisis(Goal, Context)` → classified as `normative_crisis`, delegates to `reorganization_engine:handle_normative_crisis/2`, retries at same limit.
- `unknown_operation(Op, Goal)` → classified as `unknown_operation`, picks the first available strategy for `Op` from `oracle_server:list_available_strategies/2`, runs oracle + synthesis + validation, retries.
- `incoherence(Commitments)` → classified as `incoherence_crisis`, delegates to `reorganization_engine:handle_incoherence/1`.
Each classification carries a `crisis_meta` dict with `response` and `skeleton_signal` — narrative strings addressed to an "LLM oracle," not structured pedagogy.

**Oracle server (`oracle_server.pl`).** `query_oracle/4` dispatches to 24 `execute_strategy/6` clauses (plus a terminal `not_implemented` throw) spanning addition (Counting On, COBO, Chunking, RMB, Rounding), subtraction (Counting Back, COBO-MA, CBBO-TA, Decomposition, Rounding, Sliding, Chunking A/B/C), multiplication (C2C, CBO, Commutative Reasoning, DR), division (CBO, Dealing by Ones, IDP, UCR), and fractions (PFS, FCS via `jason_fsm`). `list_available_strategies/2` enumerates the catalog per operation. `strategy_appropriate_for/3` picks the lowest-cost strategy by `estimate_strategy_cost/5` (closed-form estimates for COBO, RMB, Chunking, Rounding, default 50). `extract_interpretation/7` formats a natural-language gloss per strategy — addressed to the learner, not the teacher.

**Machine learner cache (`more_machine_learner.pl`).** `run_learned_strategy/5` is a dynamic predicate. `load_knowledge/0` consults `learned_knowledge.pl` at initialization; `save_knowledge/0` pretty-prints all stored clauses back out. `solve/4` either runs a learned strategy or falls back to `solve_foundationally/4`, which is a counting-by-successor loop gated on both operands being recollections. Declares operators `=>` (1050, xfy), `neg` (500, fx), `rdiv` (550, xfy).

**Knowledge manager (`knowledge_manager.pl`).** Admin surface: reset, backup, restore, inspect, count. Operates on `learned_knowledge.pl` and the dynamic `run_learned_strategy/5` store. `reset_learned_knowledge/0` retracts everything and reconsults `object_level.pl`.

**Reflective monitor (`reflective_monitor.pl`).** `reflect/2` walks a trace, extracts commitments and failures, updates a stress map (`stress/2` counts per predicate signature), and returns a trigger: `goal_failure(Failures)` or `incoherence(Commitments)` via `incompatibility_semantics:incoherent/1`. `get_stress_map/1` and `reset_stress_map/0` expose the counters.

**Reorganization engine (`reorganization_engine.pl`).** `reorganize_system/2` peano-converts operands, calls `more_machine_learner:discover_strategy/3` (note: not exported by `more_machine_learner.pl` in the stripped code — a dangling reference), and persists via `save_knowledge/0`. `handle_normative_crisis/2` drives `propose_context_shift/3` (n→z for negative subtraction, z→q for division), calls `set_domain/1`, and introduces new clauses — `subtract(M,S,debt(R))` for integers, `divide(D,V,fraction(D,V))` for rationals. `handle_incoherence/1` scores commitments via `reflective_monitor:conceptual_stress/2` and retracts the highest-stress one. `specialize_add_rule/0` retracts generic `add/3` and asserts a `recursive_add`-based specialization.

**Crisis processor (`crisis_processor.pl`).** Curriculum-driven demo runner. `run_crisis_demo/0` exercises `count(5), add(3,2), count(100), multiply(15,8)` to show the inference-limit threshold empirically. `monitor_task_execution/1` catches `perturbation(resource_exhaustion)`, records `inference_crisis/3`, and falls back to chunking or C2C. `process_crisis_curriculum/1` reads a task file line-by-line.

**Object level (`object_level.pl`).** The primordial knowledge: `add(A,B,Sum)` enumerates both operands (walking the Peano structure) and runs `recursive_add/3`. Three clauses total. `add/3` is dynamic so the reorganization engine can rewrite it.

**Domain axioms (`axioms_domains.pl`).** Not a module — loaded by `arche-trace/incompatibility_semantics.pl` as an include. Defines `current_domain/1` (default `n`), domain switching `set_domain/1`, `prohibition/2` (subtraction in N when minuend < subtrahend; division in N when dividend < divisor and divisor ≠ 0), `check_norms/1` (throws `normative_crisis/2`), and two `proves_impl/2` rules for `iterate/3` and `partition/3` that do rational arithmetic on tally-backed recollections.

**Server (`server.pl`).** Serves port 8080 via `http_server`. Endpoints: `POST /api/compute` (runs `run_computation/2` with CORS, collects events, tension state + history, and learned-strategy summary into a JSON reply), `GET /api/strategies?operation=...`, `GET /api/knowledge`, `GET /api/tension`, `POST /api/reset`, `GET /bridge`, `GET /fractal`, `GET /landing` (all three serve more-zeeman HTML with an injected `<base href="/assets/">`), `GET /assets/*` (prefix, serves more-zeeman static files), `GET /` (serves inline frontend). `build_goal/4` converts integers to peano on the way in; `event_to_dict/2` recursively sanitizes for JSON.

**Event log (`event_log.pl`).** Dynamic `stored_event/2` keyed by wall-clock time. `emit/2` stamps and stores; `get_events/1` retrieves; `events_to_json/1` formats manually (no `http_json` dep at this layer).

**Reorganization log (`reorganization_log.pl`).** Separate from event log. Narrative DCG that renders `orr_cycle_start`, `disequilibrium`, `reorganization_start`, `retracted`, `asserted`, `reorganization_success`, `reorganization_failure`, `equilibrium` as English sentences.

**FSM synthesis engine (`fsm_synthesis_engine.pl`).** Shim only. `synthesize_strategy_from_oracle/4` and `/5` print "[FSM Synthesis] Archived" and fail. `peano_to_int/2` and `int_to_peano/2` survive as utility predicates.

**Curriculum processor (`curriculum_processor.pl`).** Alternate task runner: builds tally representations, threads results through `grounded_arithmetic` and `ens_partition`, asserts learned facts.

**Interactive UI (`interactive_ui.pl`).** A menu-driven REPL (`start/0`) with five options: learn addition strategy, critique normative rule, show learned, save, exit.

**Tests (10 files in `tests/`).** `test_basic_functionality`, `test_complete_system`, `test_comprehensive`, `test_force_learn_all`, `test_full_curriculum`, `test_full_loop`, `test_native`, `test_oracle_integration`, `test_orr_cycle`, `test_three_wires`. Range in size from 442 bytes (`test_native`) to 15.5 KB (`test_complete_system`).

## What axioms / inferences are asserted

The learner's mechanical claims, not its aspirational ones.

**Resource-bounded interpretation.** Every `solve/6` clause decrements an inference budget by a context-multiplied cost. Budget below cost throws `perturbation(resource_exhaustion)`. Default budget = 20. Context multiplier for `compressive` is 2 (inference side) / 2.5 (tension side); `expansive` is 1 / 0.3; `neutral` is 1 / 1.0. The two counters — inference budget and tension level — are distinct quantities.

**Tension as second-order signal.** Tension accumulates with damping (0.02 per step) and can cross into instability well before the inference budget is exhausted. `check_stability/1` uses a second-difference of three-element windows; `check_crisis/0` requires ≥ 9 history entries before it will throw. This is a geometric signal layered on an arithmetic one: the `skeleton_signal` string for tension_instability reads "the catastrophe geometry triggered crisis before the inference counter ran out."

**Crisis taxonomy.** Five perturbation shapes, five handlers, five `classify_crisis/3` clauses:
- `resource_exhaustion` → `efficiency_crisis` → oracle + synthesize + validate + doubled budget.
- `tension_instability` → `tension_instability` → falls through to resource path.
- `unknown_operation(Op, Goal)` → `unknown_operation` → oracle for `Op`'s first listed strategy.
- `normative_crisis(Goal, Context)` → `normative_crisis` → domain expansion (n→z→q) with new vocabulary (`debt/1`, `fraction/2`).
- `incoherence(Commitments)` → `incoherence_crisis` → retract the most-stressed commitment.

**Oracle is a dispatcher, not a reasoner.** `query_oracle/4` pattern-matches on operation + strategy name and calls the corresponding `math/` automaton with `run_*/4` or `run_*/5`. It interprets via `format/3` into natural-language glosses. The oracle does not reason about *why* a strategy fits; `strategy_appropriate_for/3` uses closed-form cost heuristics (e.g. `min(A,B) + 2` for Counting On, `Bases + Ones + 4` for COBO).

**Peano object-level arithmetic.** `object_level:add(A,B,Sum)` enumerates both operands (walks every successor) before running `recursive_add/3`. This is "Counting All," intentionally slow: it's the cost surface against which every other strategy is measured. `multiply` and `divide` are *not* defined at the object level — the meta-interpreter throws `unknown_operation` for them, which is how the learner is pushed into the oracle.

**Peano–integer bridge.** Five files (`meta_interpreter`, `execution_handler`, `crisis_processor`, `server`, `fsm_synthesis_engine`) each define their own copy of `peano_to_int/2` and/or `int_to_peano/2`. Each copy is identical in shape.

**Persistence via `assertz`.** Learned strategies are `run_learned_strategy/5` clauses stored in the dynamic database and serialized to `learned_knowledge.pl` via `portray_clause/2`. On restart, `load_knowledge/0` consults that file. No schema, no versioning.

**Normative prohibitions in N.** `axioms_domains.pl` prohibits subtraction when minuend < subtrahend and division when dividend < divisor (and divisor ≠ 0), *in domain `n` only*. Set `current_domain(z)` and the prohibitions lift.

**Discovery is not synthesis.** `more_machine_learner:discover_strategy/3` is referenced by `reorganization_engine:reorganize_system/2` but is not defined in the stripped `more_machine_learner.pl`. The call would fail. The path through which learning actually happens is: oracle returns a strategy name and result, `synthesize_from_oracle/1` invokes the shim (which fails), validation runs the same goal through `solve/6` again, and the "learned strategy" is whatever clause the synthesis shim would have asserted — i.e. nothing. The dynamic `run_learned_strategy/5` store is populated only by `learned_knowledge.pl` being consulted at boot, which is itself only written by `save_knowledge/0` after a successful `discover_strategy/3` — a loop that does not close in the stripped code.

**HTTP surface.** Ten handlers. Five JSON endpoints, three HTML bridges to `more-zeeman/`, one asset prefix, one catch-all frontend. CORS is wildcard. Port 8080. Fine for single-user local dev; `tension_dynamics.pl`, `event_log.pl`, `more_machine_learner.pl`, and `reflective_monitor.pl` all mutate global state via `retractall`/`assertz`/`assert` without locks, which will race under concurrent requests (noted in project-level CLAUDE.md; not documented in the code).

## What the module does NOT do

The negative space.

- **No empirical validation against student trajectories.** The cost constants (2.5 / 1.0 / 0.3, damping 0.02, relaxation 0.4, stability threshold 9) are stipulated, not fit to data. `crisis_processor.pl`'s demo tasks (`count(5)`, `multiply(15,8)`) are chosen to trip the 20-inference limit, not to mirror a real learning sequence.
- **No genuine FSM synthesis.** `fsm_synthesis_engine.pl` is a shim that fails. The "synthesize from oracle" path always reaches the shim and fails there. What looks like synthesis is oracle-wrapping: the strategy name comes from a catalog, the execution comes from the `strategies/math/` automaton, and the `run_learned_strategy/5` clause (if written) encodes that pairing, not a derivation.
- **No cross-session state guarantees.** `learned_knowledge.pl` is a flat file that `consult/1` reads on startup. No schema validation, no migration, no locking. `learned_knowledge_v2.pl` exists as a stripped file but holds no content that would imply versioning.
- **No concurrency safety.** `retractall`/`assertz` in `tension_dynamics.pl`, `event_log.pl`, `reflective_monitor.pl`, `object_level.pl` (dynamic `add/3`), `more_machine_learner.pl` (dynamic `run_learned_strategy/5`), `knowledge_manager.pl`, and the crisis processor all mutate global state without locks. `config.pl` declares dynamic predicates but only asserts defaults at load time.
- **No pedagogical reasoning in the oracle.** `strategy_appropriate_for/3` picks by a numerical cost heuristic; `extract_interpretation/7` is a `format/3` template. The oracle does not model a teacher's sense of developmental readiness. The `skeleton_signal` strings in `classify_crisis/3` explicitly defer that judgment to "an LLM oracle" that is not present in the code.
- **No belief revision over strategies.** `handle_incoherence/1` retracts the most-stressed commitment at the object level (predicate clauses). It does not revise learned strategies or the oracle's catalog. `validate_synthesis/3` will retract a faulty specialization, but that's a single-shot filter, not belief revision.
- **No mutual recognition between oracle and learner.** Information flows one way: oracle → learner. The learner does not change the oracle, does not score the oracle's strategies against its own struggle, does not reject an oracle recommendation based on anything other than validation failure. The "I-Thou" register in design docs is not present in the mechanics.
- **No fraction integration with the crisis loop.** `execute_strategy/6` has PFS/FCS clauses, but fractions are not integrated with `object_level:add/3` or the ORR retry path. The fraction crisis (`fraction/2` structurally incompatible with `recollection/1`) is not triggered by the learner's own activity; it has to be routed through the oracle explicitly.
- **No per-module test-independence guarantee for learner.** Tests under `learner/tests/` require other modules (`math/`, `strategies/`, `formalization/`, `arche_trace/`) to be loadable. The learner is the most tightly coupled module in the repo.
- **No meta-reflection on learning.** `reflective_monitor` counts failures by predicate signature. It does not notice that the learner has reached for the same strategy three times, or that a crisis cluster suggests a representational mismatch. Reflection is mechanical failure-counting.
- **No model of teacher intervention timing.** The crisis_meta skeleton_signal strings ask "is this the right moment to introduce a shortcut, or should the learner struggle longer?" — but the code has no mechanism for deferring oracle consultation. Every crisis triggers oracle consultation immediately.

## Research goal

The ORR cycle (Observe, React, Reorganize) is implemented as `solve/6` + `execution_handler:run_computation/2` + `oracle_server:query_oracle/4` + `reorganization_engine`. Crisis classification is mechanical (resource exhaustion, tension instability, unknown operation, normative crisis, incoherence); the learning trajectory is not empirically validated against student behavior. Genuine FSM synthesis is archived; the "synthesize from oracle" path reaches a shim that fails.

## Cross-check against Round 2

Triangulated against [Round 2 section map](_round2-section-map.md) ranges 11–70 (config, meta-interpreter, tension), 136–144 (object_level Peano), and 163–232 (oracle, ML, reflective monitor, crisis_processor).

**Agreements (mechanics).** Round 2 §Navigating the Constrained Topography (L11–38) matches: `max_inferences(20)`, `max_retries(5)`, the seven `cognitive_cost/2` entries and their values, the four `server_mode/1` dispatch tables, `calculate_recollection_cost/2 = Length * StepCost`. Round 2 §Lifecycle of a Goal (L40–52) matches: the six-arity `solve/6` surface, the `neutral → true` base case, conjunction threading, built-in dispatch, and the `fail(Goal)` terminal marker. The inference-cost multipliers (compressive=2, expansive=1, neutral=1) match. Round 2 §Tension Dynamics (L54–70) matches: the separate tension multipliers (compressive=2.5, neutral=1.0, expansive=0.3), damping 0.02, relaxation 0.4, history_window 20, check_stability requiring ≥9, three-window second-difference. Round 2 §Mathematical Topography (L136–144) matches: `enumerate/1` as backtracking generator before `recursive_add/3`, the base case and recursive rule. Round 2 §Oracle Server (L163–180) matches: `decompose_operation/4` validation, `strategy_appropriate_for/3` with `estimate_strategy_cost/5`, the per-operation strategy tables, `extract_interpretation/7` as `format/3` templating. Round 2 §Machine Learning (L181–203) matches: the tiered `solve/4` (learned-first, foundational-fallback), `solve_foundationally/4` requiring both operands to be recollections, `count_loop/4` calling `proves([] => [o(plus(X,1,Y))])` as the successor (a striking code detail — the count loop does not use `is/2`). Round 2 §Failure Diagnostics (L205–219) matches: `parse_trace` → commitments + failures, `increment_stress/1` via `functor/3` signature. Round 2 §Crisis Triggers (L220–232) matches: the `process_crisis_curriculum/1` line-at-a-time file reader, the 5-second wall-clock performance threshold in `check_for_crisis_indicators/2`, the `attempt_chunking_*` fallbacks.

**Disagreements, none substantive.** Round 2 at L68 says stability computation "demands a minimum of 9 integers" — the code has three branches (≥9, ≥6, else), with the ≥6 branch computing a degraded first-difference and the default returning 1.0. Round 2 omits the ≥6 fallback. Not a contradiction of the ≥9 path; a completeness gap. Round 2 at L191 describes `reflect_and_learn/1` and `critique_and_bootstrap/1` as "placeholder routines" — but these predicates are not present in the stripped `more_machine_learner.pl` at all; only `count_trace_steps/2` and `get_calculation_trace/2` survive. The "placeholder" label is Round 2's, not the code's. Round 2 is describing predicates absent from the stripped source, which is silent drift, not honest labeling. Round 2 at L189 describes `count_loop` using `proves( =>)` to derive the successor — this is confirmed in `more_machine_learner.pl` line 57: `successor(X, Y) :- proves([] => [o(plus(X, 1, 1))])`, a cross-module wire into the sequent calculus that Round 2 correctly flags as significant.

**Voice disagreement (flagged but not substantive).** Round 2's prose ("violently mutated," "brutally dissects," "aggressively validates") is purple-Gothic by default. The mechanics under the prose are accurate.

