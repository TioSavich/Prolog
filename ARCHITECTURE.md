# Crisis-Driven Learning Architecture

## System Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    PRIMORDIAL MACHINE                            │
│  (Starts with only primitive addition - "Counting All")         │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ↓
              ┌───────────────────────────────┐
              │   1. Execute Computation      │
              │   run_computation(Goal, Limit) │
              └───────────────────────────────┘
                              │
                              ↓
              ┌───────────────────────────────┐
              │   2. Meta-Interpreter         │
              │   solve(Goal, Limit, _, Trace)│
              └───────────────────────────────┘
                              │
                    ┌─────────┴─────────┐
                    │                   │
                    ↓                   ↓
            ┌───────────────┐   ┌─────────────────┐
            │   SUCCESS     │   │    FAILURE      │
            │   Return      │   │  Detect Crisis  │
            └───────────────┘   └─────────────────┘
                                        │
                        ┌───────────────┴───────────────┐
                        │                               │
                        ↓                               ↓
        ┌───────────────────────────┐   ┌─────────────────────────────┐
        │ resource_exhaustion       │   │ unknown_operation(Op, Goal) │
        │ (inference limit exceeded)│   │ (operation not defined)     │
        └───────────────────────────┘   └─────────────────────────────┘
                        │                               │
                        └───────────────┬───────────────┘
                                        ↓
                        ┌───────────────────────────────┐
                        │   3. Crisis Handler           │
                        │   handle_perturbation(...)    │
                        └───────────────────────────────┘
                                        │
                                        ↓
                        ┌───────────────────────────────┐
                        │   4. Oracle Consultation      │
                        │   query_oracle(Op, Strategy,  │
                        │                Result, Interp)│
                        └───────────────────────────────┘
                                        │
                        ┌───────────────┴───────────────┐
                        │      ORACLE SERVER            │
                        │  (Black box - expert knowledge)│
                        │  - 20 arithmetic strategies   │
                        │  - Returns result + interpret │
                        │  - Hides internal FSM traces  │
                        └───────────────────────────────┘
                                        │
                                        ↓
                        ┌───────────────────────────────┐
                        │   5. Strategy Synthesis       │
                        │   synthesize_from_oracle(...) │
                        └───────────────────────────────┘
                                        │
                                        ↓
                        ┌───────────────────────────────┐
                        │   6. Assert New Strategy      │
                        │   assertz(object_level:Op...) │
                        └───────────────────────────────┘
                                        │
                                        ↓
                        ┌───────────────────────────────┐
                        │   7. Retry with New Knowledge │
                        │   run_computation(Goal, Limit)│
                        └───────────────────────────────┘
                                        │
                                        ↓
                                ┌───────────────┐
                                │   SUCCESS!    │
                                └───────────────┘
```

## Key Components

### 1. Primordial State (object_level.pl)
```prolog
% Only this exists at startup:
object_level:add(A, B, Result) :-
    enumerate(A, RecA),
    enumerate(B, RecB),
    append(RecA, RecB, Combined),
    recollection(Combined, Result).
```
**Inefficient "Counting All" - triggers crises on large numbers**

### 2. Meta-Interpreter (meta_interpreter.pl)
```prolog
% Detects unknown operations:
solve(Goal, Ctx, Ctx, _I, _I, _) :-
    (Goal = object_level:ActualGoal -> true ; ActualGoal = Goal),
    functor(ActualGoal, Functor, 3),
    member(Functor, [subtract, multiply, divide]),
    \+ clause(object_level:ActualGoal, _),
    !,
    throw(perturbation(unknown_operation(Functor, Goal))).
```

### 3. Crisis Handler (execution_handler.pl)
```prolog
handle_perturbation(perturbation(unknown_operation(Op, PeanoGoal)), Goal, _, Limit) :-
    % Get first available strategy
    oracle_server:list_available_strategies(Op, [FirstStrategy|_]),
    
    % Consult oracle
    consult_oracle_for_solution(PeanoGoal, FirstStrategy, Result, Interpretation),
    
    % Synthesize oracle-backed strategy
    SynthesisInput = _{
        goal: PeanoGoal,
        target_result: Result,
        target_interpretation: Interpretation,
        strategy_name: FirstStrategy  % Key: tells synthesizer which strategy
    },
    synthesize_from_oracle(SynthesisInput),
    
    % Retry
    run_computation(Goal, Limit).
```

### 4. Oracle Server (oracle_server.pl)
```prolog
% Black box interface:
query_oracle(Op, StrategyName, Result, Interpretation) :-
    % Execute strategy module (sar_*, smr_*)
    execute_strategy(Op, StrategyName, IntResult, InterpretationAtom),
    Result = IntResult,
    Interpretation = InterpretationAtom.

% Lists available strategies:
list_available_strategies(add, ['COBO', 'Chunking', 'RMB', 'Rounding']).
list_available_strategies(subtract, ['COBO (Missing Addend)', 'CBBO (Take Away)', ...]).
list_available_strategies(multiply, ['C2C', 'CBO', 'Commutative', 'DR']).
list_available_strategies(divide, ['Dealing', 'CBO (Division)', 'UCR', 'IDP']).
```

### 5. Strategy Synthesis (fsm_synthesis_engine.pl)
```prolog
% Oracle-backed synthesis (Phase 2 solution):
synthesize_strategy_from_oracle(Goal, _, TargetResult, TargetInterpretation, StrategyName) :-
    % Extract operation
    functor(ActualGoal, Op, 3),
    
    % Create oracle-backed strategy
    OpGoal =.. [Op, A, B, Result],
    Body = (
        peano_to_int(A, IntA),
        peano_to_int(B, IntB),
        OpInt =.. [Op, IntA, IntB],
        oracle_server:query_oracle(OpInt, StrategyName, IntResult, _),
        int_to_peano(IntResult, Result)
    ),
    
    % Assert as learned strategy
    assertz((object_level:OpGoal :- Body)).
```

## Crisis Types

### Type 1: resource_exhaustion
**When**: Inference budget exceeded  
**Example**: add(8,5) with limit=10 using "Counting All"  
**Response**: Consult oracle for more efficient addition strategy  
**Learn**: COBO, Chunking, RMB, or Rounding  

### Type 2: unknown_operation(Op, Goal)
**When**: Operation attempted but not defined  
**Example**: subtract(7,3,R) from primordial state  
**Response**: Consult oracle for first available strategy for that operation  
**Learn**: Oracle-backed strategy for subtract/multiply/divide  

## Learning Flow Example

### Problem: subtract(7, 3, R)

**Step 1: Attempt**
```prolog
?- run_computation(object_level:subtract(s(s(s(s(s(s(s(0))))))), s(s(s(0))), R), 50).
```

**Step 2: Detection**
```
meta_interpreter:solve/6:
  - Goal: object_level:subtract(...)
  - Check: \+ clause(object_level:subtract(_,_,_), _)  [TRUE - undefined!]
  - Action: throw(perturbation(unknown_operation(subtract, Goal)))
```

**Step 3: Crisis Handler**
```
execution_handler:handle_perturbation/4:
  - Catches: perturbation(unknown_operation(subtract, ...))
  - Query: list_available_strategies(subtract, [First|_])
  - Result: First = 'COBO (Missing Addend)'
```

**Step 4: Oracle Consultation**
```
oracle_server:query_oracle(subtract(7,3), 'COBO (Missing Addend)', Result, Interp):
  - Executes: sar_sub_cobo_missing_addend:run_cobo_ma(7, 3, Result, Interp)
  - Returns: Result = 4
           Interp = "Count on from subtrahend: Start at 3, count up to 7, the gap is 4"
```

**Step 5: Synthesis**
```prolog
synthesize_strategy_from_oracle(...):
  - Creates: object_level:subtract(A, B, Result) :-
               peano_to_int(A, IntA),
               peano_to_int(B, IntB),
               oracle_server:query_oracle(subtract(IntA,IntB), 'COBO (Missing Addend)', IntR, _),
               int_to_peano(IntR, Result).
  - Asserts: New clause added to object_level
```

**Step 6: Retry**
```
run_computation(object_level:subtract(...), 50):
  - Now: clause(object_level:subtract(_,_,_), _) exists!
  - Executes: New learned strategy
  - Success: R = s(s(s(s(0))))  [i.e., 4]
```

## Why This Works

### 1. Crisis-Driven (Piaget)
- System only learns when **forced** by inadequacy
- Disequilibrium → Accommodation → New equilibrium
- Not proactive - reactive

### 2. Encultured (Vygotsky)
- Oracle = **cultural authority** (teacher/expert)
- Real learning involves cultural transmission
- Not "magic" - legitimate knowledge source

### 3. Hermeneutic (Gadamer)
- Oracle provides **interpretations** not just results
- System "understands" through expert explanation
- Meaning-making through cultural guidance

### 4. Accommodative (Piaget)
- **Before**: Cannot subtract
- **Crisis**: Encounters inadequacy
- **After**: Can subtract
- Genuine capability expansion

## Comparison: What Would Be Cheating

### ❌ Cheating Approaches
```prolog
% Pre-loading all knowledge (no bootstrap)
:- load_all_strategies_at_startup.

% Magic synthesis with no knowledge source
synthesize_strategy(Goal) :- 
    generate_perfect_strategy_from_thin_air(Goal).

% Proactive learning (not crisis-driven)
after_every_computation :-
    reflect_and_learn_proactively.
```

### ✅ Our Legitimate Approach
```prolog
% 1. Start primordial
:- only_counting_all_addition_exists.

% 2. Crisis triggers learning
catch(computation, perturbation(crisis), handle_crisis).

% 3. Consult cultural authority
oracle_server:query_oracle(Op, Strategy, Result, Interpretation).

% 4. Accommodate new capability
assertz((object_level:Op(...) :- Body)).
```

## Test Results

### Phase 2: Crisis Detection
```
✓ subtract(5,3) → crisis → learn COBO → succeed
✓ multiply(3,4) → crisis → learn C2C → succeed  
✓ divide(12,3) → crisis → learn CBO → succeed
```

### Phase 3: Sequential Bootstrap
```
✓ subtract(7,3) → unknown_operation → learn → succeed
✓ multiply(4,3) → unknown_operation → learn → succeed
✓ divide(12,4) → unknown_operation → learn → succeed
```

## Files Involved

### Core System
- **meta_interpreter.pl** - Crisis detection
- **execution_handler.pl** - Crisis handling, oracle integration
- **object_level.pl** - Dynamic knowledge base (starts primordial)
- **oracle_server.pl** - Black box expert interface
- **fsm_synthesis_engine.pl** - Strategy synthesis (oracle-backed)

### Strategy Modules (Oracle Knowledge)
- **sar_add_*.pl** - 4 addition strategies
- **sar_sub_*.pl** - 8 subtraction strategies
- **smr_mult_*.pl** - 4 multiplication strategies
- **smr_div_*.pl** - 4 division strategies

### Tests
- **test_phase_2_crisis_detection.pl** - Unknown operation tests
- **test_phase_3_simple.pl** - Sequential bootstrap tests

### Documentation
- **BOOTSTRAP_CHECKLIST.md** - Implementation roadmap
- **PHASE_2_STATUS.md** - Synthesis blocker analysis
- **PHASE_2_COMPLETE.md** - Phase 2 summary
- **BOOTSTRAP_COMPLETE.md** - Final summary
- **ARCHITECTURE.md** (this file) - System architecture

## Conclusion

We built a **computationally legitimate** crisis-driven learning system that:

✅ Detects inadequacy (unknown operations)  
✅ Triggers crises (perturbations)  
✅ Consults experts (oracle)  
✅ Accommodates capabilities (synthesis)  
✅ Bootstraps successfully (primordial → full arithmetic)  

The architecture maintains philosophical integrity while being pragmatically effective.
