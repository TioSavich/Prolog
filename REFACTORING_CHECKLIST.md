# UMEDCA Refactoring Checklist

## Overview
This checklist guides the implementation of the architectural refactoring to separate the "primordial machine" from the "normative oracle," transforming the codebase into a true computational autoethnography.

---

## Phase 1: Build the Primordial Machine (Bootstrap Kernel)

### 1.1 Create Entry Point
- [ ] Create new file: `primordial_start.pl`
  - [ ] This will be the main execution entry point
  - [ ] Must enforce strict separation from full strategy library
  - [ ] Should only load essential kernel modules

### 1.2 Configure Kernel Module Loading
- [ ] In `primordial_start.pl`, load ONLY these modules:
  - [ ] `config.pl` (system-wide settings, inference limits)
  - [ ] `grounded_arithmetic.pl` (foundational embodiment: recollection/tally/successor)
  - [ ] `object_level.pl` (dynamic knowledge base)
  - [ ] `meta_interpreter.pl` (ORR cycle component)
  - [ ] `reflective_monitor.pl` (ORR cycle component)
  - [ ] `reorganization_engine.pl` (ORR cycle component)
  - [ ] `execution_handler.pl` (ORR cycle component)
  - [ ] `more_machine_learner.pl` (learning engine)

### 1.3 Simplify object_level.pl to Primordial State
- [ ] Ensure ONLY the inefficient `add/3` predicate exists
  - [ ] This predicate must rely on `enumerate/1`
  - [ ] Implements "Counting All" strategy (most primitive)
- [ ] **CRITICAL:** Remove or comment out ALL other rules:
  - [ ] `subtract/3`
  - [ ] `multiply/3`
  - [ ] `divide/3`
  - [ ] Any other arithmetic operations
- [ ] Document why each removal enforces emergence principle

### 1.4 Configure Inference Limits
- [ ] In `config.pl`, set global inference limit
  - [ ] Define: `max_inferences(10)`
  - [ ] Verify this causes "Counting All" to fail on `add(8,5)`
  - [ ] Document reasoning: small enough to force early crisis

---

## Phase 2: Refine Learning-through-Crisis Mechanism

### 2.1 Make Crisis the Exclusive Learning Trigger
- [ ] Modify `execution_handler.pl`:
  - [ ] Review `run_computation/2` predicate
  - [ ] Ensure `catch/3` traps perturbations from meta_interpreter
  - [ ] Verify ONLY `perturbation(resource_exhaustion)` triggers learning
  - [ ] Remove any proactive/optional learning paths

- [ ] Modify `reorganization_engine.pl`:
  - [ ] Ensure reorganization ONLY called on resource_exhaustion
  - [ ] Learning must be reactive accommodation to failure
  - [ ] No learning from successful executions

### 2.2 Create Developmental Curriculum
- [ ] Verify `crisis_curriculum.txt` is primary curriculum file
- [ ] Structure curriculum as crisis-inducing progression:
  - [ ] Task 1: `add(2,3)` - solvable within 10 steps (success)
  - [ ] Task 2: `add(8,5)` - exceeds 10 steps (FIRST CRISIS)
  - [ ] Task 3: `add(3,8)` - tests generalization/commutativity
  - [ ] Task 4: `multiply(3,4)` - NEW crisis type (missing predicate)
  - [ ] Continue progression with increasingly complex tasks
- [ ] Document each task's intended crisis/learning goal
- [ ] Each crisis should model finite→infinite dialectic

---

## Phase 3: Implement Developmental Knowledge System

### 3.1 Modify Learning Strategy Assertion
- [ ] In `more_machine_learner.pl`:
  - [ ] When synthesizing new strategy, use `assertz/1` to add clause
  - [ ] Add to dynamic database or `learned_knowledge.pl` for persistence
  - [ ] **CRITICAL:** NEVER call `retract/1` on previous strategies
  - [ ] Document: preserves "geological record" of development

### 3.2 Re-architect Strategy Invocation Hierarchy
- [ ] In `execution_handler.pl`, modify `run_computation/2`:
  - [ ] DO NOT immediately default to `object_level:add/3`
  - [ ] FIRST: Query learned strategies in reverse order (LIFO)
  - [ ] Use `clause/2` to find all matching `run_learned_strategy` clauses
  - [ ] Try each sequentially, newest to oldest
  - [ ] FALLBACK hierarchy:
    1. [ ] Most recent learned strategy
    2. [ ] Second most recent learned strategy
    3. [ ] ... (all learned strategies)
    4. [ ] Primordial `object_level:add/3` (last resort)

### 3.3 Add Strategy Selection Logging
- [ ] Log which strategy is successfully selected
- [ ] Include strategy name (e.g., `rmb(10)`, `cobo`)
- [ ] Add to execution trace for visibility
- [ ] Makes developmental stage explicit in output

---

## Phase 4: Build the Oracle Server

### 4.1 Create Oracle Interface
- [ ] Create new file: `oracle_server.pl`
  - [ ] Load `hermeneutic_calculator.pl` dispatcher
  - [ ] This indirectly loads ALL `sar_*.pl` and `smr_*.pl` modules
  - [ ] Expose ONLY one predicate: `query_oracle/4`

### 4.2 Implement query_oracle/4
- [ ] Signature: `query_oracle(+Operation, +StrategyName, -Result, -Interpretation)`
- [ ] Example: `query_oracle(add(8,5), rmb, Result, Interp)`
- [ ] Implementation:
  - [ ] Use `hermeneutic_calculator:calculate/6` to execute strategy
  - [ ] Capture final numerical result
  - [ ] Capture final textual interpretation string
  - [ ] **CRITICAL:** Discard step-by-step execution trace
  - [ ] Enforce black box constraint (no internal states exposed)

### 4.3 Test Oracle Isolation
- [ ] Verify oracle runs in separate logical context
- [ ] Primordial machine CANNOT access internal oracle predicates
- [ ] Only `query_oracle/4` interface is available

---

## Phase 5: Transform Learner into Synthesis Engine

### 5.1 Remove Pattern Detection Heuristics
- [ ] In `more_machine_learner.pl`, DELETE:
  - [ ] `detect_cob_pattern/2`
  - [ ] `detect_rmb_pattern/2`
  - [ ] `construct_and_validate_cob/2`
  - [ ] `construct_and_validate_rmb/3`
  - [ ] ANY other hard-coded strategy pattern detectors
- [ ] Document: learner can no longer have "innate" strategy knowledge

### 5.2 Integrate Oracle into Crisis Response
- [ ] In `reorganization_engine.pl`:
  - [ ] On `resource_exhaustion` for goal like `add(8,5)`:
    1. [ ] Query oracle: `query_oracle(add(8,5), Strategy, Result, Interp)`
    2. [ ] Oracle returns (e.g.) `Result = 13`, `Interp = 'Count on from bigger'`
    3. [ ] Pass to learner: `synthesize_strategy/4`

### 5.3 Implement Synthesis Engine Core
- [ ] Create new predicate: `synthesize_strategy/4`
  - [ ] Signature: `synthesize_strategy(+Goal, +FailedTrace, +TargetResult, +TargetInterpretation)`
  - [ ] This is the new heart of learning system
  - [ ] Task: Generate new `transition/4` rules for FSM

- [ ] Implement FSM Search:
  - [ ] Search space: all possible FSMs
  - [ ] Building blocks: primitives from `grounded_utils.pl`
    - [ ] `successor/2`
    - [ ] `predecessor/2`
    - [ ] `decompose_base10/2`
  - [ ] Search constraints:
    1. [ ] Synthesized FSM must produce `TargetResult`
    2. [ ] FSM execution must not exceed `max_inferences` limit
  - [ ] Use `TargetInterpretation` as heuristic hint (optional/advanced)

### 5.4 Define Meta-Abilities for Search
- [ ] Implement segmentation/analysis of failed traces
- [ ] Implement recombination of primitives
- [ ] Implement generalization (variabilization)
- [ ] These are domain-general, not math-specific

---

## Phase 6: Define Cost Function (Theory Operationalization)

### 6.1 Embodied Representation Costs
- [ ] Cost of `recollection([tally|...])` operations:
  - [ ] MUST be proportional to list length
  - [ ] Models effort of manipulating tokens
  - [ ] "Counting All" exhaustion is embodied exhaustion
- [ ] Document: this is not optimization, this is theory

### 6.2 Modal Shift Costs
- [ ] Modal operators consume inference budget:
  - [ ] `$s(comp_nec(...))`
  - [ ] `$s(exp_poss(...))`
  - [ ] Represent cognitive events (reflection, restructuring)
- [ ] Document: thinking is not free

### 6.3 Measure Abstraction as Cost Reduction
- [ ] Learned strategy cost = sum of:
  - [ ] Primitive operations
  - [ ] Modal shifts
- [ ] Abstraction = significant cost reduction vs enumeration
- [ ] This reduction measures developmental progress

---

## Phase 7: Implement Computational Hermeneutics

### 7.1 Oracle Provides Result + Interpretation
- [ ] Oracle returns numerical result (e.g., `13`)
- [ ] Oracle returns interpretation string (e.g., `'Rearranging to Make Bases'`)
- [ ] Oracle NEVER returns execution trace

### 7.2 Learner as Hermeneutic Engine
- [ ] Learner cannot use interpretation as lookup key
- [ ] Must use vocabulary as CONSTRAINT on search
- [ ] Must figure out which primitives correspond to concepts in string
- [ ] Synthesize FSM (P) that makes interpretation (V) intelligible

### 7.3 Test Recognition vs Imitation
- [ ] Verify learner reconstructs internal rational structure
- [ ] Verify learner doesn't match pre-defined templates
- [ ] Test: Can learner recognize same strategy from different interpretations?

---

## Phase 8: Document Divasion Architecture

### 8.1 Architectural Reflection
- [ ] Document how meta-interpreter creates entanglement
- [ ] System is simultaneously:
  - [ ] INSIDE current strategy logic (Observation)
  - [ ] OUTSIDE, reflecting on limitations (Reflection/Reorganization)

### 8.2 Crisis as Divasion Event
- [ ] Document crisis as computational manifestation of divasion
- [ ] Moment when classical formalism breaks down
- [ ] Machine suspended between failed and emergent structures
- [ ] Contradiction: demand to solve vs inability to solve

### 8.3 Productive Limitation
- [ ] Classical logic cannot tolerate contradiction
- [ ] This intolerance DRIVES reorganization
- [ ] Bootstrapping is sublation (Aufhebung) of contradiction
- [ ] System achieves self-transcendence within rigid formalism

---

## Phase 9: Testing & Validation

### 9.1 Test Primordial Machine Initialization
- [ ] Run `primordial_start.pl`
- [ ] Verify only kernel modules loaded
- [ ] Verify only `add/3` (Counting All) exists
- [ ] Verify `max_inferences(10)` is active

### 9.2 Test First Crisis
- [ ] Run `add(2,3)` - should succeed
- [ ] Run `add(8,5)` - should fail with `resource_exhaustion`
- [ ] Verify crisis triggers reorganization
- [ ] Verify oracle is consulted

### 9.3 Test Learning from Crisis
- [ ] After first crisis, verify new strategy asserted
- [ ] Verify old strategy NOT retracted
- [ ] Run `add(8,5)` again - should succeed with new strategy
- [ ] Verify strategy selection logged

### 9.4 Test Strategy Hierarchy
- [ ] After multiple learning cycles:
  - [ ] Verify newest strategy tried first
  - [ ] Verify fallback to older strategies
  - [ ] Verify primordial strategy is last resort
- [ ] Check `learned_knowledge.pl` shows geological record

### 9.5 Test Oracle Isolation
- [ ] Verify primordial machine cannot directly access `sar_*.pl`
- [ ] Verify only `query_oracle/4` provides access
- [ ] Verify internal oracle traces are hidden

### 9.6 Run Full Curriculum
- [ ] Execute entire `crisis_curriculum.txt`
- [ ] Document each crisis and learned response
- [ ] Verify developmental trajectory matches theory
- [ ] Check final capability vs initial capability

---

## Phase 10: Documentation & Theoretical Alignment

### 10.1 Code Documentation
- [ ] Add docstrings explaining philosophical grounding
- [ ] Reference Hegelian concepts where applicable
- [ ] Document each crisis type and intended learning
- [ ] Explain cost function as theory operationalization

### 10.2 Philosophical Alignment Verification
- [ ] Verify "Sense-Certainty" → "Counting All" mapping
- [ ] Verify finite/infinite dialectic in curriculum
- [ ] Verify sublation in learning process
- [ ] Verify computational autoethnography in knowledge record

### 10.3 Create Visualization/Logging
- [ ] Log complete developmental history
- [ ] Visualize strategy hierarchy (geological layers)
- [ ] Graph cost reduction over time (abstraction progress)
- [ ] Export trace for analysis

### 10.4 Write Research Documentation
- [ ] Document refactoring rationale
- [ ] Explain primordial machine vs oracle architecture
- [ ] Describe computational hermeneutics implementation
- [ ] Analyze results vs philosophical claims

---

## Critical Notes

### DO NOT PROCEED WITHOUT:
1. **Backup entire codebase** before starting any changes
2. **Test each phase** before moving to next
3. **Document each change** with philosophical justification
4. **Verify no regression** in existing functionality during transition

### Key Philosophical Commitments:
- **Emergence over preloading**: Everything learned, not given
- **Crisis drives learning**: Failure is productive
- **History preservation**: No retraction, only accumulation
- **Recognition over imitation**: Reconstruct internal structure
- **Embodiment grounds abstraction**: Cost = phenomenological effort

### Architecture Principles:
- **Separation of concerns**: Primordial machine ≠ Oracle
- **Interface restriction**: Black box oracle access only
- **Hierarchical fallback**: Try newest first, oldest last
- **Cost as theory**: Not optimization, but operationalization

---

## Success Criteria

### The refactor is complete when:
1. [ ] System starts with ONLY successor-based Counting All
2. [ ] First crisis occurs predictably on `add(8,5)`
3. [ ] Oracle provides result + interpretation, NOT trace
4. [ ] Learner synthesizes FSM from constraints
5. [ ] New strategies accumulate without erasure
6. [ ] Strategy selection follows LIFO hierarchy
7. [ ] Cost function reflects embodied effort
8. [ ] Full curriculum produces developmental trajectory
9. [ ] Code mirrors philosophical claims in paper
10. [ ] System demonstrates computational autoethnography

---

## Emergency Rollback Plan

### If refactor breaks system:
1. [ ] Restore from backup
2. [ ] Identify which phase caused break
3. [ ] Implement that phase in isolation
4. [ ] Test thoroughly before integration
5. [ ] Document what went wrong and why

---

## Post-Refactor Analysis

### After completion, verify:
- [ ] Computational behavior matches theoretical claims
- [ ] Learning trajectory shows genuine emergence
- [ ] No architectural drift remains
- [ ] Oracle/machine separation is clean
- [ ] Cost function is theoretically grounded
- [ ] Recognition mechanism works as intended
- [ ] Divasion is architecturally represented

### Generate final report including:
- [ ] Before/after architecture comparison
- [ ] Learning trajectory visualization
- [ ] Cost reduction over development
- [ ] Strategy hierarchy diagram
- [ ] Theoretical claims validated
- [ ] Remaining limitations acknowledged
