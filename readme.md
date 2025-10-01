# Crisis-Driven Arithmetic Learning System# A Synthesis of Incompatibility Semantics, CGI, and Piagetian Constructivism with FSM Engine Architecture



A Prolog implementation of a self-reorganizing cognitive system that learns arithmetic through **crisis-driven accommodation**. The system bootstraps from a primordial state (only basic addition) to full 4-operation arithmetic by detecting computational inadequacy and consulting expert knowledge.## 1. Introduction



## Philosophical FoundationThis project presents a novel synthesis of three influential frameworks in philosophy, cognitive science, and education, implemented as a computational model in SWI-Prolog with a unified Finite State Machine (FSM) engine architecture.



This system implements **enactivist/Piagetian** learning principles:*   **Robert Brandom's Incompatibility Semantics:** A theory asserting that the meaning of a concept is defined by what it is incompatible with. We understand what something *is* by understanding what it rules out.

- **Crisis-driven**: Learning occurs only when current capabilities prove inadequate*   **Cognitively Guided Instruction (CGI):** An educational approach focused on understanding and building upon students' intuitive problem-solving strategies.

- **Accommodative**: System expands capabilities through genuine structural change*   **Piagetian Constructivism:** A theory of cognitive development emphasizing the learner's active construction of knowledge through assimilation and accommodation, driven by the resolution of cognitive conflict (disequilibrium).

- **Encultured**: Learns from expert knowledge (oracle as cultural authority)

- **Hermeneutic**: Understands through interpretation, not just computationThis synthesis aims to provide a formal, computational model for understanding conceptual development and designing instruction that respects the learner's constructive processes.



The architecture embodies **computational divasion** - the system is simultaneously inside its own execution (observing) and outside (reflecting on inadequacy).## 2. Core Concepts



## System ArchitectureThe core idea of this synthesis is that learning (Constructivism) occurs when a learner recognizes an incompatibility (Brandom) between their existing cognitive structures and new information or experiences. Instruction (CGI) facilitates this process by analyzing the learner's current strategies and introducing experiences that highlight relevant incompatibilities, prompting the necessary cognitive shifts (accommodation).



```This is modeled in the repository through several key components:

Primordial State → Crisis → Oracle Consultation → Synthesis → Accommodation- **Incompatibility Semantics**: The core logic for determining entailment and contradiction is implemented in `incompatibility_semantics.pl`.

     (add only)     ↓         (expert guidance)      ↓       (new capability)- **Student Strategy Models**: The CGI aspect is modeled through a library of student problem-solving strategies (`sar_*.pl` for addition/subtraction and `smr_*.pl` for multiplication/division), which simulate how students with different conceptual understandings might approach a problem.

                 Unknown                          Learn- **Learning Cycle**: The Piagetian process of learning through disequilibrium is modeled by the **Observe-Reorganize-Reflect (ORR)** cycle, which can detect failures in its own knowledge and attempt to repair itself.

                 Operation                      Strategy- **FSM Engine Architecture**: All student strategy models are unified under a common Finite State Machine engine that provides consistent execution, modal logic integration, and cognitive cost tracking.

```- **Grounded Fractional Arithmetic**: A comprehensive system implementing Jason's partitive fractional schemes using nested unit representation instead of rational numbers, providing embodied cognitive modeling of fractional reasoning.



### Core Components## 3. System Architecture



1. **Primordial Machine** (`object_level.pl`)The system is composed of several distinct parts that work together, unified by a common FSM engine architecture.

   - Starts with only "Counting All" addition (inefficient enumerate-based)

   - All other operations undefined### 3.1. FSM Engine Architecture (Core Framework)

A unified finite state machine engine that standardizes all student strategy execution:

2. **Meta-Interpreter** (`meta_interpreter.pl`)- **`fsm_engine.pl`**: The core FSM execution engine that provides consistent state transition handling, modal logic integration, and cognitive cost tracking across all student strategies.

   - Executes computations with resource limits- **`grounded_arithmetic.pl`**: The foundational grounded arithmetic system that eliminates dependency on arithmetic backstops by providing embodied mathematical operations with cognitive cost tracking.

   - Detects crises (resource_exhaustion, unknown_operation)- **`grounded_utils.pl`**: Utility functions supporting the grounded arithmetic foundation.

   - Traces execution for reflection

### 3.2. Grounded Fractional Arithmetic System (New Addition)

3. **Crisis Handler** (`execution_handler.pl`)A comprehensive framework implementing Jason's partitive fractional schemes with embodied cognition:

   - Catches perturbations (crises)- **`composition_engine.pl`**: Implements embodied grouping operations for unit composition with cognitive cost tracking.

   - Orchestrates Oracle-Reorganize-Reflect (ORR) cycle- **`fraction_semantics.pl`**: Defines equivalence rules for fractional reasoning including grouping (D copies of 1/D equals 1) and composition (nested fractions).

   - Manages learning and retry- **`grounded_ens_operations.pl`**: Core Equal-N-Sharing (ENS) operations that create nested unit structures through structural partitioning.

- **`normalization.pl`**: Iterative normalization engine that applies equivalence rules until quantities are fully simplified.

4. **Oracle Server** (`oracle_server.pl`)- **`jason.pl`**: Completely refactored implementation of partitive fractional schemes using nested unit representation instead of rational numbers.

   - Black box interface to 20 expert arithmetic strategies- **`test_fractional_arithmetic.pl`**: Comprehensive test suite for the grounded fractional arithmetic system.

   - Provides: Result + Interpretation (not implementation details)

   - Strategies: `sar_*.pl` (addition/subtraction), `smr_*.pl` (multiplication/division)### 3.3. The ORR Cycle (Cognitive Core)

This is the heart of the system's learning capability, inspired by Piagetian mechanisms.

5. **Synthesis Engine** (`fsm_synthesis_engine.pl`)- **`execution_handler.pl`**: The main driver that orchestrates the ORR cycle.

   - Creates oracle-backed strategies (call expert when needed)- **`meta_interpreter.pl`**: The **Observe** phase. It runs a given goal while producing a detailed execution trace, making the system's reasoning process observable to itself.

   - Converts between Peano numbers and integers- **`reflective_monitor.pl`**: The **Reflect** phase. It analyzes the trace from the meta-interpreter to detect signs of "disequilibrium" (e.g., goal failures, contradictions).

   - Asserts new capabilities into `object_level`- **`reorganization_engine.pl`**: The **Reorganize** phase. Triggered by disequilibrium, it attempts to modify the system's own knowledge base to resolve the conflict.



## Crisis Types### 3.4. Knowledge Base

- **`object_level.pl`**: Contains the system's foundational, and potentially flawed, knowledge (e.g., an inefficient rule for addition). This is the knowledge that the ORR cycle operates on and modifies.

### 1. Unknown Operation- **`incompatibility_semantics.pl`**: Defines the core logical and mathematical rules of the "world," including what concepts are incompatible with each other, and provides modal logic operators (s/1, comp_nec/1, exp_poss/1).

**When**: Operation attempted but not defined (e.g., `subtract(7,3,R)` from primordial state)  - **`learned_knowledge.pl`**: An auto-generated file where new, more efficient strategies discovered by the `more_machine_learner.pl` module are stored.

**Response**: Consult oracle for first available strategy, synthesize oracle-backed implementation  

**Example**: System learns subtraction when first needed### 3.5. API Server

- **`working_server.pl`**: The production-ready server for powering the web-based GUI. It contains stable, optimized analysis logic and is used by the startup script.

### 2. Resource Exhaustion

**When**: Inference budget exceeded (e.g., `add(8,5)` with limit=10 using slow "Counting All")  ## 4. FSM Engine Architecture (Major Innovation)

**Response**: Consult oracle for more efficient strategy  

**Example**: System learns COBO addition to replace enumerationThis system features a revolutionary **Finite State Machine (FSM) Engine** that unifies all student strategy models under a common computational framework. This represents a significant architectural advancement providing:



## Quick Start### 4.1. Unified Execution Model

- **Consistent Interface**: All 17+ student strategies (`sar_*.pl`, `smr_*.pl`) use the same FSM engine interface via `run_fsm_with_base/5`

### Prerequisites- **Code Reduction**: ~70% reduction in duplicate state machine code across strategy files

- SWI-Prolog 8.0 or higher- **Standardized Transitions**: All strategies use `transition/4` predicates with consistent parameter patterns

- All strategy modules (`sar_*.pl`, `smr_*.pl`)

### 4.2. Modal Logic Integration

### Run Bootstrap Test- **Cognitive Operators**: Every state transition integrates modal logic operators:

```bash  - `s/1`: Basic cognitive operations and state changes

# Test complete bootstrap: Add → Subtract → Multiply → Divide  - `comp_nec/1`: Necessary computational steps and systematic processes  

swipl -g test_all -t halt test_phase_3_simple.pl  - `exp_poss/1`: Possible expansions and completion states

```- **Semantic Grounding**: Modal operators provide semantic meaning to computational steps, connecting to Brandom's incompatibility semantics



**Expected Result**:### 4.3. Cognitive Cost Tracking

```- **Embodied Cognition**: Every cognitive operation has an associated cost via `incur_cost/1`

TEST 1: Learn Subtraction (7 - 3) ✓- **Resource Awareness**: The system tracks computational resources as cognitive resources

  Crisis: unknown_operation(subtract, ...)- **Performance Analysis**: Enables comparison of strategy efficiency in cognitive terms

  Oracle: "COBO (Missing Addend)"

  Result: 7 - 3 = 4### 4.4. Grounded Arithmetic Foundation  

- **Elimination of Arithmetic Backstop**: No reliance on hardcoded arithmetic; all operations are grounded in embodied cognitive processes

TEST 2: Learn Multiplication (4 * 3) ✓- **Constructivist Mathematics**: Numbers and operations emerge from cognitive actions rather than being pre-given

  Crisis: unknown_operation(multiply, ...)- **Peano Arithmetic**: Foundation built on successor functions and recursive operations

  Oracle: "C2C"

  Result: 4 * 3 = 12### 4.5. FSM Engine Benefits

- **Maintainability**: Single engine handles all strategy execution, reducing maintenance burden

TEST 3: Learn Division (12 ÷ 4) ✓- **Extensibility**: New strategies easily added by implementing the FSM interface

  Crisis: unknown_operation(divide, ...)- **Debugging**: Unified tracing and debugging across all strategies

  Oracle: "CBO (Division)"- **Performance**: Optimized execution engine with consistent performance characteristics

  Result: 12 ÷ 4 = 3

## 5. Getting Started

ALL TESTS PASSED

```### 5.1. Prerequisites

- **SWI-Prolog**: Ensure it is installed and accessible in your system's PATH.

### Run Individual Tests- **Python 3**: Required for the simple web server that serves the frontend files.

```bash

# Test unknown operation crisis detection### 5.2. Running the Web-Based GUI (Recommended)

swipl -g test_all -t halt test_phase_2_crisis_detection.plThis is the easiest way to interact with the semantic and strategy analysis features. This mode uses the stable `working_server.pl`.



# Test oracle strategy wiringIn a terminal, run the provided shell script:

swipl -g test_all -t halt test_phase_1_1_complete.pl```bash

```./start_system.sh

```

### Interactive UseThis script starts both the Prolog API server (on port 8083) and the Python frontend server (on port 3000).

```prolog

?- ['config.pl'].Once the servers are running, open your web browser to: **http://localhost:3000**

?- use_module(execution_handler).

?- use_module(oracle_server).### 5.3. Running the Full ORR System (For Developers)

To experiment with the system's learning capabilities, you need to run the full `api_server.pl`.

% Try an operation (will learn if needed)

?- execution_handler:run_computation(**Step 1: Start the Prolog API Server**

       object_level:subtract(s(s(s(s(s(0))))), s(s(0)), R), ```bash

       50swipl api_server.pl

   ).```

% First time: Crisis → Learn → SuccessThis will start the server on port 8000 (by default).

% R = s(s(s(0)))  % i.e., 5 - 2 = 3

**Step 2: Interact via API Client**

% Try again (uses learned strategy)You can now send POST requests to the endpoints, for example, to trigger the ORR cycle:

?- execution_handler:run_computation(```bash

       object_level:subtract(s(s(s(s(0)))), s(0), R),# This will trigger the ORR cycle for the goal 5 + 5 = X

       50curl -X POST -H "Content-Type: application/json" \

   ).     -d '{"goal": "add(s(s(s(s(s(0))))), s(s(s(s(s(0))))), X)"}' \

% R = s(s(s(0)))  % i.e., 4 - 1 = 3     http://localhost:8000/solve

``````



## How It Works## 6. File Structure Guide



### Example: Learning Subtraction- **Frontend & Visualization**:

  - `index.html`, `script.js`, `style.css`: Frontend files for the web GUI.

**1. Initial State**  - `cognition_viz.html`: Advanced cognitive visualization interface.

```prolog  - `serve_local.py`: A simple Python HTTP server for the frontend.

% Only addition exists  - `start_system.sh`: The main startup script for the web GUI.

object_level:add(A, B, Result) :- 

    enumerate(A, RecA),- **FSM Engine Architecture**:

    enumerate(B, RecB),  - `fsm_engine.pl`: Core finite state machine execution engine providing unified strategy execution.

    append(RecA, RecB, Combined),  - `grounded_arithmetic.pl`: Foundational grounded arithmetic system with cognitive cost tracking.

    recollection(Combined, Result).  - `grounded_utils.pl`: Utility functions supporting grounded arithmetic operations.



% Subtraction NOT defined- **Grounded Fractional Arithmetic System**:

% \+ clause(object_level:subtract(_,_,_), _)  - `composition_engine.pl`: Embodied grouping operations for fractional unit composition.

```  - `fraction_semantics.pl`: Equivalence rules for fractional reasoning (grouping and composition).

  - `grounded_ens_operations.pl`: Core Equal-N-Sharing operations creating nested unit structures.

**2. Attempt**  - `normalization.pl`: Iterative normalization engine applying equivalence rules.

```prolog  - `jason.pl`: Refactored partitive fractional schemes using nested unit representation.

?- run_computation(object_level:subtract(s(s(s(s(s(0))))), s(s(0)), R), 50).  - `test_fractional_arithmetic.pl`: Comprehensive test suite for fractional arithmetic.

```

- **API Server**:

**3. Crisis Detection** (meta_interpreter.pl)  - `working_server.pl`: Production server that powers the web GUI with stable, optimized logic.

```prolog

solve(Goal, ...) :-- **Cognitive Core (ORR Cycle)**:

    functor(ActualGoal, subtract, 3),  - `execution_handler.pl`: Orchestrates the ORR cycle.

    \+ clause(object_level:ActualGoal, _),  - `meta_interpreter.pl`: The "Observe" phase; runs goals and produces traces.

    !,  - `reflective_monitor.pl`: The "Reflect" phase; analyzes traces for disequilibrium.

    throw(perturbation(unknown_operation(subtract, Goal))).  - `reorganization_engine.pl`: The "Reorganize" phase; modifies the knowledge base.

```  - `reorganization_log.pl`: Logs the events of the ORR cycle.



**4. Crisis Handler** (execution_handler.pl)- **Knowledge & Learning**:

```prolog  - `object_level.pl`: The initial, dynamic knowledge base of the system.

handle_perturbation(perturbation(unknown_operation(Op, Goal)), ...) :-  - `incompatibility_semantics.pl`: The core rules of logic and mathematics, providing modal logic operators.

    % Get first strategy  - `more_machine_learner.pl`: The module that implements the "protein folding" learning analogy.

    oracle_server:list_available_strategies(subtract, [FirstStrategy|_]),  - `learned_knowledge.pl`: **Auto-generated file** for storing learned strategies. Do not edit manually.

    % FirstStrategy = 'COBO (Missing Addend)'

    - **Student Strategy Models (FSM Engine Powered)**:

    % Consult oracle  - `sar_*.pl`: Models for Student Addition and Subtraction Reasoning (all converted to FSM engine).

    consult_oracle_for_solution(Goal, FirstStrategy, Result, Interpretation),  - `smr_*.pl`: Models for Student Multiplication and Division Reasoning (all converted to FSM engine).

    % Result = 3  - `hermeneutic_calculator.pl`: A dispatcher to run specific student strategies.

    % Interpretation = "Count on from subtrahend: Start at 2, count up to 5, gap is 3"

    - **Testing & Validation**:

    % Synthesize strategy  - `test_basic_functionality.pl`: Basic functionality tests for core components.

    synthesize_from_oracle(...),  - `test_comprehensive.pl`: Comprehensive testing suite for the entire system.

      - `test_orr_cycle.pl`: Specific tests for the ORR learning cycle.

    % Retry  - `test_synthesis.pl`: `plunit` tests for the `incompatibility_semantics` module.

    run_computation(Goal, Limit).  - `test_full_loop.pl`: End-to-end testing of the complete system.

```

- **Command-Line Interfaces**:

**5. Synthesis** (fsm_synthesis_engine.pl)  - `main.pl`: A simple entry point to run a test query through the ORR cycle.

```prolog  - `interactive_ui.pl`: A text-based menu for interacting with the learning system.

synthesize_strategy_from_oracle(..., subtract, ...) :-

    % Create oracle-backed strategy- **Configuration & Utilities**:

    assertz((  - `config.pl`: System configuration settings.

        object_level:subtract(A, B, Result) :-  - `jason.pl`: Fraction and arithmetic helper functions.

            peano_to_int(A, IntA),  - `strategies.pl`: Strategy coordination and management.

            peano_to_int(B, IntB),  - `counting2.pl`, `counting_on_back.pl`: Additional counting strategies.

            oracle_server:query_oracle(  - Various Python scripts for external interfaces and testing.

                subtract(IntA, IntB), 

                'COBO (Missing Addend)', ## 7. For Developers

                IntResult, 

                _### 7.1. FSM Engine Architecture

            ),All student strategy models have been converted to use the unified FSM engine. When implementing new strategies:

            int_to_peano(IntResult, Result)- Implement `transition/4` predicates defining state transitions

    )).- Use modal logic operators (`s/1`, `comp_nec/1`, `exp_poss/1`) in transitions

```- Include cognitive cost tracking with `incur_cost/1`

- Provide `accept_state/1`, `final_interpretation/2`, and `extract_result_from_history/2` predicates

**6. Retry → Success**- Call `run_fsm_with_base(ModuleName, InitialState, Parameters, Base, History)` to execute

```prolog

% Now subtraction works!### 7.2. Running Tests

object_level:subtract(s(s(s(s(s(0))))), s(s(0)), R).The repository uses `plunit` for testing. The main test files include:

% R = s(s(s(0)))  % 5 - 2 = 3- `test_synthesis.pl`: Tests for the `incompatibility_semantics` module

```- `test_basic_functionality.pl`: Basic system functionality tests  

- `test_comprehensive.pl`: Comprehensive system testing

## Available Strategies (Oracle Knowledge)- `test_orr_cycle.pl`: ORR cycle specific tests



### Addition (4 strategies)To run the tests, start SWI-Prolog and run:

- **COBO** - Count On from Bigger Operand```prolog

- **Chunking** - Decompose and recombine?- [test_synthesis].

- **RMB** - Round, Manipulate, Balance?- run_tests.

- **Rounding** - Round to nearest 10```



### Subtraction (8 strategies)### 7.3. Code Documentation

- **COBO (Missing Addend)** - Count on to find gapThe Prolog source code is documented using **PlDoc**. This format allows for generating HTML documentation directly from the source comments.

- **CBBO (Take Away)** - Count back

- **Decomposition** - Break into parts## 8. Contributing

- **Rounding** - Round and adjustWe welcome contributions to the theoretical development, the Prolog implementation, and the frontend interface. Please open an issue to discuss potential changes.

- **Sliding** - Slide both operands

- **Chunking A/B/C** - Various chunk-based approaches## 9. License

[Note: Specify your license here.]
### Multiplication (4 strategies)
- **C2C** - Count-to-Count (repeated addition)
- **CBO** - Count By Ones to base
- **Commutative** - Use commutativity (swap operands)
- **DR** - Distributive Reasoning

### Division (4 strategies)
- **Dealing** - Deal into groups
- **CBO** - Count By Ones
- **UCR** - Unit Conversion Reasoning
- **IDP** - Iterative Decomposition Product (requires multiplication facts)

## Key Files

### Core System
- `meta_interpreter.pl` - Embodied execution with crisis detection
- `execution_handler.pl` - ORR cycle controller, crisis handling
- `object_level.pl` - Dynamic knowledge base (starts primordial)
- `oracle_server.pl` - Black box expert interface
- `fsm_synthesis_engine.pl` - Strategy synthesis
- `config.pl` - System configuration (inference limits)
- `grounded_arithmetic.pl` - Primitive operations (successor, enumerate, etc.)

### Strategy Modules (Oracle Knowledge)
- `sar_add_*.pl` - Addition strategies (4 files)
- `sar_sub_*.pl` - Subtraction strategies (8 files)
- `smr_mult_*.pl` - Multiplication strategies (4 files)
- `smr_div_*.pl` - Division strategies (4 files)

### Tests
- `test_phase_3_simple.pl` - **Main bootstrap test** (sequential learning)
- `test_phase_2_crisis_detection.pl` - Unknown operation detection
- `test_phase_1_1_complete.pl` - Oracle strategy wiring (19/20 functional)

### Documentation
- `README.md` (this file) - System overview and usage
- `ARCHITECTURE.md` - Detailed system architecture
- `BOOTSTRAP_CHECKLIST.md` - Implementation roadmap and status
- `DIVASION_ARCHITECTURE.md` - Philosophical foundations
- `REFACTORING_CHECKLIST.md` - Historical development log

## Design Principles

### 1. Crisis-Driven Learning (Piaget)
- System does NOT learn proactively
- Learning ONLY triggered by computational inadequacy
- Disequilibrium → Accommodation → New equilibrium

### 2. Encultured Cognition (Vygotsky)
- Oracle represents cultural/expert mathematical knowledge
- Learning through cultural transmission (not pure discovery)
- Expert guidance legitimate (not "cheating")

### 3. Hermeneutic Understanding (Gadamer)
- Oracle provides interpretations, not just results
- System "understands" through expert explanations
- Example: "Count on from subtrahend: Start at 3, count up to 7, the gap is 4"

### 4. Computational Divasion
- System simultaneously INSIDE (executing) and OUTSIDE (reflecting)
- Meta-interpreter creates architectural entanglement
- Crisis = moment when self-unity breaks down

### 5. Oracle-Backed Strategies
- Strategies call oracle (not standalone FSM code)
- Legitimate because:
  - Crisis-driven (only learned when needed)
  - Cultural authority (oracle = expert knowledge)
  - Genuine accommodation (capability expansion)
  - Hermeneutic (provides understanding)

## Configuration

Edit `config.pl` to adjust system parameters:

```prolog
% Inference limit (forces crisis on complex problems)
max_inferences(10).  % Default: triggers crisis on add(8,5)

% Cognitive costs (embodied computation)
cognitive_cost(recollection, 1).  % Per tally
cognitive_cost(modal_shift, 3).   % Per modal operator
```

## Testing

Run all tests:
```bash
# Bootstrap test (recommended)
swipl -g test_all -t halt test_phase_3_simple.pl

# Crisis detection
swipl -g test_all -t halt test_phase_2_crisis_detection.pl

# Oracle wiring (all 20 strategies)
swipl -g test_all -t halt test_phase_1_1_complete.pl

# Complete system test
swipl -g test_all -t halt test_complete_system.pl
```

## Current Status

✅ **Phases 1-3 Complete**
- Phase 1: Oracle strategy wiring (19/20 functional)
- Phase 2: Unknown operation crisis detection
- Phase 3: Sequential bootstrap (primordial → full arithmetic)

### Test Results
```
Phase 2: Crisis Detection - ALL PASSING ✓
  - subtract(5,3): Crisis → Learn → Success
  - multiply(3,4): Crisis → Learn → Success
  - divide(12,3): Crisis → Learn → Success

Phase 3: Sequential Bootstrap - ALL PASSING ✓
  - subtract(7,3): unknown_operation → accommodate → success
  - multiply(4,3): unknown_operation → accommodate → success
  - divide(12,4): unknown_operation → accommodate → success
```

### Known Limitations
- **IDP division** requires learned multiplication facts (correct behavior, not a bug)
- **One strategy per operation** - currently learns first available only
- **Oracle-backed** - strategies call oracle rather than being standalone FSM code
- **No persistence** - learned strategies not saved between sessions (yet)

## Future Work

### Short Term
- Strategy persistence (save/load learned knowledge)
- Multiple strategies per operation (learn through different crises)
- Full 20+ problem curriculum

### Long Term
- True FSM synthesis (generate standalone code from oracle guidance)
- Strategy selection based on problem characteristics
- Cost function for strategy preference
- Fraction arithmetic extension

## Philosophical Claims

This system demonstrates:
1. ✅ **Genuine emergent learning** - No pre-loaded templates or patterns
2. ✅ **Computational hermeneutics** - Recognition of structure, not imitation
3. ✅ **Crisis-driven accommodation** - Learning only from inadequacy
4. ✅ **Enactivist cognition** - Embodied, situated, crisis-driven
5. ✅ **Computational autoethnography** - System studies its own development
6. ✅ **Self-transcendence** - Bootstraps within classical formal constraints

## License

Research code - see repository for details.

## Citation

If you use this system in research, please cite the UMEDCA (Universal Machine for Enactive, Developmentally-grounded Computational Arithmetic) project.

## Contact

See repository for contact information and contribution guidelines.
