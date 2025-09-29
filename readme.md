# A Synthesis of Incompatibility Semantics, CGI, and Piagetian Constructivism with FSM Engine Architecture

## 1. Introduction

This project presents a novel synthesis of three influential frameworks in philosophy, cognitive science, and education, implemented as a computational model in SWI-Prolog with a unified Finite State Machine (FSM) engine architecture.

*   **Robert Brandom's Incompatibility Semantics:** A theory asserting that the meaning of a concept is defined by what it is incompatible with. We understand what something *is* by understanding what it rules out.
*   **Cognitively Guided Instruction (CGI):** An educational approach focused on understanding and building upon students' intuitive problem-solving strategies.
*   **Piagetian Constructivism:** A theory of cognitive development emphasizing the learner's active construction of knowledge through assimilation and accommodation, driven by the resolution of cognitive conflict (disequilibrium).

This synthesis aims to provide a formal, computational model for understanding conceptual development and designing instruction that respects the learner's constructive processes.

## 2. Core Concepts

The core idea of this synthesis is that learning (Constructivism) occurs when a learner recognizes an incompatibility (Brandom) between their existing cognitive structures and new information or experiences. Instruction (CGI) facilitates this process by analyzing the learner's current strategies and introducing experiences that highlight relevant incompatibilities, prompting the necessary cognitive shifts (accommodation).

This is modeled in the repository through several key components:
- **Incompatibility Semantics**: The core logic for determining entailment and contradiction is implemented in `incompatibility_semantics.pl`.
- **Student Strategy Models**: The CGI aspect is modeled through a library of student problem-solving strategies (`sar_*.pl` for addition/subtraction and `smr_*.pl` for multiplication/division), which simulate how students with different conceptual understandings might approach a problem.
- **Learning Cycle**: The Piagetian process of learning through disequilibrium is modeled by the **Observe-Reorganize-Reflect (ORR)** cycle, which can detect failures in its own knowledge and attempt to repair itself.
- **FSM Engine Architecture**: All student strategy models are unified under a common Finite State Machine engine that provides consistent execution, modal logic integration, and cognitive cost tracking.

## 3. System Architecture

The system is composed of several distinct parts that work together, unified by a common FSM engine architecture.

### 3.1. FSM Engine Architecture (New Core Framework)
A unified finite state machine engine that standardizes all student strategy execution:
- **`fsm_engine.pl`**: The core FSM execution engine that provides consistent state transition handling, modal logic integration, and cognitive cost tracking across all student strategies.
- **`grounded_arithmetic.pl`**: The foundational grounded arithmetic system that eliminates dependency on arithmetic backstops by providing embodied mathematical operations with cognitive cost tracking.
- **`grounded_utils.pl`**: Utility functions supporting the grounded arithmetic foundation.

### 3.2. The ORR Cycle (Cognitive Core)
This is the heart of the system's learning capability, inspired by Piagetian mechanisms.
- **`execution_handler.pl`**: The main driver that orchestrates the ORR cycle.
- **`meta_interpreter.pl`**: The **Observe** phase. It runs a given goal while producing a detailed execution trace, making the system's reasoning process observable to itself.
- **`reflective_monitor.pl`**: The **Reflect** phase. It analyzes the trace from the meta-interpreter to detect signs of "disequilibrium" (e.g., goal failures, contradictions).
- **`reorganization_engine.pl`**: The **Reorganize** phase. Triggered by disequilibrium, it attempts to modify the system's own knowledge base to resolve the conflict.

### 3.3. Knowledge Base
- **`object_level.pl`**: Contains the system's foundational, and potentially flawed, knowledge (e.g., an inefficient rule for addition). This is the knowledge that the ORR cycle operates on and modifies.
- **`incompatibility_semantics.pl`**: Defines the core logical and mathematical rules of the "world," including what concepts are incompatible with each other, and provides modal logic operators (s/1, comp_nec/1, exp_poss/1).
- **`learned_knowledge.pl`**: An auto-generated file where new, more efficient strategies discovered by the `more_machine_learner.pl` module are stored.

### 3.4. API Server
- **`working_server.pl`**: The production-ready server for powering the web-based GUI. It contains stable, optimized analysis logic and is used by the startup script.

## 4. FSM Engine Architecture (Major Innovation)

This system features a revolutionary **Finite State Machine (FSM) Engine** that unifies all student strategy models under a common computational framework. This represents a significant architectural advancement providing:

### 4.1. Unified Execution Model
- **Consistent Interface**: All 17+ student strategies (`sar_*.pl`, `smr_*.pl`) use the same FSM engine interface via `run_fsm_with_base/5`
- **Code Reduction**: ~70% reduction in duplicate state machine code across strategy files
- **Standardized Transitions**: All strategies use `transition/4` predicates with consistent parameter patterns

### 4.2. Modal Logic Integration
- **Cognitive Operators**: Every state transition integrates modal logic operators:
  - `s/1`: Basic cognitive operations and state changes
  - `comp_nec/1`: Necessary computational steps and systematic processes  
  - `exp_poss/1`: Possible expansions and completion states
- **Semantic Grounding**: Modal operators provide semantic meaning to computational steps, connecting to Brandom's incompatibility semantics

### 4.3. Cognitive Cost Tracking
- **Embodied Cognition**: Every cognitive operation has an associated cost via `incur_cost/1`
- **Resource Awareness**: The system tracks computational resources as cognitive resources
- **Performance Analysis**: Enables comparison of strategy efficiency in cognitive terms

### 4.4. Grounded Arithmetic Foundation  
- **Elimination of Arithmetic Backstop**: No reliance on hardcoded arithmetic; all operations are grounded in embodied cognitive processes
- **Constructivist Mathematics**: Numbers and operations emerge from cognitive actions rather than being pre-given
- **Peano Arithmetic**: Foundation built on successor functions and recursive operations

### 4.5. FSM Engine Benefits
- **Maintainability**: Single engine handles all strategy execution, reducing maintenance burden
- **Extensibility**: New strategies easily added by implementing the FSM interface
- **Debugging**: Unified tracing and debugging across all strategies
- **Performance**: Optimized execution engine with consistent performance characteristics

## 5. Getting Started

### 5.1. Prerequisites
- **SWI-Prolog**: Ensure it is installed and accessible in your system's PATH.
- **Python 3**: Required for the simple web server that serves the frontend files.

### 5.2. Running the Web-Based GUI (Recommended)
This is the easiest way to interact with the semantic and strategy analysis features. This mode uses the stable `working_server.pl`.

In a terminal, run the provided shell script:
```bash
./start_system.sh
```
This script starts both the Prolog API server (on port 8083) and the Python frontend server (on port 3000).

Once the servers are running, open your web browser to: **http://localhost:3000**

### 5.3. Running the Full ORR System (For Developers)
To experiment with the system's learning capabilities, you need to run the full `api_server.pl`.

**Step 1: Start the Prolog API Server**
```bash
swipl api_server.pl
```
This will start the server on port 8000 (by default).

**Step 2: Interact via API Client**
You can now send POST requests to the endpoints, for example, to trigger the ORR cycle:
```bash
# This will trigger the ORR cycle for the goal 5 + 5 = X
curl -X POST -H "Content-Type: application/json" \
     -d '{"goal": "add(s(s(s(s(s(0))))), s(s(s(s(s(0))))), X)"}' \
     http://localhost:8000/solve
```

## 6. File Structure Guide

- **Frontend & Visualization**:
  - `index.html`, `script.js`, `style.css`: Frontend files for the web GUI.
  - `cognition_viz.html`: Advanced cognitive visualization interface.
  - `serve_local.py`: A simple Python HTTP server for the frontend.
  - `start_system.sh`: The main startup script for the web GUI.

- **FSM Engine Architecture**:
  - `fsm_engine.pl`: Core finite state machine execution engine providing unified strategy execution.
  - `grounded_arithmetic.pl`: Foundational grounded arithmetic system with cognitive cost tracking.
  - `grounded_utils.pl`: Utility functions supporting grounded arithmetic operations.

- **API Server**:
  - `working_server.pl`: Production server that powers the web GUI with stable, optimized logic.

- **Cognitive Core (ORR Cycle)**:
  - `execution_handler.pl`: Orchestrates the ORR cycle.
  - `meta_interpreter.pl`: The "Observe" phase; runs goals and produces traces.
  - `reflective_monitor.pl`: The "Reflect" phase; analyzes traces for disequilibrium.
  - `reorganization_engine.pl`: The "Reorganize" phase; modifies the knowledge base.
  - `reorganization_log.pl`: Logs the events of the ORR cycle.

- **Knowledge & Learning**:
  - `object_level.pl`: The initial, dynamic knowledge base of the system.
  - `incompatibility_semantics.pl`: The core rules of logic and mathematics, providing modal logic operators.
  - `more_machine_learner.pl`: The module that implements the "protein folding" learning analogy.
  - `learned_knowledge.pl`: **Auto-generated file** for storing learned strategies. Do not edit manually.

- **Student Strategy Models (FSM Engine Powered)**:
  - `sar_*.pl`: Models for Student Addition and Subtraction Reasoning (all converted to FSM engine).
  - `smr_*.pl`: Models for Student Multiplication and Division Reasoning (all converted to FSM engine).
  - `hermeneutic_calculator.pl`: A dispatcher to run specific student strategies.

- **Testing & Validation**:
  - `test_basic_functionality.pl`: Basic functionality tests for core components.
  - `test_comprehensive.pl`: Comprehensive testing suite for the entire system.
  - `test_orr_cycle.pl`: Specific tests for the ORR learning cycle.
  - `test_synthesis.pl`: `plunit` tests for the `incompatibility_semantics` module.
  - `test_full_loop.pl`: End-to-end testing of the complete system.

- **Command-Line Interfaces**:
  - `main.pl`: A simple entry point to run a test query through the ORR cycle.
  - `interactive_ui.pl`: A text-based menu for interacting with the learning system.

- **Configuration & Utilities**:
  - `config.pl`: System configuration settings.
  - `jason.pl`: Fraction and arithmetic helper functions.
  - `strategies.pl`: Strategy coordination and management.
  - `counting2.pl`, `counting_on_back.pl`: Additional counting strategies.
  - Various Python scripts for external interfaces and testing.

## 7. For Developers

### 7.1. FSM Engine Architecture
All student strategy models have been converted to use the unified FSM engine. When implementing new strategies:
- Implement `transition/4` predicates defining state transitions
- Use modal logic operators (`s/1`, `comp_nec/1`, `exp_poss/1`) in transitions
- Include cognitive cost tracking with `incur_cost/1`
- Provide `accept_state/1`, `final_interpretation/2`, and `extract_result_from_history/2` predicates
- Call `run_fsm_with_base(ModuleName, InitialState, Parameters, Base, History)` to execute

### 7.2. Running Tests
The repository uses `plunit` for testing. The main test files include:
- `test_synthesis.pl`: Tests for the `incompatibility_semantics` module
- `test_basic_functionality.pl`: Basic system functionality tests  
- `test_comprehensive.pl`: Comprehensive system testing
- `test_orr_cycle.pl`: ORR cycle specific tests

To run the tests, start SWI-Prolog and run:
```prolog
?- [test_synthesis].
?- run_tests.
```

### 7.3. Code Documentation
The Prolog source code is documented using **PlDoc**. This format allows for generating HTML documentation directly from the source comments.

## 8. Contributing
We welcome contributions to the theoretical development, the Prolog implementation, and the frontend interface. Please open an issue to discuss potential changes.

## 9. License
[Note: Specify your license here.]