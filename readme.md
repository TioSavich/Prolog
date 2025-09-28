# A Synthesis of Incompatibility Semantics, CGI, and Piagetian Constructivism

## 1. Introduction

This project presents a novel synthesis of three influential frameworks in philosophy, cognitive science, and education, implemented as a computational model in SWI-Prolog.

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

## 3. System Architecture

The system is composed of several distinct parts that work together.

### 3.1. The ORR Cycle (Cognitive Core)
This is the heart of the system's learning capability, inspired by Piagetian mechanisms.
- **`execution_handler.pl`**: The main driver that orchestrates the ORR cycle.
- **`meta_interpreter.pl`**: The **Observe** phase. It runs a given goal while producing a detailed execution trace, making the system's reasoning process observable to itself.
- **`reflective_monitor.pl`**: The **Reflect** phase. It analyzes the trace from the meta-interpreter to detect signs of "disequilibrium" (e.g., goal failures, contradictions).
- **`reorganization_engine.pl`**: The **Reorganize** phase. Triggered by disequilibrium, it attempts to modify the system's own knowledge base to resolve the conflict.

### 3.2. Knowledge Base
- **`object_level.pl`**: Contains the system's foundational, and potentially flawed, knowledge (e.g., an inefficient rule for addition). This is the knowledge that the ORR cycle operates on and modifies.
- **`incompatibility_semantics.pl`**: Defines the core logical and mathematical rules of the "world," including what concepts are incompatible with each other.
- **`learned_knowledge.pl`**: An auto-generated file where new, more efficient strategies discovered by the `more_machine_learner.pl` module are stored.

### 3.3. API Servers
There are multiple servers for different purposes:
- **`working_server.pl`**: A robust, simplified server for powering the web-based GUI. It contains hard-coded analysis logic for stability.
- **`api_server.pl`**: The full-featured development server that exposes the entire ORR cycle and the dynamic knowledge base.
- **`simple_api_server.pl`** and **`test_server.pl`**: Lightweight servers for testing and demonstration.

## 4. Getting Started

### 4.1. Prerequisites
- **SWI-Prolog**: Ensure it is installed and accessible in your system's PATH.
- **Python 3**: Required for the simple web server that serves the frontend files.

### 4.2. Running the Web-Based GUI (Recommended)
This is the easiest way to interact with the semantic and strategy analysis features. This mode uses the stable `working_server.pl`.

In a terminal, run the provided shell script:
```bash
./start_system.sh
```
This script starts both the Prolog API server (on port 8083) and the Python frontend server (on port 3000).

Once the servers are running, open your web browser to: **http://localhost:3000**

### 4.3. Running the Full ORR System (For Developers)
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

## 5. File Structure Guide

- **`index.html`, `script.js`, `style.css`**: Frontend files for the web GUI.
- **`serve_local.py`**: A simple Python HTTP server for the frontend.
- **`start_system.sh`**: The main startup script for the web GUI.

- **API Servers**:
  - `working_server.pl`: Powers the web GUI with stable, hard-coded logic.
  - `api_server.pl`: Full-featured server with access to the ORR learning cycle.
  - `simple_api_server.pl`, `test_server.pl`: Minimal servers for testing.

- **Cognitive Core (ORR Cycle)**:
  - `execution_handler.pl`: Orchestrates the ORR cycle.
  - `meta_interpreter.pl`: The "Observe" phase; runs goals and produces traces.
  - `reflective_monitor.pl`: The "Reflect" phase; analyzes traces for disequilibrium.
  - `reorganization_engine.pl`: The "Reorganize" phase; modifies the knowledge base.
  - `reorganization_log.pl`: Logs the events of the ORR cycle.

- **Knowledge & Learning**:
  - `object_level.pl`: The initial, dynamic knowledge base of the system.
  - `incompatibility_semantics.pl`: The core rules of logic and mathematics.
  - `more_machine_learner.pl`: The module that implements the "protein folding" learning analogy.
  - `learned_knowledge.pl`: **Auto-generated file** for storing learned strategies. Do not edit manually.

- **Student Strategy Models**:
  - `sar_*.pl`: Models for Student Addition and Subtraction Reasoning.
  - `smr_*.pl`: Models for Student Multiplication and Division Reasoning.
  - `hermeneutic_calculator.pl`: A dispatcher to run specific student strategies.

- **Command-Line Interfaces & Tests**:
  - `main.pl`: A simple entry point to run a test query through the ORR cycle.
  - `interactive_ui.pl`: A text-based menu for interacting with the learning system.
  - `test_synthesis.pl`: `plunit` tests for the `incompatibility_semantics` module.

## 6. For Developers

### 6.1. Running Tests
The repository uses `plunit` for testing. The main test file is `test_synthesis.pl`. To run the tests, start SWI-Prolog and run:
```prolog
?- [test_synthesis].
?- run_tests.
```

### 6.2. Code Documentation
The Prolog source code is documented using **PlDoc**. This format allows for generating HTML documentation directly from the source comments.

## 7. Contributing
We welcome contributions to the theoretical development, the Prolog implementation, and the frontend interface. Please open an issue to discuss potential changes.

## 8. License
[Note: Specify your license here.]