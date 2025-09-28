# A Synthesis of Incompatibility Semantics, CGI, and Piagetian Constructivism

## Introduction

This project presents a novel synthesis of three influential frameworks in philosophy, cognitive science, and education:

*   **Robert Brandom's Incompatibility Semantics:** A theory asserting that the meaning of a concept is defined by what it is incompatible with. We understand what something *is* by understanding what it rules out.
*   **Cognitively Guided Instruction (CGI):** An educational approach focused on understanding and building upon students' intuitive problem-solving strategies.
*   **Piagetian Constructivism:** A theory of cognitive development emphasizing the learner's active construction of knowledge through assimilation and accommodation, driven by the resolution of cognitive conflict (disequilibrium).

This synthesis aims to provide a formal, computational model (implemented in Prolog) for understanding conceptual development and designing instruction that respects the learner's constructive processes.

## Theoretical Background

The core idea of this synthesis is that learning (Constructivism) occurs when a learner recognizes an incompatibility (Brandom) between their existing cognitive structures and new information or experiences. Instruction (CGI) facilitates this process by analyzing the learner's current strategies and introducing experiences that highlight relevant incompatibilities, prompting the necessary cognitive shifts (accommodation).

## Features

*   **Concept Explorer (Brandom):** A tool to define and explore the semantic relationships between concepts based on what they entail and what they exclude.
*   **Strategy Analyzer (CGI/Piaget):** An interface to input observed student strategies and receive an analysis of the underlying conceptual structures, the likely developmental stage, and pedagogical recommendations.
*   **Prolog Backend:** A robust logical engine that manages the knowledge base and performs inferences.

## How to Use (Web-Based GUI)

To make this framework accessible, we provide a web-based Graphical User Interface (GUI). This interface allows interaction with the underlying logic without requiring knowledge of Prolog.

### Running Locally with VS Code

#### Prerequisites
- SWI-Prolog installed on your system
- Python 3 (for serving HTML files)
- VS Code (optional, but recommended for development)

#### Quick Start (Recommended)

Open a terminal in VS Code and run:

```bash
cd /Users/tio/Documents/GitHub/Prolog
./start_system.sh
```

This single script will:
- Start the Prolog API server on port 8083
- Start the frontend HTTP server on port 3000  
- Display status messages and instructions
- Handle cleanup when you press Ctrl+C

Then open your browser to: **http://localhost:3000**

#### Manual Start (Alternative)

If you prefer to start the servers manually:

**Step 1: Start the Prolog API Server**
```bash
cd /Users/tio/Documents/GitHub/Prolog
swipl -g "main" working_server.pl
```

**Step 2: Start the Frontend Server** (in a new terminal)
```bash
cd /Users/tio/Documents/GitHub/Prolog
python3 serve_local.py
```

**Step 3: Access the Interface**
Open your web browser and navigate to: **http://localhost:3000**

### Using the Interface

The GUI is divided into two main tabs:

1.  **Concept Explorer:**
    *   Enter a statement or concept (e.g., "The shape is a square").
    *   Click "Analyze" to see what the system infers about this statement, including its entailments and its incompatibilities.
2.  **Strategy Analyzer:**
    *   Select a problem context (e.g., "Early Mathematics - Addition").
    *   Describe the student's observed strategy (e.g., "The student counted out both sets separately and then counted all the items starting from one").
    *   Click "Analyze Strategy" to see the CGI classification, the conceptual implications, and recommendations for inducing productive disequilibrium.

### Troubleshooting

- **Connection Error**: Ensure both servers are running. The Prolog server should be on port 8083 and the HTTP server on port 3000.
- **CORS Issues**: Use the provided `serve_local.py` script rather than opening `index.html` directly in your browser.
- **Port Conflicts**: If port 8083 or 3000 are in use, you can modify the ports in `working_server.pl` and `script.js` respectively.
- **Server Won't Start**: Try using the `start_system.sh` script which handles process management automatically.
- **Prolog Errors**: The `working_server.pl` is a simplified version that avoids complex module dependencies.

## For Developers

The system consists of:
- **Prolog Backend** (`working_server.pl`): Simplified server that handles semantic analysis and CGI strategy classification
- **Web Frontend** (`index.html`, `script.js`, `style.css`): User interface that makes API calls to the Prolog server
- **Local Server** (`serve_local.py`): Simple Python HTTP server for local development
- **Startup Script** (`start_system.sh`): Automated script to start both servers
- **Original Server** (`api_server.pl`): More complex server with full ORR system integration (may require additional setup)

### API Endpoints

The Prolog server (running on port 8083) provides the following REST endpoints:

- `POST /analyze_semantics`: Analyzes statements using incompatibility semantics
- `POST /analyze_strategy`: Analyzes student strategies using CGI frameworks  
- `GET /test`: Simple health check endpoint

**Additional endpoints in the full system** (`api_server.pl`):
- `POST /solve`: Runs the ORR cycle for problem solving
- `GET /log`: Returns reorganization logs
- `GET /knowledge`: Returns current knowledge base

### Development Workflow

1. Make changes to Prolog files (mainly `working_server.pl`)
2. Stop the system with Ctrl+C and restart with `./start_system.sh`
3. Refresh the browser to see changes in the frontend
4. For frontend changes (`script.js`, `style.css`, `index.html`), just refresh the browser

### File Structure

**Core Files:**
- `working_server.pl` - Main Prolog API server (simplified, working version)
- `script.js` - Frontend JavaScript with API calls
- `index.html` - Web interface
- `start_system.sh` - Automated startup script
- `serve_local.py` - Local HTTP server for frontend

**Additional Files:**
- `api_server.pl` - Full-featured server (may need additional setup)
- `incompatibility_semantics.pl` - Core semantic analysis logic
- Various strategy modules (`sar_*.pl`, `smr_*.pl`) - Mathematical strategy implementations

## Contributing

We welcome contributions to the theoretical development, the Prolog implementation, and the frontend interface.

## License

[**Note:** *Specify your license here.*]