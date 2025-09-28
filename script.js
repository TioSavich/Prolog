// --- Configuration ---
const API_BASE_URL = 'http://localhost:8083';

// --- Prolog API Backend ---
const PrologBackend = {
    // Brandom's Incompatibility Semantics
    async analyzeSemantics(statement) {
        try {
            const response = await fetch(`${API_BASE_URL}/analyze_semantics`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ statement: statement })
            });
            
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            
            return await response.json();
        } catch (error) {
            console.error('Error analyzing semantics:', error);
            return {
                statement: statement,
                implies: ['Error: Could not connect to Prolog server'],
                incompatibleWith: [`Please ensure the Prolog server is running on port ${API_BASE_URL.split(':').pop()}`]
            };
        }
    },

    // CGI and Piagetian Analysis
    async analyzeStrategy(problemContext, strategyDescription) {
        try {
            const response = await fetch(`${API_BASE_URL}/analyze_strategy`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ 
                    problemContext: problemContext,
                    strategy: strategyDescription 
                })
            });
            
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            
            return await response.json();
        } catch (error) {
            console.error('Error analyzing strategy:', error);
            return {
                classification: "Connection Error",
                stage: "Unknown",
                implications: `Could not connect to Prolog server. Please ensure the server is running.`,
                incompatibility: "",
                recommendations: `Check that the Prolog API server is started and accessible at ${API_BASE_URL}.`
            };
        }
    }
};

// --- Frontend Logic ---

function openTab(evt, tabName) {
    var i, tabcontent, tablinks;

    tabcontent = document.getElementsByClassName("tab-content");
    for (i = 0; i < tabcontent.length; i++) {
        tabcontent[i].classList.remove("active");
    }

    tablinks = document.getElementsByClassName("tab-button");
    for (i = 0; i < tablinks.length; i++) {
        tablinks[i].classList.remove("active");
    }

    document.getElementById(tabName).classList.add("active");
    // Check if evt is defined (for the initial load)
    if (evt) {
        evt.currentTarget.classList.add("active");
    }
}

async function analyzeIncompatibility() {
    const input = document.getElementById('conceptInput').value;
    const resultDiv = document.getElementById('incompatibilityResult');

    if (!input.trim()) {
        resultDiv.innerHTML = "<i>Please enter a statement to analyze.</i>";
        return;
    }

    // Show loading state
    resultDiv.innerHTML = "<i>Analyzing...</i>";

    const results = await PrologBackend.analyzeSemantics(input);

    if (results) {
        let html = `<h3>Semantic Analysis for: "${results.statement}"</h3>`;

        html += `<h4>Entailments (What it implies):</h4><ul>`;
        results.implies.forEach(item => {
            html += `<li>${item}</li>`;
        });
        html += `</ul>`;

        html += `<h4>Incompatibilities (What it excludes):</h4><ul>`;
        results.incompatibleWith.forEach(item => {
            html += `<li>${item}</li>`;
        });
        html += `</ul>`;

        resultDiv.innerHTML = html;
    } else {
        resultDiv.innerHTML = "<i>Error occurred during analysis.</i>";
    }
}

async function analyzeCGI() {
    const problemContext = document.getElementById('problemContext').value;
    const strategyInput = document.getElementById('strategyInput').value;
    const resultDiv = document.getElementById('cgiResult');

    if (!strategyInput.trim()) {
        resultDiv.innerHTML = "<i>Please describe the student's strategy.</i>";
        return;
    }

    // Show loading state
    resultDiv.innerHTML = "<i>Analyzing strategy...</i>";

    const analysis = await PrologBackend.analyzeStrategy(problemContext, strategyInput);

    if (analysis) {
        let html = `<h3>Analysis Results</h3>`;
        html += `<p><strong>Context:</strong> ${problemContext}</p>`;

        if (analysis.classification !== "Unclassified" && analysis.classification !== "Connection Error") {
            html += `<p><strong>Strategy Classification (CGI):</strong> ${analysis.classification}</p>`;
            html += `<p><strong>Developmental Stage (Piaget):</strong> ${analysis.stage}</p>`;
        }

        html += `<h4>Conceptual Implications:</h4><p>${analysis.implications}</p>`;

        if (analysis.incompatibility) {
            html += `<h4>Semantic Conflict:</h4>`;
            html += `<div class="incompatibility-highlight">${analysis.incompatibility}</div>`;
        }

        if (analysis.recommendations) {
            html += `<h4>Pedagogical Recommendations:</h4><p>${analysis.recommendations}</p>`;
        }

        resultDiv.innerHTML = html;

    } else {
        resultDiv.innerHTML = "<i>Error occurred during analysis.</i>";
    }
}

// Initialize the first tab on load
document.addEventListener('DOMContentLoaded', (event) => {
    //openTab(null, 'CGI');
});