document.addEventListener('DOMContentLoaded', () => {
    const form = document.getElementById('queryForm');
    const inputA = document.getElementById('inputA');
    const inputB = document.getElementById('inputB');
    const inputLimit = document.getElementById('inputLimit');
    const terminalBody = document.getElementById('terminalBody');
    const strategyList = document.getElementById('strategyList');
    const budgetProgress = document.getElementById('budgetProgress');
    const budgetLabel = document.getElementById('budgetVisualLabel');
    const resetBtn = document.getElementById('resetBtn');
    const runBtn = document.getElementById('runBtn');
    const calcOp = document.getElementById('calcOp');
    const calcStrategy = document.getElementById('calcStrategy');
    const calcOperandA = document.getElementById('calcOperandA');
    const calcOperandB = document.getElementById('calcOperandB');
    const calcResult = document.getElementById('calcResult');
    const calcRunBtn = document.getElementById('calcRunBtn');
    
    // API endpoints
    const API_BASE = 'http://localhost:8080/api';
    const HERMENEUTIC_API = 'http://localhost:8083';

    // State tracker
    let isComputing = false;

    // Load initial state
    fetchState();
    if (calcOp && calcStrategy) {
        loadHermeneuticStrategies();
        calcOp.addEventListener('change', loadHermeneuticStrategies);
    }
    if (calcRunBtn) {
        calcRunBtn.addEventListener('click', runHermeneuticCalculation);
    }

    form.addEventListener('submit', async (e) => {
        e.preventDefault();
        if (isComputing) return;

        const a = parseInt(inputA.value, 10);
        const b = parseInt(inputB.value, 10);
        const limit = parseInt(inputLimit.value, 10);
        const op = document.getElementById('opSelect').value;

        await runQuery(op, a, b, limit);
    });

    resetBtn.addEventListener('click', async () => {
        if (!confirm('Are you sure you want to reset all learned knowledge?')) return;
        appendTerminal('system', 'Sending reset command to knowledge manager...');
        try {
            const res = await fetch(`${API_BASE}/reset`, { method: 'POST' });
            const data = await res.json();
            appendTerminal('system', 'Knowledge reset successful.');
            appendTerminal('system', data.output);
            updateStrategies([]);
        } catch (e) {
            appendTerminal('error', 'Failed to connect to backend: ' + e.message);
        }
    });

    async function runQuery(op, a, b, limit) {
        isComputing = true;
        runBtn.disabled = true;
        
        appendTerminal('prompt', `?- run_computation(object_level:${op}(${a}, ${b}, R), ${limit}).`);
        
        // Setup initial budget visually
        updateBudget(100);

        try {
            const response = await fetch(`${API_BASE}/solve`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ op, a, b, limit })
            });

            const data = await response.json();
            
            // Process the terminal output visually line by line
            const lines = data.output.split('\n').filter(line => line.trim() !== '');
            await typeTerminalLines(lines);

            // Determine crisis by checking output lines
            const isCrisis = data.output.includes('CRISIS: Resource Exhaustion Detected');
            if (isCrisis) {
                updateBudget(0);
                setTimeout(() => showCrisisModal('Inference budget exhausted while executing primordial strategy. Oracle was consulted to synthesize a more efficient approach.'), 1000);
            } else {
                updateBudget(50); // Dummy visual representation of used budget on success
            }

            // Update learned strategies
            updateStrategies(data.learned_strategies);

        } catch(e) {
            appendTerminal('error', 'Connection error: Prolog server may be down.');
            appendTerminal('error', e.toString());
        } finally {
            isComputing = false;
            runBtn.disabled = false;
        }
    }

    async function fetchState() {
        try {
            const res = await fetch(`${API_BASE}/state`);
            const data = await res.json();
            updateStrategies(data.learned_strategies || []);
        } catch (e) {
            appendTerminal('error', 'Could not query initial state from Prolog backend.');
        }
    }

    function appendTerminal(type, text) {
        const line = document.createElement('div');
        line.className = 'terminal-line';
        
        let prefix = '';
        if (type === 'prompt') {
            prefix = '<span class="prompt">swipl&gt;</span> ';
        }
        
        const content = document.createElement('span');
        content.className = `text ${type}-text`;
        content.textContent = text;

        // Add special colors based on regex
        if (text.includes('CRISIS:')) content.className = 'text crisis-text';
        if (text.includes('Oracle Says:')) content.className = 'text oracle-text';
        if (text.includes('Computation successful.')) content.className = 'text success-text';
        if (text.includes('[DEBUG]')) content.className = 'text debug-text';
        if (text.includes('✓')) content.className = 'text success-text';

        line.innerHTML = prefix;
        line.appendChild(content);
        
        terminalBody.appendChild(line);
        terminalBody.scrollTop = terminalBody.scrollHeight;
    }

    async function typeTerminalLines(lines) {
        for (const line of lines) {
            appendTerminal('standard', line);
            // Artificial delay to simulate thinking/processing
            await new Promise(r => setTimeout(r, 40)); 
        }
    }

    function updateStrategies(strategies) {
        strategyList.innerHTML = '';
        if (!strategies || strategies.length === 0) {
            strategyList.innerHTML = '<li class="empty-state">Primordial State (No abstractions)</li>';
            return;
        }
        
        // Reverse so newest is on top
        [...strategies].reverse().forEach(strat => {
            const li = document.createElement('li');
            li.textContent = strat;
            strategyList.appendChild(li);
        });
    }

    function updateBudget(percent) {
        budgetLabel.textContent = `${percent}%`;
        budgetProgress.style.width = `${percent}%`;
        
        budgetProgress.className = 'progress-fill';
        if (percent < 40) budgetProgress.className = 'progress-fill warning';
        if (percent < 10) budgetProgress.className = 'progress-fill danger';
    }

    function showCrisisModal(message) {
        const modal = document.getElementById('crisisOverlay');
        const msgEl = document.getElementById('crisisMessage');
        const dismissBtn = document.getElementById('dismissCrisisBtn');
        
        msgEl.textContent = message;
        modal.classList.remove('hidden');

        dismissBtn.onclick = () => {
            modal.classList.add('hidden');
        };
    }

    async function loadHermeneuticStrategies() {
        if (!calcStrategy || !calcOp) return;
        calcStrategy.innerHTML = '<option>Loading strategies...</option>';
        calcStrategy.disabled = true;
        try {
            const response = await fetch(`${HERMENEUTIC_API}/calculator/strategies?op=${encodeURIComponent(calcOp.value)}`);
            const data = await response.json();
            if (!response.ok || data.error) {
                throw new Error(data.error || 'Unable to load strategies');
            }
            calcStrategy.innerHTML = '';
            data.strategies.forEach((name) => {
                const option = document.createElement('option');
                option.value = name;
                option.textContent = name;
                calcStrategy.appendChild(option);
            });
            calcStrategy.disabled = false;
        } catch (error) {
            calcStrategy.innerHTML = `<option>${error.message}</option>`;
        }
    }

    async function runHermeneuticCalculation(event) {
        event.preventDefault();
        if (!calcStrategy || calcStrategy.disabled || !calcResult || !calcOp || !calcOperandA || !calcOperandB) {
            return;
        }

        const num1 = Number(calcOperandA.value);
        const num2 = Number(calcOperandB.value);
        if (Number.isNaN(num1) || Number.isNaN(num2)) {
            calcResult.innerHTML = '<span class="error-text">Please enter numeric operands.</span>';
            return;
        }

        calcResult.innerHTML = '<i>Running hermeneutic strategy...</i>';

        try {
            const response = await fetch(`${HERMENEUTIC_API}/calculator/calculate`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    op: calcOp.value,
                    num1,
                    num2,
                    strategy: calcStrategy.value
                })
            });
            const data = await response.json();
            if (!response.ok || data.error) {
                throw new Error(data.error || 'Calculation failed');
            }
            renderHermeneuticResult(data);
        } catch (error) {
            calcResult.innerHTML = `<span class="error-text">${error.message}</span>`;
        }
    }

    function renderHermeneuticResult(payload) {
        if (!calcResult) return;
        let html = `<div><strong>Result:</strong> ${payload.result}</div>`;
        html += `<div class="calc-meta"><span>${payload.op}</span> • <span>${payload.strategy}</span></div>`;
        if (payload.history && payload.history.length) {
            html += '<ol class="calc-history">';
            payload.history.forEach((step) => {
                html += `<li><code>${step}</code></li>`;
            });
            html += '</ol>';
        }
        calcResult.innerHTML = html;
    }
});
