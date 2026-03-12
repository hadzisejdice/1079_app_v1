/* ============================================================
   1079Magic V2 — App Controller
   Wires UI events → engines → renders results
   ============================================================ */

'use strict';

(async function () {

  // ── Init ──
  UI.init();
  await MagicEngine.loadTiers();

  // ── Compute Magic Ratio ──
  function runMagicRatio() {
    const inputs = UI.getInputs();
    const errors = UI.validate(inputs);
    if (errors.length) {
      UI.showValidation(errors);
      UI.switchTab('input');
      return;
    }
    UI.showValidation([]);
    UI.switchTab('magic');
    UI.showLoading('magic-loading');
    UI.hideResults('magic-results');

    // Use requestAnimationFrame so loading shows
    requestAnimationFrame(() => {
      setTimeout(() => {
        const focusMode = UI.isFocusActive('magic-focus-toggle');
        const result = MagicEngine.compute(inputs, focusMode);
        renderMagicResults(result, inputs);
        UI.hideLoading('magic-loading');
        UI.showResults('magic-results');
      }, 100);
    });
  }

  function renderMagicResults(result, inputs) {
    const { rally, joins, leftover, fractions, stock0, recN, scoreboard, tierKey } = result;

    // Call composition
    UI.renderCompBar('magic-call-comp', rally.inf, rally.cav, rally.arc);

    // Join composition (aggregate)
    let jI = 0, jC = 0, jA = 0;
    joins.forEach(p => { jI += p.inf; jC += p.cav; jA += p.arc; });
    UI.renderCompBar('magic-join-comp', jI, jC, jA);

    // Recommended marches
    const recEl = document.getElementById('magic-rec-marches');
    if (recEl) recEl.textContent = recN > 0 ? recN : inputs.numMarches;

    const recBtn = document.getElementById('magic-use-rec');
    if (recBtn) {
      if (recN > 0 && recN !== inputs.numMarches) {
        recBtn.style.display = 'inline-flex';
        recBtn.onclick = () => {
          document.getElementById('numMarches').value = recN;
          runMagicRatio();
        };
      } else {
        recBtn.style.display = 'none';
      }
    }

    // Scoreboard
    const tbody = document.getElementById('magic-scoreboard-body');
    if (tbody && scoreboard) {
      tbody.innerHTML = scoreboard.map((row, idx) => `
        <tr class="${idx === 0 ? 'highlight' : ''}">
          <td>${idx === 0 ? '✅' : row.rank}</td>
          <td>${row.callTrip}</td>
          <td class="score-val">${row.callScore.toLocaleString()}</td>
          <td>${row.joinTrip}</td>
          <td class="score-val">${row.joinScore.toLocaleString()}</td>
          <td class="score-val" style="font-weight:700;">${row.totalScore.toLocaleString()}</td>
        </tr>
      `).join('');
    }

    // Inventory
    const totalUsed = MagicEngine.sumTroops(rally) + joins.reduce((s, p) => s + MagicEngine.sumTroops(p), 0);
    const totalStock = MagicEngine.sumTroops(stock0);
    UI.renderInventory('magic-inventory-result', [
      { label: 'Used', value: totalUsed.toLocaleString(), status: 'ok' },
      { label: 'Stock', value: totalStock.toLocaleString(), status: 'ok' },
      { label: 'Left Inf', value: leftover.inf.toLocaleString(), status: leftover.inf > 0 ? 'warn' : 'ok' },
      { label: 'Left Cav', value: leftover.cav.toLocaleString(), status: leftover.cav > 0 ? 'warn' : 'ok' },
      { label: 'Left Arc', value: leftover.arc.toLocaleString(), status: leftover.arc > 0 ? 'warn' : 'ok' },
    ]);

    // Score simulator
    const simBox = document.getElementById('magic-sim-box');
    if (simBox && scoreboard && scoreboard.length > 0) {
      const best = scoreboard[0];
      simBox.style.display = 'block';
      simBox.innerHTML = `
        <div style="display:flex; justify-content:space-between; flex-wrap:wrap; gap:12px;">
          <div>
            <div style="font-size:0.78rem; color:var(--text-muted); margin-bottom:4px;">Call Score</div>
            <div style="font-family:var(--font-mono); font-size:1.1rem; font-weight:700; color:var(--accent-light);">${best.callScore.toLocaleString()}</div>
          </div>
          <div>
            <div style="font-size:0.78rem; color:var(--text-muted); margin-bottom:4px;">Join Score</div>
            <div style="font-family:var(--font-mono); font-size:1.1rem; font-weight:700; color:var(--cyan-light);">${best.joinScore.toLocaleString()}</div>
          </div>
          <div>
            <div style="font-size:0.78rem; color:var(--text-muted); margin-bottom:4px;">Total Score</div>
            <div style="font-family:var(--font-mono); font-size:1.3rem; font-weight:800; color:var(--text-primary);">${best.totalScore.toLocaleString()}</div>
          </div>
        </div>
      `;
    }
  }

  // ── Compute Option-A ──
  function runOptionA() {
    const inputs = UI.getInputs();
    const errors = UI.validate(inputs);
    if (errors.length) {
      UI.showValidation(errors);
      UI.switchTab('input');
      return;
    }
    UI.showValidation([]);
    UI.switchTab('optiona');
    UI.showLoading('oa-loading');
    UI.hideResults('oa-results');

    requestAnimationFrame(() => {
      setTimeout(() => {
        const focusMode = UI.isFocusActive('oa-focus-toggle');
        const result = OptionAEngine.compute(inputs, focusMode);
        renderOptionAResults(result, inputs);
        UI.hideLoading('oa-loading');
        UI.showResults('oa-results');
      }, 100);
    });
  }

  function renderOptionAResults(result, inputs) {
    const { plotData, rally, joins, leftover, callFrac, joinFrac, bounded, stock0, recN, callTriplet, joinTriplet, bestTriplet } = result;

    // Plot
    OptionAEngine.renderPlot('oa-plot', plotData);

    // Call comp
    UI.renderCompBar('oa-call-comp', rally.inf, rally.cav, rally.arc);

    // Join comp
    let jI = 0, jC = 0, jA = 0;
    joins.forEach(p => { jI += p.inf; jC += p.cav; jA += p.arc; });
    UI.renderCompBar('oa-join-comp', jI, jC, jA);

    // Fill comp inputs
    const callInput = document.getElementById('oa-comp-call');
    const joinInput = document.getElementById('oa-comp-join');
    if (callInput && !callInput._userEdited) callInput.value = callTriplet;
    if (joinInput && !joinInput._userEdited) joinInput.value = joinTriplet;

    // Recommended
    const recEl = document.getElementById('oa-rec-marches');
    if (recEl) recEl.textContent = recN || inputs.numMarches;

    const recBtn = document.getElementById('oa-use-rec');
    if (recBtn) {
      if (recN && recN !== inputs.numMarches) {
        recBtn.style.display = 'inline-flex';
        recBtn.onclick = () => {
          document.getElementById('numMarches').value = recN;
          runOptionA();
        };
      } else {
        recBtn.style.display = 'none';
      }
    }

    // Target fractions
    const targetEl = document.getElementById('oa-target-fracs');
    if (targetEl) {
      targetEl.innerHTML = `
        <div style="font-size:0.82rem; color:var(--text-muted); display:flex; gap:12px; flex-wrap:wrap;">
          <span>Best: <span class="tag amber">${bestTriplet}</span></span>
          <span>Call: <span class="tag cyan">${callTriplet}</span></span>
          <span>Join: <span class="tag green">${joinTriplet}</span></span>
        </div>
      `;
    }

    // Inventory
    const totalUsed = (rally.inf + rally.cav + rally.arc) + joins.reduce((s, p) => s + p.inf + p.cav + p.arc, 0);
    const totalStock = stock0.inf + stock0.cav + stock0.arc;
    UI.renderInventory('oa-inventory-result', [
      { label: 'Used', value: totalUsed.toLocaleString(), status: 'ok' },
      { label: 'Stock', value: totalStock.toLocaleString(), status: 'ok' },
      { label: 'Left Inf', value: leftover.inf.toLocaleString(), status: leftover.inf > 0 ? 'warn' : 'ok' },
      { label: 'Left Cav', value: leftover.cav.toLocaleString(), status: leftover.cav > 0 ? 'warn' : 'ok' },
      { label: 'Left Arc', value: leftover.arc.toLocaleString(), status: leftover.arc > 0 ? 'warn' : 'ok' },
    ]);
  }

  // ── Wire Event Listeners ──

  // Compute buttons
  document.getElementById('btn-magic').addEventListener('click', runMagicRatio);
  document.getElementById('btn-optiona').addEventListener('click', runOptionA);

  // Focus toggle re-compute
  document.addEventListener('focus-toggle', (e) => {
    const { id } = e.detail;
    if (id === 'magic-focus-toggle') runMagicRatio();
    if (id === 'oa-focus-toggle') runOptionA();
  });

  // Comp input changes (Option-A)
  const oaCallInput = document.getElementById('oa-comp-call');
  const oaJoinInput = document.getElementById('oa-comp-join');
  if (oaCallInput) {
    oaCallInput.addEventListener('input', () => {
      oaCallInput._userEdited = true;
      // Debounced recompute
      clearTimeout(oaCallInput._debounce);
      oaCallInput._debounce = setTimeout(runOptionA, 400);
    });
  }
  if (oaJoinInput) {
    oaJoinInput.addEventListener('input', () => {
      oaJoinInput._userEdited = true;
      clearTimeout(oaJoinInput._debounce);
      oaJoinInput._debounce = setTimeout(runOptionA, 400);
    });
  }

  // Tab switching shortcuts from results
  document.querySelectorAll('.mode-tab').forEach(tab => {
    tab.addEventListener('click', () => {
      const target = tab.dataset.tab;
      // Auto-compute when switching to result tabs if we have data
      const inputs = UI.getInputs();
      if (target === 'magic' && UI.validate(inputs).length === 0) {
        const magicResults = document.getElementById('magic-results');
        if (!magicResults || !magicResults.classList.contains('show')) {
          // Only auto-compute if not already shown
        }
      }
    });
  });

  // ── Keyboard shortcuts ──
  document.addEventListener('keydown', (e) => {
    if (e.target.tagName === 'INPUT' || e.target.tagName === 'SELECT') return;
    if (e.key === '1') UI.switchTab('input');
    if (e.key === '2') UI.switchTab('magic');
    if (e.key === '3') UI.switchTab('optiona');
  });

})();
