/* ============================================================
   1079Magic V2 — UI Controller
   Handles: tabs, collapsibles, focus toggles, composition bars
   ============================================================ */

'use strict';

window.UI = (function () {
  // ── DOM Helpers ──
  const $ = (sel, ctx = document) => ctx.querySelector(sel);
  const $$ = (sel, ctx = document) => [...ctx.querySelectorAll(sel)];

  // ── Tab System ──
  function initTabs() {
    const tabs = $$('.mode-tab');
    const panels = $$('.tab-panel');

    tabs.forEach(tab => {
      tab.addEventListener('click', () => {
        const target = tab.dataset.tab;
        tabs.forEach(t => { t.classList.remove('active'); t.setAttribute('aria-selected', 'false'); });
        tab.classList.add('active');
        tab.setAttribute('aria-selected', 'true');
        panels.forEach(p => p.classList.add('hidden'));
        const panel = $(`#panel-${target}`);
        if (panel) {
          panel.classList.remove('hidden');
          panel.style.animation = 'none';
          panel.offsetHeight; // reflow
          panel.style.animation = 'fadeIn 0.3s ease-out';
        }
      });
    });
  }

  function switchTab(tabId) {
    const tab = $(`#tab-${tabId}`);
    if (tab) tab.click();
  }

  // ── Collapsible ──
  function initCollapsibles() {
    $$('.collapsible-trigger').forEach(trigger => {
      trigger.addEventListener('click', () => {
        const contentId = trigger.getAttribute('aria-controls') || trigger.id.replace('-trigger', '-content');
        const content = $(`#${contentId}`);
        if (!content) return;
        const isOpen = trigger.classList.contains('open');
        trigger.classList.toggle('open');
        content.classList.toggle('open');
        trigger.setAttribute('aria-expanded', !isOpen);
      });
    });
  }

  // ── Focus Toggle ──
  function initFocusToggles() {
    $$('.focus-toggle').forEach(toggle => {
      toggle.addEventListener('click', () => {
        toggle.classList.toggle('active');
        const event = new CustomEvent('focus-toggle', { detail: { id: toggle.id, active: toggle.classList.contains('active') } });
        document.dispatchEvent(event);
      });
    });
  }

  function isFocusActive(id) {
    const el = $(`#${id}`);
    return el ? el.classList.contains('active') : false;
  }

  // ── Composition Bar Renderer ──
  function renderCompBar(containerId, inf, cav, arc) {
    const container = $(`#${containerId}`);
    if (!container) return;
    const total = inf + cav + arc;
    if (total <= 0) { container.innerHTML = '<span class="text-muted" style="font-size:0.82rem;">No data</span>'; return; }
    const pi = (inf / total * 100).toFixed(1);
    const pc = (cav / total * 100).toFixed(1);
    const pa = (arc / total * 100).toFixed(1);

    container.innerHTML = `
      <div class="comp-bar-container">
        <div class="comp-bar-inf" style="width:${pi}%"></div>
        <div class="comp-bar-cav" style="width:${pc}%"></div>
        <div class="comp-bar-arc" style="width:${pa}%"></div>
      </div>
      <div class="comp-values">
        <span class="comp-val inf">${Math.round(inf).toLocaleString()}</span>
        <span class="comp-val cav">${Math.round(cav).toLocaleString()}</span>
        <span class="comp-val arc">${Math.round(arc).toLocaleString()}</span>
      </div>
    `;
  }

  function renderCompBarPct(containerId, pi, pc, pa) {
    const container = $(`#${containerId}`);
    if (!container) return;
    container.innerHTML = `
      <div class="comp-bar-container">
        <div class="comp-bar-inf" style="width:${pi}%"></div>
        <div class="comp-bar-cav" style="width:${pc}%"></div>
        <div class="comp-bar-arc" style="width:${pa}%"></div>
      </div>
      <div class="comp-values">
        <span class="comp-val inf">${pi}%</span>
        <span class="comp-val cav">${pc}%</span>
        <span class="comp-val arc">${pa}%</span>
      </div>
    `;
  }

  // ── Inventory chips ──
  function renderInventory(containerId, data) {
    const container = $(`#${containerId}`);
    if (!container) return;
    if (!data) { container.innerHTML = ''; return; }
    let html = '<div class="inventory-bar">';
    data.forEach(item => {
      const cls = item.status === 'ok' ? 'ok' : (item.status === 'warn' ? 'warn' : 'err');
      html += `<div class="inv-chip ${cls}"><span class="inv-label">${item.label}</span> <span class="inv-val">${item.value}</span></div>`;
    });
    html += '</div>';
    container.innerHTML = html;
  }

  // ── Validation ──
  function showValidation(errors) {
    const box = $('#validationBox');
    const list = $('#validationList');
    if (!box || !list) return;
    if (!errors || errors.length === 0) {
      box.classList.remove('show');
      return;
    }
    list.innerHTML = errors.map(e => `<li>${e}</li>`).join('');
    box.classList.add('show');
  }

  // ── Loading states ──
  function showLoading(id) {
    const el = $(`#${id}`);
    if (el) el.style.display = 'block';
  }

  function hideLoading(id) {
    const el = $(`#${id}`);
    if (el) el.style.display = 'none';
  }

  function showResults(id) {
    const el = $(`#${id}`);
    if (el) { el.classList.add('show'); el.style.display = 'block'; }
  }

  function hideResults(id) {
    const el = $(`#${id}`);
    if (el) { el.classList.remove('show'); el.style.display = 'none'; }
  }

  // ── Get input values ──
  function getInputs() {
    const nval = (id) => {
      const el = $(`#${id}`);
      if (!el) return 0;
      const v = parseFloat(el.value);
      return Number.isFinite(v) ? v : 0;
    };
    return {
      inf_atk: nval('inf_atk'), inf_let: nval('inf_let'),
      cav_atk: nval('cav_atk'), cav_let: nval('cav_let'),
      arc_atk: nval('arc_atk'), arc_let: nval('arc_let'),
      tier: ($('#tier') || {}).value || 'T10',
      stock_inf: nval('stock_inf'), stock_cav: nval('stock_cav'), stock_arc: nval('stock_arc'),
      rallyCap: nval('rallyCap'), joinCap: nval('joinCap'),
      numMarches: nval('numMarches') || 3,
    };
  }

  // ── Validate ──
  function validate(inputs) {
    const errors = [];
    if (inputs.inf_atk <= 0 && inputs.cav_atk <= 0 && inputs.arc_atk <= 0) errors.push('Enter at least one Attack stat.');
    if (inputs.rallyCap <= 0) errors.push('Rally size must be > 0.');
    if (inputs.joinCap <= 0) errors.push('Join cap must be > 0.');
    if (inputs.stock_inf + inputs.stock_cav + inputs.stock_arc <= 0) errors.push('Enter your troop counts.');
    return errors;
  }

  // ── Intersection Observer for scroll animations ──
  function initScrollAnimations() {
    if (!('IntersectionObserver' in window)) return;
    const observer = new IntersectionObserver((entries) => {
      entries.forEach(entry => {
        if (entry.isIntersecting) {
          entry.target.style.animationPlayState = 'running';
          observer.unobserve(entry.target);
        }
      });
    }, { threshold: 0.1 });

    $$('.animate-in').forEach(el => {
      el.style.animationPlayState = 'paused';
      observer.observe(el);
    });
  }

  // ── Init ──
  function init() {
    initTabs();
    initCollapsibles();
    initFocusToggles();
    // Don't pause animations on initial load — let them play
    // initScrollAnimations();
  }

  return {
    init,
    switchTab,
    getInputs,
    validate,
    showValidation,
    showLoading, hideLoading,
    showResults, hideResults,
    renderCompBar, renderCompBarPct,
    renderInventory,
    isFocusActive,
    $, $$
  };
})();
