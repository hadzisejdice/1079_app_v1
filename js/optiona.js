/* ============================================================
   1079Magic V2 — Option-A Engine
   Ported from app2.js — all game math preserved exactly
   ============================================================ */

'use strict';

window.OptionAEngine = (function () {
  const INF_MIN_PCT = 0.075;
  const INF_MAX_PCT = 0.10;
  const CAV_MIN_PCT = 0.10;

  // ── Helpers (reuse from MagicEngine) ──
  const { attackFactor, getArcherCoefByTier, computeExactOptimalFractions, enforceCompositionBounds, parseTriplet, toPctTriplet } = window.MagicEngine;

  function cloneStock(s) { return { inf: Math.max(0, s.inf | 0), cav: Math.max(0, s.cav | 0), arc: Math.max(0, s.arc | 0) }; }

  function evaluateForPlot(fin, fcav, farc, stats, tierRaw) {
    const Ainf = attackFactor(stats.inf_atk, stats.inf_let);
    const Acav = attackFactor(stats.cav_atk, stats.cav_let);
    const Aarc = attackFactor(stats.arc_atk, stats.arc_let);
    const KARC = getArcherCoefByTier(tierRaw);
    const termInf = (1 / 1.45) * Ainf * Math.sqrt(fin);
    const termCav = Acav * Math.sqrt(fcav);
    const termArc = KARC * Aarc * Math.sqrt(farc);
    return termInf + termCav + termArc;
  }

  function formatTriplet(fin, fcav, farc) {
    const S = fin + fcav + farc;
    if (S <= 0) return '0/0/100';
    const nf = fin / S, nc = fcav / S;
    let i = Math.round(nf * 100), c = Math.round(nc * 100);
    let a = 100 - i - c;
    if (a < 0) { a = 0; if (i + c > 100) { if (i >= c) i -= (i + c - 100); else c -= (i + c - 100); } }
    return `${i}/${c}/${a}`;
  }

  function formatTriplet1dp(fin, fcav, farc) {
    const S = fin + fcav + farc;
    if (S <= 0) return '0.0/0.0/100.0';
    const ip = (fin / S) * 100, cp = (fcav / S) * 100, ap = (farc / S) * 100;
    let is = ip.toFixed(1), cs = cp.toFixed(1), as_ = ap.toFixed(1);
    const diff = 100 - parseFloat(is) - parseFloat(cs) - parseFloat(as_);
    if (Math.abs(diff) >= 0.05) as_ = (parseFloat(as_) + diff).toFixed(1);
    return `${is}/${cs}/${as_}`;
  }

  function percentile(arr, p) {
    const a = [...arr].sort((x, y) => x - y);
    const idx = (a.length - 1) * p;
    const lo = Math.floor(idx), hi = Math.ceil(idx);
    if (lo === hi) return a[lo];
    return a[lo] * (1 - (idx - lo)) + a[hi] * (idx - lo);
  }

  function parseCompToFractions(str) {
    if (typeof str !== 'string') return null;
    const parts = str.replace(/%/g, '').trim().split(/[,\s/]+/).map(s => s.trim()).filter(Boolean).map(Number);
    if (parts.some(v => !Number.isFinite(v) || v < 0) || parts.length === 0) return null;
    let i = parts[0] ?? 0, c = parts[1] ?? 0;
    let a = parts.length >= 3 ? parts[2] : Math.max(0, 100 - (i + c));
    const sum = i + c + a;
    if (sum <= 0) return null;
    return { fin: i / sum, fcav: c / sum, farc: a / sum };
  }

  // ── Rally Builder ──
  function buildRally(fractions, rallySize, stock) {
    if (rallySize <= 0) return { inf: 0, cav: 0, arc: 0 };
    const iMin = Math.ceil(INF_MIN_PCT * rallySize);
    const iMax = Math.floor(INF_MAX_PCT * rallySize);
    const cMin = Math.ceil(CAV_MIN_PCT * rallySize);
    let inf = Math.min(stock.inf, Math.max(iMin, Math.min(iMax, Math.round(fractions.fin * rallySize))));
    let cav = Math.min(stock.cav, Math.max(cMin, Math.round(fractions.fcav * rallySize)));
    if (inf + cav > rallySize) cav = Math.max(cMin, rallySize - inf);
    let arc = Math.min(stock.arc, Math.max(0, rallySize - inf - cav));
    let used = inf + cav + arc;
    if (used < rallySize && stock.cav - cav > 0) { const add = Math.min(rallySize - used, stock.cav - cav); cav += add; used += add; }
    if (used < rallySize && stock.inf - inf > 0) { const add = Math.min(rallySize - used, stock.inf - inf, Math.max(0, iMax - inf)); inf += add; used += add; }
    stock.inf -= inf; stock.cav -= cav; stock.arc -= arc;
    return { inf, cav, arc };
  }

  // ── Round Robin ──
  function fillRoundRobin(total, caps) {
    const n = caps.length;
    const out = Array(n).fill(0);
    let t = Math.max(0, Math.floor(total));
    let progress = true;
    while (t > 0 && progress) {
      progress = false;
      for (let i = 0; i < n && t > 0; i++) {
        if (out[i] < caps[i]) { out[i] += 1; t -= 1; progress = true; }
      }
    }
    return out;
  }

  // ── Join Formation Builder (arc-first) ──
  function buildOptionAFormations(stock, formations, cap) {
    const n = Math.max(1, formations);
    const JOIN_INF_MIN = 0.01;
    const JOIN_CAV_MIN = 0.03;
    const infMinPer = Math.ceil(JOIN_INF_MIN * cap);
    const infMaxPer = Math.floor(INF_MAX_PCT * cap);
    const cavMinPer = Math.ceil(JOIN_CAV_MIN * cap);

    const infAlloc = Array(n).fill(0);
    const cavAlloc = Array(n).fill(0);
    const arcAlloc = Array(n).fill(0);

    // Step 1: Min infantry
    const infMinGive = fillRoundRobin(Math.min(stock.inf, infMinPer * n), Array(n).fill(infMinPer));
    for (let i = 0; i < n; i++) { infAlloc[i] = infMinGive[i]; stock.inf -= infMinGive[i]; }

    // Step 2: Min cavalry
    const cavMinGive = fillRoundRobin(Math.min(stock.cav, cavMinPer * n), Array(n).fill(cavMinPer));
    for (let i = 0; i < n; i++) { cavAlloc[i] = cavMinGive[i]; stock.cav -= cavMinGive[i]; }

    // Step 3: Fill with archers
    const arcCaps = Array(n).fill(0).map((_, i) => Math.max(0, cap - infAlloc[i] - cavAlloc[i]));
    const arcGive = fillRoundRobin(stock.arc, arcCaps);
    for (let i = 0; i < n; i++) { arcAlloc[i] = arcGive[i]; stock.arc -= arcGive[i]; }

    // Step 4: Fill gaps with cav
    for (let i = 0; i < n; i++) {
      const space = cap - infAlloc[i] - cavAlloc[i] - arcAlloc[i];
      if (space > 0 && stock.cav > 0) { const add = Math.min(space, stock.cav); cavAlloc[i] += add; stock.cav -= add; }
    }

    // Step 5: Fill remaining with inf
    for (let i = 0; i < n; i++) {
      const space = cap - infAlloc[i] - cavAlloc[i] - arcAlloc[i];
      if (space > 0 && stock.inf > 0) { const add = Math.min(space, stock.inf, Math.max(0, infMaxPer - infAlloc[i])); infAlloc[i] += add; stock.inf -= add; }
    }

    const packs = [];
    for (let i = 0; i < n; i++) packs.push({ inf: infAlloc[i], cav: cavAlloc[i], arc: arcAlloc[i] });
    return { packs, leftover: cloneStock(stock) };
  }

  function buildJoinManually(stock, marchCount, cap, fractions) {
    const packs = [];
    for (let m = 0; m < marchCount; m++) {
      let i = Math.min(Math.round(fractions.fin * cap), stock.inf);
      let c = Math.min(Math.round(fractions.fcav * cap), stock.cav);
      let a = Math.min(Math.round(fractions.farc * cap), stock.arc);
      stock.inf -= i; stock.cav -= c; stock.arc -= a;
      packs.push({ inf: i, cav: c, arc: a });
    }
    return { packs, leftover: cloneStock(stock) };
  }

  // ── Recommendation ──
  function meetsTargetFill(fill) { return fill >= 0.822; }
  function computeRecommendationScore(fullCount, minFill, avgFill, leftover) {
    const totalLeft = leftover.inf + leftover.cav + leftover.arc;
    const cavPenalty = leftover.cav * 3;
    return fullCount * 1e9 + (minFill * 0.822) * 1e6 + avgFill * 1e3 - (totalLeft + cavPenalty);
  }

  function computeRecommendedMarches(maxMarches, fractions, rallySize, joinCap, stock) {
    let best = null;
    for (let n = 1; n <= maxMarches; n++) {
      const s = cloneStock(stock);
      const rally = buildRally(fractions, rallySize, s);
      const result = buildOptionAFormations(cloneStock(s), n, joinCap);
      const totals = result.packs.map(p => p.inf + p.cav + p.arc);
      const fills = totals.map(t => t / joinCap);
      const minFill = fills.length ? Math.min(...fills) : 0;
      const avgFill = fills.length ? fills.reduce((a, b) => a + b, 0) / fills.length : 0;
      const fullCount = fills.filter(f => meetsTargetFill(f)).length;
      const score = computeRecommendationScore(fullCount, minFill, avgFill, result.leftover);
      if (!best || score > best.score) best = { marchCount: n, minFill, avgFill, fullCount, score };
    }
    return best;
  }

  // ── Compute Plot Data ──
  function computePlotData(stats, tierRaw) {
    const opt = computeExactOptimalFractions(stats, tierRaw);
    const bounded = enforceCompositionBounds(opt.fin, opt.fcav, opt.farc);

    const samples = [];
    const vals = [];
    const steps = 55;
    for (let i = 0; i <= steps; i++) {
      for (let j = 0; j <= steps - i; j++) {
        const fin = i / steps, fcav = j / steps, farc = 1 - fin - fcav;
        const d = evaluateForPlot(fin, fcav, farc, stats, tierRaw);
        samples.push({ fin, fcav, farc, d });
        vals.push(d);
      }
    }
    const vmax = Math.max(...vals);
    const rel = vals.map(v => v / (vmax || 1));
    const vminClip = percentile(rel, 0.05);
    const vmaxClip = percentile(rel, 0.95);

    return { samples, rel, vminClip, vmaxClip, bounded };
  }

  // ── Render Plot ──
  function renderPlot(containerId, plotData) {
    const { samples, rel, vminClip, vmaxClip, bounded } = plotData;

    const fieldTrace = {
      type: 'scatterternary',
      mode: 'markers',
      a: samples.map(s => s.fin),
      b: samples.map(s => s.fcav),
      c: samples.map(s => s.farc),
      marker: {
        size: 3, opacity: 0.95,
        color: rel, colorscale: 'Viridis',
        cmin: vminClip, cmax: vmaxClip,
        line: { width: 0 },
        colorbar: {
          thickness: 14, len: 0.6, tickformat: '.2f',
          x: 0.5, xanchor: 'center', y: -0.15, yanchor: 'top', orientation: 'h'
        }
      },
      hovertemplate: '<b>Inf</b>: %{a:.2f}<br><b>Cav</b>: %{b:.2f}<br><b>Arc</b>: %{c:.2f}<br><b>Rel</b>: %{marker.color:.3f}<extra></extra>',
      name: 'Surface'
    };

    const bestTrace = {
      type: 'scatterternary',
      mode: 'markers+text',
      a: [bounded.fin], b: [bounded.fcav], c: [bounded.farc],
      marker: { size: 12, color: '#f59e0b', line: { color: 'white', width: 1.6 } },
      text: ['Best'], textposition: 'top center',
      textfont: { color: '#fbbf24', size: 12 },
      hovertemplate: 'Best (bounded)<br>Inf: %{a:.2f}<br>Cav: %{b:.2f}<br>Arc: %{c:.2f}<extra></extra>',
      name: 'Best'
    };

    const layout = {
      template: 'plotly_dark',
      paper_bgcolor: 'rgba(10,14,23,0.8)',
      plot_bgcolor: 'rgba(10,14,23,0.8)',
      font: { color: '#94a3b8', family: 'Outfit, system-ui', size: 12 },
      margin: { l: 36, r: 40, b: 100, t: 52 },
      title: { text: 'Optimal Troop Composition', x: 0.02, font: { size: 16, color: '#f1f5f9' } },
      showlegend: false,
      ternary: {
        sum: 1,
        bgcolor: 'rgba(6,8,13,0.9)',
        domain: { x: [0.02, 0.96], y: [0.15, 0.98] },
        aaxis: { title: { text: 'Infantry' }, min: 0, ticks: 'outside', tickformat: '.1f', ticklen: 4, gridcolor: 'rgba(255,255,255,0.08)' },
        baxis: { title: { text: 'Cavalry' }, min: 0, ticks: 'outside', tickformat: '.1f', ticklen: 4, gridcolor: 'rgba(255,255,255,0.08)' },
        caxis: { title: { text: 'Archery' }, min: 0, ticks: 'outside', tickformat: '.1f', ticklen: 4, gridcolor: 'rgba(255,255,255,0.08)' }
      }
    };

    if (typeof Plotly !== 'undefined') {
      Plotly.react(containerId, [fieldTrace, bestTrace], layout, { responsive: true, displayModeBar: false });
    }
  }

  // ── Main Compute ──
  function compute(inputs, focusMode) {
    const stats = {
      inf_atk: inputs.inf_atk || 1, inf_let: inputs.inf_let || 1,
      cav_atk: inputs.cav_atk || 1, cav_let: inputs.cav_let || 1,
      arc_atk: inputs.arc_atk || 1, arc_let: inputs.arc_let || 1
    };
    const tierRaw = inputs.tier;

    // Compute optimal fractions
    const opt = computeExactOptimalFractions(stats, tierRaw);
    const bounded = enforceCompositionBounds(opt.fin, opt.fcav, opt.farc);

    // Plot
    const plotData = computePlotData(stats, tierRaw);

    // Build rally
    const stock = { inf: inputs.stock_inf, cav: inputs.stock_cav, arc: inputs.stock_arc };
    const rallySize = inputs.rallyCap;
    const joinCap = inputs.joinCap;
    const formations = inputs.numMarches;

    const stockForRally = cloneStock(stock);
    const rally = buildRally(bounded, rallySize, stockForRally);

    // Build joins (arc-first by default)
    const result = buildOptionAFormations(cloneStock(stockForRally), formations, joinCap);
    const { packs, leftover } = result;

    // Compute fractions from actual
    const rallyTotal = rally.inf + rally.cav + rally.arc;
    const callFrac = rallyTotal > 0 ? { fin: rally.inf / rallyTotal, fcav: rally.cav / rallyTotal, farc: rally.arc / rallyTotal } : bounded;

    let joinInf = 0, joinCav = 0, joinArc = 0;
    packs.forEach(p => { joinInf += p.inf; joinCav += p.cav; joinArc += p.arc; });
    const joinTotal = joinInf + joinCav + joinArc;
    const joinFrac = joinTotal > 0 ? { fin: joinInf / joinTotal, fcav: joinCav / joinTotal, farc: joinArc / joinTotal } : { fin: 0.33, fcav: 0.33, farc: 0.34 };

    // Recommendation
    const rec = computeRecommendedMarches(formations, bounded, rallySize, joinCap, cloneStock(stock));

    return {
      plotData,
      rally,
      joins: packs,
      leftover,
      callFrac,
      joinFrac,
      bounded,
      stock0: stock,
      recN: rec ? rec.marchCount : formations,
      recMinFill: rec ? rec.minFill : 0,
      callTriplet: formatTriplet(callFrac.fin, callFrac.fcav, callFrac.farc),
      joinTriplet: formatTriplet(joinFrac.fin, joinFrac.fcav, joinFrac.farc),
      bestTriplet: formatTriplet(bounded.fin, bounded.fcav, bounded.farc),
    };
  }

  return {
    compute,
    renderPlot,
    buildRally,
    buildOptionAFormations,
    buildJoinManually,
    parseCompToFractions,
    formatTriplet,
    formatTriplet1dp,
  };
})();
