/* ============================================================
   1079Magic V2 — Magic Ratio Engine
   Ported from app1.js — all game math preserved exactly
   ============================================================ */

'use strict';

window.MagicEngine = (function () {

  // ── Constants ──
  const WINDOWS = 5;
  const BEAR_TROOPS = 5000;
  const BEAR_DEF = 10;
  const BEAR_HP = 83.3333;
  const BEAR_DEF_PER_TROOP = (BEAR_HP * BEAR_DEF) / 100;
  const BEAR_ATK_BONUS = 0.25;
  const BASE_LETHALITY = 10;
  const SKILLMOD_INF = 1.0;
  const SKILLMOD_CAV = 1.0;
  const SKILLMOD_ARC = 1.10;

  const INF_MIN_PCT = 0.075;
  const INF_MAX_PCT = 0.10;
  const CAV_MIN_PCT = 0.10;

  const FILL_THRESHOLD = 0.923;
  const MIN_INF_PCT_MARCH = 0.05;

  const REC_FILL_GATE = 0.85;
  const REC_ARC_MIN_PCT = 0.10;
  const REC_MAX_SINGLE = 0.80;

  let TIERS = null;

  // ── Utilities ──
  function sumTroops(o) { return (o.inf | 0) + (o.cav | 0) + (o.arc | 0); }
  function cloneStock(s) { return { inf: Math.max(0, s.inf | 0), cav: Math.max(0, s.cav | 0), arc: Math.max(0, s.arc | 0) }; }

  function toPctTriplet(fr) {
    const S = (fr.i ?? 0) + (fr.c ?? 0) + (fr.a ?? 0) || 1;
    const pi = Math.round((fr.i ?? 0) / S * 100);
    const pc = Math.round((fr.c ?? 0) / S * 100);
    const pa = 100 - pi - pc;
    return `${pi}/${pc}/${pa}`;
  }

  function parseTriplet(str) {
    if (!str) return null;
    const arr = str.replace(/%/g, '').trim().split(/[,\s/]+/).map(Number).filter(Number.isFinite);
    if (!arr.length) return null;
    let i = arr[0] ?? 0, c = arr[1] ?? 0, a = arr[2] ?? (100 - (i + c));
    const S = i + c + a;
    if (S <= 0) return null;
    return { i: i / S, c: c / S, a: a / S };
  }

  // ── Damage model ──
  function perTroopAttack(baseAtk) { return baseAtk * (1 + BEAR_ATK_BONUS); }
  function computeFormationDamage(march, tierKey) {
    const t = TIERS?.tiers?.[tierKey];
    if (!t) return { finalScore: 0 };
    const total = sumTroops(march);
    if (total <= 0) return { finalScore: 0 };
    const infDmg = march.inf * perTroopAttack(t.inf[0]) * SKILLMOD_INF;
    const cavDmg = march.cav * perTroopAttack(t.cav[0]) * SKILLMOD_CAV;
    const arcDmg = march.arc * perTroopAttack(t.arc[0]) * SKILLMOD_ARC;
    const totalDmg = infDmg + cavDmg + arcDmg;
    const lethality = BASE_LETHALITY;
    const killed = Math.floor(totalDmg / BEAR_DEF_PER_TROOP * (lethality / 100));
    return { finalScore: killed };
  }

  // ── Coefficients ──
  function coeffsByTier(tierKey) {
    const t = TIERS?.tiers?.[tierKey];
    if (!t) return { inf: 1, cav: 1, arc: 1 };
    return {
      inf: perTroopAttack(t.inf[0]) / BEAR_DEF_PER_TROOP / 100 * SKILLMOD_INF,
      cav: perTroopAttack(t.cav[0]) / BEAR_DEF_PER_TROOP / 100 * SKILLMOD_CAV,
      arc: perTroopAttack(t.arc[0]) / BEAR_DEF_PER_TROOP / 100 * SKILLMOD_ARC
    };
  }

  function magicWeightsSquared(tierKey, mode) {
    const K = coeffsByTier(tierKey);
    const mult = (mode === 'magic12') ? { inf: 1, cav: 1, arc: 2 } : { inf: 1, cav: 1, arc: 1 };
    return {
      wInf: (K.inf * mult.inf) ** 2,
      wCav: (K.cav * mult.cav) ** 2,
      wArc: (K.arc * mult.arc) ** 2
    };
  }

  // ── Archer Priority Allocation ──
  function allocateArchersToJoins(stockIn, X, joinCap, infMin, cavMin) {
    const s = cloneStock(stockIn);
    const joins = [];
    for (let i = 0; i < X; i++) {
      if (joinCap <= 0) { joins.push({ inf: 0, cav: 0, arc: 0 }); continue; }
      const infAlloc = Math.min(s.inf, Math.ceil(joinCap * infMin));
      s.inf -= infAlloc;
      const cavAlloc = Math.min(s.cav, Math.ceil(joinCap * cavMin));
      s.cav -= cavAlloc;
      const space = joinCap - infAlloc - cavAlloc;
      const arcAlloc = Math.min(s.arc, Math.max(0, space));
      s.arc -= arcAlloc;
      let fill = joinCap - infAlloc - cavAlloc - arcAlloc;
      let extraCav = 0, extraInf = 0;
      if (fill > 0) { extraCav = Math.min(s.cav, fill); s.cav -= extraCav; fill -= extraCav; }
      if (fill > 0) { extraInf = Math.min(s.inf, fill); s.inf -= extraInf; fill -= extraInf; }
      joins.push({ inf: infAlloc + extraInf, cav: cavAlloc + extraCav, arc: arcAlloc });
    }
    return { joins, leftover: s };
  }

  function allocateArchersToCall(stockIn, rallySize, infMin, infMax, cavMin) {
    const s = cloneStock(stockIn);
    let inf = Math.min(s.inf, Math.ceil(rallySize * infMin));
    s.inf -= inf;
    let cav = Math.min(s.cav, Math.ceil(rallySize * cavMin));
    s.cav -= cav;
    let used = inf + cav;
    let space = rallySize - used;
    let arc = Math.min(s.arc, space);
    s.arc -= arc;
    used += arc;
    space = rallySize - used;
    if (space > 0) { const extraCav = Math.min(s.cav, space); cav += extraCav; s.cav -= extraCav; used += extraCav; space = rallySize - used; }
    if (space > 0) { const extraInf = Math.min(s.inf, space); inf += extraInf; s.inf -= extraInf; }
    return { rally: { inf, cav, arc }, leftover: s };
  }

  function planArcherPriorityAlloc(stock0, rallySize, X, joinCap, infMin, infMax, cavMin) {
    const s = cloneStock(stock0);
    const phase1 = allocateArchersToJoins(s, X, joinCap, infMin, cavMin);
    const phase2 = allocateArchersToCall(phase1.leftover, rallySize, infMin, infMax, cavMin);
    const rally = phase2.rally;
    const joins = phase1.joins;
    const finalLeftover = phase2.leftover;
    const totalUsed = sumTroops(rally) + joins.reduce((sum, p) => sum + sumTroops(p), 0);
    const TT = Math.max(1, totalUsed);
    const allTroops = { inf: 0, cav: 0, arc: 0 };
    allTroops.inf = rally.inf + joins.reduce((sum, p) => sum + p.inf, 0);
    allTroops.cav = rally.cav + joins.reduce((sum, p) => sum + p.cav, 0);
    allTroops.arc = rally.arc + joins.reduce((sum, p) => sum + p.arc, 0);
    return { rally, packs: joins, leftover: finalLeftover, fractions: { i: allTroops.inf / TT, c: allTroops.cav / TT, a: allTroops.arc / TT } };
  }

  // ── Magic Ratio Planning ──
  function planMagicAlloc(mode, stockIn, rallySize, X, cap, tierKey) {
    const s = cloneStock(stockIn);
    const R = Math.max(0, rallySize | 0);
    const C = Math.max(0, cap | 0);
    const Xn = Math.max(0, X | 0);
    const T = R + Xn * C;
    const { wInf, wCav, wArc } = magicWeightsSquared(tierKey, mode);
    const sumW = Math.max(1e-9, wInf + wCav + wArc);
    const base = { inf: T * (wInf / sumW), cav: T * (wCav / sumW), arc: T * (wArc / sumW) };
    let target = { inf: Math.min(s.inf, Math.round(base.inf)), cav: Math.min(s.cav, Math.round(base.cav)), arc: Math.min(s.arc, Math.round(base.arc)) };
    let used = target.inf + target.cav + target.arc;
    let deficit = T - used;
    const prio = [['arc', wArc], ['cav', wCav], ['inf', wInf]].sort((a, b) => b[1] - a[1]);
    while (deficit > 0) {
      let progressed = false;
      for (const [k] of prio) {
        const free = s[k] - target[k];
        if (free <= 0) continue;
        const give = Math.min(free, deficit);
        target[k] += give;
        deficit -= give;
        progressed = true;
        if (deficit <= 0) break;
      }
      if (!progressed) break;
    }
    const TT = Math.max(1, target.inf + target.cav + target.arc);
    const frac = { i: target.inf / TT, c: target.cav / TT, a: target.arc / TT };
    const rally = { inf: Math.min(s.inf, Math.round(frac.i * R)), cav: Math.min(s.cav, Math.round(frac.c * R)), arc: 0 };
    rally.arc = Math.min(s.arc, R - (rally.inf + rally.cav));
    s.inf -= rally.inf; s.cav -= rally.cav; s.arc -= rally.arc;
    const joins = [];
    for (let i = 0; i < Xn; i++) {
      if (C <= 0) { joins.push({ inf: 0, cav: 0, arc: 0 }); continue; }
      const m = { inf: Math.min(s.inf, Math.round(frac.i * C)), cav: Math.min(s.cav, Math.round(frac.c * C)), arc: 0 };
      m.arc = Math.min(s.arc, C - (m.inf + m.cav));
      s.inf -= m.inf; s.cav -= m.cav; s.arc -= m.arc;
      joins.push(m);
    }
    return { rally, packs: joins, leftover: s, fractions: frac };
  }

  // ── Call Rally Builder ──
  function buildCallRally(mode, stock, rallySize, tierKey, manual) {
    const s = cloneStock(stock);
    if (rallySize <= 0) return { rally: { inf: 0, cav: 0, arc: 0 }, stockAfter: s };
    const w = manual ? { inf: manual.i, cav: manual.c, arc: manual.a } : { inf: 1, cav: 1, arc: 1 };
    const W = Math.max(1e-9, w.inf + w.cav + w.arc);
    const t = rallySize;
    const r = { inf: Math.min(s.inf, Math.round((w.inf / W) * t)), cav: Math.min(s.cav, Math.round((w.cav / W) * t)), arc: 0 };
    const remaining = t - (r.inf + r.cav);
    r.arc = Math.min(s.arc, remaining);
    s.inf -= r.inf; s.cav -= r.cav; s.arc -= r.arc;
    return { rally: r, stockAfter: s };
  }

  function buildJoinRallies(mode, stockIn, X, cap, tierKey, manualTriplet) {
    const s = cloneStock(stockIn);
    const w = manualTriplet ? { inf: manualTriplet.i, cav: manualTriplet.c, arc: manualTriplet.a } : { inf: 1, cav: 1, arc: 1 };
    const W = Math.max(1e-9, w.inf + w.cav + w.arc);
    const packs = [];
    for (let i = 0; i < X; i++) {
      const tInf = Math.round((w.inf / W) * cap);
      const tCav = Math.round((w.cav / W) * cap);
      const p = { inf: Math.min(s.inf, tInf), cav: Math.min(s.cav, tCav), arc: 0 };
      const rem = cap - (p.inf + p.cav);
      p.arc = Math.min(s.arc, rem);
      s.inf -= p.inf; s.cav -= p.cav; s.arc -= p.arc;
      packs.push(p);
    }
    return { packs, leftover: s };
  }

  // ── Post-fix helpers ──
  function ensureMinInf(march, minInf, stockLeftover) {
    if (march.inf >= minInf) return;
    let need = minInf - march.inf;
    if (stockLeftover.inf > 0) { const take = Math.min(stockLeftover.inf, need); march.inf += take; stockLeftover.inf -= take; need -= take; }
    if (need > 0) { const fromCav = Math.min(march.cav, need); march.cav -= fromCav; march.inf += fromCav; need -= fromCav; }
    if (need > 0) { const fromArc = Math.min(march.arc, need); march.arc -= fromArc; march.inf += fromArc; }
  }

  function adjustCallRallyCapsAndBias(rally, leftover, rallySize, opts = {}) {
    const minInf = Math.ceil(rallySize * MIN_INF_PCT_MARCH);
    const maxInf = Math.floor(rallySize * (opts.maxInfPct ?? 0.20));
    const maxCav = Math.floor(rallySize * (opts.maxCavPct ?? 0.30));
    if (rally.inf < minInf) {
      let need = minInf - rally.inf;
      if (leftover.inf > 0) { const take = Math.min(leftover.inf, need); rally.inf += take; leftover.inf -= take; need -= take; }
      if (need > 0) { const fromCav = Math.min(rally.cav, need); rally.cav -= fromCav; rally.inf += fromCav; need -= fromCav; }
      if (need > 0) { const fromArc = Math.min(rally.arc, need); rally.arc -= fromArc; rally.inf += fromArc; }
    }
    function swapIntoArc(fromKey, amount) {
      if (amount <= 0 || leftover.arc <= 0) return 0;
      const give = Math.min(amount, leftover.arc);
      rally[fromKey] -= give; rally.arc += give; leftover.arc -= give; leftover[fromKey] += give;
      return give;
    }
    if (rally.inf > maxInf) {
      let cut = rally.inf - maxInf;
      const arcTaken = swapIntoArc("inf", cut); cut -= arcTaken;
      if (cut > 0 && rally.cav < maxCav && leftover.cav > 0) {
        const cavRoom = maxCav - rally.cav;
        const give = Math.min(cut, cavRoom, leftover.cav);
        rally.inf -= give; rally.cav += give; leftover.cav -= give; leftover.inf += give;
      }
    }
    if (rally.cav > maxCav) {
      let cut = rally.cav - maxCav;
      const arcFromCav = swapIntoArc("cav", cut); cut -= arcFromCav;
      if (cut > 0 && rally.inf < minInf) {
        const room = minInf - rally.inf;
        const give = Math.min(cut, room);
        rally.cav -= give; rally.inf += give; leftover.cav += give;
      }
    }
    const biasPct = opts.arcBiasPct ?? 0.03;
    let want = Math.max(0, Math.ceil(rallySize * biasPct));
    if (want > 0 && leftover.arc > 0) {
      if (rally.inf > minInf) {
        const canReduceInf = rally.inf - minInf;
        const takeFromInf = Math.min(canReduceInf, want, leftover.arc);
        rally.inf -= takeFromInf; rally.arc += takeFromInf; leftover.arc -= takeFromInf; leftover.inf += takeFromInf; want -= takeFromInf;
      }
      if (want > 0 && rally.cav > 0 && leftover.arc > 0) {
        const takeFromCav = Math.min(rally.cav, want, leftover.arc);
        rally.cav -= takeFromCav; rally.arc += takeFromCav; leftover.arc -= takeFromCav; leftover.cav += takeFromCav;
      }
    }
    if (rally.inf < minInf) {
      const need = minInf - rally.inf;
      const fromArc = Math.min(rally.arc, need); rally.arc -= fromArc; rally.inf += fromArc;
      let still = need - fromArc;
      if (still > 0) { const fromCav = Math.min(rally.cav, still); rally.cav -= fromCav; rally.inf += fromCav; }
    }
  }

  function applyPriorityPostFix(rally, joins, leftover, rallySize, joinCap) {
    const marches = [rally, ...joins];
    const mins = [Math.ceil(rallySize * MIN_INF_PCT_MARCH)];
    for (let i = 0; i < joins.length; i++) mins.push(Math.ceil(joinCap * MIN_INF_PCT_MARCH));
    for (let i = 0; i < marches.length; i++) ensureMinInf(marches[i], mins[i], leftover);
    for (const t of ["arc", "cav"]) {
      if (leftover[t] <= 0) continue;
      let progressed = true;
      while (leftover[t] > 0 && progressed) {
        progressed = false;
        for (let i = 0; i < marches.length; i++) {
          const m = marches[i];
          const minInf = mins[i];
          let excess = Math.max(0, m.inf - minInf);
          if (excess <= 0) continue;
          const give = Math.min(excess, leftover[t]);
          if (give <= 0) continue;
          m.inf -= give; m[t] += give; leftover[t] -= give; leftover.inf += give;
          progressed = true;
          if (leftover[t] <= 0) break;
        }
      }
    }
  }

  function deriveJoinFractions(joins) {
    let I = 0, C = 0, A = 0;
    for (const p of joins) { I += p.inf; C += p.cav; A += p.arc; }
    const S = Math.max(1, I + C + A);
    return { i: I / S, c: C / S, a: A / S };
  }

  // ── Sweep / Top-10 ──
  function* generateTriplets(bounds, stepPct) {
    const step = Math.max(1, stepPct | 0);
    for (let i = bounds.infMin; i <= bounds.infMax; i += step) {
      for (let c = bounds.cavMin; c <= bounds.cavMax; c += step) {
        const a = 100 - i - c;
        if (a < bounds.arcMin || a > bounds.arcMax || a < 0) continue;
        yield { i: i / 100, c: c / 100, a: a / 100, label: `${i}/${c}/${a}` };
      }
    }
  }

  function simulateTriplet(tierKey, stock0, rallySize, X, joinCap, triplet) {
    const cr = buildCallRally("ratio11", stock0, rallySize, tierKey, triplet);
    const jr = buildJoinRallies("ratio11", cr.stockAfter, X, joinCap, tierKey, triplet);
    const rally = cr.rally;
    const joins = jr.packs;
    const leftover = jr.leftover;
    const lo = cloneStock(leftover);
    applyPriorityPostFix(rally, joins, lo, rallySize, joinCap);
    const callScore = computeFormationDamage(rally, tierKey).finalScore;
    let joinScore = 0; for (const p of joins) joinScore += computeFormationDamage(p, tierKey).finalScore;
    const usedCallFrac = (() => { const T = Math.max(1, rally.inf + rally.cav + rally.arc); return { i: rally.inf / T, c: rally.cav / T, a: rally.arc / T }; })();
    const usedJoinFrac = deriveJoinFractions(joins);
    return { call: rally, joins, leftover: lo, callScore, joinScore, totalScore: callScore + joinScore, usedCallFrac, usedJoinFrac };
  }

  function findTopSetups(tierKey, stock0, rallySize, X, joinCap, opts = {}) {
    const bounds = { infMin: Math.round(MIN_INF_PCT_MARCH * 100), infMax: 40, cavMin: 10, cavMax: 45, arcMin: 0, arcMax: 100 };
    const stepPct = Math.min(Math.max(1, opts.stepPct || 1), 5);
    const results = [];
    for (const triplet of generateTriplets(bounds, stepPct)) {
      const res = simulateTriplet(tierKey, cloneStock(stock0), rallySize, X, joinCap, triplet);
      results.push({ triplet, ...res });
    }
    results.sort((a, b) => b.totalScore - a.totalScore);
    return results.slice(0, 10);
  }

  // ── Recommendation Engine ──
  function evaluateMarchQuality(p, cap) {
    const total = (p.inf | 0) + (p.cav | 0) + (p.arc | 0);
    const failures = [];
    const fill = cap > 0 ? total / cap : 0;
    if (fill < REC_FILL_GATE) failures.push(`fill ${(fill * 100).toFixed(0)}%<85%`);
    if (total > 0) {
      if (p.arc === 0 && p.inf > p.cav) failures.push('inf-only');
      if (p.cav === 0) failures.push('no-cav');
      const arcPct = p.arc / total;
      if (arcPct < REC_ARC_MIN_PCT) failures.push(`arc ${(arcPct * 100).toFixed(0)}%<10%`);
      const infPct = p.inf / total;
      const cavPct = p.cav / total;
      if (infPct > REC_MAX_SINGLE) failures.push(`inf ${(infPct * 100).toFixed(0)}%>80%`);
      if (cavPct > REC_MAX_SINGLE) failures.push(`cav ${(cavPct * 100).toFixed(0)}%>80%`);
      if (arcPct > REC_MAX_SINGLE) failures.push(`arc ${(arcPct * 100).toFixed(0)}%>80%`);
    }
    return { pass: failures.length === 0, failures };
  }

  function simulateJoinsForRec(stockAfterCall, n, cap, fractions) {
    const s = cloneStock(stockAfterCall);
    const fi = fractions.i ?? 0, fc = fractions.c ?? 0;
    const W = Math.max(1e-9, fi + fc + (fractions.a ?? 0));
    const packs = [];
    for (let i = 0; i < n; i++) {
      if (cap <= 0) { packs.push({ inf: 0, cav: 0, arc: 0 }); continue; }
      const p = { inf: Math.min(s.inf, Math.round((fi / W) * cap)), cav: Math.min(s.cav, Math.round((fc / W) * cap)), arc: 0 };
      const rem = cap - p.inf - p.cav;
      p.arc = Math.min(s.arc, Math.max(0, rem));
      s.inf -= p.inf; s.cav -= p.cav; s.arc -= p.arc;
      packs.push(p);
    }
    return packs;
  }

  function recommendMarchCount(rally, stockAfterCall, fractions, maxN, cap) {
    let bestN = 0;
    for (let n = 1; n <= Math.max(1, maxN | 0); n++) {
      const packs = simulateJoinsForRec(stockAfterCall, n, cap, fractions);
      let allPass = true;
      for (const p of packs) {
        if (!evaluateMarchQuality(p, cap).pass) { allPass = false; break; }
      }
      if (allPass) bestN = n;
    }
    return bestN;
  }

  // ── Option-A helpers (for the plot engine) ──
  function attackFactor(atk, leth) { return (1 + atk / 100) * (1 + leth / 100); }
  function getArcherCoefByTier(tierRaw) {
    const t = String(tierRaw || '').toUpperCase();
    return (t === 'T6') ? (4.4 / 1.25) : (2.78 / 1.45);
  }
  function computeExactOptimalFractions(stats, tierRaw) {
    const Ainf = attackFactor(stats.inf_atk, stats.inf_let);
    const Acav = attackFactor(stats.cav_atk, stats.cav_let);
    const Aarc = attackFactor(stats.arc_atk, stats.arc_let);
    const KARC = getArcherCoefByTier(tierRaw);
    const alpha = Ainf / 1.12;
    const beta = Acav;
    const gamma = KARC * Aarc;
    const a2 = alpha * alpha, b2 = beta * beta, g2 = gamma * gamma;
    const sum = a2 + b2 + g2;
    return { fin: a2 / sum, fcav: b2 / sum, farc: g2 / sum };
  }
  function enforceCompositionBounds(fin, fcav, farc) {
    let i = fin, c = fcav, a = farc;
    if (i < INF_MIN_PCT) i = INF_MIN_PCT;
    if (i > INF_MAX_PCT) i = INF_MAX_PCT;
    if (c < CAV_MIN_PCT) c = CAV_MIN_PCT;
    a = 1 - i - c;
    if (a < 0) { c = Math.max(CAV_MIN_PCT, 1 - i); a = 1 - i - c; if (a < 0) { a = 0; c = 1 - i; } }
    const S = i + c + a;
    if (S <= 0) return { fin: INF_MIN_PCT, fcav: CAV_MIN_PCT, farc: 1 - INF_MIN_PCT - CAV_MIN_PCT };
    return { fin: i / S, fcav: c / S, farc: a / S };
  }

  // ── Main Compute (Magic mode) ──
  function compute(inputs, focusMode) {
    const tierKey = inputs.tier;
    const stock0 = { inf: inputs.stock_inf, cav: inputs.stock_cav, arc: inputs.stock_arc };
    const rallySize = inputs.rallyCap;
    const joinCap = inputs.joinCap;
    const X = inputs.numMarches;
    const mode = focusMode ? 'magic12' : 'ratio11';

    let rally, joins, leftover, fractions;

    if (mode === 'magic12') {
      const top10 = findTopSetups(tierKey, stock0, rallySize, X, joinCap, { stepPct: 1 });
      let rallyBest, joinsBest, leftoverBest;
      if (top10 && top10.length) {
        const best = top10[0];
        rallyBest = best.call; joinsBest = best.joins; leftoverBest = best.leftover;
        adjustCallRallyCapsAndBias(rallyBest, leftoverBest, rallySize, { maxInfPct: 0.20, maxCavPct: 0.30, arcBiasPct: 0.03 });
        applyPriorityPostFix(rallyBest, joinsBest, leftoverBest, rallySize, joinCap);
        const archerOpt = planArcherPriorityAlloc(stock0, rallySize, X, joinCap, INF_MIN_PCT, INF_MAX_PCT, CAV_MIN_PCT);
        if (archerOpt.leftover.arc < leftoverBest.arc) {
          rallyBest = archerOpt.rally; joinsBest = archerOpt.packs; leftoverBest = archerOpt.leftover;
        }
      } else {
        const plan = planMagicAlloc('magic12', stock0, rallySize, X, joinCap, tierKey);
        rallyBest = plan.rally; joinsBest = plan.packs; leftoverBest = plan.leftover;
        adjustCallRallyCapsAndBias(rallyBest, leftoverBest, rallySize, { maxInfPct: 0.20, maxCavPct: 0.30, arcBiasPct: 0.03 });
        applyPriorityPostFix(rallyBest, joinsBest, leftoverBest, rallySize, joinCap);
        const archerOpt = planArcherPriorityAlloc(stock0, rallySize, X, joinCap, INF_MIN_PCT, INF_MAX_PCT, CAV_MIN_PCT);
        if (archerOpt.leftover.arc < leftoverBest.arc) {
          rallyBest = archerOpt.rally; joinsBest = archerOpt.packs; leftoverBest = archerOpt.leftover;
        }
      }
      rally = rallyBest; joins = joinsBest; leftover = leftoverBest;
      const tCall = Math.max(1, sumTroops(rally));
      fractions = { i: rally.inf / tCall, c: rally.cav / tCall, a: rally.arc / tCall };

      // Also get scoreboard from top10
      const scoreboard = (top10 || []).map((entry, idx) => {
        const callTrip = toPctTriplet(entry.usedCallFrac);
        const joinTrip = toPctTriplet(entry.usedJoinFrac);
        return { rank: idx + 1, callTrip, callScore: entry.callScore, joinTrip, joinScore: entry.joinScore, totalScore: entry.totalScore };
      });

      // Recommendation
      const stockAfterCall = {
        inf: Math.max(0, stock0.inf - rally.inf),
        cav: Math.max(0, stock0.cav - rally.cav),
        arc: Math.max(0, stock0.arc - rally.arc)
      };
      const recN = recommendMarchCount(rally, stockAfterCall, fractions, X, joinCap);

      return { rally, joins, leftover, fractions, stock0, recN, scoreboard, tierKey };

    } else {
      // ratio11 mode (non-focus)
      let stats = {
        inf_atk: inputs.inf_atk, inf_let: inputs.inf_let,
        cav_atk: inputs.cav_atk, cav_let: inputs.cav_let,
        arc_atk: inputs.arc_atk, arc_let: inputs.arc_let
      };
      for (const k of Object.keys(stats)) {
        if (!Number.isFinite(stats[k]) || stats[k] <= 0) stats[k] = 1;
      }
      let opt = computeExactOptimalFractions(stats, tierKey);
      opt = enforceCompositionBounds(opt.fin, opt.fcav, opt.farc);
      const frac = { i: opt.fin, c: opt.fcav, a: opt.farc };
      const cr = buildCallRally('ratio11', stock0, rallySize, tierKey, frac);
      const jr = buildJoinRallies('ratio11', cr.stockAfter, X, joinCap, tierKey, frac);
      rally = cr.rally; joins = jr.packs; leftover = jr.leftover;

      const archerOpt = planArcherPriorityAlloc(stock0, rallySize, X, joinCap, INF_MIN_PCT, INF_MAX_PCT, CAV_MIN_PCT);
      if (archerOpt.leftover.arc < leftover.arc) {
        rally = archerOpt.rally; joins = archerOpt.packs; leftover = archerOpt.leftover;
        fractions = archerOpt.fractions;
      } else {
        const tCall = Math.max(1, sumTroops(rally));
        fractions = { i: rally.inf / tCall, c: rally.cav / tCall, a: rally.arc / tCall };
      }

      // Score final
      const callScore = computeFormationDamage(rally, tierKey).finalScore;
      let joinScore = 0; for (const p of joins) joinScore += computeFormationDamage(p, tierKey).finalScore;
      const callTrip = toPctTriplet((() => { const T = Math.max(1, sumTroops(rally)); return { i: rally.inf / T, c: rally.cav / T, a: rally.arc / T }; })());
      const joinTrip = toPctTriplet(deriveJoinFractions(joins));
      const scoreboard = [{ rank: 1, callTrip, callScore, joinTrip, joinScore, totalScore: callScore + joinScore }];

      const stockAfterCall = {
        inf: Math.max(0, stock0.inf - rally.inf),
        cav: Math.max(0, stock0.cav - rally.cav),
        arc: Math.max(0, stock0.arc - rally.arc)
      };
      const recN = recommendMarchCount(rally, stockAfterCall, fractions, X, joinCap);

      return { rally, joins, leftover, fractions, stock0, recN, scoreboard, tierKey };
    }
  }

  // ── Load tiers ──
  async function loadTiers() {
    if (TIERS) return TIERS;
    try {
      const res = await fetch('tiers.json', { cache: 'no-store' });
      TIERS = await res.json();
    } catch (e) {
      console.error('tiers.json failed', e);
      TIERS = {
        tiers: {
          "T6": { "inf": [243, 730], "cav": [730, 243], "arc": [974, 183] },
          "T9": { "inf": [400, 1200], "cav": [1200, 400], "arc": [1600, 300] },
          "T10": { "inf": [472, 1416], "cav": [1416, 470], "arc": [1888, 354] },
          "T10.TG1": { "inf": [491, 1473], "cav": [1473, 491], "arc": [1964, 368] },
          "T10.TG2": { "inf": [515, 1546], "cav": [1546, 515], "arc": [2062, 387] },
          "T10.TG3": { "inf": [541, 1624], "cav": [1624, 541], "arc": [2165, 402] },
          "T10.TG4": { "inf": [568, 1705], "cav": [1705, 568], "arc": [2273, 426] },
          "T10.TG5": { "inf": [597, 1790], "cav": [1790, 597], "arc": [2387, 448] }
        }
      };
    }
    return TIERS;
  }

  return {
    compute,
    loadTiers,
    sumTroops,
    toPctTriplet,
    parseTriplet,
    computeFormationDamage,
    computeExactOptimalFractions,
    enforceCompositionBounds,
    attackFactor,
    getArcherCoefByTier
  };
})();
