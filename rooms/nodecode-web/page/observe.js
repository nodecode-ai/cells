// observe.js -- where the time and money went, drawn from the gateway's read
// models (surface/observe.lisp): one session's timeline -- the Run view and
// its inspector -- and the usage and activity across every session, with the
// gateway's health -- the Dashboard.
// Each number leads with a plain label and carries, small beside it, the name
// an engineer would search for; a number the gateway did not send is a dash,
// never a zero. What is worth a look is the gateway's call; this only says it
// in words. Nothing here listens: every control is a data-act the page's own
// handlers read.

import { t, lang } from "./i18n.js";
import { iconButton } from "./icons.js";

// Dates and weekdays in the page's language: an English page keeps the
// browser's own English, a page in another language its language's.
const LOCALE = lang === "en" && !/^en\b/i.test(navigator.language || "") ? "en" : lang === "en" ? undefined : lang;

export const el = (tag, className, text) => {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
};

export const button = (className, text, data) => {
  const node = el("button", className, text);
  node.type = "button";
  Object.assign(node.dataset, data);
  return node;
};

// The same as a mark (icons.js) that says WORD; a trash mark takes the danger
// tone under the hand. While its act runs it is held, its tooltip saying BUSY.
export function mark(name, word, data, busy) {
  const node = iconButton(name, word);
  Object.assign(node.dataset, data);
  if (name === "trash") node.classList.add("drop");
  if (busy) {
    node.title = busy;
    node.disabled = true;
    node.setAttribute("aria-busy", "true");
  }
  return node;
}

const oneLine = (text) => (text || "").replace(/\s+/g, " ").trim();
const sum = (items, pick) => items.reduce((total, item) => total + (pick(item) || 0), 0);
// Sentences said one after another: a space between them, none after a
// full-width stop, which carries its own.
const says = (sentences) => sentences.filter(Boolean).join(" ").replace(/([。！？；：]) +/g, "$1");

// --- how a number reads -----------------------------------------------------------

export function ms(value) {
  if (value == null) return "—";
  if (value < 1000) return `${Math.round(value)} ms`;
  const s = value / 1000;
  if (s < 10) return `${s.toFixed(1)} s`;
  if (s < 59.5) return `${Math.round(s)} s`;
  // Whole seconds first, so 59.6 s past a minute reads as the next minute.
  const whole = Math.round(s);
  const m = Math.floor(whole / 60);
  if (m < 60) return `${m}m ${String(whole % 60).padStart(2, "0")}s`;
  return `${Math.floor(m / 60)}h ${String(m % 60).padStart(2, "0")}m`;
}

export function usd(value) {
  if (value == null) return "—";
  if (value === 0) return "$0";
  if (value < 0.01) return `$${value.toFixed(3)}`;
  if (value < 100) return `$${value.toFixed(2)}`;
  return `$${Math.round(value).toLocaleString("en-US")}`;
}

export const tokens = (n) => (n == null ? "—"
  : n >= 1e6 ? `${(n / 1e6).toFixed(1)}M`
  : n >= 1e4 ? `${Math.round(n / 1000)}k`
  : n >= 1000 ? `${(n / 1000).toFixed(1)}k`
  : String(n));
const count = (n) => (n == null ? "—" : n.toLocaleString("en-US"));
const pct = (share) => (share == null || !isFinite(share) ? "—" : `${Math.round(100 * share)}%`);

// A duration the way the headline says it.
function spoken(value) {
  const s = Math.round((value || 0) / 1000);
  if (s < 60) return plural(s, t("{n} second"), t("{n} seconds"));
  const m = Math.round(s / 60);
  if (m < 60) return plural(m, t("{n} minute"), t("{n} minutes"));
  const hours = plural(Math.floor(m / 60), t("{n} hour"), t("{n} hours"));
  return m % 60 ? t("{hours} {n} min", { hours, n: m % 60 }) : hours;
}

const promptOf = (c) => (c.input || 0) + (c.cached || 0) + (c.cache_write || 0);

// --- the flags ---------------------------------------------------------------------

const FLAGS = {
  failed: { label: t("Failed"), tone: "danger" },
  cache_miss: { label: t("Cache miss"), tone: "hot" },
  slow_start: { label: t("Slow start"), tone: "hot" },
  retried: { label: t("Retried"), tone: "hot" },
  switched_model: { label: t("Switched model"), tone: "hot" },
  memory_trimmed: { label: t("Memory trimmed"), tone: "quiet" },
};
const ORDER = Object.keys(FLAGS);

// What a turn no one asked is, by the origin the gateway names it (NLK:TURN-ORIGIN):
// the words the transcript draws it by. Such a turn has no number.
export const ORIGIN_WORDS = { reflection: t("Reflection recap"), wake: t("Woke") };

// A turn as its place says it: the operator's number, or what began it.
const turnWord = (turn) => ORIGIN_WORDS[turn.origin] || t("turn {n}", { n: turn.number });

function chip(flag, times) {
  const known = FLAGS[flag] || { label: flag, tone: "quiet" };
  const node = el("span", "flag", times > 1 ? `${known.label} ×${times}` : known.label);
  node.dataset.tone = known.tone;
  return node;
}

// What the timeline flagged, in words: one finding per thing to look at,
// the gravest first, then the newest.
function findings(data) {
  const out = [];
  const usual = data.totals.usual_wait_ms;
  for (const turn of data.turns) {
    turn.calls.forEach((call, index) => {
      if (!call.flags.includes("cache_miss")) return;
      const fresh = (call.input || 0) + (call.cache_write || 0);
      out.push({
        flag: "cache_miss", turn: turn.n, call: index,
        line: call.diverged_at != null
          ? t("Paid full price for {tokens} tokens: the prompt changed at message {message}.", { tokens: tokens(fresh), message: call.diverged_at })
          : t("Paid full price for {tokens} tokens: the provider did not reuse the prompt.", { tokens: tokens(fresh) }),
        tag: call.diverged_in ? `prefix-diverged-in ${call.diverged_in}` : t("cached {cached} of {prompt}", { cached: count(call.cached || 0), prompt: count(promptOf(call)) }),
      });
    });
    const slow = turn.calls.map((call, index) => [call, index]).filter(([call]) => call.flags.includes("slow_start"));
    if (slow.length) {
      const [call, index] = slow.reduce((a, b) => (b[0].wait_ms > a[0].wait_ms ? b : a));
      out.push({
        flag: "slow_start", times: slow.length, turn: turn.n, call: index,
        line: t("Waited {wait} for the first word. This session usually waits {usual}.", { wait: ms(call.wait_ms), usual: ms(usual) }),
        tag: `ttft-ms ${count(call.wait_ms)} · p50 ${count(usual)}`,
      });
    }
    if (turn.retries.length) {
      const status = turn.retries.find((retry) => retry.status)?.status;
      const n = turn.retries.length;
      out.push({
        flag: "retried", turn: turn.n,
        line: status
          ? (n === 1 ? t("The provider failed once ({status}); nodecode tried again.", { status })
            : t("The provider failed {n} times ({status}); nodecode tried again.", { n, status }))
          : n === 1 ? t("The provider failed once; nodecode tried again.") : t("The provider failed {n} times; nodecode tried again.", { n }),
        tag: `provider_retry${status ? ` status ${status}` : ""}`,
      });
    }
    for (const fallback of turn.fallbacks) {
      out.push({ flag: "switched_model", turn: turn.n, line: t("Moved from {from} to {to} ({reason}).", fallback), tag: "provider_fallback" });
    }
    if (turn.flags.includes("memory_trimmed")) {
      out.push({ flag: "memory_trimmed", turn: turn.n, line: t("Older turns were set aside to make room. They stay in the transcript."), tag: "context.evicted" });
    }
    if (turn.status === "failed") {
      out.push({ flag: "failed", turn: turn.n, line: turn.detail ? t("The turn stopped: {detail}", { detail: oneLine(turn.detail).slice(0, 160) }) : t("The turn stopped."), tag: "turn.failed" });
    }
  }
  return out.sort((a, b) => ORDER.indexOf(a.flag) - ORDER.indexOf(b.flag) || b.turn - a.turn);
}

// --- pieces the views share ---------------------------------------------------------

function tile(label, value, tag) {
  const node = el("div", "tile");
  node.append(el("span", "label", label), el("span", "value", value), el("span", "tag", tag));
  return node;
}

function headline(micro, main, sub) {
  const node = el("section", "headline");
  node.append(el("p", "micro", micro), el("h2", "display", main));
  if (sub) node.append(el("p", "sub", sub));
  return node;
}

export function sectionHead(title, ...rest) {
  const head = el("div", "obs-head");
  head.append(el("h3", "micro", title), ...rest);
  return head;
}

function legend() {
  const node = el("div", "legend");
  for (const [kind, text] of [["wait", t("Waiting for the first word")], ["write", t("Model writing")], ["step", t("Steps")], ["hot", t("Flagged")]]) {
    const item = el("span");
    item.append(el("i", `swatch ${kind}`), text);
    node.append(item);
  }
  return node;
}

// --- the Run view: one session ------------------------------------------------------

export function drawRun(data, ui) {
  const root = el("div", "observe-run");
  if (!data) { root.append(el("p", "obs-note", ui.error ? t("Could not read this session: {error}", { error: ui.error }) : t("Reading this session…"))); return root; }
  const all = data.totals;
  if (!data.turns.length) { root.append(el("p", "obs-note", t("Nothing has run in this session yet."))); return root; }
  const found = findings(data);
  const modelMs = (all.wait_ms || 0) + (all.stream_ms || 0);
  const stepMs = all.step_ms || 0;
  const modelShare = modelMs + stepMs ? Math.round((100 * modelMs) / (modelMs + stepMs)) : null;
  const lastModel = data.turns.flatMap((turn) => turn.calls).at(-1)?.model;
  const sub = says([
    modelShare != null ? t("The model took {model}% of the time and steps took {steps}%.", { model: modelShare, steps: 100 - modelShare }) : "",
    all.cost_usd == null ? (lastModel ? t("No price is known for {model}, so cost shows as a dash.", { model: lastModel })
      : t("No price is known for this model, so cost shows as a dash."))
      : all.cost_complete === false ? t("Some calls have no price, so the cost is a floor.") : "",
    found.length ? (found.length === 1 ? t("One thing is worth a look below.") : t("{n} things are worth a look below.", { n: found.length })) : "",
  ]);
  const said = { turns: plural(all.turns, t("{n} turn"), t("{n} turns")), time: spoken(all.ms), cost: usd(all.cost_usd) };
  root.append(headline(t("This session"), all.cost_usd != null ? t("{turns} in {time}, about {cost}.", said) : t("{turns} in {time}.", said), sub));

  const tiles = el("section", "tiles");
  tiles.setAttribute("aria-label", t("Totals"));
  tiles.append(
    tile(t("Time"), ms(all.ms), t("wall time")),
    tile(t("Cost"), usd(all.cost_usd), all.cost_complete === false && all.cost_usd != null ? t("cost_usd · partly priced") : "cost_usd"),
    tile(t("Model calls"), count(all.calls), t("provider requests")),
    tile(t("From cache"), pct((all.cached || 0) / ((all.input || 0) + (all.cached || 0) + (all.cache_write || 0))), t("cache hit")),
    tile(t("First word, typical"), ms(all.usual_wait_ms), "TTFT p50"),
    tile(t("Memory at peak"), all.window ? pct(all.peak_prompt / all.window) : tokens(all.peak_prompt),
      all.window ? t("context {used} / {window}", { used: tokens(all.peak_prompt), window: tokens(all.window) }) : t("context tokens")),
  );
  root.append(tiles);

  if (found.length) {
    const section = el("section", "findings");
    section.append(sectionHead(t("Worth a look"), el("span", "reading", plural(found.length, t("{n} flag"), t("{n} flags")))));
    const grid = el("div", "cards");
    const shown = ui.all ? found : found.slice(0, 4);
    for (const finding of shown) {
      const card = button("card", undefined, { act: finding.call != null ? "call" : "turn", turn: finding.turn, ...(finding.call != null ? { call: finding.call } : {}) });
      const top = el("span", "card-top");
      top.append(chip(finding.flag, finding.times), el("span", "reading", turnWord(data.turns.find((each) => each.n === finding.turn))));
      card.append(top, el("span", "line", finding.line), el("span", "tag", finding.tag));
      grid.append(card);
    }
    section.append(grid);
    if (found.length > shown.length) section.append(button("word more", t("{n} more", { n: found.length - shown.length }), { act: "all-flags" }));
    root.append(section);
  }

  const section = el("section", "turns");
  section.append(sectionHead(t("Turns"), legend(), el("span", "reading", t("time · cost"))));
  const longest = Math.max(1, ...data.turns.map((turn) => turn.ms || 0));
  const list = el("div", "turn-list");
  for (const turn of data.turns) {
    const open = ui.open === turn.n;
    const row = button("trow", undefined, { act: "turn", turn: turn.n });
    row.setAttribute("aria-expanded", String(open));
    const wait = sum(turn.calls, (call) => call.wait_ms);
    const write = sum(turn.calls, (call) => call.stream_ms);
    const steps = sum(turn.steps, (step) => step.ms);
    const bar = el("span", "bar");
    const hot = turn.flags.includes("slow_start") || turn.flags.includes("cache_miss");
    for (const [kind, value] of [[hot ? "wait hot" : "wait", wait], ["write", write], ["step", steps]]) {
      const part = el("i", kind);
      part.style.width = `${(100 * value) / longest}%`;
      bar.append(part);
    }
    const flags = el("span", "flags");
    const first = ORDER.find((flag) => turn.flags.includes(flag));
    if (first) flags.append(chip(first));
    if (turn.status === "running") flags.append(el("span", "reading live-word", t("running")));
    const said = ORIGIN_WORDS[turn.origin];
    row.append(el("span", "n", turn.number == null ? "" : String(turn.number)),
      el("span", said ? "prompt lst-note" : "prompt", said || turn.prompt || t("(no prompt)")), bar,
      el("span", "num", ms(turn.ms)), el("span", "num", turn.calls.length ? usd(turn.cost_usd) : ""), flags);
    list.append(row);
    if (open) list.append(turnDetail(turn, ui));
  }
  section.append(list);
  root.append(section);
  return root;
}

// One turn opened: its clock (drawTimeline) and what it came to.
function turnDetail(turn, ui) {
  const detail = el("div", "tdetail");
  const worst = turn.calls.map((call, index) => [call, index]).filter(([call]) => call.flags.length)
    .sort((a, b) => (b[0].wait_ms || 0) - (a[0].wait_ms || 0))[0];
  const caption = el("div", "caption");
  const parts = { calls: plural(turn.calls.length, t("{n} model call"), t("{n} model calls")), steps: plural(turn.steps.length, t("{n} step"), t("{n} steps")) };
  const said = [t("{calls} and {steps}.", parts)];
  if (worst) {
    const [call, index] = worst;
    const values = { n: index + 1, wait: ms(call.wait_ms) };
    said.push(!call.flags.includes("cache_miss") ? t("Call {n} waited {wait} for its first word.", values)
      : call.wait_ms != null ? t("Call {n} re-read the prompt at full price and waited {wait} for its first word.", values)
      : t("Call {n} re-read the prompt at full price.", values));
  } else if (turn.steps.length) {
    const longest = turn.steps.reduce((a, b) => ((b.ms || 0) > (a.ms || 0) ? b : a));
    said.push(longest.title ? t("The longest step took {time}: {title}.", { time: ms(longest.ms), title: longest.title })
      : t("The longest step took {time}.", { time: ms(longest.ms) }));
  }
  caption.append(el("p", "", says(said)));
  if (turn.calls.length) {
    const pick = worst ? worst[1] : turn.calls.length - 1;
    caption.append(button("word open-call", t("Open call {n}", { n: pick + 1 }), { act: "call", turn: turn.n, call: pick }));
  }
  detail.append(drawTimeline(turn, ui), caption);
  return detail;
}

// --- the clock: one turn's calls and steps over time --------------------------------
// The window is the whole turn unless ui.zoom ({ n, from, to }) narrows it. The
// page's own handlers move the window (the wheel, a pinch, a drag, the keys) and
// redraw this alone; every bar carries its start and end for a double click to
// fit the window to.

const EVERY = [1, 2, 5, 10, 20, 50, 100, 200, 500, 1e3, 2e3, 5e3, 10e3, 15e3, 30e3, 60e3,
  120e3, 300e3, 600e3, 900e3, 1800e3, 3600e3, 7200e3];

// How long a turn's clock runs, in ms past its start: the whole turn, never
// less than 1.
export const turnSpan = (turn) => Math.max(turn.ms || 0, 1);

// A place on the clock in the unit the ticks' spacing EVERY asks for:
// milliseconds under a second, seconds to the digits the spacing needs, then
// minutes.
function clock(value, every) {
  if (value < 0.5) return "0";
  const digits = every >= 1000 ? 0 : every >= 100 ? 1 : every >= 10 ? 2 : 3;
  if (value < 1000 && every < 1000) return `${Math.round(value)} ms`;
  if (value < 60e3) return `${(value / 1000).toFixed(digits)} s`;
  if (every >= 60e3) return ms(value).replace(/m 00s$/, "m");
  const seconds = ((value % 60e3) / 1000).toFixed(digits);
  return `${Math.floor(value / 60e3)}m${Number(seconds) ? ` ${seconds}s` : ""}`;
}

export function drawTimeline(turn, ui) {
  const whole = turnSpan(turn);
  const zoom = ui.zoom?.n === turn.n ? ui.zoom : null;
  const from = zoom ? zoom.from : 0;
  const to = zoom ? zoom.to : whole;
  const view = {
    shown: (start, length) => start <= to && start + length >= from,
    // A bar at START, LENGTH long, placed in the window and marked with both.
    place(node, start, length) {
      node.style.left = `${(100 * (start - from)) / (to - from)}%`;
      node.style.width = `${(100 * length) / (to - from)}%`;
      node.dataset.start = String(start);
      node.dataset.end = String(start + length);
      return node;
    },
  };
  const box = el("div", "timeline");
  box.tabIndex = 0;
  box.setAttribute("aria-label", t("{turn} over time: plus and minus zoom, 0 shows all of it, the arrows move along it", { turn: turnWord(turn) }));
  const bar = el("div", "zoom-bar");
  for (const [text, zoomTo, label] of [["−", "out", t("Zoom out (−)")], ["+", "in", t("Zoom in (+)")], [t("Fit"), "fit", t("Show the whole turn (0)")]]) {
    const node = button("word", text, { act: "zoom", zoom: zoomTo });
    node.title = label;
    node.setAttribute("aria-label", label);
    node.disabled = zoomTo !== "in" && !zoom;
    bar.append(node);
  }
  const every = (to - from) / 10;
  const reading = zoom ? t("{from} to {to} of {whole}", { from: clock(from, every), to: clock(to, every), whole: ms(whole) })
    : t("whole turn, {time}", { time: ms(whole) });
  bar.append(el("span", "reading", reading), el("span", "hint", t("Ctrl+scroll or pinch to zoom · drag to move · double-click a bar")));
  const lanes = el("div", "lanes");
  lanes.append(el("span"), ticks(from, to), el("span", "lane-label", t("Model calls")), callTrack(turn, view),
    el("span", "lane-label", t("Steps")), stepTrack(turn.steps, view));
  box.append(bar, lanes);
  return box;
}

function ticks(from, to) {
  const most = matchMedia("(max-width: 1023px)").matches ? 4 : 8;
  const every = EVERY.find((step) => (to - from) / step <= most) || EVERY.at(-1);
  const track = el("div", "track ticks");
  // The last label keeps its width's room at the right edge.
  for (let value = Math.ceil(from / every) * every; value <= to - ((to - from) * 0.6) / most; value += every) {
    const tick = el("span", "tick", clock(value, every));
    tick.style.left = `${(100 * (value - from)) / (to - from)}%`;
    track.append(tick);
  }
  return track;
}

// TURN's model calls, each opening the inspector.
function callTrack(turn, view) {
  const track = el("div", "track");
  turn.calls.forEach((call, index) => {
    const length = (call.wait_ms || 0) + (call.stream_ms || 0);
    if (call.at_ms == null || !view.shown(call.at_ms, length)) return;
    const block = view.place(button("call", undefined, { act: "call", turn: turn.n, call: index }), call.at_ms, length);
    const wait = el("i", call.flags.length ? "wait hot" : "wait");
    wait.style.width = `${(100 * (call.wait_ms || 0)) / (length || 1)}%`;
    block.append(wait, el("i", "write"));
    const values = { n: index + 1, model: call.model, wait: ms(call.wait_ms), write: ms(call.stream_ms), cost: usd(call.cost_usd) };
    block.title = t("Call {n} · {model} · waited {wait} · wrote {write} · {cost}", values);
    block.setAttribute("aria-label", t("Model call {n}: waited {wait}, wrote for {write}", values));
    track.append(block);
  });
  return track;
}

function stepTrack(steps, view) {
  const track = el("div", "track");
  for (const step of steps) {
    if (step.at_ms == null || !view.shown(step.at_ms, step.ms || 0)) continue;
    const block = view.place(el("i", "step"), step.at_ms, step.ms || 0);
    block.title = [step.title || step.tool, ms(step.ms)].filter(Boolean).join(" · ");
    track.append(block);
  }
  return track;
}

// --- the inspector: one model call -------------------------------------------------------

const FINISH = {
  tool_calls: t("It asked to run steps"),
  stop: t("It finished its answer"),
  length: t("It hit the output limit"),
  end_turn: t("It finished its answer"),
  tool_use: t("It asked to run steps"),
};

// The inspector's tabs: the id an address names one by, its label, its title.
export const INSPECTOR_TABS = [["summary", t("Summary")], ["tokens", t("Tokens")], ["request", t("What was sent"), t("raw request body")], ["raw", t("Raw")]];

export function drawInspector(data, turnN, callIndex, tab, request, open = new Set()) {
  const turn = data.turns.find((each) => each.n === turnN);
  const call = turn?.calls[callIndex];
  const root = el("div", "inspector");
  if (!call) return root;
  const head = el("div", "sheet-head");
  const nav = el("span", "nav");
  const prev = button("word", "‹", { act: "call", turn: turnN, call: callIndex - 1 });
  prev.disabled = callIndex === 0;
  prev.setAttribute("aria-label", t("Previous call"));
  const next = button("word", "›", { act: "call", turn: turnN, call: callIndex + 1 });
  next.disabled = callIndex >= turn.calls.length - 1;
  next.setAttribute("aria-label", t("Next call"));
  nav.append(prev, next, mark("close", t("Close"), { act: "close-drawer" }));
  head.append(el("span", "micro", t("{turn} · model call {n} of {calls}", { turn: turnWord(turn), n: callIndex + 1, calls: turn.calls.length })), nav);

  const prompt = promptOf(call);
  const fresh = (call.input || 0) + (call.cache_write || 0);
  const usual = data.totals.usual_wait_ms;
  const miss = call.flags.includes("cache_miss");
  const slow = call.flags.includes("slow_start");
  const title = miss ? t("Paid full price for the prompt") : slow ? t("Slow to start") : t("One model call");
  const sentences = [];
  if (miss) sentences.push(call.diverged_at != null
    ? t("The prompt changed at message {message}, so {tokens} tokens were read without the cache.", { message: call.diverged_at, tokens: tokens(fresh) })
    : t("{fresh} of {prompt} tokens were read without the cache.", { fresh: tokens(fresh), prompt: tokens(prompt) }));
  if (call.wait_ms != null) sentences.push(slow && usual
    ? t("The first word took {wait}, about {times} times this session's usual {usual}.", { wait: ms(call.wait_ms), times: Math.round(call.wait_ms / usual), usual: ms(usual) })
    : t("The first word came after {wait}.", { wait: ms(call.wait_ms) }));
  if (call.cost_usd != null) sentences.push(t("This call cost {cost}.", { cost: usd(call.cost_usd) }));
  const top = el("div", "insp-top");
  const chips = el("div", "chips");
  for (const flag of call.flags) chips.append(chip(flag));
  if (call.flags.length) top.append(chips);
  top.append(el("h3", "", title), el("p", "", says(sentences)));

  const tabs = el("div", "tabs");
  tabs.setAttribute("role", "tablist");
  for (const [id, label, title] of INSPECTOR_TABS) {
    const node = button("tab", label, { tab: id });
    node.setAttribute("role", "tab");
    node.setAttribute("aria-selected", String(tab === id));
    if (title) node.title = title;
    tabs.append(node);
  }
  const body = el("div", "insp-body");
  if (tab === "tokens") body.append(tokensPane(call));
  else if (tab === "request") body.append(requestPane(call, turn, data.session_id, request, open));
  else if (tab === "raw") {
    body.append(el("p", "obs-note", t("This call as the gateway's timeline read it from the stored facts (turn.usage, turn.provider_request).")));
    body.append(el("pre", "raw", JSON.stringify(call, null, 2)));
  } else body.append(summaryPane(call, turn, data));
  root.append(head, top, tabs, body);
  return root;
}

function summaryPane(call, turn, data) {
  const prompt = promptOf(call);
  const window = data.totals.window;
  const steps = turn.steps.filter((step) => call.at_ms != null && step.at_ms != null
    && step.at_ms >= call.at_ms && step.at_ms < (turn.calls[turn.calls.indexOf(call) + 1]?.at_ms ?? Infinity));
  const rate = call.output && call.stream_ms ? Math.round(call.output / (call.stream_ms / 1000)) : null;
  const writing = { time: ms(call.stream_ms), n: count(call.output), rate };
  const rows = [
    [t("Model"), call.effort ? t("{model}, {effort} effort", { model: call.model, effort: call.effort }) : call.model, "model · reasoning-effort"],
    [t("Memory sent"), !prompt ? "—" : window ? t("{n} tokens, {share} of {window}", { n: count(prompt), share: pct(prompt / window), window: tokens(window) })
      : t("{n} tokens", { n: count(prompt) }), t("input + cached + cache write")],
    [t("From cache"), call.cached != null ? t("{n} tokens ({share})", { n: count(call.cached), share: pct(call.cached / (prompt || 1)) }) : "—", "cached-input-tokens", call.flags.includes("cache_miss")],
    [t("Prompt changed at"), call.diverged_at == null ? "—" : call.messages ? t("Message {n} of {total}", { n: call.diverged_at, total: call.messages })
      : t("Message {n}", { n: call.diverged_at }), call.diverged_in ? `prefix-diverged-in ${call.diverged_in}` : "prefix-diverged-in", call.flags.includes("cache_miss")],
    [t("First word after"), ms(call.wait_ms), call.wait_ms != null ? `ttft-ms ${count(call.wait_ms)}` : "ttft-ms", call.flags.includes("slow_start")],
    [t("Writing"), call.stream_ms == null ? "—" : call.output == null ? writing.time
      : rate ? t("{time}, {n} tokens, {rate} a second", writing) : t("{time}, {n} tokens", writing), "stream-duration-ms · output-tokens"],
    [t("Thinking"), call.reasoning != null ? t("{n} of those tokens", { n: count(call.reasoning) }) : "—", "reasoning-tokens"],
    [t("Cost"), usd(call.cost_usd), call.cost_usd != null ? "cost_usd" : t("cost_usd · no price known")],
    [t("Tries"), call.attempts ? (call.attempts === 1 ? t("1, no retries") : String(call.attempts)) : "—", "transport-attempts"],
    [t("Ended because"), FINISH[call.finish] || call.finish || "—", call.finish ? `finish-reason ${call.finish}` : "finish-reason"],
  ];
  const list = el("dl", "facts");
  for (const [label, value, tag, hot] of rows) {
    const row = el("div", hot ? "fact hot" : "fact");
    const dd = el("dd");
    dd.append(el("span", "value", value), el("span", "tag", tag));
    row.append(el("dt", "", label), dd);
    list.append(row);
  }
  const node = el("div");
  node.append(list);
  if (steps.length) {
    node.append(el("p", "micro steps-head", t("Steps it asked for")));
    const ol = el("ol", "steps");
    for (const step of steps) {
      const li = el("li");
      li.append(el("i", "swatch step"), el("span", "name", step.title || step.tool), el("span", "num", ms(step.ms)));
      ol.append(li);
    }
    node.append(ol);
  }
  return node;
}

function stack(parts) {
  const bar = el("div", "stack");
  const total = sum(parts, (part) => part[1]) || 1;
  const key = el("div", "stack-key");
  for (const [label, value, kind] of parts) {
    if (!value) continue;
    const part = el("i", kind);
    part.style.width = `${(100 * value) / total}%`;
    bar.append(part);
    const item = el("span");
    item.append(el("i", `swatch ${kind}`), `${label} ${count(value)} (${pct(value / total)})`);
    key.append(item);
  }
  const node = el("div", "stack-box");
  node.append(bar, key);
  return node;
}

function tokensPane(call) {
  const node = el("div", "tokens-pane");
  const prompt = promptOf(call);
  node.append(el("p", "line", t("What it sent: {n} tokens", { n: count(prompt) })),
    stack([[t("From cache"), call.cached, "write"], [t("Full price"), call.input, call.flags.includes("cache_miss") ? "wait hot" : "step"], [t("Written to cache"), call.cache_write, "wait"]]));
  const answer = (call.output || 0) - (call.reasoning || 0);
  node.append(el("p", "line", t("What it wrote: {n} tokens", { n: count(call.output) })),
    stack([[t("Thinking"), call.reasoning, "step"], [t("Answer and step requests"), answer > 0 ? answer : null, "write"]]));
  node.append(el("p", "obs-note", call.cost_usd != null
    ? t("This call cost {cost} at the model's catalog price when it ran.", { cost: usd(call.cost_usd) })
    : t("No price is known for this model, so its cost is unknown.")));
  return node;
}

// --- what was sent: the call's request body, byte for byte -----------------------------
// The body as the gateway kept it (the request read, surface/observe.lisp): its
// size, how much of it the request kept before it had already sent -- the part a
// provider's cache can reuse, so where the two part is what explains a miss --
// and the JSON itself, every long string and every array or object below the top
// folded until clicked, so a megabyte opens as a dozen lines.

const LONG = 280;
export const bytes = (n) => (n >= 1048576 ? `${(n / 1048576).toFixed(1)} MB` : n >= 1024 ? `${Math.round(n / 1024)} KB` : `${n} B`);

function requestPane(call, turn, sessionId, request, open) {
  const node = el("div", "request-pane");
  if (request?.status !== "ok") {
    node.append(el("p", "obs-note", request?.status === "missing"
      ? t("This call's request was not kept: it ran before nodecode kept requests, or while request_bodies.keep_mb was 0, or it was among the oldest dropped past that cap.")
      : request?.status === "error" ? t("Could not read it: {error}", { error: request.error }) : t("Reading what was sent…")));
    return node;
  }
  const size = request.bytes.length;
  const shared = request.shared;
  const [key, list] = Object.entries(request.value || {}).find(([name, v]) => Array.isArray(v) && ["input", "messages", "contents"].includes(name)) || [];
  const [top, index] = (request.divergedIn || "").match(/^([^[]*)(?:\[(\d+)\])?$/)?.slice(1) || [];
  const [session, turnId] = request.previous || [];
  const before = !request.previous ? t("nothing: this one was kept whole")
    : session !== sessionId ? t("a call in another session, {session}", { session })
    : turnId === turn.turn_id ? t("the call before it in this turn") : t("the last call of an earlier turn");
  const miss = call.flags.includes("cache_miss");
  const at = { path: `${top}[${index}]`, n: index, total: list ? count(list.length) : "" };
  const rows = [
    [t("Size"), t("{size} · {n} bytes", { size: bytes(size), n: count(size) }), t("raw request body · application/json")],
    [t("Messages"), list ? count(list.length) : "—", key ? `${key}[]` : t("no message array")],
    [t("Compared with"), before, "x-previous"],
    [t("Same as before"), shared == null ? "—" : shared >= size ? t("all of it") : t("the first {size} ({share})", { size: bytes(shared), share: pct(shared / size) }),
      shared == null ? "x-shared-bytes" : `x-shared-bytes ${count(shared)}`],
    [t("Changed at"), shared == null || shared >= size ? "—" : index == null ? top || t("the start")
      : list ? t("{path}, message {n} of {total}", at) : t("{path}, message {n}", at),
    `prefix-diverged-in ${request.divergedIn || "—"}`, miss],
  ];
  const facts = el("dl", "facts");
  for (const [label, value, tag, hot] of rows) {
    const row = el("div", hot ? "fact hot" : "fact");
    const dd = el("dd");
    dd.append(el("span", "value", value), el("span", "tag", tag));
    row.append(el("dt", "", label), dd);
    facts.append(row);
  }
  node.append(facts);
  if (shared != null) node.append(stack([[t("Same as before"), Math.min(shared, size), "write"], [t("New or changed"), size - Math.min(shared, size), miss ? "wait hot" : "step"]]));
  const actions = el("div", "request-actions");
  actions.append(mark("copy", t("Copy"), { act: "copy-request" }), mark("download", t("Download"), { act: "download-request" }));
  node.append(actions);
  if (request.value === undefined) { node.append(el("pre", "raw", request.text.slice(0, 200000))); return node; }
  const tree = el("div", "json");
  jsonLine(tree, 0, null, request.value, "", open, (name, at) => {
    if (top == null || shared == null || shared >= size) return "";
    const keys = Object.keys(request.value);
    if (at == null) return keys.indexOf(name) < keys.indexOf(top) ? "same" : name === top ? "diverged" : "";
    return name !== top || index == null ? "" : at < Number(index) ? "same" : at === Number(index) ? "diverged" : "";
  });
  node.append(tree);
  return node;
}

// One line of the JSON tree, and its children when unfolded: a container folds
// to a preview of its first fields, a long string to its head. MARK says whether
// a top-level key, or an item of the top-level array the change fell in, is the
// same as before or where it changed.
function jsonLine(root, depth, name, value, path, open, mark, item) {
  const line = el("div", `jl ${depth === 1 ? mark(name) : depth === 2 ? mark(item, name) : ""}`);
  line.style.setProperty("--d", depth);
  const key = name === null ? "" : el("span", "jk", `${name}: `);
  root.append(line);
  const folded = !open.has(path);
  if (value === null || typeof value !== "object") {
    line.append(key);
    const text = JSON.stringify(value);
    if (typeof value !== "string" || value.length <= LONG) line.append(el("span", typeof value === "string" ? "js" : "jv", text));
    else if (folded) line.append(el("span", "js", `${JSON.stringify(value.slice(0, LONG)).slice(0, -1)}…"`), button("fold", t("+{n} characters", { n: count(value.length - LONG) }), { act: "fold", path }));
    else {
      line.append(button("fold", t("{n} characters, fold", { n: count(value.length) }), { act: "fold", path }));
      const whole = el("div", "jstr", value);
      whole.style.setProperty("--d", depth + 1);
      root.append(whole);
    }
    return;
  }
  const array = Array.isArray(value);
  const entries = array ? value.map((each, at) => [at, each]) : Object.entries(value);
  if (depth === 0) line.append(array ? "[" : "{");
  else {
    // The whole line is the fold, its key with it, so a preview wraps under
    // its own key instead of dropping below it.
    const preview = array ? plural(entries.length, t("{n} item"), t("{n} items"))
      : entries.filter(([, v]) => v === null || typeof v !== "object").slice(0, 3)
        .map(([k, v]) => `${k}: ${JSON.stringify(typeof v === "string" && v.length > 40 ? `${v.slice(0, 40)}…` : v)}`).join(", ")
        || plural(entries.length, t("{n} key"), t("{n} keys"));
    const fold = button("fold", undefined, { act: "fold", path });
    fold.append(key, folded ? `${array ? "[" : "{"} ${preview} ${array ? "]" : "}"}` : array ? "[" : "{");
    fold.setAttribute("aria-expanded", String(!folded));
    line.append(fold);
    if (folded) return;
  }
  for (const [k, v] of entries) jsonLine(root, depth + 1, k, v, `${path}/${k}`, open, mark, depth === 1 ? name : item);
  const end = el("div", "jl", array ? "]" : "}");
  end.style.setProperty("--d", depth);
  root.append(end);
}

// --- the meter: the session's last turn, in the strip ----------------------------------

export function meterOf(data) {
  const turn = data?.turns?.at(-1);
  if (!turn || turn.status === "running") return null;
  const calls = turn.calls;
  const prompt = sum(calls, promptOf);
  const cached = sum(calls, (call) => call.cached);
  const parts = [t("Last turn {time}", { time: ms(turn.ms) })];
  if (turn.cost_usd != null) parts.push(usd(turn.cost_usd));
  if (prompt) parts.push(t("{share} from cache", { share: pct(cached / prompt) }));
  const firstWait = calls.find((call) => call.wait_ms != null)?.wait_ms;
  const title = [
    t("wall {time}", { time: ms(turn.ms) }),
    firstWait != null ? `TTFT ${ms(firstWait)}` : null,
    t("input {input} · cached {cached} · output {output}", { input: count(sum(calls, (call) => call.input)), cached: count(cached), output: count(sum(calls, (call) => call.output)) }),
    turn.cost_usd != null ? `cost_usd ${turn.cost_usd.toFixed(4)}` : t("no price known"),
  ].filter(Boolean).join(" · ");
  const flag = ORDER.find((each) => turn.flags.includes(each));
  return { text: parts.join(" · "), title, flag: flag ? FLAGS[flag].label : null };
}

// --- the Dashboard: every session over a range ------------------------------------------
// Three reads fill it, each where it stands as it lands: usage (the spend, the
// models and folders with how often each went wrong, the sessions), activity
// (what the agent did and what each step cost, what went wrong, the latest
// calls) and health (what is on right now). Plain words lead; the name an
// engineer searches for sits under each number.

export const RANGES = [["today", t("Today")], ["7d", t("7 days")], ["30d", t("30 days")], ["all", t("All")]];
const RANGE_WORDS = { today: t("Today"), "7d": t("This week"), "30d": t("These 30 days"), all: t("All told") };
const JUMPS = [["spend", t("Spend")], ["models", t("Models")], ["work", t("Work")], ["cost", t("Cost")], ["problems", t("Problems")], ["sessions", t("Sessions")], ["calls", t("Calls")]];
const DAYS = [t("Mon"), t("Tue"), t("Wed"), t("Thu"), t("Fri"), t("Sat"), t("Sun")];
const tail = (dir) => (dir || "").split("/").filter(Boolean).slice(-2).join("/") || "—";
// ONE and MANY are the counted word's two forms: a bare word ("session"),
// which follows the count, or a text with {n} where the count goes ("{n}
// sessions"), which a translation needs to put its own measure word there.
export const plural = (n, one, many) => {
  const form = n === 1 ? one : many;
  return form.includes("{n}") ? form.replace("{n}", count(n)) : `${count(n)} ${form}`;
};

// A retry's HTTP status in words; the code itself rides beside it.
function retryWord(status) {
  if (status == null) return t("Connection dropped or timed out");
  if (status === 429) return t("Rate limited");
  if (status === 529 || status === 503) return t("Provider overloaded");
  if (status === 408) return t("Timed out");
  if (status >= 500) return t("Provider error");
  return t("Request refused");
}

export function when(iso) {
  const date = new Date(iso);
  if (isNaN(date)) return "—";
  const time = date.toLocaleTimeString(LOCALE, { hour: "2-digit", minute: "2-digit", hourCycle: "h23" });
  return date.toDateString() === new Date().toDateString()
    ? time : `${date.toLocaleDateString(LOCALE, { month: "short", day: "numeric" })} ${time}`;
}

export function uptime(startedAt) {
  if (!startedAt) return null;
  const minutes = Math.max(0, Math.round((Date.now() / 1000 - startedAt) / 60));
  if (minutes < 60) return t("up {m}m", { m: minutes });
  const hours = Math.floor(minutes / 60);
  return hours < 48 ? t("up {h}h {m}m", { h: hours, m: String(minutes % 60).padStart(2, "0") })
    : t("up {n} days", { n: Math.floor(hours / 24) });
}

// Output tokens over the time spent writing them, across every model.
function speedOf(models) {
  const timed = models.filter((m) => m.speed && m.output);
  const seconds = sum(timed, (m) => m.output / m.speed);
  return seconds ? Math.round(sum(timed, (m) => m.output) / seconds) : null;
}

export function waiting(text) {
  return el("p", "obs-note", text);
}

function section(id, title, ...rest) {
  const node = el("section", "dash-section");
  node.id = `dash-${id}`;
  node.append(sectionHead(title, ...rest));
  return node;
}

function runButton(className, text, id, turn) {
  const node = button(className, text, { act: "run", id, turn: turn || "" });
  return node;
}

export function drawDashboard(parts, range, home) {
  const root = el("div", "observe-usage dashboard");
  const top = el("div", "dash-bar");
  const switcher = el("div", "scope ranges");
  switcher.setAttribute("role", "radiogroup");
  switcher.setAttribute("aria-label", t("Range"));
  for (const [id, label] of RANGES) {
    const node = button("sw", label, { range: id });
    node.setAttribute("role", "radio");
    node.setAttribute("aria-checked", String(id === range));
    switcher.append(node);
  }
  // The sections wait behind one mark, its menu under it; a choice scrolls there.
  const jumps = el("nav", "dash-jumps");
  jumps.setAttribute("aria-label", t("Sections"));
  const opener = iconButton("list", t("Jump to"));
  opener.dataset.pops = "dash-jumps";
  opener.setAttribute("aria-controls", "dash-jumps");
  opener.setAttribute("aria-expanded", "false");
  const menu = el("div", "pop below");
  menu.id = "dash-jumps";
  for (const [id, label] of JUMPS) menu.append(button("choice", label, { jump: id }));
  jumps.append(opener, menu);
  top.append(switcher, jumps);
  root.append(top);

  const data = parts.usage?.range === range ? parts.usage : null;
  const activity = parts.activity?.range === range ? parts.activity : null;
  if (!data || data.error) { root.append(waiting(data?.error ? t("Could not read usage: {error}", { error: data.error }) : t("Adding it up…"))); return root; }
  const all = data.totals;
  const said = { range: RANGE_WORDS[range] || t("In this range"), cost: usd(all.cost_usd),
    calls: plural(all.calls, t("{n} model call"), t("{n} model calls")), sessions: plural(all.sessions, t("{n} session"), t("{n} sessions")) };
  const main = all.cost_usd != null ? t("{range}, {cost} across {sessions}.", said) : t("{range}, {calls} across {sessions}.", said);
  const steps = activity?.steps;
  const sub = says([
    all.turns ? t("{turns}, {failed} failed.", { turns: plural(all.turns, t("{n} turn"), t("{n} turns")), failed: count(all.failed_turns) }) : "",
    all.saved_usd > 0.005 ? t("Reusing the prompt from cache saved about {cost}.", { cost: usd(all.saved_usd) })
      : all.saved_usd < -0.005 ? t("Writing the prompt to cache cost about {cost} more than reading it back saved.", { cost: usd(-all.saved_usd) })
      : all.cached_share != null ? t("{share} of the prompt came from cache.", { share: pct(all.cached_share) }) : "",
    // Under half a second of steps rounds to `over 0 seconds'.
    !steps?.count ? "" : steps.ms >= 500 ? t("The agent took {steps} over {time}.", { steps: plural(steps.count, t("{n} step"), t("{n} steps")), time: spoken(steps.ms) })
      : steps.ms ? t("The agent took {steps} in under a second.", { steps: plural(steps.count, t("{n} step"), t("{n} steps")) })
      : t("The agent took {steps}.", { steps: plural(steps.count, t("{n} step"), t("{n} steps")) }),
    !all.unpriced ? "" : all.cost_usd != null
      ? plural(all.unpriced, t("{n} call has no price, so the spend is a floor."), t("{n} calls have no price, so the spend is a floor."))
      : plural(all.unpriced, t("{n} call has no price."), t("{n} calls have no price.")),
  ]);
  root.append(headline(data.from ? t("Since {date} · API-equivalent prices", { date: new Date(data.from).toLocaleDateString(LOCALE, { month: "short", day: "numeric" }) })
    : t("Everything · API-equivalent prices"), main, sub));

  root.append(nowStrip(parts.health, parts.identity));

  const speed = speedOf(data.models);
  const tiles = el("section", "tiles eight");
  tiles.setAttribute("aria-label", t("Totals"));
  tiles.append(
    tile(t("Spend"), usd(all.cost_usd), all.unpriced ? t("sum cost_usd · {n} unpriced", { n: count(all.unpriced) }) : t("sum cost_usd")),
    tile(t("Saved by the cache"), all.saved_usd < 0 ? `−${usd(-all.saved_usd)}` : usd(all.saved_usd || null), t("reads' discount less writes' premium")),
    tile(t("Model calls"), count(all.calls), t("provider requests")),
    tile(t("Turns"), count(all.turns), t("{n} failed · turn.failed", { n: count(all.failed_turns) })),
    tile(t("From cache"), pct(all.cached_share), t("cache hit rate")),
    tile(t("First word, typical"), ms(data.wait_p50_ms), data.wait_p90_ms != null ? `TTFT p50 · p90 ${ms(data.wait_p90_ms)}` : "TTFT p50"),
    tile(t("Writing speed"), speed ? `${count(speed)} tok/s` : "—", t("output tokens / stream time")),
    tile(t("Steps"), steps ? count(steps.count) : "…", steps ? t("{share} failed · tool calls", { share: pct(steps.count ? steps.failed / steps.count : null) }) : t("tool calls")),
  );
  root.append(tiles);

  const spendRow = el("div", "obs-two");
  const spend = section("spend", data.days.some((day) => day.cost_usd != null) ? t("Spend by day") : t("Model calls by day"));
  spend.append(dayChart(data.days));
  spendRow.append(spend, activity ? heatmap(activity.hours) : waitingBox(t("When turns ran")));
  root.append(spendRow);

  const models = section("models", t("Models"));
  const spent = sum(data.models, (m) => m.cost_usd);
  const failedHead = { text: t("Went wrong"), tag: t("error rate"),
    title: t("Failed attempts over attempts. An attempt failed when the provider's error was retried, when the turn moved to another model, or when the error ended the turn; every other attempt is a model call that answered.") };
  models.append(table([t("Model"), t("Calls"), failedHead, t("Share of spend"), t("From cache"), t("First word"), t("Speed"), t("Written"), t("Spend")], data.models.map((m) => [
    { text: m.model || "—", title: m.provider, mono: true },
    count(m.calls),
    wentWrong(m.error_rate, m.failed, m.statuses.length
      ? t("{failed} of {attempts} went wrong: {statuses}", { failed: count(m.failed), attempts: plural(m.attempts, t("{n} attempt"), t("{n} attempts")),
        statuses: m.statuses.map((s) => `${s.status ?? t("no status")} ×${count(s.count)}`).join(", ") })
      : t("{failed} of {attempts} went wrong", { failed: count(m.failed), attempts: plural(m.attempts, t("{n} attempt"), t("{n} attempts")) })),
    { node: share(spent && m.cost_usd != null ? m.cost_usd / spent : null) },
    { text: pct(m.cached_share), hot: m.cached_share != null && m.cached_share < 0.5 },
    ms(m.wait_p50_ms),
    m.speed ? `${count(m.speed)} tok/s` : "—",
    tokens(m.output),
    usd(m.cost_usd),
  ]), "wide", ["18%", "8%", "10%", "14%", "9%", "9%", "10%", "8%", "9%"]));
  models.append(el("p", "obs-note", t("From cache under half is marked. Went wrong is the share of attempts that failed; hover it for the statuses. First word is the typical wait (TTFT p50); speed is output tokens over the time spent writing them.")));
  root.append(models);

  // What each step cost stands beside what the agent did, over what went wrong.
  const workRow = el("div", "obs-two even");
  const side = el("div", "dash-col");
  side.append(activity ? costSection(activity) : waitingBox(t("Where the money went"), "cost"),
    activity ? problemsSection(activity, all) : waitingBox(t("What went wrong"), "problems"));
  workRow.append(activity ? workSection(activity) : waitingBox(t("What the agent did"), "work"), side);
  root.append(workRow);

  const lower = el("div", "obs-two");
  const sessions = section("sessions", all.cost_usd != null ? t("Costliest sessions") : t("Busiest sessions"));
  const list = el("ol", "session-list");
  for (const s of data.sessions) {
    const li = el("li");
    const pick = button("srow", undefined, { act: "session", id: s.id });
    pick.title = [s.id, home(s.folder)].filter(Boolean).join(" · ");
    pick.append(el("span", "name", s.title || s.id), el("span", "reading", tail(s.folder)),
      el("span", "num", ms(s.ms)), el("span", "num strong", s.cost_usd != null ? usd(s.cost_usd) : plural(s.calls, t("{n} call"), t("{n} calls"))));
    li.append(pick);
    list.append(li);
  }
  if (!data.sessions.length) list.append(el("li", "obs-note", t("Nothing ran in this range.")));
  sessions.append(list);
  const folders = el("section", "dash-section");
  folders.append(sectionHead(t("By folder")), table([t("Folder"), t("Sessions"), t("Time"), t("Spend"),
    { text: t("Went wrong"), tag: t("error rate"), title: t("Failed turns over the turns that ended in the folder; a turn someone stopped ended, it did not fail.") }],
  data.folders.map((f) => [
    { text: tail(f.folder), title: home(f.folder), mono: true }, count(f.sessions), ms(f.ms), usd(f.cost_usd),
    f.turns ? wentWrong(f.error_rate, f.failed_turns, t("{failed} of {turns} failed", { failed: count(f.failed_turns), turns: plural(f.turns, t("{n} turn"), t("{n} turns")) })) : "—",
  ]), "by-folder"));
  lower.append(sessions, folders);
  root.append(lower);

  const calls = section("calls", t("Latest model calls"));
  if (!activity) calls.append(waiting(t("Adding it up…")));
  else {
    calls.append(table([t("Session"), t("When"), t("Model"), t("Sent"), t("From cache"), t("Written"), t("First word"), t("Writing"), t("Cost")], activity.calls.map((c) => [
      { node: runButton("cell-link", c.title || c.session_id, c.session_id, c.turn_id), title: c.session_id },
      when(c.at),
      { text: c.model || "—", title: c.provider, mono: true },
      tokens(promptOf(c) || null),
      pct(promptOf(c) ? (c.cached || 0) / promptOf(c) : null),
      tokens(c.output),
      ms(c.wait_ms),
      ms(c.stream_ms),
      usd(c.cost_usd),
    ]), "wide", ["21%", "12%", "14%", "8%", "9%", "8%", "9%", "9%", "8%"]));
    calls.append(el("p", "obs-note", t("Open one to see its turn on the Run view, where the inspector says why a call was slow or paid full price.")));
  }
  root.append(calls);
  return root;
}

function waitingBox(title, id) {
  const node = id ? section(id, title) : el("section", "dash-section");
  if (!id) node.append(sectionHead(title));
  node.append(waiting(t("Adding it up…")));
  return node;
}

function nowStrip(health, identity) {
  const node = el("section", "now");
  node.setAttribute("aria-label", t("Right now"));
  node.append(el("h3", "micro", t("Right now")));
  const list = el("div", "now-list");
  if (identity) {
    const item = el("div", "now-item");
    item.append(el("i", "led pulse"), el("span", "label", "nodecode"),
      el("span", "detail", [identity.version ? `v${identity.version}` : "", uptime(identity.started_at)].filter(Boolean).join(" · ")));
    list.append(item);
  }
  for (const fact of Array.isArray(health) ? health : []) {
    const item = el("div", "now-item");
    item.dataset.tone = fact.tone;
    item.append(el("i", `led${fact.tone === "ok" ? " off" : ""}`), el("span", "label", fact.label), el("span", "detail", fact.detail));
    if (fact.remedy) item.title = fact.remedy;
    list.append(item);
  }
  if (!list.children.length) list.append(waiting(health?.error ? t("Could not read health: {error}", { error: health.error }) : t("Looking…")));
  node.append(list);
  return node;
}

function dayChart(days) {
  const priced = days.some((day) => day.cost_usd != null);
  const bars = el("div", "chart");
  const value = (day) => (priced ? day.cost_usd || 0 : day.calls);
  const top = Math.max(...days.map(value), 0) || 1;
  const every = days.length > 14 ? Math.ceil(days.length / 8) : 1;
  days.forEach((day, index) => {
    const column = el("div", "day");
    const date = new Date(`${day.day}T12:00:00`);
    column.title = [date.toLocaleDateString(LOCALE, { weekday: "short", month: "short", day: "numeric" }),
      [priced ? usd(day.cost_usd) : "", plural(day.calls, t("{n} call"), t("{n} calls"))].filter(Boolean).join(" ")].join(" · ");
    const label = days.length <= 7
      ? date.toLocaleDateString(LOCALE, { weekday: "short", day: "numeric" })
      : index % every === 0 ? date.toLocaleDateString(LOCALE, { month: "short", day: "numeric" }) : "";
    // The bar and the amount over it share the room above the date, the
    // amount's line kept out of the bar's share so the tallest still fits.
    const room = el("span", "room");
    const bar = el("i", "col");
    bar.style.height = `calc((100% - 20px) * ${value(day) / top})`;
    const amount = days.length <= 14 ? (priced ? usd(day.cost_usd) : count(day.calls)) : "";
    room.append(el("span", "amount", value(day) ? amount : ""), bar);
    column.append(room, el("span", "when", label));
    bars.append(column);
  });
  return bars;
}

// Turns started, by local weekday and hour: when the work happens.
function heatmap(hours) {
  const node = el("section", "dash-section");
  const flat = hours.flat();
  const top = Math.max(...flat, 0);
  let busiest = null;
  hours.forEach((row, day) => row.forEach((n, hour) => { if (n && n === top && !busiest) busiest = { day, hour }; }));
  node.append(sectionHead(t("When turns ran")));
  node.append(el("p", "dash-line", busiest
    ? t("Busiest: {day} around {hour}, {turns} in that hour.", { day: DAYS[busiest.day], hour: `${String(busiest.hour).padStart(2, "0")}:00`, turns: plural(top, t("{n} turn"), t("{n} turns")) })
    : t("No turns in this range.")));
  const grid = el("div", "heat");
  grid.setAttribute("role", "img");
  grid.setAttribute("aria-label", t("Turns by weekday and hour, {n} in all", { n: count(sum(flat, (n) => n)) }));
  grid.append(el("span"));
  for (let hour = 0; hour < 24; hour++) grid.append(el("span", "hour", hour % 6 === 0 ? String(hour).padStart(2, "0") : ""));
  hours.forEach((row, day) => {
    grid.append(el("span", "wday", DAYS[day]));
    row.forEach((n, hour) => {
      const cell = el("i", n ? "on" : "");
      if (n) cell.style.setProperty("--k", String(0.18 + 0.82 * (n / (top || 1))));
      cell.title = `${DAYS[day]} ${String(hour).padStart(2, "0")}:00 · ${plural(n, t("{n} turn"), t("{n} turns"))}`;
      grid.append(cell);
    });
  });
  node.append(grid, el("p", "tag", t("turn.started by local hour")));
  return node;
}

function share(fraction) {
  if (fraction == null) return el("span", "", "—");
  const node = el("span", "share");
  const bar = el("i");
  bar.style.width = `${Math.round(100 * (fraction || 0))}%`;
  node.append(bar, el("span", "", fraction == null ? "—" : pct(fraction)));
  return node;
}

// A share that went wrong: quiet at zero, marked above it, a tenth of a
// percent under ten, its counts in the title.
function wentWrong(fraction, failed, title) {
  const text = !failed ? "0%" : fraction < 0.001 ? "<0.1%" : fraction < 0.1 ? `${(100 * fraction).toFixed(1)}%` : pct(fraction);
  return { text, title, hot: failed > 0, calm: !failed };
}

// Where the money went: each model call's cost split evenly over the steps it
// asked for, by the families the Work list sorts them into, and a call that
// asked for none writing the reply. When most calls have no price (the
// gateway says, BY), where the calls themselves went, split the same way.
function costSection(activity) {
  const spend = activity.spend;
  const priced = spend.rounds - spend.unpriced;
  const money = spend.by === "cost";
  const node = section("cost", money ? t("Where the money went") : t("Where the model calls went"),
    el("span", "reading", money ? t("cost per tool") : t("model calls per tool")));
  const said = { calls: plural(spend.rounds, t("{n} model call"), t("{n} model calls")), cost: usd(spend.cost_usd), n: count(spend.unpriced) };
  node.append(el("p", "dash-line", !spend.rounds ? t("No model calls in this range.") : money ? says([
    t("{cost} over {calls}, each call's cost split evenly over the steps it asked for; a call that asked for none was writing the reply.", said),
    spend.unpriced ? plural(spend.unpriced, t("{n} call has no price and is left out."), t("{n} calls have no price and are left out.")) : "",
  ]) : priced ? says([
    t("{n} of these {calls} have no price, so they are counted instead, each split evenly over the steps it asked for; a call that asked for none was writing the reply.", said),
    plural(priced, t("The {n} call with a price cost {cost}.", { cost: said.cost }), t("The {n} calls with a price cost {cost}.", { cost: said.cost })),
  ]) : t("No price is known for these {calls}, so they are counted instead, each split evenly over the steps it asked for; a call that asked for none was writing the reply.", said)));
  // A tool no family names rides one line under the list, as in the Work list.
  const list = el("ul", "families spend");
  for (const p of spend.parts.filter((part) => !part.tool)) {
    const li = el("li");
    const label = p.reply ? t("Writing the reply") : p.voided ? t("Attempts that failed") : `${p.verb} ${p.many}`;
    const counted = p.reply ? plural(p.calls, t("{n} call"), t("{n} calls")) : p.voided ? plural(p.calls, t("{n} attempt"), t("{n} attempts"))
      : plural(p.calls, t("{n} step"), t("{n} steps"));
    const fraction = !money ? p.rounds / spend.rounds : p.cost_usd != null ? p.cost_usd / spend.cost_usd : null;
    li.append(el("span", "what", label), fraction != null ? share(fraction) : el("span", "num", "—"),
      el("span", "num", money || p.cost_usd != null ? usd(p.cost_usd) : ""), el("span", "num quiet-num", counted));
    li.title = p.reply ? t("model calls that asked for no step") : p.voided ? t("turn.usage voided: attempts that failed after they began to stream")
      : t("family {family} · {n} model calls' worth", { family: p.family, n: p.rounds.toFixed(1) });
    list.append(li);
  }
  node.append(list);
  const others = spend.parts.filter((part) => part.tool);
  if (others.length) {
    node.append(el("p", "tag", t("Other tools: {tools}", { tools: others.map((o) => `${o.tool} ${count(o.calls)}${o.cost_usd != null ? ` (${usd(o.cost_usd)})` : ""}`).join(", ") })));
  }
  return node;
}

function workSection(activity) {
  const node = section("work", t("What the agent did"));
  const steps = activity.steps;
  const said = { steps: plural(steps.count, t("{n} step"), t("{n} steps")), time: spoken(steps.ms) };
  node.append(el("p", "dash-line", !steps.count ? t("The agent took no steps in this range.") : says([
    steps.ms ? t("{steps}, {time} of work.", said) : t("{steps}.", said),
    steps.failed ? plural(steps.failed, t("{n} step returned an error."), t("{n} steps returned an error.")) : t("None returned an error."),
  ])));
  const top = Math.max(...activity.families.map((f) => f.calls), 1);
  const list = el("ul", "families");
  for (const f of activity.families) {
    const li = el("li");
    const words = el("span", "what", `${f.verb} ${count(f.items)} ${f.items === 1 ? f.one : f.many}`);
    const bar = el("span", "fbar");
    const fill = el("i");
    fill.style.width = `${Math.max(1, Math.round(100 * f.calls / top))}%`;
    bar.append(fill);
    li.append(words, bar, el("span", "num", plural(f.calls, t("{n} step"), t("{n} steps"))),
      el("span", "num quiet-num", f.failed ? t("{n} failed", { n: count(f.failed) }) : ""));
    li.title = t("family {family}", { family: f.family });
    list.append(li);
  }
  node.append(list);
  if (activity.other_tools.length) {
    node.append(el("p", "tag", t("Other tools: {tools}", { tools: activity.other_tools.map((o) => (o.failed
      ? t("{tool} {n} ({failed} failed)", { tool: o.tool, n: count(o.calls), failed: count(o.failed) }) : `${o.tool} ${count(o.calls)}`)).join(", ") })));
  }
  if (activity.slowest.length) {
    node.append(el("h4", "micro sub-head", t("Slowest steps")));
    const slow = el("ol", "session-list");
    for (const s of activity.slowest) {
      const li = el("li");
      const pick = runButton("srow two", undefined, s.session_id, s.turn_id);
      pick.title = `${s.tool} · ${s.session_id}`;
      pick.append(el("span", "name", s.step), el("span", "reading", s.title || s.session_id), el("span", "num strong", ms(s.ms)));
      li.append(pick);
      slow.append(li);
    }
    node.append(slow);
  }
  return node;
}

function problemsSection(activity, totals) {
  const node = section("problems", t("What went wrong"));
  const failed = activity.failed_turns;
  const retries = sum(activity.retries, (r) => r.count);
  const switches = sum(activity.fallbacks, (f) => f.count);
  const of = { total: count(totals.turns) };
  const said = says([
    !failed.count ? "" : totals.turns ? plural(failed.count, t("{n} turn failed of {total}.", of), t("{n} turns failed of {total}.", of))
      : plural(failed.count, t("{n} turn failed."), t("{n} turns failed.")),
    failed.cancelled ? plural(failed.cancelled, t("{n} turn was stopped."), t("{n} turns were stopped.")) : "",
    retries ? plural(retries, t("The provider failed {n} time; nodecode tried again."), t("The provider failed {n} times; nodecode tried again.")) : "",
    switches ? plural(switches, t("It moved to another model {n} time."), t("It moved to another model {n} times.")) : "",
  ]);
  node.append(el("p", "dash-line", said || t("Nothing went wrong in this range.")));
  if (activity.retries.length) {
    node.append(table([t("Provider failure"), t("Status"), t("Times")], activity.retries.map((r) => [
      { text: retryWord(r.status), title: r.detail },
      { text: r.status != null ? String(r.status) : t("none"), mono: true },
      count(r.count),
    ]), "retries", ["60%", "22%", "18%"]));
  }
  for (const f of activity.fallbacks) {
    node.append(el("p", "tag", t("Moved {from} → {to} ({reason}) ×{n}", { from: f.from, to: f.to, reason: f.reason, n: count(f.count) })));
  }
  if (failed.latest.length) {
    node.append(el("h4", "micro sub-head", t("Latest failed turns")));
    const list = el("ol", "session-list");
    for (const f of failed.latest) {
      const li = el("li");
      const pick = runButton("srow two", undefined, f.session_id, f.turn_id);
      pick.title = [f.session_id, f.status ? t("status {status}", { status: f.status }) : ""].filter(Boolean).join(" · ");
      pick.append(el("span", "name bad", f.detail || t("failed")), el("span", "reading", `${f.title || f.session_id} · ${when(f.at)}`));
      li.append(pick);
      list.append(li);
    }
    node.append(list);
  }
  return node;
}

// A table of rows, each head text or { text, tag, title }, each cell text or
// { text | node, title, mono, hot, calm }, its columns WIDTHS wide when
// given; a classed one scrolls inside its own box rather than the page.
function table(heads, rows, className, widths) {
  const node = el("table", ["obs-table", className].filter(Boolean).join(" "));
  if (widths) {
    const cols = el("colgroup");
    for (const width of widths) {
      const col = el("col");
      col.style.width = width;
      cols.append(col);
    }
    node.append(cols);
  }
  const thead = el("thead");
  const tr = el("tr");
  for (const head of heads) {
    const spec = typeof head === "object" ? head : { text: head };
    const th = el("th", "", spec.text);
    if (spec.tag) th.append(el("span", "tag", spec.tag));
    if (spec.title) th.title = spec.title;
    tr.append(th);
  }
  thead.append(tr);
  const tbody = el("tbody");
  for (const cells of rows) {
    const row = el("tr");
    for (const cell of cells) {
      const spec = typeof cell === "object" ? cell : { text: cell };
      const td = el("td", [spec.mono ? "mono" : "", spec.hot ? "hot" : "", spec.calm ? "calm" : ""].filter(Boolean).join(" "), spec.node ? undefined : spec.text);
      if (spec.node) td.append(spec.node);
      if (spec.title) td.title = spec.title;
      row.append(td);
    }
    tbody.append(row);
  }
  if (!rows.length) {
    const row = el("tr");
    const td = el("td", "obs-note", t("Nothing in this range."));
    td.colSpan = heads.length;
    row.append(td);
    tbody.append(row);
  }
  node.append(thead, tbody);
  if (!className) return node;
  const box = el("div", "table-scroll");
  box.append(node);
  return box;
}
