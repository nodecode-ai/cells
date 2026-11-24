// Helpers: the forks a session's running call made (the kernel's fork verb),
// each working in a session of its own. The gateway says how they stand
// (session_fork_progress), and the rows view sets it on the eval row that
// forked as its "forks": each fork's n, label, session_id, the turn_id
// running there, status, calls, the note of its newest call, and once it
// ends its time and the first line of its answer.
//
// Over the message box, while the call runs, a band lists them: what each is
// doing, a Steer that sends it a line (steer_turn: it ends its step and runs
// that next, and the call then answers what the steered turn says) and a Stop
// (cancel_turn), and Stop all while two or more run. The gateway hears both
// for a session under the one this tab follows, so nothing here follows the
// forks themselves. Under the call in the transcript the same forks stand as
// a tree, each line opening its session.

import { el, ms, plural } from "./observe.js";
import { t } from "./i18n.js";

const STOP_MARK = '<svg viewBox="0 0 15 15" width="12" height="12" fill="none" aria-hidden="true"><path d="M3.5 3.5L11.5 11.5M11.5 3.5L3.5 11.5" stroke="currentColor" stroke-width="1.6" stroke-linecap="round"/></svg>';

const running = (fork) => fork.status === "running";

// The call ROW's forks, or an empty list.
export const forksOf = (row) => (Array.isArray(row?.metadata?.forks) ? row.metadata.forks : []);

// How FORK ended, in a word, or "" while it runs or once it completed.
function endWord(fork) {
  if (fork.status === "cancelled") return t("stopped");
  if (fork.status === "failed") return t("failed");
  if (fork.status === "timeout") return t("out of time");
  return "";
}

// Its calls and, once it ended, its time.
function statsText(fork) {
  const calls = plural(fork.calls || 0, t("{n} call"), t("{n} calls"));
  return fork.ms != null ? `${calls} · ${ms(fork.ms)}` : calls;
}

// The mark a fork's line opens on: the lamp while it runs, then how it ended.
function markOf(fork) {
  if (running(fork)) return el("i", "led live pulse");
  const done = fork.status === "completed";
  const mark = el("span", done ? "fork-mark done" : "fork-mark ended", done ? "✓" : "✗");
  mark.setAttribute("aria-label", done ? t("done") : endWord(fork));
  return mark;
}

// What FORK is doing or said: its newest call's note while it runs, its
// answer's first line once it ended (a stop's reason when it said nothing).
const wordsOf = (fork) => (running(fork) ? fork.note || t("starting") : fork.answer || endWord(fork));

export function makeHelpers({ state, command, open, toast, redraw }) {
  const bandEl = document.getElementById("helpers");
  // What this tab asked of a fork, by its session: a stop until it ends, and
  // the words of each steer it sent, which its line shows until the call ends.
  const stopping = new Set();
  const steers = new Map();
  // The fork whose steer box is open, and what it holds, kept across redraws.
  let steering = null;
  let drawn = null; // { call, lines: Map(session -> parts) }

  // The current session's newest call with forks while any of them runs --
  // its turn may have ended meanwhile, a call past its yield going on in the
  // background -- as that call's row and forks, or null.
  function liveCall(v) {
    if (!v) return null;
    for (let at = v.order.length - 1; at >= 0; at--) {
      const row = v.rows.get(v.order[at]);
      if (row?.kind !== "tool_call") continue;
      const forks = forksOf(row);
      if (!forks.length) continue;
      return forks.some(running) ? { row, forks } : null;
    }
    return null;
  }

  function stop(fork) {
    if (!fork.turn_id || stopping.has(fork.session_id)) return;
    const id = command("cancel_turn", { session_id: fork.session_id, turn_id: fork.turn_id });
    state.sent.set(id, { helper: "stop", fork: fork.session_id, n: fork.n });
    stopping.add(fork.session_id);
  }

  function steer(fork, text) {
    const said = text.trim();
    if (!said || !fork.turn_id) return false;
    const request = { session_id: fork.session_id, content: [{ type: "text", text: said }], generate_response: true, expected_turn_id: fork.turn_id };
    const id = command("steer_turn", { request });
    state.sent.set(id, { helper: "steer", fork: fork.session_id, n: fork.n, text: said });
    if (!steers.has(fork.session_id)) steers.set(fork.session_id, []);
    steers.get(fork.session_id).push(said);
    return true;
  }

  // A command this module sent was answered: a refusal says why and takes
  // back what the line showed. => true when the result was one of ours.
  function result(sent, answer) {
    if (!sent?.helper) return false;
    if (answer.status !== "rejected") return true;
    const why = answer.code === "turn_mismatch" || answer.code === "admission_failed"
      ? t("it had already finished") : answer.message || answer.code;
    if (sent.helper === "stop") {
      stopping.delete(sent.fork);
      toast(t("Couldn't stop fork {n}: {why}", { n: sent.n, why }), { error: true });
    } else {
      const words = steers.get(sent.fork) || [];
      words.splice(words.lastIndexOf(sent.text), 1);
      toast(t("Couldn't steer fork {n}: {why}", { n: sent.n, why }), { error: true });
    }
    drawn = null;
    render(state.views.get(state.current));
    return true;
  }

  // One fork's line in the band, its parts kept so a redraw only rewrites them.
  function bandLine(fork) {
    const box = el("div", "helper");
    box.dataset.fork = fork.session_id;
    const line = el("div", "helper-line");
    const mark = el("span", "helper-mark");
    const n = el("span", "helper-n");
    const name = el("button", "helper-name");
    name.type = "button";
    name.dataset.act = "helper-open";
    const stats = el("span", "helper-stats");
    const words = el("span", "helper-words");
    const acts = el("span", "helper-acts");
    const steerButton = el("button", "quiet helper-steer", t("Steer"));
    steerButton.type = "button";
    steerButton.dataset.act = "helper-steer";
    const stopButton = el("button", "exec-stop helper-stop");
    stopButton.type = "button";
    stopButton.dataset.act = "helper-stop";
    stopButton.innerHTML = STOP_MARK;
    const state_ = el("span", "state");
    acts.append(state_, steerButton, stopButton);
    line.append(mark, n, name, stats, words, acts);
    const said = el("div", "helper-said");
    const form = el("form", "helper-box");
    form.dataset.act = "helper-send";
    const input = el("input", "control helper-input");
    input.type = "text";
    input.autocomplete = "off";
    input.enterKeyHint = "send";
    const send = el("button", "solid helper-send", t("Send"));
    send.type = "submit";
    form.append(input, send);
    box.append(line, said, form);
    return { box, mark, n, name, stats, words, steerButton, stopButton, state: state_, said, form, input };
  }

  function fill(parts, fork) {
    const live = running(fork);
    const sig = `${fork.status}|${fork.n}`;
    if (parts.sig !== sig) {
      parts.sig = sig;
      parts.mark.replaceChildren(markOf(fork));
      parts.box.dataset.status = fork.status;
    }
    parts.n.textContent = t("fork {n}", { n: fork.n });
    parts.name.textContent = fork.label || t("fork {n}", { n: fork.n });
    parts.name.title = t("Open fork {n}'s session", { n: fork.n });
    parts.stats.textContent = statsText(fork);
    parts.words.textContent = wordsOf(fork);
    parts.words.classList.toggle("note", live);
    const asked = stopping.has(fork.session_id);
    parts.state.textContent = asked && live ? t("stopping") : "";
    parts.state.hidden = !(asked && live);
    parts.steerButton.hidden = !live || asked;
    parts.stopButton.hidden = !live || asked;
    parts.steerButton.setAttribute("aria-expanded", String(steering === fork.session_id));
    parts.steerButton.setAttribute("aria-label", t("Steer fork {n}", { n: fork.n }));
    parts.stopButton.title = t("Stop");
    parts.stopButton.setAttribute("aria-label", t("Stop fork {n}", { n: fork.n }));
    const words = steers.get(fork.session_id) || [];
    // What this tab steered it with; a tab opened since knows only how often.
    const said = words.length ? t("You steered it: {words}", { words: words.join(" · ") })
      : fork.steered ? plural(fork.steered, t("Steered once"), t("Steered {n} times")) : "";
    parts.said.hidden = !said;
    parts.said.textContent = said;
    const open = live && !asked && steering === fork.session_id;
    parts.form.hidden = !open;
    parts.input.placeholder = t("Tell fork {n}: it ends its step and does this next", { n: fork.n });
    parts.input.setAttribute("aria-label", t("Steer fork {n}", { n: fork.n }));
  }

  // The band over the message box, for V, the session on screen.
  function render(v) {
    const call = liveCall(v);
    if (!call) {
      if (drawn) { bandEl.replaceChildren(); drawn = null; }
      bandEl.hidden = true;
      steering = null;
      stopping.clear();
      steers.clear();
      return;
    }
    for (const fork of call.forks) if (!running(fork)) stopping.delete(fork.session_id);
    if (steering && !call.forks.some((fork) => fork.session_id === steering && running(fork))) steering = null;
    if (!drawn || drawn.call !== call.row.id) {
      const head = el("div", "helpers-head");
      const count = el("span", "helpers-count");
      const all = el("button", "word helpers-stop-all", t("Stop all"));
      all.type = "button";
      all.dataset.act = "helper-stop-all";
      head.append(el("span", "micro", t("Helpers")), count, all);
      drawn = { call: call.row.id, head, count, all, lines: new Map() };
      bandEl.replaceChildren(head);
    }
    const live = call.forks.filter(running);
    drawn.count.textContent = live.length ? plural(live.length, t("{n} running"), t("{n} running")) : t("all done");
    drawn.all.hidden = live.filter((fork) => !stopping.has(fork.session_id)).length < 2;
    for (const fork of call.forks) {
      let parts = drawn.lines.get(fork.session_id);
      if (!parts) {
        parts = bandLine(fork);
        drawn.lines.set(fork.session_id, parts);
        bandEl.append(parts.box);
      }
      fill(parts, fork);
    }
    bandEl.hidden = false;
  }

  function forkFor(node) {
    const id = node.closest(".helper")?.dataset.fork;
    const call = liveCall(state.views.get(state.current));
    return call?.forks.find((fork) => fork.session_id === id) || null;
  }

  function again() {
    render(state.views.get(state.current));
  }

  bandEl.addEventListener("click", (event) => {
    const act = event.target.closest("[data-act]");
    if (!act) return;
    if (act.dataset.act === "helper-stop-all") {
      const call = liveCall(state.views.get(state.current));
      for (const fork of call?.forks || []) if (running(fork)) stop(fork);
      steering = null;
      again();
      return;
    }
    const fork = forkFor(act);
    if (!fork) return;
    if (act.dataset.act === "helper-open") open(fork.session_id);
    else if (act.dataset.act === "helper-stop") {
      stop(fork);
      if (steering === fork.session_id) steering = null;
      again();
    } else if (act.dataset.act === "helper-steer") {
      steering = steering === fork.session_id ? null : fork.session_id;
      again();
      if (steering) drawn?.lines.get(fork.session_id)?.input.focus();
    }
  });
  bandEl.addEventListener("submit", (event) => {
    event.preventDefault();
    const fork = forkFor(event.target);
    const parts = fork && drawn?.lines.get(fork.session_id);
    if (!parts) return;
    if (steer(fork, parts.input.value)) {
      parts.input.value = "";
      steering = null;
      again();
      redraw();
    }
  });
  bandEl.addEventListener("keydown", (event) => {
    if (event.key !== "Escape" || !event.target.classList.contains("helper-input")) return;
    event.preventDefault();
    event.stopPropagation();
    const fork = forkFor(event.target);
    steering = null;
    again();
    if (fork) drawn?.lines.get(fork.session_id)?.steerButton.focus();
  });

  // The call ROW's forks as a tree under it in the transcript: one line a
  // fork, its mark, its name opening its session, its calls and time, and
  // what it is doing or said.
  function tree(row) {
    const forks = forksOf(row);
    if (!forks.length) return null;
    const list = el("div", "fork-tree");
    list.setAttribute("role", "list");
    forks.forEach((fork, at) => {
      const line = el("div", `fork-line${running(fork) ? " live" : ""}`);
      line.setAttribute("role", "listitem");
      const name = el("button", "helper-name", fork.label || t("fork {n}", { n: fork.n }));
      name.type = "button";
      name.dataset.act = "fork-open";
      name.dataset.session = fork.session_id;
      name.title = t("Open fork {n}'s session", { n: fork.n });
      const words = el("span", running(fork) ? "helper-words note" : "helper-words", wordsOf(fork));
      const ending = endWord(fork);
      line.append(el("span", "fork-rail", at === forks.length - 1 ? "╰" : "├"), markOf(fork),
        el("span", "helper-n", t("fork {n}", { n: fork.n })), name,
        el("span", "helper-stats", ending ? `${statsText(fork)} · ${ending}` : statsText(fork)), words);
      list.append(line);
    });
    return list;
  }

  return { render, result, tree };
}
