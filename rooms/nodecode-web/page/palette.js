// palette.js -- one box that goes anywhere: Ctrl+K (⌘K) opens it over any
// pane, Ctrl+P (Cmd+P) opens it on the session's files, Ctrl+Shift+P on the
// commands. What it offers is what the page already has -- the sessions the
// list holds, the views and boards an address can name, the Control tabs, the
// gateway's own slash catalog, and the files of the session's folder (the
// files route's ?find=1) -- matched as the operator types, the letters that
// matched lit. A pick goes where a link would: a place is an address, so Back
// walks out of it; a command is put in the box to be sent, never sent.

import { el } from "./observe.js";
import { t } from "./i18n.js";

const RECENT_KEY = "nodecode.palette.recent";
const RECENT_MAX = 12;
const FILES_FRESH_MS = 30_000;
const SHOWN = { recent: 5, session: 6, action: 4, view: 6, file: 8, command: 6 };
const ORDER = ["recent", "session", "action", "view", "file", "command"];
const LEAN = { file: 700, command: 500 };
// The modifier as this machine's keyboard names it.
const MOD = /Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent) ? "⌘" : "Ctrl+";
const GROUP = { recent: t("Recent"), session: t("Sessions"), view: t("Go to"), action: t("Actions"), file: t("Files"), command: t("Commands") };
const MODES = {
  all: { kinds: ORDER, hint: t("Go to a session, a view, a file or a command") },
  files: { kinds: ["file"], hint: t("Find a file in this session's folder") },
  commands: { kinds: ["command"], hint: t("Find a command") },
};
const SEPARATOR = /[\s/_.:-]/;

// How well QUERY's letters fall in TEXT, in order: null when they do not all
// fall, else { score, hits } -- higher is better, HITS the matched indices. A
// run of letters, a word's first letter and an early start count most, so
// "rv" finds Run view's R and v before a stray r and v further on.
export function fuzzy(query, text) {
  const q = query.toLowerCase();
  const s = text.toLowerCase();
  if (!q) return { score: 0, hits: [] };
  const at = s.indexOf(q);
  if (at >= 0) {
    const start = at === 0 || SEPARATOR.test(s[at - 1]);
    return { score: 1000 + (start ? 300 : 0) - at * 3 - (s.length - q.length), hits: Array.from({ length: q.length }, (_, i) => at + i) };
  }
  // Two walks: one that takes a word's first letter over the same letter
  // mid-word, one that takes the leftmost; the first can strand a later
  // letter ("stagng" taking "to"'s t), so the better walk that matched wins.
  const walk = (starts) => {
    const hits = [];
    let score = 0;
    let from = 0;
    for (const ch of q) {
      if (ch === " ") continue;
      let found = -1;
      for (let i = from; i < s.length; i++) {
        if (s[i] !== ch) continue;
        if (!starts || i === 0 || SEPARATOR.test(s[i - 1])) { found = i; break; }
        if (found < 0) found = i;
      }
      if (found < 0) return null;
      const last = hits[hits.length - 1];
      score += 10;
      if (found === 0 || SEPARATOR.test(s[found - 1])) score += 30;
      if (last != null && found === last + 1) score += 20;
      if (last != null) score -= Math.min(found - last - 1, 12);
      hits.push(found);
      from = found + 1;
    }
    return hits.length ? { score: score - hits[0], hits } : null;
  };
  const a = walk(true);
  const b = walk(false);
  return !a ? b : !b ? a : a.score >= b.score ? a : b;
}

// A file is matched on its name first and its path after: "app" finds
// page/app.js before apps/x/readme.md.
function fileMatch(query, path) {
  const slash = path.lastIndexOf("/");
  const base = fuzzy(query, path.slice(slash + 1));
  if (base) return { score: base.score + 500, hits: base.hits.map((i) => i + slash + 1) };
  return fuzzy(query, path);
}

// TEXT with the letters at HITS lit; the first DIR characters (a file's
// folder) quieter than the name after them.
function lit(text, hits, dir = 0) {
  const span = el("span", "name");
  const on = new Set(hits);
  const part = (from, to) => {
    const holder = from < dir ? el("span", "dir") : span;
    let run = "";
    let lighting = false;
    const flush = () => {
      if (!run) return;
      holder.append(lighting ? el("mark", "", run) : document.createTextNode(run));
      run = "";
    };
    for (let i = from; i < to; i++) {
      if (on.has(i) !== lighting) { flush(); lighting = on.has(i); }
      run += text[i];
    }
    flush();
    if (holder !== span) span.append(holder);
  };
  if (dir > 0) part(0, dir);
  part(dir, text.length);
  return span;
}

// SOURCE: what the page holds, read when the palette opens and as it types --
//   sessions()      the list's entries, newest first, each { id, name, detail }
//   current()       the session the page shows, or null
//   views()         the places beside it: [{ id, name, detail, hash }]
//   actions()       [{ id, name, detail, run }]
//   commands()      the slash catalog: [{ name, description }]
//   files(id)       a promise of { files, cut } for session ID's folder
//   go(hash)        the page taken to an address
//   openSession(id) the page on session ID
//   compose(text)   TEXT put in the box, the box focused
export function makePalette(source) {
  const root = el("div", "palette");
  root.id = "palette";
  root.hidden = true;
  root.setAttribute("role", "dialog");
  root.setAttribute("aria-modal", "true");
  root.setAttribute("aria-label", t("Command palette"));
  const box = el("div", "palette-box");
  const input = el("input", "palette-input");
  input.id = "palette-input";
  input.type = "text";
  input.autocomplete = "off";
  input.spellcheck = false;
  input.setAttribute("role", "combobox");
  input.setAttribute("aria-expanded", "true");
  input.setAttribute("aria-controls", "palette-list");
  input.setAttribute("aria-autocomplete", "list");
  const list = el("ol", "palette-list");
  list.id = "palette-list";
  list.setAttribute("role", "listbox");
  list.setAttribute("aria-label", t("Results"));
  const foot = el("div", "palette-foot");
  box.append(input, list, foot);
  root.append(box);
  document.body.append(root);

  const p = { mode: "all", items: [], at: 0, back: null, files: new Map(), asked: 0 };

  const recent = () => {
    try { return JSON.parse(localStorage.getItem(RECENT_KEY)) || []; } catch { return []; }
  };
  const remember = (key) => {
    const keys = [key, ...recent().filter((each) => each !== key)].slice(0, RECENT_MAX);
    try { localStorage.setItem(RECENT_KEY, JSON.stringify(keys)); } catch {}
  };

  // The files of the session shown, read once a while: a folder moves on.
  function filesOf(id) {
    const had = p.files.get(id);
    if (had && (had.loading || Date.now() - had.at < FILES_FRESH_MS)) return had;
    const entry = { at: Date.now(), loading: true, files: had?.files || [], cut: false, error: null };
    p.files.set(id, entry);
    source.files(id).then(
      ({ files, cut }) => Object.assign(entry, { loading: false, files, cut, at: Date.now() }),
      (error) => Object.assign(entry, { loading: false, error }),
    ).finally(() => { if (!root.hidden && p.session === id) draw(); });
    return entry;
  }

  // Every candidate of KIND, as { kind, key, name, detail, pick }.
  function candidates(kind) {
    const current = source.current();
    switch (kind) {
      case "session":
        return source.sessions().map((s) => ({ kind, key: `session:${s.id}`, name: s.name, detail: s.detail, here: s.id === current, pick: () => source.openSession(s.id) }));
      case "view":
        return source.views().map((v) => ({ kind, key: `view:${v.id}`, name: v.name, detail: v.detail, pick: () => source.go(v.hash) }));
      case "action":
        return source.actions().map((a) => ({ kind, key: `action:${a.id}`, name: a.name, detail: a.detail, pick: a.run }));
      case "command":
        return (source.commands() || []).map((c) => ({ kind, key: `command:${c.name}`, name: `/${c.name}`, detail: c.description || "", pick: () => source.compose(`/${c.name} `) }));
      case "file": {
        if (!current) return [];
        const entry = filesOf(current);
        return entry.files.map((path) => {
          const slash = path.lastIndexOf("/");
          return { kind, key: `file:${current}:${path}`, name: path, detail: "", file: true, dir: slash > 0 ? path.slice(0, slash) : "", pick: () => source.go(`s=${encodeURIComponent(current)}&v=files&path=${encodeURIComponent(path).replace(/%2F/g, "/")}`) };
        });
      }
      default:
        return [];
    }
  }

  function query() {
    const text = input.value.trim();
    if (p.mode === "all" && text.startsWith(">")) return { mode: "commands", text: text.slice(1).trim() };
    return { mode: p.mode, text };
  }

  function gather() {
    const { mode, text } = query();
    p.session = source.current();
    const groups = [];
    const taken = new Set();
    for (const kind of MODES[mode].kinds) {
      if (kind === "recent") {
        if (text || mode !== "all") continue;
        const all = new Map(["session", "view", "action"].flatMap((k) => candidates(k)).map((item) => [item.key, item]));
        const rows = recent().map((key) => all.get(key)).filter((item) => item && !item.here).slice(0, SHOWN.recent);
        for (const row of rows) taken.add(row.key);
        if (rows.length) groups.push({ kind, rows: rows.map((row) => ({ ...row, hits: [] })) });
        continue;
      }
      // With nothing typed, everything's files and commands are their own modes' to show.
      if (!text && mode === "all" && (kind === "file" || kind === "command")) continue;
      const cap = mode === "all" ? SHOWN[kind] : 60;
      const rows = [];
      for (const item of candidates(kind)) {
        if (taken.has(item.key)) continue;
        const m = !text ? { score: 0, hits: [] } : item.file ? fileMatch(text, item.name) : fuzzy(text, item.name) || (item.detail && fuzzy(text, item.detail) && { score: -500, hits: [] });
        if (m) rows.push({ ...item, score: m.score, hits: m.hits });
      }
      if (text) rows.sort((a, b) => b.score - a.score);
      if (rows.length) groups.push({ kind, rows: rows.slice(0, cap), best: rows[0].score ?? 0, more: rows.length - cap });
    }
    // Typed: the group holding the best match leads -- in the box over
    // everything a place wins a near tie with a file or a command, which have
    // their own keys.
    const weight = (g) => (g.best ?? 0) - (mode === "all" ? LEAN[g.kind] || 0 : 0);
    if (text) groups.sort((a, b) => weight(b) - weight(a) || ORDER.indexOf(a.kind) - ORDER.indexOf(b.kind));
    return { mode, text, groups };
  }

  function draw() {
    const { mode, text, groups } = gather();
    input.placeholder = MODES[mode].hint;
    root.dataset.mode = mode;
    p.items = groups.flatMap((g) => g.rows);
    if (p.at >= p.items.length) p.at = Math.max(0, p.items.length - 1);
    const rows = [];
    let n = 0;
    for (const g of groups) {
      rows.push(el("li", "palette-group", GROUP[g.kind]));
      for (const item of g.rows) {
        const i = n++;
        const li = el("li", "palette-item");
        li.id = `palette-${i}`;
        li.dataset.index = i;
        li.dataset.kind = item.kind;
        li.setAttribute("role", "option");
        li.setAttribute("aria-selected", String(i === p.at));
        li.append(lit(item.name, item.hits, item.file && item.dir ? item.dir.length + 1 : 0));
        const detail = item.file ? "" : item.here ? t("open now") : item.detail;
        if (detail) li.append(el("span", "detail", detail));
        rows.push(li);
      }
      if (g.more > 0) rows.push(el("li", "palette-more", t("{n} more: type to narrow", { n: g.more })));
    }
    if (!p.items.length) {
      const files = p.session && p.files.get(p.session);
      const word = mode === "files" && !p.session ? t("A new session has no folder to look in yet")
        : mode === "files" && files?.loading ? t("Reading the folder…")
        : mode === "files" && files?.error ? t("Couldn't read the folder: {why}", { why: files.error.message })
        : t("Nothing matches {text}", { text: text || "…" });
      rows.push(el("li", "palette-none", word));
    }
    list.replaceChildren(...rows);
    input.setAttribute("aria-activedescendant", p.items.length ? `palette-${p.at}` : "");
    const files = p.session && p.files.get(p.session);
    foot.replaceChildren(
      el("span", "", t("↑↓ to move, Enter to go, Esc to close")),
      el("span", "", mode === "all" ? t("> commands · {mod}P files", { mod: MOD }) : files?.cut && mode === "files" ? t("The folder is large: only part of it is listed") : ""),
    );
    list.querySelector("[aria-selected='true']")?.scrollIntoView({ block: "nearest" });
  }

  function move(by) {
    if (!p.items.length) return;
    p.at = (p.at + by + p.items.length) % p.items.length;
    for (const li of list.querySelectorAll(".palette-item")) li.setAttribute("aria-selected", String(Number(li.dataset.index) === p.at));
    input.setAttribute("aria-activedescendant", `palette-${p.at}`);
    list.querySelector("[aria-selected='true']")?.scrollIntoView({ block: "nearest" });
  }

  function pick(index = p.at) {
    const item = p.items[index];
    if (!item) return;
    if (item.kind !== "file" && item.kind !== "command") remember(item.key);
    close(false);
    item.pick();
  }

  function open(mode = "all") {
    if (!root.hidden && p.mode === mode) { input.select(); return; }
    p.back = root.hidden ? document.activeElement : p.back;
    p.mode = mode;
    p.at = 0;
    input.value = "";
    root.hidden = false;
    draw();
    input.focus();
  }

  function close(restore = true) {
    if (root.hidden) return;
    root.hidden = true;
    if (restore && p.back?.isConnected) p.back.focus();
    p.back = null;
  }

  input.addEventListener("input", () => { p.at = 0; draw(); });
  input.addEventListener("keydown", (event) => {
    if (event.key === "ArrowDown") { event.preventDefault(); move(1); }
    else if (event.key === "ArrowUp") { event.preventDefault(); move(-1); }
    else if (event.key === "Enter" && !event.isComposing) { event.preventDefault(); pick(); }
    else if (event.key === "Escape") { event.preventDefault(); event.stopPropagation(); close(); }
    else if (event.key === "Backspace" && !input.value && p.mode !== "all") { event.preventDefault(); p.mode = "all"; draw(); }
  });
  list.addEventListener("mousemove", (event) => {
    const li = event.target.closest(".palette-item");
    if (li && Number(li.dataset.index) !== p.at) { p.at = Number(li.dataset.index); move(0); }
  });
  list.addEventListener("mousedown", (event) => event.preventDefault());
  list.addEventListener("click", (event) => {
    const li = event.target.closest(".palette-item");
    if (li) pick(Number(li.dataset.index));
  });
  root.addEventListener("mousedown", (event) => { if (event.target === root) close(); });

  // The keys that open it, before anything on the page takes them: a box
  // with focus still opens it, the way an editor's palette opens.
  document.addEventListener("keydown", (event) => {
    const mod = event.ctrlKey !== event.metaKey && (event.ctrlKey || event.metaKey) && !event.altKey;
    if (!mod || event.isComposing) return;
    const key = event.key.toLowerCase();
    const mode = key === "k" && !event.shiftKey ? "all" : key === "p" ? (event.shiftKey ? "commands" : "files") : null;
    if (!mode) return;
    event.preventDefault();
    event.stopPropagation();
    if (!root.hidden && p.mode === mode) close();
    else open(mode);
  }, true);

  return { open, close, get shown() { return !root.hidden; }, redraw: () => { if (!root.hidden) draw(); } };
}
