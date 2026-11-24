// keys.js -- the page's shortcuts as one keymap: every command a key can
// run, the key this browser keeps for it, and the one listener that runs it.
// A key is an accelerator as Electron spells it ("CmdOrCtrl+Shift+P"):
// CmdOrCtrl is ⌘ on a Mac and Ctrl elsewhere, so one keymap reads on both.
// What the operator changes is kept in this browser (localStorage), as the
// theme is, and follows in its other tabs; a default changed later reaches
// every key the operator left alone. Inside the desktop app the app's own
// commands -- the terminal, the review panel -- join the list through its
// door (window.nodecodeDesktop.keys), the app is told every key, so its menu
// says and catches what this page says, and the keys the app keeps for
// itself are refused here as the browser's are in a tab.

import { t } from "./i18n.js";

export const KEYS_KEY = "nodecode-web-keys";
export const MAC = /Mac|iPhone|iPad/.test(globalThis.navigator?.platform || globalThis.navigator?.userAgent || "");

// A key's name from the event's code when its own name changes with Shift or
// the layout's dead keys: the digits, the marks, the keys with a name.
const NAMED = {
  Backquote: "`", Minus: "-", Equal: "=", BracketLeft: "[", BracketRight: "]", Backslash: "\\", Semicolon: ";",
  Quote: "'", Comma: ",", Period: ".", Slash: "/", Enter: "Enter", NumpadEnter: "Enter", Escape: "Esc", Tab: "Tab",
  Space: "Space", Backspace: "Backspace", Delete: "Delete", Insert: "Insert", Home: "Home", End: "End",
  PageUp: "PageUp", PageDown: "PageDown", ArrowUp: "Up", ArrowDown: "Down", ArrowLeft: "Left", ArrowRight: "Right",
};
// Electron's other spellings of the same keys and modifiers.
const ALIASES = {
  commandorcontrol: "CmdOrCtrl", cmdorctrl: "CmdOrCtrl", mod: "CmdOrCtrl", control: "Ctrl", ctrl: "Ctrl",
  alt: "Alt", option: "Alt", altgr: "Alt", shift: "Shift", super: "Super", meta: "Super", win: "Super",
  command: "Cmd", cmd: "Cmd", return: "Enter", enter: "Enter", escape: "Esc", esc: "Esc", space: "Space",
  tab: "Tab", backspace: "Backspace", delete: "Delete", insert: "Insert", home: "Home", end: "End",
  pageup: "PageUp", pagedown: "PageDown", up: "Up", down: "Down", left: "Left", right: "Right", plus: "=",
};
const ORDER = ["CmdOrCtrl", "Ctrl", "Alt", "Shift", "Super"];
const KEYS = new Set([...Object.values(NAMED)]);
const FKEY = /^F([1-9]|1\d|2[0-4])$/;

// The key EVENT says, without its modifiers, or "" for a modifier alone or a
// key this keymap has no name for. A letter is the layout's own letter, so
// Ctrl+K is K on any keyboard; the rest by where they sit.
function keyName(event) {
  if (/^[a-z]$/i.test(event.key || "")) return event.key.toUpperCase();
  const code = event.code || "";
  const found = /^Key([A-Z])$/.exec(code) || /^(?:Digit|Numpad)(\d)$/.exec(code);
  if (found) return found[1];
  if (FKEY.test(code)) return code;
  return NAMED[code] || "";
}

// EVENT as an accelerator in this keymap's order, or "" when no key but
// modifiers is down.
export function chordOf(event, mac = MAC) {
  const key = keyName(event);
  if (!key) return "";
  const mods = [];
  if (mac ? event.metaKey : event.ctrlKey) mods.push("CmdOrCtrl");
  if (mac && event.ctrlKey) mods.push("Ctrl");
  if (event.altKey) mods.push("Alt");
  if (event.shiftKey) mods.push("Shift");
  if (!mac && event.metaKey) mods.push("Super");
  return [...mods, key].join("+");
}

// TEXT, an accelerator in any of Electron's spellings, in this keymap's own:
// modifiers in one order, Ctrl off a Mac and Cmd on one both CmdOrCtrl. ""
// for no key, or for words that are not a key.
export function normalize(text, mac = MAC) {
  if (typeof text !== "string" || !text.trim()) return "";
  const parts = text.trim().split("+").map((part) => part.trim());
  const last = parts.pop();
  let key = ALIASES[last.toLowerCase()] || last;
  if (key.length === 1) key = key.toUpperCase();
  if (!(key.length === 1 || KEYS.has(key) || FKEY.test(key)) || ORDER.includes(key) || key === "Cmd") return "";
  const mods = new Set();
  for (const part of parts) {
    let mod = ALIASES[part.toLowerCase()];
    if (mod === "Cmd") mod = mac ? "CmdOrCtrl" : "Super";
    if (mod === "Ctrl" && !mac) mod = "CmdOrCtrl";
    if (!ORDER.includes(mod)) return "";
    mods.add(mod);
  }
  return [...ORDER.filter((mod) => mods.has(mod)), key].join("+");
}

// CHORD as this machine's keyboard names it, a part a cap: ["Ctrl", "Shift",
// "P"], or on a Mac ["⇧", "⌘", "P"] in its own order.
const SHOWN = { Up: "↑", Down: "↓", Left: "←", Right: "→" };
export function caps(chord, mac = MAC) {
  if (!chord) return [];
  const parts = chord.split("+");
  const key = SHOWN[parts.at(-1)] || parts.at(-1);
  const mods = new Set(parts.slice(0, -1));
  if (!mac) return [...ORDER.filter((mod) => mods.has(mod)).map((mod) => (mod === "CmdOrCtrl" ? "Ctrl" : mod === "Super" ? "Win" : mod)), key];
  const mark = { Ctrl: "⌃", Alt: "⌥", Shift: "⇧", CmdOrCtrl: "⌘", Super: "⌘" };
  return [...["Ctrl", "Alt", "Shift", "CmdOrCtrl"].filter((mod) => mods.has(mod)).map((mod) => mark[mod]), key];
}
export const label = (chord, mac = MAC) => caps(chord, mac).join(mac ? "" : "+");

// What every box answers to, which no shortcut can take.
const EDITING = [
  ["CmdOrCtrl+C", t("Copy")], ["CmdOrCtrl+V", t("Paste")], ["CmdOrCtrl+X", t("Cut")], ["CmdOrCtrl+A", t("Select all")],
  ["CmdOrCtrl+Z", t("Undo")], ["CmdOrCtrl+Shift+Z", t("Redo")], ["CmdOrCtrl+Y", t("Redo")], ["CmdOrCtrl+S", t("Save")],
];
// What a browser keeps for itself: the page never sees these keys.
const BROWSER = [
  ["CmdOrCtrl+W", t("Close tab")], ["CmdOrCtrl+T", t("New tab")], ["CmdOrCtrl+N", t("New window")],
  ["CmdOrCtrl+Shift+N", t("New private window")], ["CmdOrCtrl+Shift+T", t("Reopen closed tab")],
  ["CmdOrCtrl+Shift+W", t("Close window")], ["CmdOrCtrl+Q", t("Quit")],
  [MAC ? "Ctrl+Tab" : "CmdOrCtrl+Tab", t("Next tab")], [MAC ? "Ctrl+Shift+Tab" : "CmdOrCtrl+Shift+Tab", t("Previous tab")],
];

// Why CHORD cannot be a shortcut, or "": a plain key types, and what a box
// or the browser (the app, through its door) keeps is theirs. HELD is the
// keeper's list, [[chord, what], ...].
export function refusal(chord, held = BROWSER, mac = MAC) {
  if (!chord) return t("That is not a key a shortcut can use.");
  const parts = chord.split("+");
  const key = parts.at(-1);
  const mods = parts.slice(0, -1);
  if (!FKEY.test(key) && !mods.some((mod) => mod !== "Shift"))
    return mac ? t("A shortcut needs ⌘, ⌃ or ⌥, or an F key: a plain key types.") : t("A shortcut needs Ctrl or Alt, or an F key: a plain key types.");
  if (mac && !FKEY.test(key) && key.length === 1 && mods.every((mod) => mod === "Alt" || mod === "Shift"))
    return t("On a Mac ⌥ with a letter or a mark types a character: add ⌘ or ⌃.");
  const editing = EDITING.find(([each]) => each === chord);
  if (editing) return t("{key} is {what} in every box.", { key: label(chord, mac), what: editing[1] });
  const kept = held.find(([each]) => each === chord);
  if (kept) return held === BROWSER
    ? t("The browser keeps {key} for itself ({what}): a page never sees it.", { key: label(chord, mac), what: kept[1] })
    : t("The app keeps {key} for itself ({what}).", { key: label(chord, mac), what: kept[1] });
  return "";
}

function kept() {
  try {
    const value = JSON.parse(localStorage.getItem(KEYS_KEY) || "{}");
    return value && typeof value === "object" && !Array.isArray(value) ? value : {};
  } catch {
    return {};
  }
}

// The keymap: ADD the page's commands, each { id, name, group, chord, run,
// when? } -- CHORD its default, "" for none; WHEN, if given, says whether it
// can run now. DOOR is the desktop app's, when the page runs inside it:
// { commands, reserved, bind(list), onRun(run), recording(on) }.
export function makeKeys({ door = globalThis.window?.nodecodeDesktop?.keys } = {}) {
  const commands = new Map();
  const listeners = new Set();
  let over = kept();
  let recording = false;
  const held = door ? (door.reserved || []).map(({ chord, name }) => [normalize(chord), name]) : BROWSER;
  for (const each of door?.commands || []) {
    commands.set(each.id, { id: each.id, name: each.name, group: each.group || "desktop", chord: normalize(each.default ?? each.chord ?? ""), desktop: true });
  }

  const chord = (id) => {
    const command = commands.get(id);
    if (!command) return "";
    return Object.hasOwn(over, id) ? normalize(over[id]) : command.chord;
  };
  // The command bound to CHORD, other than EXCEPT, or null.
  const holder = (value, except = null) => {
    if (!value) return null;
    for (const command of commands.values()) if (command.id !== except && chord(command.id) === value) return command;
    return null;
  };
  const list = () => [...commands.values()].map((command) => ({
    id: command.id, name: command.name, group: command.group, desktop: !!command.desktop,
    chord: chord(command.id), default: command.chord, changed: Object.hasOwn(over, command.id),
  }));

  function save() {
    try {
      if (Object.keys(over).length) localStorage.setItem(KEYS_KEY, JSON.stringify(over));
      else localStorage.removeItem(KEYS_KEY);
    } catch {}
  }
  function changed() {
    try { door?.bind?.(list()); } catch {}
    for (const listener of listeners) listener();
  }
  function put(id, value) {
    const command = commands.get(id);
    if (!command) return;
    if (value === command.chord) delete over[id];
    else over[id] = value;
  }

  // Bind ID to CHORD ("" takes its key away). Answers why not, or "": a key
  // that cannot be one, or that another command holds -- TAKE takes it from
  // that one, which is then left with none.
  function set(id, value, { take = false } = {}) {
    const next = normalize(value);
    if (value && !next) return t("That is not a key a shortcut can use.");
    if (next) {
      const why = refusal(next, held);
      if (why) return why;
      const other = holder(next, id);
      if (other && !take) return t("{key} already runs {name}.", { key: label(next), name: other.name });
      if (other) put(other.id, "");
    }
    put(id, next);
    save();
    changed();
    return "";
  }
  // Back to ID's default, which another command may have taken since: that
  // one keeps it, and ID is left with none.
  function reset(id) {
    const command = commands.get(id);
    if (!command) return;
    delete over[id];
    const other = holder(command.chord, id);
    if (other) over[id] = "";
    save();
    changed();
  }
  function resetAll() {
    over = {};
    save();
    changed();
  }

  function run(id) {
    const command = commands.get(id);
    if (!command || command.desktop || (command.when && !command.when())) return false;
    command.run();
    return true;
  }

  // The one listener, before anything on the page takes the key: a box with
  // focus still runs a shortcut, as an editor's does. The app catches a
  // typed key of its own commands before the page sees it; one that still
  // arrives (a synthetic one) is handed to it.
  document.addEventListener("keydown", (event) => {
    if (recording || event.isComposing || event.repeat) return;
    const command = holder(chordOf(event));
    if (!command || (command.when && !command.when())) return;
    event.preventDefault();
    event.stopPropagation();
    if (command.desktop) door?.run?.(command.id);
    else command.run();
  }, true);
  // A key changed in another tab of this browser.
  window.addEventListener("storage", (event) => {
    if (event.key !== KEYS_KEY) return;
    over = kept();
    changed();
  });
  try { door?.onRun?.((id) => run(id)); } catch {}

  return {
    add(each) {
      for (const command of each) commands.set(command.id, { ...command, chord: normalize(command.chord || "") });
      changed();
    },
    chord, holder, list, set, reset, resetAll, run,
    label: (id) => label(chord(id)),
    refusal: (value) => refusal(value, held),
    // While the operator presses a new key nothing runs, here or in the app.
    record(on) {
      recording = !!on;
      try { door?.recording?.(recording); } catch {}
    },
    get recording() { return recording; },
    get desktop() { return !!door; },
    onChange(listener) {
      listeners.add(listener);
      return () => listeners.delete(listener);
    },
  };
}
