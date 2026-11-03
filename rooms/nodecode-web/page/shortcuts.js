// shortcuts.js -- Settings: Shortcuts, every key the page answers and the
// desktop app's with them, each changed by pressing the new one. keys.js
// keeps the keymap and refuses what cannot be a shortcut; here is the list
// and the moment of pressing: while a row listens nothing runs, Esc puts it
// back, and a key another command holds is asked about, never taken.

import { el, button, mark, headline, plural } from "./observe.js";
import { block, note } from "./control.js";
import { chordOf, caps, label } from "./keys.js";
import { t } from "./i18n.js";

const GROUPS = [["palette", t("Palette")], ["go", t("Go to")], ["session", t("This session")], ["desktop", t("Desktop app")]];

// The prompt box's own keys, which /help lists too: shown, not changed.
export const BOX = [
  ["Enter", t("Send; while a turn runs, steer")],
  ["Tab", t("While a turn runs, queue this for after it")],
  ["Shift+Enter", t("A new line")],
  ["Up", t("The prompt before, or the last queued one back")],
  ["Esc", t("Stop the running turn")],
];

export function makeShortcuts({ c, redraw, toast }) {
  let keys = null;
  // LISTEN: the command whose row waits for a key. ASK: a key another command
  // holds, { id, chord, other }. REFUSED: why the key pressed cannot be, by id.
  const s = c.shortcuts = { listen: null, ask: null, refused: null, find: "" };

  function stop() {
    if (!s.listen) return;
    s.listen = null;
    keys?.record(false);
  }

  // The key pressed while a row listens: the page's, the app's and the
  // browser's own listeners never see it.
  window.addEventListener("keydown", (event) => {
    if (!s.listen || !keys) return;
    event.preventDefault();
    event.stopImmediatePropagation();
    if (event.repeat || event.isComposing) return;
    const id = s.listen;
    if (event.key === "Escape" && !event.ctrlKey && !event.metaKey && !event.altKey && !event.shiftKey) {
      stop();
      redraw();
      return;
    }
    const chord = chordOf(event);
    if (!chord) return;
    stop();
    s.refused = null;
    s.ask = null;
    const why = keys.refusal(chord);
    const other = !why && keys.holder(chord, id);
    if (why) s.refused = { id, why };
    else if (other) s.ask = { id, chord, other: other.id };
    else keys.set(id, chord);
    redraw();
    if (!why && !other) focusRow(id);
  }, true);

  // A press anywhere but the listening key stops it listening.
  document.addEventListener("pointerdown", (event) => {
    if (!s.listen || event.target.closest?.("[data-listening]")) return;
    stop();
    redraw();
  }, true);

  const focusRow = (id) => queueMicrotask(() => document.querySelector(`.short-row[data-id="${CSS.escape(id)}"] [data-act="short-listen"]`)?.focus());
  const nameOf = (id) => keys?.list().find((row) => row.id === id)?.name || id;

  function capsOf(chord) {
    const node = el("span", "short-caps");
    for (const cap of caps(chord)) node.append(el("kbd", "short-cap", cap));
    return node;
  }

  function row(command) {
    const li = el("li", "ctl-row short-row");
    li.dataset.id = command.id;
    li.dataset.find = `${command.name} ${label(command.chord)}`.toLowerCase();
    const listening = s.listen === command.id;
    const name = el("span", "ctl-name", command.name);
    // The key is the row's button: a press on it listens for the new one.
    const key = button("short-key", "", { act: "short-listen", id: command.id });
    key.setAttribute("aria-label", listening ? t("Press the new keys for {name}", { name: command.name })
      : command.chord ? t("{name}: {key}. Change it", { name: command.name, key: label(command.chord) })
      : t("{name}: no key. Give it one", { name: command.name }));
    if (listening) {
      key.dataset.listening = "";
      key.append(el("span", "short-listen", t("Press the keys, Esc to leave it")));
    } else if (command.chord) key.append(capsOf(command.chord));
    else key.append(el("span", "short-none", t("none")));
    const acts = el("span", "ctl-acts");
    if (command.changed) {
      const back = mark("rewind", command.default ? t("Back to {key}", { key: label(command.default) }) : t("Back to none"), { act: "short-reset", id: command.id });
      acts.append(back);
    }
    if (command.chord) acts.append(mark("close", t("Remove the key"), { act: "short-clear", id: command.id }));
    li.append(name, key, acts);
    const out = [li];
    if (s.ask?.id === command.id) {
      const ask = el("li", "ctl-sub short-ask");
      ask.setAttribute("role", "alert");
      ask.append(el("span", "", t("{key} already runs {name}. Move it here? {name} is left with no key.", { key: label(s.ask.chord), name: nameOf(s.ask.other) })),
        button("quiet", t("Move it here"), { act: "short-take" }), button("word", t("Cancel"), { act: "short-cancel" }));
      out.push(ask);
    }
    if (s.refused?.id === command.id) {
      const why = el("li", "ctl-sub refused short-refused", s.refused.why);
      why.setAttribute("role", "alert");
      out.push(why);
    }
    return out;
  }

  function draw() {
    const root = el("div", "ctl-tab");
    if (!keys) return root;
    const all = keys.list();
    const changed = all.filter((command) => command.changed).length;
    const where = keys.desktop ? t("Kept in this app; its menu shows the same keys.") : t("Kept in this browser, for every tab of it.");
    root.append(headline(t("Shortcuts"), plural(all.length, t("{n} shortcut"), t("{n} shortcuts")),
      changed ? `${t("{n} changed", { n: changed })}. ${where}` : `${t("As they come")}. ${where}`));
    const bar = el("div", "ctl-find short-bar");
    const find = el("input", "control");
    Object.assign(find, { type: "search", placeholder: t("Find a shortcut by name or key"), value: s.find });
    find.dataset.input = "short-find";
    find.setAttribute("aria-label", t("Find a shortcut by name or key"));
    bar.append(find);
    if (changed) bar.append(button("word", t("Reset all"), { act: "short-reset-all" }));
    root.append(bar);
    for (const [group, title] of GROUPS) {
      const rows = all.filter((command) => (command.group === group || (group === "desktop" && command.desktop && !GROUPS.some(([g]) => g === command.group && g !== "desktop"))));
      if (!rows.length) continue;
      const section = block(title);
      const list = el("ol", "ctl-list short-list");
      list.append(...rows.flatMap(row));
      section.append(list);
      root.append(section);
    }
    const box = block(t("In the prompt box"));
    const list = el("ol", "ctl-list short-list");
    for (const [chord, what] of BOX) {
      const li = el("li", "ctl-row short-row short-fixed");
      li.dataset.find = `${what} ${label(chord)}`.toLowerCase();
      const key = el("span", "short-key");
      key.append(capsOf(chord));
      li.append(el("span", "ctl-name", what), key);
      list.append(li);
    }
    box.append(note(t("The box's own keys; they are not changed here.")), list);
    root.append(box);
    sift(root);
    return root;
  }

  // The rows the search leaves; a section left with none goes too.
  function sift(root) {
    const needle = s.find.trim().toLowerCase();
    for (const list of root.querySelectorAll(".short-list")) {
      let hits = 0;
      for (const li of list.children) {
        if (!li.classList.contains("short-row")) continue;
        const hit = !needle || li.dataset.find.includes(needle);
        li.hidden = !hit;
        for (let sub = li.nextElementSibling; sub && !sub.classList.contains("short-row"); sub = sub.nextElementSibling) sub.hidden = !hit;
        if (hit) hits++;
      }
      list.closest(".ctl-block").hidden = !hits;
    }
  }

  function click(d) {
    if (!keys || !d.act?.startsWith("short-")) return false;
    const id = d.id;
    switch (d.act) {
      case "short-listen":
        s.ask = null;
        s.refused = null;
        if (s.listen === id) stop();
        else { s.listen = id; keys.record(true); }
        break;
      case "short-take": {
        const { id: to, chord, other } = s.ask || {};
        s.ask = null;
        if (to) {
          const why = keys.set(to, chord, { take: true });
          if (why) s.refused = { id: to, why };
          else toast(t("{key} now runs {name}; {other} has no key.", { key: label(chord), name: nameOf(to), other: nameOf(other) }));
        }
        break;
      }
      case "short-cancel": s.ask = null; break;
      case "short-reset": stop(); s.ask = null; s.refused = null; keys.reset(id); break;
      case "short-clear": stop(); s.ask = null; s.refused = null; keys.set(id, ""); break;
      case "short-reset-all": stop(); s.ask = null; s.refused = null; keys.resetAll(); toast(t("Every shortcut is back to how it came.")); break;
      default: return false;
    }
    redraw();
    if (d.act === "short-listen" && s.listen) focusRow(id);
    return true;
  }

  function input(event) {
    if (event.target.dataset.input !== "short-find") return false;
    s.find = event.target.value;
    const root = event.target.closest(".ctl-tab");
    if (root) sift(root);
    return true;
  }

  return {
    draw, click, input, stop,
    use(map) {
      keys = map;
      keys.onChange(() => { if (c.tab === "shortcuts") redraw(); });
    },
    // How the tab stands in a few words, for the wide window's list.
    glance() {
      if (!keys) return ["", ""];
      const n = keys.list().filter((command) => command.changed).length;
      return [n ? t("{n} changed", { n }) : t("as they come"), ""];
    },
  };
}
