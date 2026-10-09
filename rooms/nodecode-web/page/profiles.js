// profiles.js -- the Profiles tab of the Control pane: `nodecode profile' from
// the page. A profile is a home of its own -- its config, keys, store, layer
// and gateway -- and so an organism of its own. The gateway's profile routes
// (surface/control.lisp) are the kernel's verbs (organism/profile.lisp), and
// every refusal shown here is theirs, in their words. This page belongs to
// one gateway and never moves into another profile: a row says how to open
// that profile's own page from a terminal. The builder is one form in the
// order the verb reads it -- a name, what to start from, a description, a
// SOUL.md -- ending in the line that says what Create will do.
//
// Precedent: Hermes' profiles page and profile builder (hermes-agent
// web/src/pages/ProfilesPage.tsx, ProfileBuilderPage.tsx) -- the name rule
// checked as it is typed, clone-from with a clone-everything choice, the SOUL
// editor under its profile, the copyable terminal command, nothing written
// before Create. Its model, skills and MCP steps have no verb here.

import { el, button, mark, plural } from "./observe.js";
import { block, note, form, formKey, field, confirmBox, bytes, about } from "./control.js";
import { t } from "./i18n.js";

// The name rule PROFILE-NAME-P holds a name to, checked as it is typed; the gateway still decides.
const NAME = /^[a-z0-9][a-z0-9_-]{0,63}$/;
const CLONES = [
  ["settings", t("Its settings"), t("config.jsonc without its channels, SOUL.md, themes and the installed cells are copied; memory, skills, secrets and auth.json are shared by link, so a correction lives in one place. A fresh store and a fresh layer.")],
  ["all", t("A fork"), t("Everything a clone of its settings copies, your own cell folders and the layer with its git history too, and memory, skills, secrets and auth.json copied rather than shared. Never the store.")],
  ["blank", t("Nothing"), t("An empty home: the setup walk runs at its first launch.")],
];
const SOUL = t("SOUL.md is the channel kit's: a channel (Discord, Telegram, Slack) gives its text to every conversation it holds as the agent's voice, from the next message on, unless its section in config.jsonc names a soul_file of its own. A shell and this page's sessions do not read it. Saved empty, the file is taken out.");

const fresh = () => ({ name: "", clone: "settings", from: "", description: "", soul: "" });

export function makeProfiles({ c, api, act, toast, redraw, download }) {
  // SOUL is the file open for editing, with what the hand typed into it; REFUSED
  // the profile whose Delete was asked of one that stays.
  const p = { list: null, soul: null, refused: null, draft: fresh() };
  const row = (name) => p.list?.profiles?.find((r) => r.name === name);
  const query = (name) => `name=${encodeURIComponent(name)}`;

  async function load() {
    try { p.list = await api("GET", "/api/gateway/profiles"); } catch (error) { p.list = { error: error.message }; }
  }

  // The profile a clone starts from when none is picked: this gateway's own.
  const source = () => p.draft.from || p.list.this;

  // What Create will do, said before it is pressed, or what stands in its way.
  function review() {
    const d = p.draft;
    const name = d.name.trim();
    if (!name) return { text: t("Name it, and this line says what Create makes."), bad: true };
    if (!NAME.test(name)) return { text: t("{name} is not a profile name: a-z, 0-9, _ and -, 64 at most, starting with a letter or digit.", { name }), bad: true };
    if (row(name)) return { text: t("A profile named {name} already exists.", { name }), bad: true };
    const from = source();
    const start = { settings: t("the settings of {from}, its memory, skills and keys shared by link", { from }),
      all: t("a fork of {from}, its memory, skills and keys copied", { from }), blank: t("an empty home") }[d.clone];
    const soul = d.soul.trim() ? t("SOUL.md as written above") : d.clone === "blank" ? t("no SOUL.md") : t("{from}'s SOUL.md if it has one", { from });
    const made = { name, path: `${p.list.folder}${name}/`, start, soul, description: d.description.trim(), here: p.list.this };
    return { text: made.description
      ? t('Create makes {name} at {path}: {start}; {soul}; described as "{description}". A store and a layer of its own; this page stays on {here}.', made)
      : t("Create makes {name} at {path}: {start}; {soul}. A store and a layer of its own; this page stays on {here}.", made) };
  }

  function showReview() {
    const line = document.getElementById("prof-review");
    if (!line) return;
    const r = review();
    line.textContent = r.text;
    line.toggleAttribute("data-bad", !!r.bad);
    const go = document.getElementById("prof-create");
    if (go) go.disabled = !!r.bad || !!c.busy;
  }

  // --- the list -------------------------------------------------------------------

  function facts(r) {
    const made = r.clone === "all" ? t("a fork of {source}", { source: r.source }) : r.clone === "settings" ? t("the settings of {source}", { source: r.source })
      : r.source ? t("from {source}", { source: r.source }) : "";
    const serving = r.port ? t("serving, pid {pid} on port {port}", { pid: r.pid, port: r.port }) : t("serving, pid {pid}", { pid: r.pid });
    return [r.home, r.pid ? serving : t("not running"),
      r.store_bytes != null ? t("store {size}", { size: bytes(r.store_bytes) }) : t("no store yet"), made,
      r.shares?.length ? t("shares {names}", { names: r.shares.join(", ") }) : "", r.soul ? t("has a SOUL.md") : ""].filter(Boolean).join(" · ");
  }

  function drawRow(rows, r) {
    const li = el("li", "ctl-row");
    li.dataset.level = r.pid ? "started" : "off";
    const name = el("span", "ctl-name");
    name.append(el("i", `led${r.pid ? "" : " off"}`), ` ${r.name}`);
    name.title = r.home;
    const what = el("span", "ctl-cell-what");
    const says = el("span", "ctl-prof-says");
    if (r.current) says.append(el("span", "ctl-prof-mark", t("this page")));
    if (r.sticky) says.append(el("span", "ctl-prof-mark", t("boots by default")));
    says.append(el("span", "", r.description || (r.name === "default" ? t("The default home.") : t("No description."))));
    what.append(says, el("span", "tag", facts(r)));
    const acts = el("span", "ctl-acts");
    if (!r.sticky) {
      const use = button("word", c.busy === `prof-use:${r.name}` ? t("Making default…") : t("Make default"), { act: "prof-use", name: r.name });
      use.title = t("The profile a bare nodecode runs: {command}", { command: `nodecode profile use ${r.name}` });
      acts.append(use);
    }
    acts.append(button("word", p.soul?.name === r.name ? t("Close SOUL") : "SOUL", { act: "prof-soul", name: r.name }),
      mark("download", t("Export"), { act: "prof-export", name: r.name }, c.busy === `prof-export:${r.name}` && t("Saving…")),
      mark("trash", t("Delete"), { act: "prof-ask-drop", name: r.name }));
    for (const b of acts.children) b.disabled = !!c.busy;
    li.append(name, what, acts);
    rows.append(li);
    if (!r.current) {
      const command = `nodecode -p ${r.name} web`;
      const open = el("li", "ctl-sub ctl-prof-open");
      open.append(r.pid ? t("Its own page, from a terminal:")
        : t("Its own page, from a terminal, once it runs ({command}):", { command: `nodecode -p ${r.name} gateway on` }),
      " ", el("code", "", command), mark("copy", t("Copy"), { act: "prof-copy", text: command }));
      rows.append(open);
    }
    if (p.refused === r.name && r.kept) rows.append(el("li", "ctl-sub refused", t("Not deleted: {why}.", { why: r.kept })));
    if (c.confirm === `prof-drop:${r.name}`) {
      const gone = { name: r.name, shares: r.shares?.join(", "), source: r.source };
      rows.append(inset(confirmBox(r.shares?.length
        ? t("Delete {name}? Its whole home goes for good: its store with every session, its layer, its config, and whatever memory and keys it holds of its own. What it shares by link ({shares}) stays with {source}.", gone)
        : t("Delete {name}? Its whole home goes for good: its store with every session, its layer, its config, and whatever memory and keys it holds of its own.", gone),
      t("Delete"), "prof-drop", { name: r.name })));
    }
    if (p.soul?.name === r.name) rows.append(inset(drawSoul(p.soul)));
  }

  function inset(node) {
    const li = el("li", "ctl-inset");
    li.append(node);
    return li;
  }

  function drawSoul(s) {
    const panel = el("div", "ctl-prof-soul");
    panel.append(el("p", "micro", t("SOUL.md of {name}", { name: s.name })), note(SOUL));
    const text = el("textarea", "control");
    text.dataset.input = "prof-soul";
    text.rows = 12;
    text.value = s.draft;
    text.placeholder = t("How the agent sounds in a channel: its voice, its stance, its boundaries.");
    text.setAttribute("aria-label", t("SOUL.md of {name}", { name: s.name }));
    const actions = el("div", "ctl-actions");
    const save = button("solid", c.busy === "prof-soul" ? t("Saving…") : t("Save"), { act: "prof-soul-save" });
    save.disabled = !!c.busy;
    actions.append(save, button("word", t("Close"), { act: "prof-soul", name: s.name }),
      el("span", "reading", s.present ? s.file : t("No SOUL.md yet; Save writes {file}", { file: s.file })));
    panel.append(text, actions);
    return panel;
  }

  // --- the builder ------------------------------------------------------------------

  function drawBuilder() {
    const d = p.draft;
    const l = p.list;
    const box = block(t("New profile"));
    const f = form("prof-create");
    f.classList.add("ctl-prof-build");
    const name = field("name", t("1 · Name"), { value: d.name, placeholder: "coder", autocomplete: "off", spellcheck: "false",
      pattern: "[a-z0-9][a-z0-9_\\-]{0,63}", title: t("a-z, 0-9, _ and -, starting with a letter or digit"), required: "" });

    const start = el("div", "ctl-field ctl-prof-start");
    const choices = el("div", "scope");
    choices.setAttribute("role", "radiogroup");
    choices.setAttribute("aria-label", t("Start from"));
    for (const [id, label] of CLONES) {
      const sw = button("sw", label, { act: "prof-clone", clone: id });
      sw.setAttribute("role", "radio");
      sw.setAttribute("aria-checked", String(d.clone === id));
      choices.append(sw);
    }
    start.append(el("span", "micro", t("2 · Start from")), choices);

    const from = el("label", "ctl-field ctl-prof-from");
    const select = el("select", "control");
    select.name = "from";
    select.disabled = d.clone === "blank";
    const here = l.profiles.find((r) => r.current);
    // A home set by NODECODE_HOME is not on the roster; the verb's own default reaches it.
    if (!here) select.append(Object.assign(el("option", "", t("{name} (this page)", { name: l.this })), { value: "" }));
    for (const r of l.profiles) {
      const option = el("option", "", r.current ? t("{name} (this page)", { name: r.name }) : r.name);
      option.value = r.current ? "" : r.name;
      option.selected = option.value === d.from;
      select.append(option);
    }
    from.append(el("span", "micro", t("From")), select);

    const what = el("p", "ctl-prof-what", CLONES.find(([id]) => id === d.clone)[2]);
    const description = field("description", t("3 · Description (optional)"), { value: d.description, autocomplete: "off",
      placeholder: t("What this profile is for") });
    description.classList.add("wide");
    const soul = field("soul", t("4 · SOUL.md (optional)"), { multiline: true, rows: "4",
      placeholder: d.clone === "blank" ? t("How the agent sounds in this profile's channels.") : t("How the agent sounds in this profile's channels. Empty keeps the SOUL.md the clone carries.") });
    soul.classList.add("wide");
    soul.querySelector("textarea").value = d.soul;

    const last = el("div", "ctl-field wide ctl-prof-last");
    const line = el("p", "ctl-prof-review");
    line.id = "prof-review";
    const go = button("solid", c.busy === "prof-create" ? t("Creating…") : t("Create"), {});
    go.type = "submit";
    go.id = "prof-create";
    const bar = el("div", "ctl-actions");
    bar.append(line, go);
    last.append(el("span", "micro", t("5 · Review")), bar);

    f.append(name, start, from, what, description, soul, last);
    box.append(f);
    queueMicrotask(showReview);
    return box;
  }

  // --- the tab ----------------------------------------------------------------------

  function draw() {
    const root = el("div", "ctl-tab");
    const l = p.list;
    if (!l || l.error) {
      const line = note(l?.error ? t("Could not read the profiles: {error}", { error: l.error }) : t("Looking…"));
      if (l?.error) line.append(" ", button("word", t("Retry"), { act: "reread" }));
      root.append(line);
      return root;
    }
    const [infoMark, words] = about(c, "profiles",
      t("A profile is a home of its own: its config, keys, store, layer and gateway, so each one runs as an organism of its own. This page belongs to the gateway of {name} and stays there; another profile's page opens from a terminal with the command under its row.", { name: l.this }),
      t("Make default writes the profile a bare nodecode runs from the next launch on; nothing running moves. Export saves a home as an archive without its store, secrets and credentials, which nodecode profile import brings back. Named profiles live in {folder}.", { folder: l.folder }));
    const list = block(t("Profiles"), infoMark, el("span", "reading", plural(l.profiles.length, t("{n} profile"), t("{n} profiles"))));
    list.append(words);
    const rows = el("ol", "ctl-list");
    for (const r of l.profiles) drawRow(rows, r);
    list.append(rows);
    if (!l.profiles.some((r) => r.current)) list.append(note(t("This gateway runs {home}, a home NODECODE_HOME names, which is not among the profiles.", { home: l.home })));
    root.append(list, drawBuilder());
    return root;
  }

  async function copy(text) {
    try {
      await navigator.clipboard.writeText(text);
      toast(t("Copied: {text}", { text }));
    } catch {
      toast(t("Could not copy; select it by hand: {text}", { text }), { error: true });
    }
  }

  function click(d) {
    switch (d.act) {
      case "prof-use":
        act(`prof-use:${d.name}`, async () => { p.list = await api("POST", "/api/gateway/profiles/use", { name: d.name }); toast(p.list.text); });
        return true;
      case "prof-soul":
        if (p.soul?.name === d.name) { p.soul = null; redraw(); return true; }
        act(`prof-soul:${d.name}`, async () => {
          const s = await api("GET", `/api/gateway/profiles/soul?${query(d.name)}`);
          p.soul = { ...s, draft: s.content };
        });
        return true;
      case "prof-soul-save":
        act("prof-soul", async () => {
          const s = await api("POST", "/api/gateway/profiles/soul", { name: p.soul.name, content: p.soul.draft });
          p.soul = { ...s, draft: s.content };
          toast(s.text);
          await load();
        });
        return true;
      case "prof-export":
        act(`prof-export:${d.name}`, async () => {
          await download(`/api/gateway/profiles/export?${query(d.name)}`, `nodecode-profile-${d.name}.tar.gz`);
          toast(t("Saved {file}", { file: `nodecode-profile-${d.name}.tar.gz` }));
        });
        return true;
      // A profile that stays says why at once; one that may go asks first.
      case "prof-ask-drop":
        if (row(d.name)?.kept) { p.refused = p.refused === d.name ? null : d.name; c.confirm = null; }
        else { c.confirm = `prof-drop:${d.name}`; p.refused = null; }
        redraw();
        return true;
      case "prof-drop":
        act("prof-drop", async () => {
          p.list = await api("DELETE", `/api/gateway/profiles?${query(d.name)}`);
          if (p.soul?.name === d.name) p.soul = null;
          toast(p.list.text);
        });
        return true;
      case "prof-clone": p.draft.clone = d.clone; redraw(); return true;
      case "prof-copy": copy(d.text); return true;
    }
    return false;
  }

  // What the hand types is kept, so a redraw never takes it back.
  function input(event) {
    const node = event.target;
    if (node.dataset.input === "prof-soul") { if (p.soul) p.soul.draft = node.value; return true; }
    if (!node.name || !node.closest('form[data-act="prof-create"]')) return false;
    p.draft[node.name] = node.value;
    showReview();
    return true;
  }

  function submit(node) {
    if (node.dataset.act !== "prof-create") return false;
    const d = p.draft;
    const body = { name: d.name.trim(), clone: d.clone };
    if (d.clone !== "blank" && d.from) body.from = d.from;
    if (d.description.trim()) body.description = d.description.trim();
    if (d.soul.trim()) body.soul = d.soul;
    act("prof-create", async () => {
      p.list = await api("POST", "/api/gateway/profiles", body);
      p.draft = fresh();
      toast(p.list.text);
    }, formKey(node));
    return true;
  }

  return { load, draw, click, input, submit };
}
