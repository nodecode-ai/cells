// files.js -- the Files view: the session's own folder, browsed, read and
// added to over the gateway's files route (surface/files.lisp). The path on
// screen is state.filesPath, relative to the folder ("" is the folder
// itself): a folder is drawn as its rows, a file beside the rows of the
// folder it is in. The route follows every link and refuses whatever leads
// out of the folder; this draws what it answers and never judges a path
// itself. Text is drawn in the mono face with its line numbers, a picture as
// itself, and anything else is offered to save. An upload sends one file's
// bytes at a time, its progress on the line under the bar, and a name
// already there is asked about before it is replaced.
// Precedent: Hermes Web's FilesPage (~/hermes-agent/web/src/pages/FilesPage.tsx)
// -- the crumb path, the Name / Size / Modified rows, the Upload button beside
// a drop onto the list.

import { el, when, bytes, headline, plural } from "./observe.js";
import { iconButton } from "./icons.js";
import { t } from "./i18n.js";

const parentOf = (path) => path.split("/").slice(0, -1).join("/");
const nameOf = (path) => path.split("/").pop();
const join = (dir, name) => (dir ? `${dir}/${name}` : name);
// A link's kind as its row's tag and its title.
const LINKS = {
  inside: [t("link"), t("A link to a place inside the folder")],
  outside: [t("link, leads outside the folder"), t("A link that leads outside the folder")],
  nowhere: [t("link, leads nowhere"), t("A link that leads nowhere")],
};

const route = (session, path, extra = "") =>
  `/api/gateway/sessions/${encodeURIComponent(session)}/files?path=${encodeURIComponent(path)}${extra}`;

// One file put into DIR of SESSION's folder as NAME, REPLACE writing over a
// file of that name already there; PROGRESS hears how many bytes have gone,
// and SIGNAL, when it aborts, stops the bytes. XMLHttpRequest, not fetch:
// only it says how much of a body has gone. It answers { ok, data, code,
// message, stopped } and never throws: DATA is DIR's listing, CODE and
// MESSAGE the route's refusal. The composer sends its files through it too.
export function putFile({ token, session, dir, name, file, replace, signal }, progress) {
  return new Promise((resolve) => {
    const xhr = new XMLHttpRequest();
    signal?.addEventListener("abort", () => xhr.abort());
    xhr.onabort = () => resolve({ ok: false, stopped: true });
    xhr.open("POST", route(session, dir, `&name=${encodeURIComponent(name)}${replace ? "&replace=1" : ""}`));
    if (token) xhr.setRequestHeader("Authorization", `Bearer ${token}`);
    xhr.setRequestHeader("Content-Type", "application/octet-stream");
    xhr.responseType = "json";
    xhr.upload.onprogress = (event) => progress(event.loaded);
    xhr.onload = () => {
      const data = xhr.response;
      resolve({ ok: xhr.status >= 200 && xhr.status < 300, data, code: data?.error?.code, message: data?.error?.message || `${xhr.status}` });
    };
    xhr.onerror = () => resolve({ ok: false, message: t("the gateway isn't answering") });
    xhr.send(file);
  });
}

// A button whose data-fs-NAME attribute holds VALUE: the view's own clicks,
// apart from the Run view's data-act ones the observe pane reads.
function act(className, text, name, value = "") {
  const node = el("button", className, text);
  node.type = "button";
  node.setAttribute(`data-fs-${name}`, value);
  return node;
}

// The same as a mark (icons.js) that says WORD.
function markAct(mark, word, name, value, className) {
  const node = iconButton(mark, word, className);
  node.setAttribute(`data-fs-${name}`, value);
  return node;
}

export function makeFiles({ state, reach, save, toast, redraw, moved, home, openImage }) {
  const f = {
    session: null, // the session the reads below are of
    asked: null, // the path last asked of the route
    stale: false, // read again when next drawn
    listing: null, // the folder on screen, as the route answered it, or { error, code, path }
    file: null, // the file open beside it: { path, name, text | url | why | error, ... }
    upload: null, // { name, loaded, total } while one goes up
    queue: [], // { file, dir, session, replace } waiting to go up
    clash: null, // an upload whose name is already there, asking whether to replace it
    // The text file being edited: { session, path, original, text, saving, said,
    // conflict }, kept until it is saved or its changes are let go, so a
    // redraw, another file or another session never loses what was typed.
    edit: null,
    area: null, // the editor's textarea, one node for the whole edit (focus and undo live in it)
    gutter: null, // its line numbers
    leave: null, // a step away from unsaved changes, asking first: { to }
  };

  const editing = () => f.edit && f.edit.session === f.session && f.file && f.edit.path === f.file.path;
  const dirty = () => !!f.edit && f.edit.text !== f.edit.original;

  // The route's answer for PATH: a folder's listing, or a file as what it is.
  async function ask(session, path) {
    const response = await reach(route(session, path));
    const type = response.headers.get("Content-Type") || "";
    if (!response.ok) {
      const data = await response.json().catch(() => null);
      throw Object.assign(new Error(data?.error?.message || `${response.status}`), { code: data?.error?.code });
    }
    if (type.startsWith("application/json")) return response.json();
    if (type.startsWith("image/")) return { kind: "file", url: URL.createObjectURL(await response.blob()) };
    return { kind: "file", text: await response.text() };
  }

  const refusal = (error, path) => ({ error: error.message, code: error.code, path });

  function dropFile() {
    if (f.file?.url) URL.revokeObjectURL(f.file.url);
    f.file = null;
  }

  // What state.filesPath names, read once per session and path: a file
  // brings the rows of its folder with it, read only when they are not the
  // rows on screen already.
  async function load() {
    const session = state.current;
    const path = state.filesPath || "";
    if (!session || (!f.stale && f.session === session && f.asked === path)) return;
    const fresh = f.stale || f.session !== session;
    if (f.session !== session) f.listing = null;
    Object.assign(f, { session, asked: path, stale: false });
    let answer;
    try { answer = await ask(session, path); } catch (error) { answer = refusal(error, path); }
    if (session !== state.current || path !== (state.filesPath || "")) {
      if (answer.url) URL.revokeObjectURL(answer.url);
      return;
    }
    dropFile();
    if (answer.kind === "directory" || !path) {
      f.listing = answer;
    } else {
      f.file = { ...answer, path, name: nameOf(path) };
      const dir = parentOf(path);
      if (fresh || f.listing?.path !== dir || f.listing?.error) {
        try { f.listing = await ask(session, dir); } catch (error) { f.listing = refusal(error, dir); }
        if (session !== state.current) return;
      }
    }
    redraw();
  }

  function go(path) {
    if (editing() && dirty() && path !== f.edit.path) {
      f.leave = { to: path };
      redraw();
      return;
    }
    if (editing() && path !== f.edit.path) stopEdit();
    state.filesPath = path;
    moved();
    redraw();
  }

  // --- editing ------------------------------------------------------------------

  // The indent a Tab puts in: a tab where the file indents with tabs, else
  // the narrowest run of leading spaces it uses (2 when it shows none).
  function indentOf(text) {
    if (/^\t/m.test(text)) return "\t";
    const runs = (text.match(/^ +(?=\S)/gm) || []).map((run) => run.length);
    return " ".repeat(Math.min(8, Math.max(2, Math.min(...runs, 8) || 2)));
  }

  function startEdit() {
    const file = f.file;
    f.edit = { session: f.session, path: file.path, original: file.text, text: file.text, saving: false, said: "", conflict: false, indent: indentOf(file.text) };
    f.area = null;
    f.focus = { start: 0, end: 0, top: 0 };
    redraw();
  }

  function stopEdit() {
    f.edit = null;
    f.area = null;
    f.gutter = null;
    f.leave = null;
  }

  // Type into the area the way a person's keys do, so the browser's undo
  // keeps it: insertText where the page may, else setRangeText.
  function typeIn(text, start, end) {
    const area = f.area;
    area.setSelectionRange(start, end);
    if (!document.execCommand?.("insertText", false, text)) area.setRangeText(text, start, end, "end");
    edited();
  }

  function linesAround(area) {
    const value = area.value;
    const from = value.lastIndexOf("\n", area.selectionStart - 1) + 1;
    const at = area.selectionEnd > area.selectionStart && value[area.selectionEnd - 1] === "\n" ? area.selectionEnd - 1 : area.selectionEnd;
    const next = value.indexOf("\n", at);
    return [from, next < 0 ? value.length : next];
  }

  function keys(event) {
    const area = f.area;
    const e = f.edit;
    if ((event.metaKey || event.ctrlKey) && !event.altKey && event.key.toLowerCase() === "s") {
      event.preventDefault();
      saveEdit();
      return;
    }
    if (event.key === "Escape" && !dirty()) {
      event.preventDefault();
      stopEdit();
      redraw();
      return;
    }
    if (event.key === "Tab" && !event.altKey && !event.metaKey && !event.ctrlKey) {
      event.preventDefault();
      const { selectionStart: start, selectionEnd: end, value } = area;
      if (!event.shiftKey && !value.slice(start, end).includes("\n")) { typeIn(e.indent, start, end); return; }
      const [from, to] = linesAround(area);
      const block = value.slice(from, to);
      const strip = new RegExp(`^(\\t| {1,${e.indent === "\t" ? 4 : e.indent.length}})`, "gm");
      const next = event.shiftKey ? block.replace(strip, "") : block.replace(/^(?=.)/gm, e.indent);
      if (next === block) return;
      typeIn(next, from, to);
      area.setSelectionRange(from, from + next.length);
      return;
    }
    if (event.key === "Enter" && !event.shiftKey && !event.altKey && !event.metaKey && !event.ctrlKey && !event.isComposing) {
      const { selectionStart: start, selectionEnd: end, value } = area;
      const line = value.slice(value.lastIndexOf("\n", start - 1) + 1, start);
      const lead = line.match(/^[ \t]*/)[0];
      if (!lead) return;
      event.preventDefault();
      typeIn(`\n${lead}`, start, end);
    }
  }

  // What a keystroke changed, painted in place: the numbers, Save, the mark.
  function edited() {
    const e = f.edit;
    e.text = f.area.value;
    if (e.said && !e.saving) e.said = "";
    numbers();
    paintHead();
  }

  function numbers() {
    const count = Math.max(1, f.area.value.split("\n").length);
    if (f.gutter.dataset.count !== String(count)) {
      f.gutter.textContent = Array.from({ length: count }, (_, at) => at + 1).join("\n");
      f.gutter.dataset.count = String(count);
    }
    f.gutter.scrollTop = f.area.scrollTop;
  }

  function paintHead() {
    const view = f.area?.closest(".fs-view");
    if (!view) return;
    const save = view.querySelector('[data-fs-act="save"]');
    if (save) save.disabled = !dirty() || f.edit.saving;
    view.toggleAttribute("data-dirty", dirty());
    const line = view.querySelector(".fs-edit-said");
    if (line) line.textContent = f.edit.said;
  }

  function editor() {
    const e = f.edit;
    if (!f.area) {
      const area = el("textarea", "fs-edit");
      Object.assign(area, { value: e.text, spellcheck: false, wrap: "off" });
      area.setAttribute("autocapitalize", "off");
      area.setAttribute("autocomplete", "off");
      area.setAttribute("aria-label", t("Edit {name}", { name: f.file.name }));
      area.addEventListener("input", edited);
      area.addEventListener("keydown", keys);
      area.addEventListener("scroll", () => { f.gutter.scrollTop = area.scrollTop; });
      f.area = area;
      f.gutter = el("pre", "fs-gutter");
      f.gutter.setAttribute("aria-hidden", "true");
      f.focus = { start: 0, end: 0, top: 0, now: true };
    }
    const box = el("div", "fs-text fs-editing");
    box.append(f.gutter, f.area);
    numbers();
    return box;
  }

  // Saved over the route's upload, as the same name in the same folder, its
  // mode kept. A file that changed on disk since it was opened is asked
  // about first, unless the person already said to save over it.
  async function saveEdit(over = false) {
    const e = f.edit;
    if (!e || e.saving || !dirty()) return;
    e.saving = true;
    e.said = t("Saving…");
    paintHead();
    if (!over) {
      let disk = null;
      try { disk = await ask(e.session, e.path); } catch { disk = null; }
      if (f.edit !== e) return;
      if (disk?.text !== undefined && disk.text !== e.original) {
        Object.assign(e, { saving: false, said: "", conflict: true, disk: disk.text });
        redraw();
        return;
      }
    }
    const text = e.text;
    const result = await putFile({ token: state.token, session: e.session, dir: parentOf(e.path), name: nameOf(e.path), file: new Blob([text], { type: "application/octet-stream" }), replace: true }, () => {});
    if (f.edit !== e) return;
    e.saving = false;
    if (!result.ok) {
      e.said = t("Not saved: {why}", { why: said(result.message) });
      paintHead();
      return;
    }
    Object.assign(e, { original: text, conflict: false, disk: undefined, said: t("Saved") });
    if (f.file?.path === e.path) Object.assign(f.file, { text, size: new Blob([text]).size, modified: undefined });
    if (result.data?.entries && f.listing?.path === parentOf(e.path)) f.listing = result.data;
    redraw();
  }

  function conflictAsk() {
    const panel = el("div", "confirm");
    panel.append(el("p", "title", t("{name} changed on disk since you opened it. Save yours over it?", { name: f.edit.path.split("/").pop() })));
    const actions = el("div", "actions");
    actions.append(act("solid", t("Save over it"), "act", "save-over"), act("word", t("Take the disk's version"), "act", "take-disk"), act("word", t("Keep editing"), "act", "keep-editing"));
    panel.append(actions);
    return panel;
  }

  function leaveAsk() {
    const panel = el("div", "confirm");
    panel.append(el("p", "title", t("Let go of your changes to {name}?", { name: f.edit.path.split("/").pop() })));
    const actions = el("div", "actions");
    actions.append(act("solid", t("Let them go"), "act", "discard"), act("word", t("Keep editing"), "act", "keep-editing"));
    panel.append(actions);
    return panel;
  }

  // --- uploads ------------------------------------------------------------------

  function upload(files) {
    const dir = f.listing?.path ?? parentOf(state.filesPath || "");
    for (const file of files) f.queue.push({ file, dir, session: state.current, replace: false });
    pump();
  }

  async function pump() {
    if (f.upload || f.clash || !f.queue.length) return;
    const job = f.queue.shift();
    f.upload = { name: job.file.name, loaded: 0, total: job.file.size };
    redraw();
    const result = await putFile({ token: state.token, session: job.session, dir: job.dir, name: job.file.name, file: job.file, replace: job.replace }, (loaded) => {
      f.upload.loaded = loaded;
      const line = document.getElementById("fs-progress");
      if (line) line.textContent = progressText();
    });
    f.upload = null;
    const here = job.session === state.current;
    if (result.ok) {
      toast(job.dir ? t("Uploaded {name} to {dir}", { name: job.file.name, dir: job.dir }) : t("Uploaded {name}", { name: job.file.name }));
      if (here && (f.listing?.path ?? null) === job.dir) f.listing = result.data;
    } else if (result.code === "file_exists" && here) {
      f.clash = job;
    } else {
      toast(t("Couldn't upload {name}: {why}", { name: job.file.name, why: result.message }), { error: true, retry: () => { f.queue.unshift(job); pump(); } });
    }
    redraw();
    pump();
  }

  function progressText() {
    const u = f.upload;
    const share = u.total ? Math.floor((100 * u.loaded) / u.total) : 100;
    const going = t("Uploading {name} · {share}% of {size}", { name: u.name, share, size: bytes(u.total) });
    return f.queue.length ? `${going} · ${t("{n} more waiting", { n: f.queue.length })}` : going;
  }

  function answerClash(replace) {
    const job = f.clash;
    f.clash = null;
    if (replace) f.queue.unshift({ ...job, replace: true });
    else toast(t("Kept the {name} already there", { name: job.file.name }));
    redraw();
    pump();
  }

  async function download(path, name) {
    try {
      const response = await reach(route(state.current, path, "&download=1"));
      if (!response.ok) {
        const data = await response.json().catch(() => null);
        throw new Error(data?.error?.message || `${response.status}`);
      }
      save(await response.blob(), name);
    } catch (error) {
      toast(t("Couldn't download {name}: {why}", { name, why: error.message }), { error: true });
    }
  }

  // --- drawing ------------------------------------------------------------------

  // The route's refusals are lower-case clauses: a sentence of their own, or
  // said after a lead-in.
  const said = (text) => `${text}${/[.!?]$/.test(text) ? "" : "."}`;
  const sentence = (text) => said(`${text[0].toUpperCase()}${text.slice(1)}`);

  function folderName() {
    const folder = f.listing?.folder || state.sessionsById.get(state.current)?.working_directory || "";
    return folder.replace(/\/+$/, "").split("/").pop() || folder || t("folder");
  }

  function crumbs() {
    const nav = el("nav", "fs-crumbs");
    nav.setAttribute("aria-label", t("Where in the folder"));
    const path = state.filesPath || "";
    const steps = path ? path.split("/") : [];
    const root = act("", folderName(), "go", "");
    root.title = home(f.listing?.folder || state.sessionsById.get(state.current)?.working_directory || "");
    nav.append(root);
    steps.forEach((step, at) => {
      nav.append(el("span", "fs-sep", "/"));
      const here = act("", step, "go", steps.slice(0, at + 1).join("/"));
      if (at === steps.length - 1) here.setAttribute("aria-current", "location");
      nav.append(here);
    });
    if (!steps.length) root.setAttribute("aria-current", "location");
    return nav;
  }

  // The page's head: the folder on screen and how many things it holds.
  function head() {
    const path = state.filesPath || "";
    const n = f.listing?.entries?.length;
    return headline(t("Files"), path ? path.split("/").at(-1) : folderName(), n == null ? "" : plural(n, t("{n} item"), t("{n} items")));
  }

  function bar() {
    const row = el("div", "fs-bar");
    const acts = el("div", "fs-acts");
    const input = el("input");
    Object.assign(input, { type: "file", multiple: true, hidden: true });
    input.setAttribute("data-fs-input", "");
    const up = markAct("upload", t("Upload"), "act", "upload", "icon boxed");
    up.title = t("Put files from this computer into the folder on screen; or drop them on the list");
    up.disabled = !!f.listing?.error;
    acts.append(markAct("refresh", t("Read again"), "act", "reread"), up, input);
    row.append(crumbs(), acts);
    return row;
  }

  function rowOf(entry, dir) {
    const path = join(dir, entry.name);
    const node = act("fs-row", "", "go", path);
    node.dataset.kind = entry.kind;
    if (entry.hidden) node.dataset.hidden = "";
    if (state.filesPath === path) node.setAttribute("aria-current", "true");
    const name = el("span", "fs-name", entry.name);
    if (entry.link) {
      const [tag, title] = LINKS[entry.link];
      name.append(el("span", "fs-link", tag));
      node.title = title;
    }
    node.append(name, el("span", "reading fs-size", entry.kind === "file" ? bytes(entry.size) : ""),
      el("span", "reading fs-when", entry.modified ? when(entry.modified) : ""));
    return node;
  }

  function list() {
    const l = f.listing;
    const box = el("div", "fs-list");
    box.setAttribute("data-fs-drop", "");
    const head = el("div", "fs-row fs-head");
    head.append(el("span", "micro", t("Name")), el("span", "micro fs-size", t("Size")), el("span", "micro fs-when", t("Changed")));
    box.append(head);
    if (!l) { box.append(el("p", "obs-note fs-note", t("Reading…"))); return box; }
    if (l.error) { box.append(el("p", "fs-note refused", sentence(l.error))); return box; }
    if (l.path) {
      const up = act("fs-row", "", "go", parentOf(l.path));
      up.dataset.kind = "up";
      up.append(el("span", "fs-name", ".."), el("span", "reading fs-size"), el("span", "reading fs-when"));
      box.append(up);
    }
    for (const entry of l.entries) box.append(rowOf(entry, l.path));
    if (!l.entries.length) box.append(el("p", "obs-note fs-note", t("Nothing in this folder yet. Upload, or drop files here, to put some in it.")));
    if (l.more) box.append(el("p", "obs-note fs-note", t("And {n} more, not listed.", { n: l.more })));
    box.append(el("div", "fs-drop", t("Drop to upload into {folder}", { folder: l.path || folderName() })));
    return box;
  }

  // What the listing's row says of the open file: its size and when it changed.
  function facts(file) {
    const row = f.listing?.entries?.find((entry) => join(f.listing.path, entry.name) === file.path);
    const size = file.size ?? row?.size;
    const modified = file.modified ?? row?.modified;
    return [size != null ? bytes(size) : "", modified ? when(modified) : ""].filter(Boolean).join(" · ");
  }

  function numbered(text) {
    const body = text.endsWith("\n") ? text.slice(0, -1) : text;
    const box = el("div", "fs-text");
    const count = body.split("\n").length;
    box.append(el("pre", "fs-gutter", Array.from({ length: count }, (_, at) => at + 1).join("\n")),
      el("pre", "fs-code", body));
    return box;
  }

  function view() {
    const file = f.file;
    const node = el("section", "fs-view");
    node.setAttribute("aria-label", file.name);
    const head = el("div", "fs-view-head");
    head.append(el("span", "fs-view-name", file.name), el("span", "reading", facts(file)));
    const acts = el("span", "fs-acts");
    if (editing()) {
      node.toggleAttribute("data-dirty", dirty());
      node.dataset.editing = "";
      head.querySelector(".fs-view-name").append(el("span", "fs-dirty", t("modified")));
      const line = el("span", "reading fs-edit-said", f.edit.said);
      line.setAttribute("role", "status");
      const save = act("solid fs-save", t("Save"), "act", "save");
      save.title = t("Save ({keys})", { keys: /Mac|iP/.test(navigator.platform) ? "⌘S" : "Ctrl+S" });
      save.disabled = !dirty() || f.edit.saving;
      acts.append(line, save, act("word", dirty() ? t("Cancel") : t("Done"), "act", "cancel"));
    } else {
      // A text file opens to edit, unless it is a link: saving would put a
      // file where the link stood.
      const row = f.listing?.entries?.find((entry) => join(f.listing.path, entry.name) === file.path);
      if (file.text !== undefined && !file.error && !row?.link) acts.append(markAct("pencil", t("Edit"), "act", "edit"));
      if (!file.error) acts.append(markAct("download", t("Download"), "act", "download"));
      acts.append(markAct("close", t("Close"), "act", "close"));
    }
    head.append(acts);
    node.append(head);
    if (editing()) {
      if (f.edit.conflict) node.append(conflictAsk());
      else if (f.leave) node.append(leaveAsk());
      node.append(editor());
      return node;
    }
    if (file.error) {
      node.append(el("p", "fs-note refused", t("Not opened: {why}", { why: said(file.error) })));
    } else if (file.url) {
      const picture = act("fs-picture", "", "act", "zoom");
      picture.setAttribute("aria-label", t("{name}, whole", { name: file.name }));
      const img = el("img");
      img.src = file.url;
      img.alt = file.name;
      picture.append(img);
      node.append(picture);
    } else if (file.text !== undefined) {
      node.append(file.text ? numbered(file.text) : el("p", "obs-note fs-note", t("This file is empty.")));
    } else {
      node.append(el("p", "obs-note fs-note", file.why === "too_large"
        ? t("Not shown: this file is too large to read here. Download saves it.")
        : t("Not shown: this file is not text or a picture. Download saves it.")));
    }
    return node;
  }

  function clashAsk() {
    const job = f.clash;
    const panel = el("div", "confirm");
    panel.append(el("p", "title", t("{name} is already in {folder}. Replace it with the one you chose?", { name: job.file.name, folder: job.dir || folderName() })));
    const actions = el("div", "actions");
    actions.append(act("solid", t("Replace"), "act", "replace"), act("word", t("Keep it"), "act", "keep"));
    panel.append(actions);
    return panel;
  }

  function draw() {
    // The editor's one textarea moves into the new nodes: what was focused
    // and selected in it comes back once they are on the page.
    if (f.area && document.activeElement === f.area) {
      f.focus = { start: f.area.selectionStart, end: f.area.selectionEnd, top: f.area.scrollTop, now: true };
    }
    load();
    const root = el("div", "fs");
    const l = f.listing;
    if (l?.code === "no_folder" || l?.code === "unknown_session") {
      root.append(headline(t("Files"), t("This session has no folder to show."), sentence(l.error)));
      return root;
    }
    root.append(head(), bar());
    if (f.upload) {
      const line = el("p", "fs-said", progressText());
      line.id = "fs-progress";
      root.append(line);
    }
    if (f.clash) root.append(clashAsk());
    const body = el("div", "fs-body");
    if (f.file) body.dataset.open = "";
    body.append(list());
    if (f.file) body.append(view());
    root.append(body);
    listen(root);
    if (f.area && f.focus?.now && editing() && !f.edit.conflict && !f.leave) {
      const { start, end, top } = f.focus;
      f.focus.now = false;
      queueMicrotask(() => {
        if (!f.area?.isConnected) return;
        f.area.focus({ preventScroll: true });
        f.area.setSelectionRange(start, end);
        f.area.scrollTop = top;
        f.gutter.scrollTop = top;
      });
    }
    return root;
  }

  // The view listens on its own nodes, drawn afresh each time.
  function listen(root) {
    const input = root.querySelector("[data-fs-input]");
    root.addEventListener("click", (event) => {
      const target = event.target.closest("[data-fs-go], [data-fs-act]");
      if (!target || target.disabled) return;
      if (target.hasAttribute("data-fs-go")) { go(target.getAttribute("data-fs-go")); return; }
      switch (target.getAttribute("data-fs-act")) {
        case "upload": input.click(); break;
        case "reread": f.stale = true; redraw(); break;
        case "download": download(f.file.path, f.file.name); break;
        case "close": go(parentOf(f.file.path)); break;
        case "edit": startEdit(); break;
        case "save": saveEdit(); break;
        case "cancel":
          if (dirty()) { f.leave = { to: null }; redraw(); } else { stopEdit(); redraw(); }
          break;
        case "save-over": f.edit.conflict = false; saveEdit(true); break;
        case "take-disk": {
          const text = f.edit.disk;
          stopEdit();
          Object.assign(f.file, { text });
          redraw();
          break;
        }
        case "keep-editing": f.edit.conflict = false; f.leave = null; f.focus = { ...(f.focus || {}), now: true }; redraw(); break;
        case "discard": {
          const to = f.leave?.to;
          stopEdit();
          if (to != null) go(to); else redraw();
          break;
        }
        case "zoom": openImage(f.file.url, f.file.name); break;
        case "replace": answerClash(true); break;
        case "keep": answerClash(false); break;
      }
    });
    input.addEventListener("change", () => {
      const files = [...input.files];
      input.value = "";
      if (files.length) upload(files);
    });
    // A drag of files over the list lights it where it stands, without a
    // redraw: every row entered and left counts, so it goes dark only once
    // the drag has left the list whole.
    const drop = root.querySelector("[data-fs-drop]");
    const carries = (event) => [...(event.dataTransfer?.types || [])].includes("Files") && !f.listing?.error;
    let depth = 0;
    const light = (step) => {
      depth = Math.max(0, depth + step);
      drop.toggleAttribute("data-over", depth > 0);
    };
    drop.addEventListener("dragenter", (event) => {
      if (!carries(event)) return;
      event.preventDefault();
      light(1);
    });
    drop.addEventListener("dragover", (event) => {
      if (!carries(event)) return;
      event.preventDefault();
      event.dataTransfer.dropEffect = "copy";
    });
    drop.addEventListener("dragleave", (event) => { if (carries(event)) light(-1); });
    drop.addEventListener("drop", (event) => {
      if (!carries(event)) return;
      event.preventDefault();
      light(-depth);
      const files = [...(event.dataTransfer.files || [])];
      if (files.length) upload(files);
    });
  }

  // A tab closed or reloaded over unsaved changes is asked about by the browser.
  addEventListener("beforeunload", (event) => {
    if (!dirty()) return;
    event.preventDefault();
    event.returnValue = "";
  });

  return {
    draw,
    // The view comes up: what is on screen is read again, the folder having
    // moved on while it was away.
    show() { f.stale = true; },
  };
}
