// board.js -- the Board: work queued as tasks, and what became of each.
//
// A task is a prompt, the folder it runs in and, in the desktop app, whether
// it gets a git worktree of its own. It waits in To do until it is started --
// its Start, or dragged onto Running -- and then it is a session: the card
// follows that session's ball (the operator's balls, 2026-10-21: red running,
// orange waiting on you, green done) into Running, Needs you or Done, and a
// click opens it. The list is the web cell's (GET/PUT /api/web/tasks, a file
// in the home): every window and tab of this organism shows one board, and a
// list read at an older version is refused, so the later window merges its
// change into the newer list and sends again rather than overwriting it.
import { el, headline, plural } from "./observe.js";
import { t } from "./i18n.js";

export const COLUMNS = [
  ["todo", t("To do")],
  ["run", t("Running")],
  ["ask", t("Needs you")],
  ["done", t("Done")],
];

// A started task whose session has not been seen running yet counts as
// running this long: the turn is on its way.
const STARTING_MS = 120000;
const DRAG_TYPE = "text/x-nodecode-task";

const firstLine = (text) => (text || "").trim().split("\n")[0].slice(0, 200);
const newTaskId = () => `task-${Date.now().toString(36)}-${Math.floor(Math.random() * 36 ** 4).toString(36)}`;

export function makeBoard({ state, reach, toast, redraw, refresh, start, send, release, open, doing, waitsLine, ball, since, home, folderName, knownFolders, askFolder, desktop }) {
  const board = { version: null, tasks: [], error: null, read: false };
  const starting = new Set(); // tasks whose Start is on its way
  let queue = Promise.resolve();
  let dragging = null;
  let editing = null; // the To do task whose text is open in its card
  let form = null;

  const worktrees = () => typeof desktop()?.newWorktree === "function";

  async function load() {
    try {
      const response = await reach("/api/web/tasks");
      const data = await response.json().catch(() => null);
      if (!response.ok) throw new Error(data?.error?.message || `${response.status}`);
      board.version = data.version;
      board.tasks = data.tasks || [];
      board.error = null;
    } catch (error) {
      board.error = error.message;
    }
    board.read = true;
    watch();
    redraw();
  }

  // CHANGE, a function of the list, applied to the newest list the board
  // holds and sent; a list another window kept first is taken and CHANGE
  // applied to it again. One change at a time, in the order they were made.
  function change(fn) {
    queue = queue.then(async () => {
      for (let tries = 0; tries < 4; tries += 1) {
        const tasks = fn(board.tasks.map((task) => ({ ...task })));
        try {
          const response = await reach("/api/web/tasks", {
            method: "PUT",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ version: board.version ?? 0, tasks }),
          });
          const data = await response.json().catch(() => null);
          if (response.status === 409 && data?.version != null) {
            board.version = data.version;
            board.tasks = data.tasks || [];
            continue;
          }
          if (!response.ok) throw new Error(data?.error?.message || `${response.status}`);
          board.version = data.version;
          board.tasks = data.tasks || [];
          board.error = null;
          redraw();
          return true;
        } catch (error) {
          toast(t("The board couldn't be kept: {why}", { why: error.message }), { error });
          redraw();
          return false;
        }
      }
      return false;
    });
    return queue;
  }

  // Where a task stands: To do until started; then its session's ball. A
  // session the list does not hold as spoken in yet (missing, or standing by)
  // counts as running while its first turn is on its way; one no longer
  // listed after that is done.
  function columnOf(task) {
    if (!task.session_id) return starting.has(task.id) ? "run" : "todo";
    if (state.running.has(task.session_id)) return "run";
    const entry = state.sessionsById.get(task.session_id);
    if (!entry || entry.standby) return arriving(task) ? "run" : "done";
    return doing(entry);
  }

  const arriving = (task) => Date.now() - Date.parse(task.started_at || 0) < STARTING_MS;

  // While a started task's session is not listed yet, the list is read again
  // every couple of seconds: a turn quicker than the activity the gateway
  // sends would otherwise wait for the list's own half-minute read.
  let polling = null;
  // A started session's first turn is listed, or runs: the board stops
  // following it (send held it so the gateway would take the turn).
  function settle() {
    for (const task of board.tasks) {
      if (!task.session_id) continue;
      const entry = state.sessionsById.get(task.session_id);
      if (state.running.has(task.session_id) || (entry && !entry.standby) || !arriving(task)) release(task.session_id);
    }
  }
  function poll() {
    settle();
    const waiting = board.tasks.some((task) => {
      if (!task.session_id || state.running.has(task.session_id)) return false;
      const entry = state.sessionsById.get(task.session_id);
      return (!entry || entry.standby) && arriving(task);
    });
    if (!waiting) { polling = null; return; }
    refresh();
    polling = setTimeout(poll, 2000);
  }
  const watch = () => { if (!polling) polling = setTimeout(poll, 1500); };

  async function startTask(id) {
    const task = board.tasks.find((each) => each.id === id);
    if (!task || task.session_id || starting.has(id)) return;
    if (!state.open) {
      toast(t("Not connected: the task stays in To do."));
      return;
    }
    starting.add(id);
    redraw();
    try {
      let session = null;
      let branch = null;
      if (task.worktree && worktrees()) {
        const made = await desktop().newWorktree(task.folder);
        if (!made?.id) throw new Error(made?.error || t("no worktree was made"));
        session = made.id;
        branch = made.branch || null;
      } else {
        session = await start(task.folder);
      }
      if (!session) return;
      const kept = await change((tasks) => tasks.map((each) => (each.id === id
        ? { ...each, session_id: session, started_at: new Date().toISOString(), ...(branch ? { branch } : {}) }
        : each)));
      if (kept) { send(session, task.prompt); watch(); }
    } catch (error) {
      toast(t("Couldn't start the task: {why}", { why: error.message }), { error });
    } finally {
      starting.delete(id);
      redraw();
    }
  }

  async function addTask() {
    const prompt = form.prompt.value;
    if (!prompt.trim()) {
      form.prompt.focus();
      return;
    }
    const typed = form.folder.value.trim();
    let folder = typed;
    form.add.disabled = true;
    // The folder is the gateway's to resolve, as the opener's is (GET
    // /api/gateway/folder): one that is not there is refused here, saying
    // why, before the task is kept.
    let refused = null;
    if (typed) {
      try {
        const answer = await askFolder(typed);
        if (answer?.exists === false) refused = answer.why || t("not a folder");
        else folder = answer?.path || typed;
      } catch (error) {
        refused = error.message;
      }
    }
    if (refused) {
      form.said.textContent = refused;
      form.add.disabled = false;
      form.folder.focus();
      return;
    }
    form.said.textContent = "";
    const task = { id: newTaskId(), prompt, folder, worktree: worktrees() && form.worktree.checked, created_at: new Date().toISOString() };
    const kept = await change((tasks) => [...tasks, task]);
    form.add.disabled = false;
    if (kept) {
      form.prompt.value = "";
      form.prompt.focus();
    }
  }

  const remove = (id) => change((tasks) => tasks.filter((task) => task.id !== id));
  const clearDone = () => change((tasks) => tasks.filter((task) => columnOf(task) !== "done"));

  // ID moved to stand before BEFORE in To do (at its end when BEFORE is null).
  function move(id, before) {
    if (id === before) return;
    change((tasks) => {
      const task = tasks.find((each) => each.id === id);
      if (!task) return tasks;
      const rest = tasks.filter((each) => each.id !== id);
      const at = before ? rest.findIndex((each) => each.id === before) : -1;
      rest.splice(at < 0 ? rest.length : at, 0, task);
      return rest;
    });
  }

  function step(id, by) {
    const todo = board.tasks.filter((task) => columnOf(task) === "todo");
    const at = todo.findIndex((task) => task.id === id);
    const to = at + by;
    if (at < 0 || to < 0 || to >= todo.length) return;
    move(id, by < 0 ? todo[to].id : todo[to + 1]?.id || null);
    queue.then(() => document.querySelector(`.task-card[data-task="${CSS.escape(id)}"]`)?.focus());
  }

  function edit(id, text) {
    editing = null;
    if (!text.trim()) { redraw(); return; }
    change((tasks) => tasks.map((task) => (task.id === id ? { ...task, prompt: text } : task)));
  }

  // The form a task is written in: made once and kept, so a redraw of the
  // columns under it never takes the words or the caret from the hand.
  function makeForm() {
    const node = el("form", "task-new");
    node.setAttribute("aria-label", t("New task"));
    const prompt = el("textarea", "control task-prompt");
    prompt.rows = 2;
    prompt.placeholder = t("What should the agent do? Ctrl+Enter adds it to To do");
    prompt.setAttribute("aria-label", t("Task"));
    const folder = el("input", "control task-folder");
    folder.type = "text";
    folder.setAttribute("list", "task-folders");
    folder.setAttribute("aria-label", t("Folder"));
    folder.spellcheck = false;
    const folders = el("datalist");
    folders.id = "task-folders";
    const wt = el("label", "task-wt");
    const worktree = el("input");
    worktree.type = "checkbox";
    wt.append(worktree, el("span", "", t("Own worktree")));
    wt.title = t("Start it in a git worktree of its own, on a branch of its own");
    const add = el("button", "solid", t("Add to To do"));
    add.type = "submit";
    const row = el("div", "task-new-row");
    row.append(folder, folders, wt, add);
    const said = el("p", "task-said");
    said.setAttribute("role", "status");
    node.append(prompt, row, said);
    node.addEventListener("submit", (event) => { event.preventDefault(); addTask(); });
    // A refusal is about the folder it was said of: another folder clears it.
    folder.addEventListener("input", () => { said.textContent = ""; });
    prompt.addEventListener("keydown", (event) => {
      if (event.key === "Enter" && (event.ctrlKey || event.metaKey)) { event.preventDefault(); addTask(); }
    });
    return { node, prompt, folder, folders, wt, worktree, add, said };
  }

  function card(task, column) {
    const li = el("li", "task-card");
    li.dataset.task = task.id;
    li.dataset.col = column;
    const entry = task.session_id ? state.sessionsById.get(task.session_id) : null;
    const where = [folderName(task.folder) || t("the gateway's folder"), task.branch || (task.worktree ? t("own worktree") : "")].filter(Boolean).join(" · ");
    if (column === "todo") {
      li.tabIndex = 0;
      li.draggable = editing !== task.id;
      li.title = [task.prompt, home(task.folder)].filter(Boolean).join("\n\n");
      if (editing === task.id) {
        const box = el("textarea", "control task-edit");
        box.value = task.prompt;
        box.rows = 3;
        box.setAttribute("aria-label", t("Task"));
        box.addEventListener("keydown", (event) => {
          if (event.key === "Escape") { event.preventDefault(); editing = null; redraw(); }
          if (event.key === "Enter" && (event.ctrlKey || event.metaKey)) { event.preventDefault(); edit(task.id, box.value); }
        });
        box.addEventListener("blur", () => { if (editing === task.id) edit(task.id, box.value); });
        li.append(box);
        queueMicrotask(() => { box.focus(); box.setSelectionRange(box.value.length, box.value.length); });
      } else {
        const title = el("p", "task-title", firstLine(task.prompt));
        title.addEventListener("dblclick", () => { editing = task.id; redraw(); });
        li.append(title);
      }
      li.append(el("p", "task-meta", where));
      const acts = el("div", "task-acts");
      const go = el("button", "quiet", starting.has(task.id) ? t("Starting…") : t("Start"));
      go.type = "button";
      go.dataset.act = "start";
      go.disabled = starting.has(task.id) || !state.open;
      go.addEventListener("click", () => startTask(task.id));
      const reword = el("button", "word", t("Edit"));
      reword.type = "button";
      reword.dataset.act = "edit";
      reword.addEventListener("click", () => { editing = task.id; redraw(); });
      const drop = el("button", "word", t("Delete"));
      drop.type = "button";
      drop.dataset.act = "delete";
      drop.addEventListener("click", () => remove(task.id));
      const more = el("span", "task-more");
      more.append(reword, drop);
      acts.append(go, more);
      li.append(acts);
      li.addEventListener("keydown", (event) => {
        if (event.target !== li || !event.altKey) return;
        if (event.key === "ArrowUp") { event.preventDefault(); step(task.id, -1); }
        if (event.key === "ArrowDown") { event.preventDefault(); step(task.id, 1); }
      });
      li.addEventListener("dragstart", (event) => {
        dragging = task.id;
        event.dataTransfer.setData(DRAG_TYPE, task.id);
        event.dataTransfer.effectAllowed = "move";
        li.classList.add("lifted");
      });
      li.addEventListener("dragend", () => {
        dragging = null;
        document.querySelectorAll(".drop-here").forEach((node) => node.classList.remove("drop-here"));
        redraw();
      });
      li.addEventListener("dragover", (event) => {
        if (!event.dataTransfer.types.includes(DRAG_TYPE)) return;
        event.preventDefault();
        event.stopPropagation();
        li.classList.add("drop-here");
      });
      li.addEventListener("dragleave", () => li.classList.remove("drop-here"));
      li.addEventListener("drop", (event) => {
        const id = event.dataTransfer.getData(DRAG_TYPE);
        if (!id) return;
        event.preventDefault();
        event.stopPropagation();
        move(id, task.id);
      });
      return li;
    }
    const go = el("button", "task-open");
    go.type = "button";
    go.dataset.session = task.session_id;
    go.dataset.act = "open";
    go.title = [task.prompt, home(task.folder), task.session_id].filter(Boolean).join("\n\n");
    go.append(ball(column));
    const words = el("span", "task-words");
    words.append(el("span", "task-title", firstLine(task.prompt)));
    const meta = el("span", "task-meta", where);
    if (column === "run") meta.append(el("span", "ses-now", ` · ${t("answering now")}`));
    else if (column === "ask" && entry) meta.append(el("span", "ses-asks", ` · ${waitsLine(entry.waits)}`));
    else if (column === "done" && !entry && !starting.has(task.id)) meta.append(el("span", "", ` · ${t("its session is gone")}`));
    words.append(meta);
    go.append(words, el("span", "reading", column === "run" ? t("now") : since(entry?.updated_at || task.started_at)));
    go.addEventListener("click", () => open(task.session_id));
    li.append(go);
    if (column === "done") {
      const clear = el("button", "quiet task-clear", t("Clear"));
      clear.type = "button";
      clear.dataset.act = "clear";
      clear.title = t("Take it off the board; its session stays in Sessions");
      clear.addEventListener("click", () => remove(task.id));
      li.append(clear);
    }
    return li;
  }

  function column(id, word, tasks, ready) {
    const section = el("section", "board-col");
    section.dataset.col = id;
    section.setAttribute("aria-label", word);
    const head = el("div", "board-col-head");
    head.append(el("h3", "micro", word), el("span", "n", ready ? String(tasks.length) : ""));
    if (id === "done" && tasks.length) {
      const clear = el("button", "word", t("Clear all"));
      clear.type = "button";
      clear.dataset.act = "clear-all";
      clear.addEventListener("click", clearDone);
      head.append(clear);
    }
    const list = el("ol", "board-cards");
    list.append(...tasks.map((task) => card(task, id)));
    if (!tasks.length && ready) {
      list.append(el("li", "board-empty", id === "todo" ? t("Nothing queued") : id === "run" ? t("Drop a task here to start it") : t("None")));
    }
    section.append(head, list);
    // A task dropped on Running starts; on To do's own space, it goes last.
    const accepts = id === "run" || id === "todo";
    if (accepts) {
      section.addEventListener("dragover", (event) => {
        if (!event.dataTransfer.types.includes(DRAG_TYPE)) return;
        event.preventDefault();
        section.classList.add("drop-here");
      });
      section.addEventListener("dragleave", (event) => {
        if (!section.contains(event.relatedTarget)) section.classList.remove("drop-here");
      });
      section.addEventListener("drop", (event) => {
        const task = event.dataTransfer.getData(DRAG_TYPE);
        section.classList.remove("drop-here");
        if (!task) return;
        event.preventDefault();
        if (id === "run") startTask(task);
        else move(task, null);
      });
    }
    return section;
  }

  function sentence(counts) {
    const total = Object.values(counts).reduce((a, b) => a + b, 0);
    if (!total) return t("Nothing on the board yet");
    const parts = [];
    if (counts.todo) parts.push(t("{n} to do", { n: counts.todo }));
    if (counts.run) parts.push(t("{n} running", { n: counts.run }));
    if (counts.ask) parts.push(plural(counts.ask, t("{n} needs you"), t("{n} need you")));
    if (counts.done) parts.push(t("{n} done", { n: counts.done }));
    return parts.join(", ");
  }

  // What the columns show, in one string: a redraw that would show the same
  // is not made, so the activity frames a turn sends leave the hand alone.
  function signature() {
    return JSON.stringify([state.open, board.read, state.listed, board.error, board.version, [...starting],
      board.tasks.map((task) => {
        const entry = task.session_id ? state.sessionsById.get(task.session_id) : null;
        return [task.id, columnOf(task), entry?.updated_at, waitsLine(entry?.waits)];
      })]);
  }
  let drawn = "";

  function draw() {
    drawn = signature();
    // The card or button the hand was on is the one it is on after the redraw.
    const was = document.activeElement?.closest?.(".board [data-task]");
    if (was) {
      const task = was.dataset.task;
      const act = document.activeElement.dataset.act || "";
      queueMicrotask(() => {
        const card = document.querySelector(`.board [data-task="${CSS.escape(task)}"]`);
        const target = (act && card?.querySelector(`[data-act="${act}"]`)) || card;
        target?.focus();
      });
    }
    const root = el("div", "board");
    if (!form) form = makeForm();
    const folders = knownFolders().slice(0, 12);
    form.folders.replaceChildren(...folders.map((dir) => Object.assign(el("option"), { value: dir })));
    if (!form.folder.value && !form.folder.matches(":focus")) form.folder.value = state.folder || folders[0] || "";
    form.folder.placeholder = t("Folder: the gateway's own when empty");
    form.wt.hidden = !worktrees();
    // Where a started task stands is its session's: until the list of
    // sessions is read, no card is placed, so none shows in a column it is
    // not in.
    const ready = board.read && state.listed;
    const grouped = Object.fromEntries(COLUMNS.map(([id]) => [id, []]));
    if (ready) for (const task of board.tasks) grouped[columnOf(task)].push(task);
    const counts = Object.fromEntries(COLUMNS.map(([id]) => [id, grouped[id].length]));
    root.append(headline(t("Tasks"), ready ? sentence(counts) : t("Reading the board…"),
      board.error ? t("The board couldn't be read: {why}", { why: board.error })
        : t("A task waits in To do until you start it. Started, it follows its session: running, waiting on you, done.")));
    root.append(form.node);
    const cols = el("div", "board-cols");
    cols.append(...COLUMNS.map(([id, word]) => column(id, word, grouped[id], ready)));
    root.append(cols);
    return root;
  }

  return {
    show: load,
    draw,
    // The columns follow the sessions: redrawn when the list or what runs
    // moved, never while a card is held in the hand.
    sessionsMoved() { settle(); if (!dragging && !editing && signature() !== drawn) redraw(); },
    counts() {
      if (!board.read || !state.listed) return null;
      const counts = { todo: 0, run: 0, ask: 0, done: 0 };
      for (const task of board.tasks) counts[columnOf(task)] += 1;
      return counts;
    },
    load,
  };
}
