// mcp.js -- the Control pane's MCP tab: the MCP cell's servers (/api/mcp),
// each with a Test that connects and lists what it offers, an Add form that
// can Test a server before it is saved, and the cell's catalog of
// well-known servers (/api/mcp/catalog), whose Add fills the form and asks
// only for the keys that server needs. A test runs in the cell on its own
// client: a configured server on the connection the agent uses, a server not
// yet added on a connection of its own, closed before the answer comes back.

import { el, button, mark, waiting, plural, ms, headline, dot } from "./observe.js";
import { block, note, form, field, confirmBox, about } from "./control.js";
import { icon } from "./icons.js";
import { t } from "./i18n.js";

const STATES = {
  ready: t("Connected"), connecting: t("Connecting"), disconnected: t("Not connected yet"), error: t("Failed"),
  refused: t("Refused by its entry in config.jsonc"), disabled: t("Turned off in config.jsonc (enabled: false)"), stopped: t("Stopped"),
};

// A catalog server as a line an engineer reads: what runs, or where it is reached.
function how(entry) {
  return entry.url || [entry.command, ...(entry.args || [])].join(" ");
}

export function makeMcp({ c, api, act, toast, redraw, reload }) {
  // OPEN: the servers whose tools are folded out; FIND: what the find holds;
  // LOGS: the end of a server's log, by name, while its Log is open.
  const m = { list: null, catalog: null, tests: {}, draft: {}, from: null, tried: null, said: null, open: new Set(), find: "", logs: {} };

  async function load() {
    const reads = [api("GET", "/api/mcp").then((data) => { m.list = data; },
      (error) => { m.list = { error: error.message, missing: error.status === 404 }; })];
    // The catalog is a file: read once, and again after an add or a remove moves its marks.
    if (!m.catalog || m.catalog.error) {
      reads.push(api("GET", "/api/mcp/catalog").then((data) => { m.catalog = data; },
        (error) => { m.catalog = { error: error.message }; }));
    }
    await Promise.all(reads);
  }

  // A server added or removed starts the cell again, which reconnects every
  // server: the view is read again once they have had a moment.
  function change(label, body, done) {
    act(label, async () => {
      try {
        m.list = await api("POST", "/api/mcp", body);
        m.said = null;
        done?.();
        toast(m.list.text);
        if (body.op !== "restart") m.catalog = null;
        setTimeout(reload, 2500);
      } catch (error) {
        if (body.op !== "add") throw error;
        m.said = error.message;
      }
    });
  }

  function test(label, body, keep) {
    act(label, async () => {
      try {
        const answer = await api("POST", "/api/mcp", { op: "test", ...body });
        m.list = answer;
        keep(answer.test);
      } catch (error) {
        keep({ ok: false, error: error.message });
      }
    });
  }

  // --- the Add form ------------------------------------------------------------------

  // The request the form's fields make: a URL with the headers it sends, or a
  // command with its arguments and the environment it gets; a key left empty
  // is left out.
  function entryOf(values) {
    const target = values.target.trim();
    const body = { name: values.name.trim() };
    const keys = (kind) => (m.from?.[kind] || []).map((k) => [k, (values[`${kind}:${k.name}`] || "").trim()]).filter(([, v]) => v);
    if (/^https?:\/\//.test(target)) {
      body.url = target;
      const headers = Object.fromEntries(keys("headers").map(([k, v]) => [k.name, `${k.prefix || ""}${v}`]));
      if (Object.keys(headers).length) body.headers = headers;
    } else {
      body.command = target;
      body.args = (values.args || "").trim().split(/\s+/).filter(Boolean);
      const env = Object.fromEntries(keys("env").map(([k, v]) => [k.name, v]));
      if (Object.keys(env).length) body.env = env;
    }
    return body;
  }

  function keyField(kind, k) {
    const wrap = field(`${kind}:${k.name}`, k.optional ? t("{ask} (optional)", { ask: k.ask }) : k.ask, {
      type: "password", autocomplete: "off", spellcheck: "false", placeholder: k.name,
      ...(k.optional ? {} : { required: "" }),
    });
    wrap.title = kind === "env" ? t("The environment variable {name}", { name: k.name })
      : k.prefix ? t("The {name} header, sent as {prefix}…", { name: k.name, prefix: k.prefix }) : t("The {name} header", { name: k.name });
    return wrap;
  }

  function addForm() {
    const box = block(t("Add a server"), m.from ? button("word", t("Clear the form"), { act: "mcp-clear" }) : "");
    if (m.from) {
      const from = el("p", "ctl-mcp-from");
      from.append(el("span", "ctl-mcp-from-name", m.from.name), ` ${m.from.what}.`);
      if (m.from.needs) from.append(el("span", "tag", ` ${t("Needs {needs} on this machine.", { needs: m.from.needs })}`));
      box.append(from);
    }
    const f = form("mcp-add",
      field("name", t("Name"), { required: "", autocomplete: "off", spellcheck: "false", placeholder: "files", pattern: "[A-Za-z0-9_\\-]+", title: t("Letters, digits, _ and -") }),
      field("target", t("Command, or a URL"), { required: "", autocomplete: "off", spellcheck: "false", placeholder: t("npx, or https://host/mcp") }));
    // A server reached at a URL from the catalog takes no arguments.
    if (!m.from?.url) f.append(field("args", t("Arguments, split at spaces"), { autocomplete: "off", spellcheck: "false", placeholder: "-y @scope/package" }));
    for (const k of m.from?.env || []) f.append(keyField("env", k));
    for (const k of m.from?.headers || []) f.append(keyField("headers", k));
    for (const input of f.querySelectorAll("input")) input.value = m.draft[input.name] ?? "";
    const tryIt = button("quiet", c.busy === "mcp-try" ? t("Testing…") : t("Test"), { op: "test" });
    const go = button("solid", c.busy === "mcp-add" ? t("Adding…") : t("Add server"), { op: "add" });
    for (const b of [tryIt, go]) { b.type = "submit"; b.disabled = !!c.busy; }
    f.append(tryIt, go);
    box.append(f);
    if (m.said) box.append(el("p", "ctl-said refused", m.said));
    if (m.tried) box.append(result(m.tried));
    const keys = (m.from?.env?.length || 0) + (m.from?.headers?.length || 0);
    box.append(note(keys
      ? t("A command runs on this machine as a child process over stdio; a URL is reached over streamable HTTP. Test tries it on a connection of its own and closes it; nothing is saved until Add server. A key is written into mcp.servers in config.jsonc, where the cell reads it. A server works with this organism's own reach: add one you trust.")
      : t("A command runs on this machine as a child process over stdio; a URL is reached over streamable HTTP. Test tries it on a connection of its own and closes it; nothing is saved until Add server. A server works with this organism's own reach: add one you trust.")));
    return box;
  }

  // --- a test's answer -----------------------------------------------------------------

  // What a test that answered took, in one sentence.
  function took(answer) {
    const took = { tools: plural(answer.tools.length, t("{n} tool"), t("{n} tools")), list: ms(answer.list_ms), connect: ms(answer.connect_ms), server: answer.server };
    return answer.reused
      ? answer.connect_ms != null
        ? t("Listed {tools} in {list} on the connection the agent uses, which connected in {connect}.", took)
        : t("Listed {tools} in {list} on the connection the agent uses.", took)
      : answer.server ? t("Connected in {connect} and listed {tools} in {list}: {server}.", took)
        : t("Connected in {connect} and listed {tools} in {list}.", took);
  }

  function result(answer) {
    const box = el("div", "ctl-mcp-test");
    if (!answer.ok) {
      box.append(el("p", "ctl-said refused", t("It did not answer: {error}", { error: answer.error })));
      if (answer.said) box.append(el("p", "micro", t("What it wrote to its stderr")), el("pre", "ctl-mcp-said", answer.said));
      return box;
    }
    box.append(el("p", "ctl-said", took(answer)));
    if (answer.tools.length) {
      const list = el("dl", "ctl-mcp-tools");
      for (const tool of answer.tools) list.append(el("dt", "", tool.name), el("dd", "", tool.description || t("No description.")));
      box.append(list);
    }
    return box;
  }

  // --- the tab -----------------------------------------------------------------------

  // The page's head: how many servers, the tools the ready ones give, any failing.
  function head(l) {
    const [infoMark, words] = about(c, "mcp", t("Tools from servers that speak the Model Context Protocol (MCP), each set under mcp.servers in config.jsonc; the agent calls them from its eval tool. Test lists what a server offers on the connection the agent uses, and reconnects one that is down. Adding or removing a server starts the MCP cell again, which reconnects every server."));
    const ready = l.servers.filter((s) => s.state === "ready");
    const failing = l.servers.filter((s) => ["error", "refused"].includes(s.state)).length;
    const node = headline(plural(l.servers.length, t("{n} server"), t("{n} servers")),
      !l.enabled ? t("MCP is off: no server starts.")
        : t("{tools} across {servers}.", { tools: plural(ready.reduce((n, s) => n + (s.tools || 0), 0), t("{n} tool"), t("{n} tools")),
          servers: plural(ready.length, t("{n} ready server"), t("{n} ready servers")) }),
      failing ? plural(failing, t("{n} is failing."), t("{n} are failing.")) : "");
    (node.querySelector(".micro") || node).append(" ", infoMark);
    return [node, words];
  }

  // --- the ledger: a line a server ---------------------------------------------------
  // Its columns aligned (the variants artifact's A, 2026-11-02): the name, how many
  // tools, stdio or http, what it runs, how long its connect took, the acts. Its
  // tools fold out under it, what each does in a card while the hand is on it;
  // the find reaches every server's tools at once.

  const failing = (s) => ["error", "refused"].includes(s.state);

  // How a server is reached, for its two columns: stdio or http, and the command
  // it runs or the URL it asks.
  function reach(s) {
    const line = s.transport || "";
    const at = line.indexOf(" ");
    return at < 0 ? [line, ""] : [line.slice(0, at), line.slice(at + 1)];
  }

  // The last error, said once: the log it ends by naming is the row's Log.
  function lastError(s) {
    const see = s.log ? `; see ${s.log}` : null;
    return see && s.error.endsWith(see) ? s.error.slice(0, -see.length) : s.error;
  }

  // A server's tools as its fold shows them: a test's answer, each with its whole
  // description, else the list the route gives, each with its first paragraph.
  function toolsOf(s) {
    const tested = m.tests[s.name];
    if (tested?.ok) return tested.tools.map((tool) => ({ name: tool.name, about: tool.description || "" }));
    return s.tool_list || s.tool_names.map((name) => ({ name, about: "" }));
  }

  function servers(l) {
    const find = el("input", "control ctl-mcp-find");
    Object.assign(find, { type: "search", placeholder: t("Find a tool across servers"), value: m.find });
    find.dataset.input = "mcp-find";
    find.setAttribute("aria-label", find.placeholder);
    const box = block(t("Servers"), ...l.servers.length ? [find] : [], button("quiet", t("Add server"), { act: "mcp-new" }));
    box.classList.add("ctl-mcp-block");
    if (!l.enabled) box.append(Object.assign(el("p", "ctl-notice", t("mcp.enabled is false in config.jsonc: no server starts until it is true.")), { title: "mcp.enabled" }));
    const rows = el("ol", "ctl-list ctl-mcp");
    if (l.servers.length) {
      const cols = el("li", "ctl-mcp-cols");
      cols.setAttribute("aria-hidden", "true");
      cols.append(...[t("Server"), t("Tools"), t("Via"), t("Runs"), t("Connect"), ""].map((word) => el("span", "micro", word)));
      rows.append(cols);
    }
    for (const s of l.servers) serverLines(rows, s);
    if (!l.servers.length) rows.append(el("li", "obs-note", t("No server is set. Add one below, or pick one from the catalog.")));
    rows.append(Object.assign(el("li", "obs-note ctl-mcp-none"), { hidden: true }));
    box.append(rows);
    sift(box);
    return box;
  }

  // One server: its line, then what stands under it -- its state or last error,
  // its log, a test that did not answer, its tools, the question Remove asks.
  function serverLines(rows, s) {
    const add = (li) => { li.dataset.server = s.name; rows.append(li); return li; };
    const ready = s.state === "ready";
    const row = add(el("li", "ctl-row ctl-mcp-row"));
    row.dataset.level = ready ? "started" : failing(s) ? "error" : "off";
    const toggle = button("ctl-name ctl-mcp-toggle", "", { act: "mcp-fold", name: s.name });
    const slot = el("span", "ctl-mcp-mark");
    slot.append(dot(ready ? "" : failing(s) ? "fail" : "off"));
    toggle.append(icon("chevron"), slot, el("span", "ctl-mcp-name", s.name));
    toggle.title = STATES[s.state] || s.state;
    const [via, runs] = reach(s);
    const target = el("span", "ctl-mcp-runs", runs);
    target.title = runs;
    row.append(toggle, el("span", "ctl-mcp-n", ready ? String(s.tools) : ""), el("span", "ctl-mcp-via", via), target,
      el("span", "ctl-mcp-ms", ready && s.connect_ms != null ? ms(s.connect_ms) : ""), acts(s));
    if (s.error) add(el("li", "ctl-sub refused", failing(s) ? lastError(s) : t("Last error: {error}", { error: lastError(s) })));
    else if (!ready) add(el("li", "ctl-sub", STATES[s.state] || s.state));
    const tail = m.logs[s.name];
    if (tail) {
      const inset = add(el("li", "ctl-inset ctl-mcp-log"));
      if (tail.error) inset.append(el("p", "ctl-said refused", t("The log could not be read: {error}", { error: tail.error })));
      else {
        if (tail.path) inset.append(el("p", "ctl-mcp-path", tail.path));
        inset.append(tail.text ? el("pre", "ctl-mcp-said", tail.text) : note(t("Nothing is written to it yet.")));
      }
    }
    const tested = m.tests[s.name];
    if (tested && !tested.ok) add(el("li", "ctl-inset")).append(result(tested));
    const fold = add(el("li", "ctl-mcp-fold"));
    if (tested?.ok) fold.append(el("p", "ctl-said", took(tested)));
    const grid = el("div", "ctl-mcp-grid");
    for (const tool of toolsOf(s)) {
      const item = el("span", "ctl-mcp-tool");
      item.tabIndex = 0;
      item.dataset.find = `${tool.name} ${tool.about}`.toLowerCase();
      item.append(el("span", "ctl-mcp-tool-name", tool.name));
      if (tool.about) item.append(el("span", "ctl-mcp-about", tool.about));
      grid.append(item);
    }
    fold.append(grid.children.length ? grid : note(ready ? t("It offers no tool.") : t("Its tools show once it is connected.")));
    hoverCard(fold);
    if (c.confirm === `mcp:${s.name}`) {
      add(el("li", "ctl-inset")).append(confirmBox(t("Remove {name}? It leaves mcp.servers in config.jsonc, and the MCP cell starts again without it.", { name: s.name }), t("Remove"), "mcp-remove", { name: s.name }));
    }
  }

  // A row's acts: Test, or Retry and its Log when it failed; Restart; Remove.
  function acts(s) {
    const box = el("span", "ctl-acts");
    const down = failing(s);
    box.append(button("word", c.busy === `mcp-test:${s.name}` ? t("Testing…") : down ? t("Retry") : t("Test"), { act: "mcp-test", name: s.name }),
      down ? mark("lines", m.logs[s.name] ? t("Hide the log") : t("Log"), { act: "mcp-log", name: s.name }, c.busy === `mcp-log:${s.name}` && t("Reading…"))
        : mark("refresh", t("Restart"), { act: "mcp-restart", name: s.name }, c.busy === `mcp:${s.name}` && t("Restarting…")),
      mark("trash", t("Remove"), { act: "mcp-ask", name: s.name }));
    for (const b of box.children) {
      b.disabled = !!c.busy || (b.dataset.act === "mcp-test" && s.state === "refused") || (b.dataset.act === "mcp-restart" && s.state === "disabled");
    }
    return box;
  }

  // What a tool does, in one card under its name while the hand or the focus is
  // on it, kept inside the fold. A phone shows it under each name instead.
  function hoverCard(fold) {
    const card = el("div", "ctl-mcp-card");
    card.hidden = true;
    const show = (event) => {
      const item = event.target.closest?.(".ctl-mcp-tool");
      const words = item?.querySelector(".ctl-mcp-about");
      if (!words) { card.hidden = true; return; }
      card.replaceChildren(el("span", "ctl-mcp-card-name", item.firstChild.textContent), words.textContent);
      card.hidden = false;
      const box = fold.getBoundingClientRect();
      const at = item.getBoundingClientRect();
      card.style.left = `${Math.max(0, Math.min(at.left - box.left, box.width - card.offsetWidth))}px`;
      // Under the name, or over it where under would run past the fold's foot.
      const under = at.bottom - box.top + 4;
      const over = at.top - box.top - card.offsetHeight - 4;
      card.style.top = `${under + card.offsetHeight > box.height && over >= 0 ? over : under}px`;
    };
    const hide = () => { card.hidden = true; };
    fold.addEventListener("pointerover", show);
    fold.addEventListener("focusin", show);
    fold.addEventListener("pointerleave", hide);
    fold.addEventListener("focusout", hide);
    fold.append(card);
  }

  // The find: a server whose name holds what was typed shows whole; another shows
  // those of its tools whose name or words hold it, folded open; a server with
  // neither steps aside with every line under it.
  function sift(scope) {
    const needle = m.find.trim().toLowerCase();
    let shown = 0;
    for (const row of scope.querySelectorAll(".ctl-mcp-row")) {
      const name = row.dataset.server;
      const lines = scope.querySelectorAll(`.ctl-mcp > li[data-server="${CSS.escape(name)}"]`);
      const fold = scope.querySelector(`.ctl-mcp-fold[data-server="${CSS.escape(name)}"]`);
      const whole = !needle || name.toLowerCase().includes(needle);
      let hits = 0;
      for (const tool of fold.querySelectorAll(".ctl-mcp-tool")) {
        tool.hidden = !whole && !tool.dataset.find.includes(needle);
        if (!tool.hidden) hits++;
      }
      const seen = whole || hits > 0;
      for (const li of lines) li.hidden = !seen;
      const open = seen && (m.open.has(name) || (!whole && hits > 0));
      fold.hidden = !open;
      row.querySelector(".ctl-mcp-toggle").setAttribute("aria-expanded", String(open));
      if (seen) shown++;
    }
    const none = scope.querySelector(".ctl-mcp-none");
    if (none) {
      none.hidden = !needle || shown > 0;
      none.textContent = t("No server or tool holds “{find}”.", { find: m.find.trim() });
    }
  }

  function catalog() {
    const k = m.catalog;
    if (!k || k.error) {
      const box = block(t("Catalog"));
      box.append(waiting(k?.error ? t("The catalog could not be read: {error}", { error: k.error }) : t("Reading the catalog…")));
      return box;
    }
    const [infoMark, words] = about(c, "catalog", t("Well-known servers, each checked against its own project's documentation. Add fills the form above with how it runs and asks only for the keys it needs; a server is someone else's code, so read its page first."));
    const box = block(t("Catalog"), infoMark);
    box.append(words);
    const rows = el("ol", "ctl-list");
    for (const entry of k.servers) {
      const li = el("li", "ctl-row");
      li.dataset.level = entry.added ? "started" : "off";
      const name = el("span", "ctl-name");
      name.append(` ${entry.name}`);
      const what = el("span", "ctl-cell-what", entry.what);
      const asks = [...(entry.env || []), ...(entry.headers || [])].filter((key) => !key.optional)
        .map((key) => `${key.ask[0].toLowerCase()}${key.ask.slice(1)}`);
      const tag = el("span", "tag", [how(entry), entry.needs ? t("needs {needs}", { needs: entry.needs }) : "",
        asks.length ? t("asks for {keys}", { keys: asks.join(" and ") }) : t("no key")].filter(Boolean).join(" · "));
      tag.append(" · ", Object.assign(el("a", "", t("its page")), { href: entry.homepage, target: "_blank", rel: "noopener noreferrer" }));
      tag.title = entry.source;
      what.append(tag);
      const acts = el("span", "ctl-acts");
      const add = button("word", entry.added ? t("Added") : t("Add"), { act: "mcp-pick", name: entry.name });
      add.disabled = !!entry.added || !!c.busy;
      acts.append(add);
      li.append(name, what, acts);
      rows.append(li);
    }
    box.append(rows);
    return box;
  }

  function draw() {
    const root = el("div", "ctl-tab");
    const l = m.list;
    if (l?.missing) {
      const box = el("div", "ctl-missing");
      box.append(el("p", "dash-line", t("The MCP cell is not loaded in this organism, so no server runs.")),
        note(t("Turn on MCP under Cells to give the agent tools from MCP servers.")));
      root.append(box);
      return root;
    }
    if (!l || l.error) {
      const line = waiting(l?.error ? t("Could not read the MCP servers: {error}", { error: l.error }) : t("Looking…"));
      if (l?.error) line.append(" ", button("word", t("Retry"), { act: "reread" }));
      root.append(line);
      return root;
    }
    root.append(...head(l), servers(l), addForm(), catalog());
    return root;
  }

  // --- what the pane hands over ------------------------------------------------------

  function click(d) {
    switch (d.act) {
      case "mcp-test": test(`mcp-test:${d.name}`, { name: d.name }, (answer) => { m.tests[d.name] = answer; if (answer.ok) m.open.add(d.name); }); return true;
      case "mcp-fold": if (!m.open.delete(d.name)) m.open.add(d.name); redraw(); return true;
      case "mcp-log": {
        if (m.logs[d.name]) { delete m.logs[d.name]; redraw(); return true; }
        act(`mcp-log:${d.name}`, async () => {
          try {
            const answer = await api("POST", "/api/mcp", { op: "log", name: d.name });
            m.list = answer;
            m.logs[d.name] = answer.tail;
          } catch (error) {
            m.logs[d.name] = { error: error.message };
          }
        });
        return true;
      }
      case "mcp-new": {
        const form = document.querySelector("form[data-act=mcp-add]");
        form?.scrollIntoView({ block: "center" });
        form?.querySelector("input[name=name]")?.focus();
        return true;
      }
      case "mcp-restart": delete m.tests[d.name]; change(`mcp:${d.name}`, { op: "restart", name: d.name }); return true;
      case "mcp-ask": c.confirm = `mcp:${d.name}`; redraw(); return true;
      case "mcp-remove": delete m.tests[d.name]; change(`mcp:${d.name}`, { op: "remove", name: d.name }); return true;
      case "mcp-clear": Object.assign(m, { from: null, draft: {}, tried: null, said: null }); redraw(); return true;
      case "mcp-pick": {
        const entry = m.catalog.servers.find((each) => each.name === d.name);
        Object.assign(m, { from: entry, tried: null, said: null,
          draft: { name: entry.name, target: entry.url || entry.command, args: (entry.args || []).join(" ") } });
        redraw();
        const form = document.querySelector("form[data-act=mcp-add]");
        form?.scrollIntoView({ block: "center" });
        form?.querySelector("input[type=password]")?.focus();
        return true;
      }
    }
    return false;
  }

  // What the hand types is kept, so a poll's redraw never takes it back; a key
  // with the rest, until the server is added or the form cleared.
  function input(event) {
    const node = event.target;
    if (node.dataset?.input === "mcp-find") {
      m.find = node.value;
      const scope = node.closest(".ctl-mcp-block");
      if (scope) sift(scope);
      return true;
    }
    if (!node.closest?.("form[data-act=mcp-add]")) return false;
    m.draft[node.name] = node.value;
    return true;
  }

  function submit(node, values, submitter) {
    if (node.dataset.act !== "mcp-add") return false;
    const body = entryOf(values);
    if (submitter?.dataset.op === "test") test("mcp-try", body, (answer) => { m.tried = answer; m.said = null; });
    else change("mcp-add", { op: "add", ...body }, () => { Object.assign(m, { from: null, draft: {}, tried: null }); });
    return true;
  }

  return { load, draw, click, input, submit };
}
