// terminal.js --- one terminal of this machine's image, drawn by xterm.js.
//
// SPDX-License-Identifier: MIT
//
// The page opens a WebSocket to /api/terminal/socket on the address that
// served it, says hello with the operator token (from the link's #t=, then
// kept for this tab), and opens a terminal or attaches to the one this tab
// showed before. Input, resizes and close go up as JSON; the pty's own bytes
// come down as binary frames. The terminal lives in the image: a reload or a
// dropped connection attaches again and redraws from the backlog.
"use strict";

const TOKEN_KEY = "nodecode-web-token";
const TERMINAL_KEY = "nodecode-terminal-id";

function safeGet(store, key) { try { return store.getItem(key); } catch { return null; } }
function safeSet(store, key, value) { try { value == null ? store.removeItem(key) : store.setItem(key, value); } catch {} }

const params = new URLSearchParams(location.hash.slice(1));
if (params.get("t")) {
  safeSet(sessionStorage, TOKEN_KEY, params.get("t"));
  params.delete("t");
  history.replaceState(null, "", location.pathname + location.search + (params.toString() ? "#" + params : ""));
}
const token = safeGet(sessionStorage, TOKEN_KEY);
const wantedCwd = params.get("cwd") || "";
let terminalId = params.get("id") || safeGet(sessionStorage, TERMINAL_KEY);

const status = document.getElementById("status");
function say(text) { status.hidden = !text; status.textContent = text || ""; }

const term = new Terminal({
  cursorBlink: true,
  scrollback: 5000,
  fontFamily: '"JetBrains Mono", "SF Mono", Menlo, Consolas, "DejaVu Sans Mono", monospace',
  fontSize: 13,
  theme: { background: "#16181c", foreground: "#d7dae0", cursor: "#d7dae0", selectionBackground: "#3a4250" },
});
const fit = new FitAddon.FitAddon();
term.loadAddon(fit);
term.open(document.getElementById("screen"));
fit.fit();
window.nodecodeTerminal = { term, get id() { return terminalId; } };

let socket = null;
let exited = false;
let retry = 0;

function send(frame) { if (socket && socket.readyState === WebSocket.OPEN) socket.send(JSON.stringify(frame)); }

function openNew() {
  exited = false;
  send({ type: "open", cols: term.cols, rows: term.rows, cwd: wantedCwd });
}

function connect() {
  if (!token) { say("Open this page from the link nodecode web prints."); return; }
  const scheme = location.protocol === "https:" ? "wss:" : "ws:";
  socket = new WebSocket(`${scheme}//${location.host}/api/terminal/socket`);
  socket.binaryType = "arraybuffer";
  socket.onopen = () => socket.send(JSON.stringify({ type: "hello", token }));
  socket.onmessage = (event) => {
    if (typeof event.data !== "string") { term.write(new Uint8Array(event.data)); return; }
    const frame = JSON.parse(event.data);
    switch (frame.type) {
      case "welcome":
        retry = 0;
        say("");
        if (terminalId) send({ type: "attach", id: terminalId }); else openNew();
        break;
      case "opened":
      case "attached":
        terminalId = frame.id;
        safeSet(sessionStorage, TERMINAL_KEY, terminalId);
        if (frame.type === "opened") term.reset();
        if (frame.cols !== term.cols || frame.rows !== term.rows) send({ type: "resize", cols: term.cols, rows: term.rows });
        document.title = frame.cwd ? frame.cwd.replace(/\/+$/, "").split("/").pop() || "/" : "Terminal";
        term.focus();
        break;
      case "exit":
        exited = true;
        terminalId = null;
        safeSet(sessionStorage, TERMINAL_KEY, null);
        term.write(`\r\n\x1b[2m[exited with code ${frame.code} - press Enter for a new shell]\x1b[0m\r\n`);
        break;
      case "error":
        if (frame.message === "no such terminal") { terminalId = null; safeSet(sessionStorage, TERMINAL_KEY, null); openNew(); }
        else say(frame.message);
        break;
    }
  };
  socket.onclose = () => {
    socket = null;
    if (!token) return;
    retry = Math.min(retry + 1, 6);
    say("Reconnecting...");
    setTimeout(connect, 250 * 2 ** retry);
  };
}

term.onData((data) => {
  if (exited) { if (data === "\r") openNew(); return; }
  send({ type: "input", data });
});
term.onResize(({ cols, rows }) => send({ type: "resize", cols, rows }));
new ResizeObserver(() => fit.fit()).observe(document.getElementById("screen"));
connect();
