#!/usr/bin/env python3
"""A stdio MCP server for the nodecode-mcp tests.

Newline-delimited JSON-RPC on stdin/stdout, nothing but the standard
library, exits on stdin EOF so a killed runner leaves no orphan. A port of
the fixture the retired Zig client tested against, with the behaviours the
Lisp client's contract needs to see:

  tools: echo (message*, per_page, includeSnapshot), fail (isError),
         late (ms: reply after a delay), explode (a non-JSON line first),
         die (exit 3 mid-call), roots (asks roots/list first), ping_me
         (asks ping first), changed (sends tools/list_changed, then lists
         echo2), image (every non-text block kind), empty (no content)
  flags: --slow-init (initialize sleeps 5 s), --pages (tools/list in two
         pages), --many N (N generated tools), --stderr-noise, --linger
         (outlives stdin EOF and SIGTERM, as an npx or uvx server can),
         --sign-in (signs in on its own, as mcp-remote does: every request
         is held until a browser comes back to its loopback listener, the
         address to open printed on stderr in mcp-remote's words, a fresh
         state per request held; sign_out forgets it again)
  more tools: sign_out (listed with --sign-in), chatty (listed with --chatty:
         prints an authorization address on stderr, answers 0.3 s later)
"""
import json
import sys
import threading
import time
import urllib.parse
import uuid
from http.server import BaseHTTPRequestHandler, HTTPServer

ARGS = sys.argv[1:]
SLOW_INIT = "--slow-init" in ARGS
PAGES = "--pages" in ARGS
MANY = int(ARGS[ARGS.index("--many") + 1]) if "--many" in ARGS else 0
if "--stderr-noise" in ARGS:
    sys.stderr.write("fixture: hello from stderr\n")
    sys.stderr.flush()

changed = False


def send(obj):
    sys.stdout.write(json.dumps(obj) + "\n")
    sys.stdout.flush()


def read():
    line = sys.stdin.readline()
    if not line:
        return None
    line = line.strip()
    if not line:
        return read()
    return json.loads(line)


def reply(req, result):
    send({"jsonrpc": "2.0", "id": req["id"], "result": result})


def error(req, code, message):
    send({"jsonrpc": "2.0", "id": req["id"], "error": {"code": code, "message": message}})


def text(s):
    return {"type": "text", "text": s}


TOOLS = [
    {
        "name": "echo",
        "description": "Echo a message back, with the arguments as JSON.",
        "inputSchema": {
            "type": "object",
            "properties": {
                "message": {"type": "string", "description": "What to echo"},
                "per_page": {"type": "integer"},
                "includeSnapshot": {"type": "boolean"},
            },
            "required": ["message"],
        },
    },
    {"name": "fail", "description": "Always answers isError.",
     "inputSchema": {"type": "object", "properties": {}}},
    {"name": "late", "description": "Replies after ms milliseconds.",
     "inputSchema": {"type": "object", "properties": {"ms": {"type": "integer"}}}},
    {"name": "explode", "description": "Writes a non-JSON line before its reply.",
     "inputSchema": {"type": "object", "properties": {}}},
    {"name": "die", "description": "Exits without replying.",
     "inputSchema": {"type": "object", "properties": {}}},
    {"name": "roots", "description": "Asks the client for roots/list first.",
     "inputSchema": {"type": "object", "properties": {}}},
    {"name": "ping_me", "description": "Pings the client first.",
     "inputSchema": {"type": "object", "properties": {}}},
    {"name": "changed", "description": "Announces a catalog change.",
     "inputSchema": {"type": "object", "properties": {}}},
    {"name": "image", "description": "Every non-text content kind.",
     "inputSchema": {"type": "object", "properties": {}}},
    {"name": "empty", "description": "No content, only structuredContent.",
     "inputSchema": {"type": "object", "properties": {}}},
    {"name": "sign_out", "description": "Forgets the sign-in: the next request waits on a browser.",
     "inputSchema": {"type": "object", "properties": {}}},
    {"name": "chatty", "description": "Prints an authorization address on stderr, answers anyway.",
     "inputSchema": {"type": "object", "properties": {}}},
]

# --sign-in: mcp-remote's way of signing in. The loopback listener takes the
# browser's answer; a request is held until it has come back.
SIGN_IN = "--sign-in" in ARGS
CHATTY = "--chatty" in ARGS
signed_in = threading.Event()
flow = {"state": None, "port": None}
if not SIGN_IN:
    signed_in.set()


class Callback(BaseHTTPRequestHandler):
    def do_GET(self):
        url = urllib.parse.urlparse(self.path)
        query = urllib.parse.parse_qs(url.query)
        ok = (url.path == "/oauth/callback" and "code" in query
              and query.get("state", [None])[0] == flow["state"])
        body = b"Authorization successful" if ok else b"Authorization failed"
        self.send_response(200 if ok else 400)
        self.send_header("Content-Type", "text/plain")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)
        if ok:
            signed_in.set()

    def log_message(self, *args):
        pass


if SIGN_IN:
    listener = HTTPServer(("127.0.0.1", 0), Callback)
    flow["port"] = listener.server_address[1]
    threading.Thread(target=listener.serve_forever, daemon=True).start()


def authorization_address(port, state):
    redirect = "http://localhost:%d/oauth/callback" % port
    return ("https://auth.invalid/authorize?response_type=code&client_id=fixture"
            "&code_challenge=x&code_challenge_method=S256&redirect_uri=%s&state=%s&scope=mcp"
            % (urllib.parse.quote(redirect, safe=""), state))


def hold_for_sign_in():
    """mcp-remote's wait: the address on stderr, then nothing until the browser answers."""
    if signed_in.is_set():
        return
    flow["state"] = uuid.uuid4().hex
    sys.stderr.write("[fixture] Connecting to remote server: https://auth.invalid/mcp\n")
    sys.stderr.write("\nPlease authorize this client by visiting:\n%s\n\n"
                     % authorization_address(flow["port"], flow["state"]))
    sys.stderr.write("[fixture] Authentication required. Waiting for authorization...\n")
    sys.stderr.flush()
    signed_in.wait()


def catalog():
    tools = [tool for tool in TOOLS
             if (tool["name"] != "sign_out" or SIGN_IN) and (tool["name"] != "chatty" or CHATTY)]
    if changed:
        tools.append({"name": "echo2", "description": "The tool that appeared.",
                      "inputSchema": {"type": "object", "properties": {}}})
    for n in range(MANY):
        tools.append({"name": "gen_%d" % n, "description": "Generated tool number %d" % n,
                      "inputSchema": {"type": "object",
                                      "properties": {"value": {"type": "string"}}}})
    return tools


def handle_call(req):
    params = req.get("params") or {}
    name = params.get("name")
    args = params.get("arguments") or {}
    global changed
    if name == "echo":
        reply(req, {"content": [text("echo: %s" % args.get("message")),
                                text(json.dumps(args, sort_keys=True))]})
    elif name == "fail":
        reply(req, {"isError": True, "content": [text("echo refused: fail requested")]})
    elif name == "late":
        time.sleep(float(args.get("ms", 0)) / 1000.0)
        reply(req, {"content": [text("late reply")]})
    elif name == "explode":
        sys.stdout.write("this line is not json\n")
        sys.stdout.flush()
        reply(req, {"content": [text("exploded and survived")]})
    elif name == "die":
        sys.stderr.write("fixture: dying on request\n")
        sys.stderr.flush()
        sys.exit(3)
    elif name == "roots":
        send({"jsonrpc": "2.0", "id": "srv-roots", "method": "roots/list"})
        answer = read()
        code = (answer or {}).get("error", {}).get("code")
        reply(req, {"content": [text("roots answered: %s" % code)]})
    elif name == "ping_me":
        send({"jsonrpc": "2.0", "id": "srv-ping", "method": "ping"})
        answer = read()
        reply(req, {"content": [text("ping answered: %s" % json.dumps((answer or {}).get("result")))]})
    elif name == "changed":
        changed = True
        send({"jsonrpc": "2.0", "method": "notifications/tools/list_changed"})
        reply(req, {"content": [text("changed")]})
    elif name == "image":
        reply(req, {"content": [
            text("before"),
            {"type": "image", "data": "AAAABBBB", "mimeType": "image/png"},
            {"type": "audio", "data": "CCCC", "mimeType": "audio/wav"},
            {"type": "resource_link", "uri": "file:///tmp/x.txt", "name": "x"},
            {"type": "resource", "resource": {"uri": "file:///tmp/y.txt", "mimeType": "text/plain", "text": "y body"}},
            {"type": "resource", "resource": {"uri": "file:///tmp/z.bin", "mimeType": "application/octet-stream", "blob": "ZZZZZZ"}},
            {"type": "mystery"},
        ]})
    elif name == "empty":
        reply(req, {"content": [], "structuredContent": {"answer": 42}})
    elif name == "sign_out":
        signed_in.clear()
        reply(req, {"content": [text("signed out")]})
    elif name == "chatty":
        sys.stderr.write("fixture: to use the dashboard too, visit %s\n"
                         % authorization_address(1, "chatty"))
        sys.stderr.flush()
        time.sleep(0.3)
        reply(req, {"content": [text("chatty answered")]})
    else:
        error(req, -32602, "unknown tool %r" % name)


def handle(req):
    method = req.get("method")
    if "id" not in req:
        return  # notifications: initialized, cancelled, ...
    if method != "ping":
        hold_for_sign_in()
    if method == "initialize":
        if SLOW_INIT:
            time.sleep(5)
        reply(req, {"protocolVersion": (req.get("params") or {}).get("protocolVersion", "2025-06-18"),
                    "capabilities": {"tools": {"listChanged": True}},
                    "serverInfo": {"name": "fixture", "version": "0.0.0"}})
    elif method == "tools/list":
        tools = catalog()
        cursor = (req.get("params") or {}).get("cursor")
        if PAGES:
            half = max(1, len(tools) // 2)
            if cursor == "page2":
                reply(req, {"tools": tools[half:]})
            else:
                reply(req, {"tools": tools[:half], "nextCursor": "page2"})
        else:
            reply(req, {"tools": tools})
    elif method == "tools/call":
        handle_call(req)
    elif method == "ping":
        reply(req, {})
    else:
        error(req, -32601, "method not found: %s" % method)


while True:
    try:
        req = read()
    except ValueError:
        continue
    if req is None:
        break
    handle(req)

if "--linger" in ARGS:
    import signal
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    time.sleep(30)
