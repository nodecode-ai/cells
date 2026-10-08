#!/usr/bin/env python3
"""A scripted language server for the nodecode-lsp tests, over stdio.

SPDX-License-Identifier: MIT

It speaks just enough LSP for the cell's tests, and a few methods of its own
(fake/...) that let a test see the wire from the server's side. Flags:

  --utf8          choose the utf-8 position encoding when offered
  --pull          serve textDocument/diagnostic and never publish
  --no-version    publish diagnostics without a version
  --stale         publish the previous text's diagnostics first, then the
                  current ones 0.3 s later
  --silent        never publish diagnostics
  --delay MS      publish this long after a change
  --progress MS   report a $/progress run this long after initialized
  --status MS     report experimental/serverStatus busy, then quiescent this
                  long after initialized
  --empty-while-loading
                  answer pulls with nothing until the progress run or the
                  status says it has loaded (rust-analyzer's way)
  --fail-init     print to stderr and exit 2 instead of answering initialize
  --save          ask for didSave with the text

A document's diagnostics: one error per line holding ERROR, one warning per
line holding WARN, at the word's column, in the negotiated units. A
definition is the first line `def NAME' for the word under the position.
"""

import json
import sys
import threading
import time

args = sys.argv[1:]
flag = lambda name: name in args
value = lambda name, default: int(args[args.index(name) + 1]) if name in args else default

stdin = sys.stdin.buffer
stdout = sys.stdout.buffer
write_lock = threading.Lock()
encoding = "utf-16"
documents = {}          # uri -> (version, text)
previous = {}           # uri -> the text before the last change
cancelled = []
saves = []              # the text each didSave carried
answers = {}            # our request id -> the client's answer
last_error = None
next_id = [1000]
loading = ["--progress" in args or "--status" in args]


def send(message, split=False, junk=False, raw=None):
    body = raw if raw is not None else json.dumps(message, ensure_ascii=False).encode("utf-8")
    header = b"Content-Length: %d\r\n\r\n" % len(body)
    with write_lock:
        if junk:
            stdout.write(b"a wrapper printed this\r\n\r\n")
        if split:
            stdout.write(header[:7])
            stdout.flush()
            time.sleep(0.05)
            stdout.write(header[7:] + body[:5])
            stdout.flush()
            time.sleep(0.05)
            stdout.write(body[5:])
        else:
            stdout.write(header + body)
        stdout.flush()


def read():
    length = None
    while True:
        line = stdin.readline()
        if not line:
            return None
        line = line.strip()
        if not line:
            if length is not None:
                break
            continue
        name, _, rest = line.partition(b":")
        if name.strip().lower() == b"content-length":
            length = int(rest.strip())
    return json.loads(stdin.read(length).decode("utf-8"))


def respond(id, result=None, error=None):
    message = {"jsonrpc": "2.0", "id": id}
    if error is not None:
        message["error"] = error
    else:
        message["result"] = result
    send(message)


def ask(method, params, id=None):
    """Send the client a request and wait for its answer."""
    if id is None:
        next_id[0] += 1
        id = next_id[0]
    event = threading.Event()
    answers[id] = event
    send({"jsonrpc": "2.0", "id": id, "method": method, "params": params})
    return id, event


def units(text):
    """The length of TEXT in the negotiated units."""
    if encoding == "utf-8":
        return len(text.encode("utf-8"))
    if encoding == "utf-16":
        return len(text.encode("utf-16-le")) // 2
    return len(text)


def index_at(line, character):
    """The code point index of CHARACTER units into LINE."""
    count = 0
    for i, ch in enumerate(line):
        if count >= character:
            return i
        count += units(ch)
    return len(line)


def diagnostics(text):
    found = []
    for number, line in enumerate(text.split("\n")):
        for word, severity in (("ERROR", 1), ("WARN", 2)):
            at = line.find(word)
            if at >= 0:
                start = units(line[:at])
                found.append({"range": {"start": {"line": number, "character": start},
                                        "end": {"line": number, "character": start + units(word)}},
                              "severity": severity, "source": "fake", "code": "F%d" % severity,
                              "message": "found %s\nsecond line of the message" % word})
    return found


def publish(uri, version, text):
    if flag("--silent") or flag("--pull"):
        return
    def go():
        time.sleep(value("--delay", 0) / 1000)
        if flag("--stale") and uri in previous:
            send({"jsonrpc": "2.0", "method": "textDocument/publishDiagnostics",
                  "params": {"uri": uri, "version": version - 1, "diagnostics": diagnostics(previous[uri])}})
            time.sleep(0.3)
        params = {"uri": uri, "diagnostics": diagnostics(text)}
        if not flag("--no-version"):
            params["version"] = version
        send({"jsonrpc": "2.0", "method": "textDocument/publishDiagnostics", "params": params})
    threading.Thread(target=go, daemon=True).start()


def word_at(uri, position):
    text = documents[uri][1]
    line = text.split("\n")[position["line"]]
    i = index_at(line, position["character"])
    start = i
    while start > 0 and (line[start - 1].isalnum() or line[start - 1] == "_"):
        start -= 1
    end = i
    while end < len(line) and (line[end].isalnum() or line[end] == "_"):
        end += 1
    return line[start:end]


def occurrences(word):
    for uri, (_, text) in documents.items():
        for number, line in enumerate(text.split("\n")):
            at = 0
            while True:
                at = line.find(word, at)
                if at < 0:
                    break
                before = line[at - 1] if at > 0 else " "
                after = line[at + len(word)] if at + len(word) < len(line) else " "
                if not (before.isalnum() or before == "_" or after.isalnum() or after == "_"):
                    start = units(line[:at])
                    yield uri, {"start": {"line": number, "character": start},
                                "end": {"line": number, "character": start + units(word)}}
                at += len(word)


def definitions(word=None, query=None):
    for uri, (_, text) in documents.items():
        for number, line in enumerate(text.split("\n")):
            if line.startswith("def "):
                name = line[4:].split("(")[0].strip()
                if (word is not None and name == word) or (query is not None and query.lower() in name.lower()):
                    yield uri, name, {"start": {"line": number, "character": 4},
                                      "end": {"line": number, "character": 4 + units(name)}}


def handle(message):
    global encoding, last_error
    method = message.get("method")
    id = message.get("id")
    params = message.get("params") or {}
    if method is None:
        if id in answers:
            event = answers.pop(id)
            answers[("answer", id)] = message
            if "error" in message:
                last_error = message["error"]
            event.set()
        return
    if method == "initialize":
        if flag("--fail-init"):
            sys.stderr.write("boom: the fake server will not start\n")
            sys.stderr.flush()
            sys.exit(2)
        offered = params.get("capabilities", {}).get("general", {}).get("positionEncodings", [])
        encoding = "utf-8" if flag("--utf8") and "utf-8" in offered else "utf-16"
        sync = {"openClose": True, "change": 1}
        if flag("--save"):
            sync["save"] = {"includeText": True}
        capabilities = {"positionEncoding": encoding, "textDocumentSync": sync,
                        "definitionProvider": True, "referencesProvider": True, "hoverProvider": True,
                        "documentSymbolProvider": True, "workspaceSymbolProvider": True,
                        "renameProvider": True}
        if flag("--pull"):
            capabilities["diagnosticProvider"] = {"interFileDependencies": False, "workspaceDiagnostics": False}
        respond(id, {"capabilities": capabilities, "serverInfo": {"name": "fake"},
                     "echo": {"processId": params.get("processId"), "rootUri": params.get("rootUri"),
                              "options": params.get("initializationOptions")}})
    elif method == "initialized":
        if "--status" in args:
            def status():
                send({"jsonrpc": "2.0", "method": "experimental/serverStatus",
                      "params": {"health": "ok", "quiescent": False}})
                time.sleep(value("--status", 0) / 1000)
                loading[0] = False
                send({"jsonrpc": "2.0", "method": "experimental/serverStatus",
                      "params": {"health": "ok", "quiescent": True}})
            threading.Thread(target=status, daemon=True).start()
        if "--progress" in args:
            def progress():
                _, event = ask("window/workDoneProgress/create", {"token": "load"})
                event.wait(5)
                send({"jsonrpc": "2.0", "method": "$/progress",
                      "params": {"token": "load", "value": {"kind": "begin", "title": "indexing"}}})
                time.sleep(value("--progress", 0) / 1000)
                loading[0] = False
                send({"jsonrpc": "2.0", "method": "$/progress",
                      "params": {"token": "load", "value": {"kind": "end"}}})
            threading.Thread(target=progress, daemon=True).start()
    elif method == "textDocument/didOpen":
        doc = params["textDocument"]
        documents[doc["uri"]] = (doc["version"], doc["text"])
        publish(doc["uri"], doc["version"], doc["text"])
    elif method == "textDocument/didChange":
        doc = params["textDocument"]
        previous[doc["uri"]] = documents.get(doc["uri"], (0, ""))[1]
        documents[doc["uri"]] = (doc["version"], params["contentChanges"][-1]["text"])
        publish(doc["uri"], doc["version"], documents[doc["uri"]][1])
    elif method == "textDocument/didSave":
        saves.append(params.get("text"))
    elif method == "textDocument/didClose":
        documents.pop(params["textDocument"]["uri"], None)
    elif method == "textDocument/diagnostic":
        empty = flag("--empty-while-loading") and loading[0]
        respond(id, {"kind": "full",
                     "items": [] if empty else diagnostics(documents[params["textDocument"]["uri"]][1])})
    elif method == "textDocument/definition":
        word = word_at(params["textDocument"]["uri"], params["position"])
        respond(id, [{"uri": uri, "range": rng} for uri, _, rng in definitions(word=word)] or None)
    elif method == "textDocument/references":
        word = word_at(params["textDocument"]["uri"], params["position"])
        respond(id, [{"uri": uri, "range": rng} for uri, rng in occurrences(word)])
    elif method == "textDocument/hover":
        word = word_at(params["textDocument"]["uri"], params["position"])
        respond(id, {"contents": {"kind": "markdown", "value": "word `%s`" % word}} if word else None)
    elif method == "textDocument/documentSymbol":
        uri = params["textDocument"]["uri"]
        respond(id, [{"name": name, "kind": 12, "range": rng, "selectionRange": rng, "children": []}
                     for u, name, rng in definitions(query="") if u == uri])
    elif method == "workspace/symbol":
        respond(id, [{"name": name, "kind": 12, "location": {"uri": uri, "range": rng}}
                     for uri, name, rng in definitions(query=params["query"])])
    elif method == "textDocument/rename":
        word = word_at(params["textDocument"]["uri"], params["position"])
        changes = {}
        for uri, rng in occurrences(word):
            changes.setdefault(uri, []).append({"range": rng, "newText": params["newName"]})
        if "--resource-op" in args:
            respond(id, {"documentChanges": [{"kind": "create", "uri": "file:///tmp/new.fk"}]})
        else:
            respond(id, {"documentChanges": [{"textDocument": {"uri": uri, "version": None}, "edits": edits}
                                             for uri, edits in changes.items()]})
    elif method == "shutdown":
        respond(id, None)
    elif method == "exit":
        sys.exit(0)
    elif method == "$/cancelRequest":
        cancelled.append(params["id"])
    elif method == "fake/echo":
        send({"jsonrpc": "2.0", "id": id, "result": params}, split="split" in params, junk="junk" in params)
    elif method == "fake/badbytes":
        send(None, raw=b'{"jsonrpc":"2.0","id":%d,"result":"a\xffb"}' % id)
    elif method == "fake/never":
        pass
    elif method == "fake/cancelled":
        respond(id, cancelled)
    elif method == "fake/collide":
        # A request of the server's own, under the very id the client is
        # waiting on: the client must answer it, then still read our reply.
        def go():
            _, event = ask("workspace/configuration", {"items": [{"section": "fake"}, {"section": "fake.depth"},
                                                                 {"section": "absent"}]}, id=id)
            event.wait(5)
            answer = answers.get(("answer", id), {})
            _, unknown = ask("fake/unknown", {})
            unknown.wait(5)
            respond(id, {"configuration": answer.get("result"), "error": last_error})
        threading.Thread(target=go, daemon=True).start()
    elif method == "fake/edit":
        def go():
            asked, event = ask("workspace/applyEdit", {"edit": params})
            event.wait(5)
            respond(id, answers.get(("answer", asked), {}).get("result"))
        threading.Thread(target=go, daemon=True).start()
    elif method == "fake/documents":
        respond(id, {"documents": {uri: list(doc) for uri, doc in documents.items()}, "saves": saves})
    elif method == "fake/crash":
        sys.exit(3)
    elif id is not None:
        respond(id, error={"code": -32601, "message": "unknown method %s" % method})


sys.stderr.write("fake server started\n")
sys.stderr.flush()
while True:
    message = read()
    if message is None:
        break
    handle(message)
