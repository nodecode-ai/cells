#!/usr/bin/env python3
"""A stand-in ACP agent for the acp cell's tests: no model, no network.

What it does is read off the prompt's first word:
  write NAME TEXT   asks permission, then writes NAME in the session folder
  run COMMAND       runs COMMAND (no permission) and shows its output
  plan              sends a plan, then says done
  wait              streams a word, then waits for session/cancel
  count             says how many prompts this session has had (kept in a
                    file, so session/load after a restart continues it)
  crash             exits mid-turn
  look              says how many images the prompt carried
  anything else     says it back
"""
import json, os, subprocess, sys, threading, time, uuid

STATE = os.environ.get("FAKE_ACP_STATE", "/tmp/fake-acp-state.json")
out_lock = threading.Lock()
pending = {}
next_id = [100]
sessions = {}
cancelled = set()


def send(obj):
    with out_lock:
        sys.stdout.write(json.dumps(obj) + "\n")
        sys.stdout.flush()


def update(sid, upd):
    send({"jsonrpc": "2.0", "method": "session/update", "params": {"sessionId": sid, "update": upd}})


def ask(method, params):
    next_id[0] += 1
    rid = next_id[0]
    ev = threading.Event()
    pending[rid] = [ev, None]
    send({"jsonrpc": "2.0", "id": rid, "method": method, "params": params})
    ev.wait()
    return pending.pop(rid)[1]


def load_state():
    try:
        return json.load(open(STATE))
    except Exception:
        return {}


def save_state(state):
    json.dump(state, open(STATE, "w"))


def say(sid, text):
    for i in range(0, len(text), 7):
        update(sid, {"sessionUpdate": "agent_message_chunk", "content": {"type": "text", "text": text[i:i + 7]}})


def prompt(rid, params):
    sid = params["sessionId"]
    cwd = sessions.get(sid, os.getcwd())
    blocks = params.get("prompt", [])
    text = " ".join(b.get("text", "") for b in blocks if b.get("type") == "text").strip()
    images = sum(1 for b in blocks if b.get("type") == "image")
    words = text.split()
    head = words[0] if words else ""
    state = load_state()
    state[sid] = state.get(sid, 0) + 1
    save_state(state)
    stop = "end_turn"
    update(sid, {"sessionUpdate": "agent_thought_chunk", "content": {"type": "text", "text": "Thinking about " + head}})
    if head == "write" and len(words) >= 3:
        name, body = words[1], " ".join(words[2:]) + "\n"
        path = os.path.join(cwd, name)
        existed = os.path.exists(path)
        old = open(path).read() if existed else None
        say(sid, "I will write " + name + ".")
        call = "call-" + uuid.uuid4().hex[:8]
        update(sid, {"sessionUpdate": "tool_call", "toolCallId": call, "title": "Preparing", "kind": "edit",
                     "status": "pending", "rawInput": {}})
        tc = {"toolCallId": call, "title": "Write " + name, "kind": "edit", "status": "pending",
              "rawInput": {"file_path": path, "content": body},
              "content": [{"type": "diff", "path": path, "oldText": old, "newText": body}],
              "locations": [{"path": path}]}
        update(sid, dict(tc, sessionUpdate="tool_call_update"))
        answer = ask("session/request_permission", {"sessionId": sid, "toolCall": tc, "options": [
            {"optionId": "allow-once", "name": "Yes", "kind": "allow_once"},
            {"optionId": "allow-always", "name": "Yes, always", "kind": "allow_always"},
            {"optionId": "reject", "name": "No", "kind": "reject_once"}]})
        outcome = (answer or {}).get("result", {}).get("outcome", {})
        if sid in cancelled:
            stop = "cancelled"
        elif outcome.get("outcome") == "selected" and outcome.get("optionId", "").startswith("allow"):
            open(path, "w").write(body)
            update(sid, {"sessionUpdate": "tool_call_update", "toolCallId": call, "status": "completed",
                         "rawOutput": "wrote " + path,
                         "content": [{"type": "diff", "path": path, "oldText": old, "newText": body}]})
            say(sid, " Wrote it.")
        else:
            update(sid, {"sessionUpdate": "tool_call_update", "toolCallId": call, "status": "failed",
                         "rawOutput": "The user refused (" + json.dumps(outcome) + ")"})
            say(sid, " Not allowed, so nothing changed.")
    elif head == "run":
        command = text[4:]
        call = "call-" + uuid.uuid4().hex[:8]
        update(sid, {"sessionUpdate": "tool_call", "toolCallId": call, "title": "Terminal", "kind": "execute",
                     "status": "pending", "rawInput": {}})
        update(sid, {"sessionUpdate": "tool_call_update", "toolCallId": call, "title": command,
                     "rawInput": {"command": command}, "status": "in_progress"})
        res = subprocess.run(command, shell=True, cwd=cwd, capture_output=True, text=True)
        update(sid, {"sessionUpdate": "tool_call_update", "toolCallId": call,
                     "status": "completed" if res.returncode == 0 else "failed",
                     "content": [{"type": "content", "content": {"type": "text",
                                  "text": "```console\n" + res.stdout + res.stderr + "```"}}]})
        say(sid, "Ran it.")
    elif head == "plan":
        update(sid, {"sessionUpdate": "plan", "entries": [
            {"content": "Read the code", "priority": "high", "status": "completed"},
            {"content": "Change it", "priority": "high", "status": "in_progress"}]})
        say(sid, "Planned.")
    elif head == "wait":
        say(sid, "Working on it")
        for _ in range(600):
            if sid in cancelled:
                break
            time.sleep(0.1)
        stop = "cancelled" if sid in cancelled else "end_turn"
    elif head == "count":
        say(sid, "This is prompt %d of this session." % state[sid])
    elif head == "crash":
        say(sid, "About to crash")
        sys.stderr.write("fake agent: crashed on purpose\n")
        sys.stderr.flush()
        os._exit(3)
    elif head == "look":
        say(sid, "I see %d image(s)." % images)
    else:
        say(sid, "You said: " + text)
    update(sid, {"sessionUpdate": "usage_update", "used": 1200, "size": 200000,
                 "cost": {"amount": 0.0125, "currency": "USD"}})
    cancelled.discard(sid)
    send({"jsonrpc": "2.0", "id": rid, "result": {"stopReason": stop, "usage": {
        "inputTokens": 1000, "outputTokens": 50, "cachedReadTokens": 150, "totalTokens": 1200}}})


def main():
    for line in sys.stdin:
        try:
            msg = json.loads(line)
        except Exception:
            continue
        method, rid = msg.get("method"), msg.get("id")
        if method is None and rid is not None:
            slot = pending.get(rid)
            if slot:
                slot[1] = msg
                slot[0].set()
            continue
        params = msg.get("params") or {}
        if method == "initialize":
            send({"jsonrpc": "2.0", "id": rid, "result": {"protocolVersion": 1, "agentInfo": {"name": "fake"},
                  "agentCapabilities": {"loadSession": True, "promptCapabilities": {"image": True}}, "authMethods": []}})
        elif method == "session/new":
            sid = "fake-" + uuid.uuid4().hex[:10]
            sessions[sid] = params.get("cwd")
            send({"jsonrpc": "2.0", "id": rid, "result": {"sessionId": sid}})
        elif method == "session/load":
            sid = params.get("sessionId")
            if sid in load_state():
                sessions[sid] = params.get("cwd")
                update(sid, {"sessionUpdate": "user_message_chunk", "content": {"type": "text", "text": "(replayed)"}})
                send({"jsonrpc": "2.0", "id": rid, "result": None})
            else:
                send({"jsonrpc": "2.0", "id": rid, "error": {"code": -32002, "message": "no such session"}})
        elif method == "session/set_mode":
            send({"jsonrpc": "2.0", "id": rid, "result": None})
        elif method == "session/prompt":
            threading.Thread(target=prompt, args=(rid, params), daemon=True).start()
        elif method == "session/cancel":
            cancelled.add(params.get("sessionId"))
        elif rid is not None:
            send({"jsonrpc": "2.0", "id": rid, "error": {"code": -32601, "message": "unknown " + str(method)}})


main()
