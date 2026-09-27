#!/usr/bin/env python3
"""A stand-in for the claude CLI, for the add-on's tests: nothing leaves the machine.

It reads the stream-json frames the add-on writes, acknowledges each shouldQuery:false
user frame with a zero-turn result, and on the frame that asks POSTs the Messages request
a CLI would make to ANTHROPIC_BASE_URL -- the replayed messages, the system prompt file,
the CLAUDE_CODE_EXTRA_BODY from --settings laid over, a cache marker on the last block --
with a CLI's headers. Then it waits to be killed. What it saw (argv, the environment
switches, the frames) is written to FAKE_CLAUDE_RECORD when that names a file.

FAKE_CLAUDE_MODE=logged-out answers the asking frame the way a CLI with no login does.
"""
import json
import os
import sys
import time
import urllib.error
import urllib.request

args = sys.argv[1:]


def flag(name):
    return args[args.index(name) + 1] if name in args else None


def emit(frame):
    sys.stdout.write(json.dumps(frame) + "\n")
    sys.stdout.flush()


def record(frames):
    path = os.environ.get("FAKE_CLAUDE_RECORD")
    if path:
        seen = {k: os.environ.get(k) for k in (
            "ANTHROPIC_BASE_URL", "ANTHROPIC_API_KEY", "CLAUDE_CODE_DISABLE_AUTO_MEMORY",
            "CLAUDE_CODE_DISABLE_CLAUDE_MDS", "CLAUDE_CODE_MAX_RETRIES", "CLAUDE_CODE_ENTRYPOINT",
            "CLAUDE_CODE_MAX_OUTPUT_TOKENS")}
        with open(path, "w") as out:
            json.dump({"argv": args, "env": seen, "cwd": os.getcwd(), "frames": frames}, out)


def request(frames):
    settings = json.load(open(flag("--settings")))
    extra = json.loads(settings["env"]["CLAUDE_CODE_EXTRA_BODY"])
    system = open(flag("--system-prompt-file")).read()
    messages = [dict(f["message"]) for f in frames]
    for m in messages:
        m.pop("model", None)
    marker = {"type": "ephemeral", "ttl": "1h"}
    messages[-1]["content"][-1]["cache_control"] = marker
    body = {"model": flag("--model").replace("[1m]", ""),
            "messages": messages,
            "system": [{"type": "text", "text": "x-anthropic-billing-header: cc_version=0.0.0; cc_entrypoint=sdk-cli;"},
                       {"type": "text", "text": system, "cache_control": marker}],
            "metadata": {"user_id": "fake"},
            "stream": True}
    body.update(extra)
    return json.dumps(body).encode("utf-8")


def main():
    frames = []
    emit({"type": "system", "subtype": "init"})
    base = os.environ["ANTHROPIC_BASE_URL"]
    try:
        urllib.request.urlopen(urllib.request.Request(base + "/api/hello", method="HEAD"), timeout=5)
    except (urllib.error.URLError, OSError):
        pass
    for line in sys.stdin:
        frame = json.loads(line)
        frames.append(frame)
        if frame.get("type") == "user" and frame.get("shouldQuery") is False:
            emit({"type": "result", "subtype": "success", "is_error": False, "num_turns": 0})
    record(frames)
    if os.environ.get("FAKE_CLAUDE_MODE") == "logged-out":
        emit({"type": "assistant", "error": "authentication_failed",
              "message": {"content": [{"type": "text", "text": "Not logged in · Please run /login"}]}})
        emit({"type": "result", "subtype": "success", "is_error": True, "num_turns": 1,
              "result": "Not logged in · Please run /login"})
        sys.exit(1)
    post = urllib.request.Request(base + "/v1/messages?beta=true", data=request(frames), method="POST", headers={
        "Authorization": "Bearer fake-oauth-token",
        "Content-Type": "application/json",
        "User-Agent": "claude-cli/0.0.0-fake (external, sdk-cli)",
        "anthropic-beta": "oauth-2025-04-20,claude-code-20250219",
        "anthropic-version": "2023-06-01",
        "x-app": "cli",
        "Accept-Encoding": "gzip, deflate, br, zstd"})
    try:
        urllib.request.urlopen(post, timeout=10)
    except urllib.error.HTTPError:
        pass
    time.sleep(30)


if __name__ == "__main__":
    main()
