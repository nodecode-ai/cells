# nodecode-acp

A Nodecode cell that runs a session in a coding agent: Claude Code, Codex,
Gemini CLI, or any program that speaks the
[Agent Client Protocol](https://agentclientprotocol.com) on stdio. Pick
`acp/claude-code` (or `acp/codex`, `acp/gemini`, `acp/<your agent>`) in
`/models` or the page's model picker, and each turn of that session is one
prompt to that agent.

The agent keeps the loop: its own tools, its own context, its own login. The
session shows what it does:

- its words as the answer and its thinking (and its plan, as a checklist)
  as the turn's thinking;
- each call it makes as a row, in Nodecode's words where they fit (Read,
  Edited, Ran `$ command`), with the agent's own title otherwise;
- what a call changed in the folder, as the row's diff, the same as a
  Nodecode tool's;
- its permission asks as the page's question card, with the agent's own
  options as the buttons (Allow / Always / Reject). No answer within
  `ask_seconds` is the agent's own "reject once";
- its token counts and, when the agent says so, its cost, on the turn's
  usage line and the Dashboard.

Stop cancels the agent's prompt (`session/cancel`); what it said and did up
to then stays in the session. An agent that dies fails the turn with the
last lines it wrote on stderr, and the next turn starts it again on the same
session. The agent's session survives a restart of Nodecode too: the next
turn loads it (`session/load`) where the agent offers that.

## Install

Run `nodecode add nc://acp`, or copy this folder into
`~/.nodecode/cells/nodecode-acp/` and restart Nodecode.

Each agent signs in with its own CLI, once, before its first turn here:

| Agent | Model | Runs | Sign in |
| --- | --- | --- | --- |
| Claude Code | `acp/claude-code` | `npx -y @agentclientprotocol/claude-agent-acp` | `claude`, then `/login` |
| Codex | `acp/codex` | `npx -y @agentclientprotocol/codex-acp` | `codex login` |
| Gemini CLI | `acp/gemini` | `gemini --experimental-acp` | `gemini`, then sign in |

The two `npx` adapters need Node.js; the first turn fetches the adapter,
which `start_seconds` allows for.

## Configure

Optional. The presets above need no config; a top-level `acp` section adds
an agent, changes a preset or takes one off:

```jsonc
{
  "acp": {
    "agents": {
      "claude-code": { "mode": "acceptEdits" },
      "opencode": { "command": "opencode", "args": ["acp"], "title": "OpenCode" },
      "gemini": { "enabled": false }
    },
    "ask_seconds": 600
  }
}
```

See [`config.example.jsonc`](config.example.jsonc).

## What it does not do

- The agent's tools are its own: Nodecode's tools, MCP servers and context
  engine are not offered to it, and its turns are not compiled by Nodecode.
- The agent reads and writes files itself, under the session's folder;
  Nodecode does not offer it the client file system or terminal (both are
  `false` in `initialize`).
- A title or summary for the session is written by Nodecode's own side-call
  model, never by the agent.

## Test

```sh
.github/test-cell.sh providers/nodecode-acp
```

The tests run a scripted agent, `test/fake-agent.py`, over the real wire and
the real turn loop: words, thinking, a plan, a command, a write asked first
and answered yes or no, a stop, a crash, a restart. No network, no model.
