# nodecode-antigravity-agy

Antigravity (agy CLI): your Google Antigravity subscription as a Nodecode provider, answered by the `agy` CLI you are already signed in to. A Nodecode rewrite of [Realtyxxx/hermes-plugin-antigravity-agy](https://github.com/Realtyxxx/hermes-plugin-antigravity-agy) by [Realtyxxx](https://github.com/Realtyxxx).

The cell publishes a catalog row for the provider `antigravity-agy`: `/models`
lists `antigravity-agy/<slug>` for the slugs the section names, and a turn on
one of them is answered by the operator's own `agy`.

A round works like this:

1. Nodecode builds the round the way it does for any OpenAI-compatible
   provider: system prompt, history, tools.
2. The cell's catalog row points the base at a listener on `127.0.0.1`; the
   relay takes that POST and writes agy's own prompt: the whole history as
   one message, the system prompt plus a tool protocol and the tools'
   schemas.
3. `agy` runs once per round in a private scratch workspace, under a custom
   agent with `excludeDefaultComponents: true`, `inheritMcp: false` and no
   tools of its own, so it acts as the model and nothing else.
4. agy's stream-json answer is read back and streamed to Nodecode as chat
   chunks: text, `tool_calls`, finish, usage. A tool use is the one
   `<call name="TOOL">{json}</call>` line the tool protocol asks for, so the
   call runs in Nodecode, never in agy.

Nothing but agy's own traffic leaves the machine, and agy owns its sign-in:
the cell never reads agy's credential files. A round that agy answers with
its default agent, or by falling back to it, is refused rather than run,
because that agent owns built-in tools.

## Get it

Tell the agent "add the antigravity-agy cell", or run:

```sh
nodecode add nc://antigravity-agy
```

## Set it up

Install the Antigravity CLI from <https://antigravity.google/docs/cli/install>,
run `agy` once to sign in, and check it answers:

```sh
agy models
```

The section is on unless it says `"enabled": false`. Nothing in it is a
secret: the login is the CLI's own.

```jsonc
{
  "antigravity-agy": {
    "command": "agy",
    "models": ["gemini-3.8-flash-low", "gemini-3.1-pro-high", "claude-sonnet-4-6"],
    "timeout_seconds": 300
  }
}
```

`command` may be a path, or a command line whose first word is the program.
`models` are the slugs exactly as `agy models` spells them; the default list
is the fallback catalog the original plugin carries.

## Use it

Pick `antigravity-agy/gemini-3.8-flash-low` (or any slug you listed) in
`/models`. Effort is baked into agy's slug, so a round's effort setting
changes nothing; choose the variant instead (`gemini-3.8-flash-high`).

## What it costs in time

agy spends several seconds booting before it reads its prompt, so a round is
slower than a hosted API's. The answer arrives in one piece rather than token
by token: the tool protocol is read out of the whole answer, and a round that
ends in a call must not show the call twice.

## Gaps

- **No pre-warming.** The original keeps up to four booted agy processes for
  the follow-up rounds of a tool loop. This cell starts one per round; a
  pre-warm pool would be the port's next step.
- **A text tool protocol, not MCP.** The original hands tool calls back
  through an MCP server that never answers, so agy makes a native function
  call. Nodecode has no MCP client to stand in that way, so the cell teaches
  agy one `<call>` line instead. A model that writes the line badly answers
  as text rather than calling a tool.
- **No cleanup of agy's conversation records.** The original trashes the
  records its own runs create under agy's data directory; this cell leaves them
  to agy.
- **Text only.** An image in the history is a placeholder, as in the
  original.

## Test

```sh
.github/test-cell.sh providers/nodecode-antigravity-agy
```

The tests start no real `agy` and reach no network: a stand-in CLI speaks the
stream-json agy prints, and a raw loopback client dials the relay.

MIT licensed; the original is Realtyxxx's, see [NOTICE](NOTICE).
