# nodecode-tool-slimmer

Tool Slimmer: sends the model only the tool schemas a request needs, and reports the tokens it saved. A Nodecode rewrite of [aliasocracy/hermes-tool-slimmer](https://github.com/aliasocracy/hermes-tool-slimmer) by [aliasocracy](https://github.com/aliasocracy).

## Get it

Tell the agent "add the tool-slimmer cell", or run `nodecode add nc://tool-slimmer`.

## Set it up

Nothing is required: a request carries at most eight tool schemas beside the
ones the cell always keeps, and it works that way the moment the folder loads.
The `tool-slimmer` section of `~/.nodecode/config.jsonc`, all optional:

| Member | Default | What it does |
| --- | --- | --- |
| `keep` | the cell's own three tools | tool names every request carries, whatever the operator typed |
| `top_k` | `8` | how many more schemas one request may carry |
| `fail_open` | `true` | when nothing in the request scores, offer every tool rather than none |

## Use it

Every turn: the words the operator typed rank against every registered tool's
name (a name word counts four) and description (one), and the turn's request
carries the pinned names plus the highest scoring ones. The list the model is
shown is the list it can call; nothing else moves.

A request whose words the cell has not seen — a turn resumed at boot, a
channel prompt, a fork — is never trimmed. A turn the model wants more of can
ask for every schema with `tool_slimmer_request_full_tools`, and the very
next request carries all of them, once.

| Verb | What it answers |
| --- | --- |
| `/tool-slimmer` | what the last request carried: schemas kept and dropped, approximate tokens saved, and the section's own values |
| `/tool-slimmer full` | this session's next request carries every tool schema |
| `/tool-slimmer reset` | clears the counters |

The model has three tools of its own: `tool_slimmer_select` ranks a list of
candidate tools against a request and answers the kept names, the scores and
the ones left out; `tool_slimmer_status` is the report above as JSON;
`tool_slimmer_request_full_tools` is the way back to the whole catalog.

Two functions are advised, once each per start: `nle:submit` keeps the
request's own words per session, and `turn-prompt` trims that session's tool
vector against them. Both are taken back by the stop.

Tokens are approximated the way the original did it — the schema's characters,
divided by four — so it is a size to compare, not a bill.
