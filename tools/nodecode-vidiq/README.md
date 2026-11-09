# nodecode-vidiq

vidIQ: live YouTube, Instagram and TikTok creator intelligence, and the
workflows that use it. A Nodecode rewrite of
[vid-io/vidiq-mcp](https://github.com/vid-io/vidiq-mcp) by [vid-io](https://github.com/vid-io).

vidIQ is a creator-intelligence service: channel and video numbers, outliers
beating their own baseline, trends, keyword demand, comments, transcripts, and
titles and thumbnails scored against evidence. This cell gives the model the
service's own tools as verbs it calls through `eval`, plus the operating rules
its twelve creator workflows were built on, and `/vidiq` for the account's
credits and authorized channels. Everything runs in vidIQ's cloud, against the
vidIQ account the token belongs to, and most calls spend that account's credits.

## Get it

Tell the agent "add the vidiq cell", or run `nodecode add nc://vidiq`.

## Set it up

The hosted endpoint is `https://mcp.vidiq.com/mcp` and it wants an OAuth
access token as a bearer header. Authorize vidIQ there in a client that
supports MCP over OAuth — vidIQ's own app in ChatGPT, the Claude.ai
connector, or any client that can sign in to a remote MCP server — or create a
vidIQ API key, then put the token in the cell's section, in `/settings`, on the
settings page, or by asking the agent to set `vidiq.access_token`:

```jsonc
{
  "vidiq": {
    // A vidIQ OAuth access token or API key. VIDIQ_ACCESS_TOKEN is read when
    // this is empty.
    "access_token": "...",
    // The hosted MCP endpoint every call goes to.
    "mcp_url": "https://mcp.vidiq.com/mcp",
    // The ceiling on one call, in seconds.
    "timeout_seconds": 180
  }
}
```

`VIDIQ_ACCESS_TOKEN` is read when the section is empty. Without a token the
verbs refuse and say so; nothing else is needed. A vidIQ account is required,
private YouTube analytics are available only for channels authorized in that
account, and availability of the Instagram and TikTok tools varies by account.

## Use it

The model learns the vocabulary from `(help :vidiq)`; every verb is a Lisp
function in the `vidiq` package, called through `eval`, and every one answers
a string — a refusal is `ERROR: VIDIQ-ERROR` carrying the service's own code
where it sent one.

- `(vidiq:user-channels)` — the channels this account authorized, with their
  ids; where channel-specific work starts. `(vidiq:balance)` — credits left.
- Research: `(vidiq:channel-search "query" :max-results 20)`,
  `(vidiq:channel-videos "channelId")`, `(vidiq:outliers :max-results 25)`,
  `(vidiq:trending-videos)`, `(vidiq:keyword-research "keyword")`,
  `(vidiq:similar-channels "channelId")`.
- Performance: `(vidiq:channel-analytics "channelId" :start-date "2026-01-01")`
  for an authorized channel, `(vidiq:channel-trends "channelId")`,
  `(vidiq:video-stats "videoId")`, `(vidiq:video-comments "videoId" :order "relevance")`,
  `(vidiq:video-transcript "videoId")`, `(vidiq:videos-by-ids '("id" "id"))`.
- Packaging: `(vidiq:generate-titles ...)`, `(vidiq:generate-thumbnail ...)`,
  `(vidiq:score-title ...)`, `(vidiq:score-thumbnail ...)`.
- Asynchronous work: a generation or refinement answers a job id, which
  `(vidiq:job-poll "jobId")` follows; `(vidiq:jobs-list)` finds work
  interrupted before its id was written down.
- Account changes: `(vidiq:update-competitors ...)` changes the competitors
  tracked in the user's vidIQ account — confirm the exact change first.
- Anything else the account lists: `(vidiq:tools)` names them and
  `(vidiq:call-tool "vidiq_some_tool" (nlk:json-object "channelId" "..."))`
  calls one.

A keyword argument is the tool's own field, in camelCase:
`:channel-id` is `channelId`, `:max-results` is `maxResults`, a list is an
array. Unknown fields are sent and may be ignored, so a call that succeeds
does not prove its filters worked.

`/vidiq` reads the balance and the authorized channels; `/vidiq tools` lists
every tool this account can call.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | The `vidiq` package, its condition and its constants. |
| `mcp.lisp` | The hosted MCP: initialize and its session, one JSON-RPC POST per message, JSON or SSE answered, the token as a bearer header. |
| `verbs.lisp` | The vidIQ tools as verbs, keyword arguments being their fields. |
| `cell.lisp` | The section, the manual topic with the twelve workflows' rules, `/vidiq`, `START-CELL`. |
| `config.example.jsonc` | The section, commented. |

## Differences from the plugin

- Hermes registered vidIQ's tools as its own and loaded twelve `SKILL.md`
  workflows beside them; here the tools are verbs taught by `(help :vidiq)`,
  and the workflows' operating rules — evidence, not instructions; missing
  data is unknown; the job lifecycle; credits and approvals — are that topic.
- The plugin leaves connection and sign-in to its host client (`hermes mcp add ...
  --auth oauth`): the cell takes the token that sign-in produced, from the
  section or `VIDIQ_ACCESS_TOKEN`, and hands it to the same endpoint as a bearer
  header.
- The plugin's Cursor rule, specialist agent and per-skill reference files
  became the one manual topic; nothing here is Cursor's or Hermes's to read.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-vidiq
```

Every vidIQ endpoint is a stubbed `nlk:http` and the token a stubbed
credential lookup, so nothing reaches the network.

Apache-2.0 licensed, as the plugin folder was. The service, its tools and their
workflows are vidIQ's: see `NOTICE` and `LICENSE`.
