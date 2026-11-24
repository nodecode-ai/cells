# nodecode-lfg-alarm

LFG Alarm: a real alarm on your phone, and on your friends' phones, ringing together. A Nodecode rewrite of [lfg-gm/lfg-alarm-mcp](https://github.com/lfg-gm/lfg-alarm-mcp) by [lfg-gm](https://github.com/lfg-gm).

The original is one `mcp.json` entry pointing at the hosted MCP server `https://mcp.lfg.gm` and one skill that teaches the model to use it. This cell makes the same connection from the image: it lists the server's tools at start and registers every one of them as a Nodecode tool, so the model gets the server's own names, descriptions and argument schemas — `set_time_alarm`, `set_event_alarm`, `set_post_alarm`, `set_price_alarm`, `ring_alarm` and the rest of what the account's plan offers — and each call is forwarded unchanged. The skill's guidance is the cell's manual, `(help :lfg-alarm)`.

## Get it

Tell the agent "add the lfg-alarm cell", or run `nodecode add nc://lfg-alarm`.

## Set it up

Alarms ring in the LFG Alarm app on the phone, so the person needs it from [lfg.gm](https://lfg.gm), signed in. The cell then needs one thing: **a personal key**, made in the LFG Alarm app under **Settings**. Name the variable that holds it, or the file:

```jsonc
"lfg-alarm": {
  "token_env": "LFG_ALARM_KEY"      // or: "token_file": "~/.config/lfg-alarm-key"
}
```

A key is named, never written into the config: `token_env` holds the name of the environment variable that carries it (export `LFG_ALARM_KEY` in the shell that starts Nodecode), and `token_file` a file only you can read. Set exactly one.`

The other members, all optional:

| Member | Default | What it does |
| --- | --- | --- |
| `url` | `https://mcp.lfg.gm` | the MCP server |
| `timeout` | `60` | one request's ceiling, in seconds |
| `max_output` | `6000` | how much of a tool's answer one result carries |

The original's README walks a person through Hermes's OAuth flow for the same server (`hermes mcp add ... --auth oauth`), which needs a browser and a callback route. A personal key reaches the same account and the same tools, so this cell takes the key; the OAuth flow, its dynamic client registration and its token refresh are left out.

## Use it

Ask in words — "wake me at 7 for my flight", "ring me when this creator goes live on Twitch", "make an alarm for Saturday at 9 called Golf and add my golf group" — and the model picks the tool. `/lfg-alarm` prints what the server offers and reconnects; `/lfg-alarm set_time_alarm` says what one tool does. From eval, the same calls:

```lisp
(lfg-alarm:remote-tools)                                  ; what the server listed
(lfg-alarm:call-remote-tool "list_my_alarms" (nlk:json-object))
```

`(help :lfg-alarm)` is the model's manual: which tool to reach for, the time zone, the reach (private, friends, public), confirming people before they are added, and handing back the alarm's link, share picture and message.

### What it keeps, and what it sends

Every call goes to LFG Alarm's server as the person, and what is asked for travels with it: alarm titles, times, notes, links, and the people added. The answers come back into the session, so they reach the model provider too. Reading and writing both go to the same host; nothing is sent anywhere else. The tools can create, change and delete alarms and ring every phone on one, as the account's plan allows.

### What the rewrite leaves out

Hermes's own OAuth sign-in (the key replaces it), and the plugin's `trust: untrusted` default, which is a Hermes gate on write-capable tools: Nodecode asks for a tool call's approval by its own means. The tool list still comes from the server at every start, as it did in Hermes, so the server can add tools without a new version of this cell.

## License

MIT. The tools, their names and their descriptions are LFG Alarm's, served by LFG Alarm's own MCP server: see `NOTICE` and `LICENSE`.
