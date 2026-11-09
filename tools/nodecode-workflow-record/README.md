# nodecode-workflow-record

Workflow Record: records a browser workflow you demonstrate once over CDP, then turns it into a replayable skill. A Nodecode rewrite of [NousResearch/hermes-labs-plugins](https://github.com/NousResearch/hermes-labs-plugins) by [NousResearch](https://github.com/NousResearch).

## Get it

Tell the agent "add the workflow-record cell", or run `nodecode add nc://workflow-record`.

## Set it up

Start the Chromium you want to record in with its debugging port open:

```sh
chromium --remote-debugging-port=9222
```

That port is the default. To record in a browser at another address, set it once in `~/.nodecode/config.jsonc`:

```jsonc
{
  "workflow-record": {
    "cdp_url": "http://127.0.0.1:9222",
    "dir": "~/.nodecode/recordings",
    "max_steps": 500
  }
}
```

No key, no account: the only traffic is to that endpoint. `dir` and `max_steps` may be left out.

## Use it

Tell the agent what to record, or type the command:

- `/record start checkout` — recording begins in the first page the browser lists. Do the workflow in that tab: click, type, press Enter, move between pages. Password, one-time-code and card fields are recorded as `{SECRET:name}`, never their value.
- `/record stop` — the steps are sorted by the page's own clock, rebased so the first is 0.0 seconds, and saved to `recordings/<slug>-<stamp>.json`. The answer names the path and the count.
- `/record list`, `/record show [NAME]` — what is on disk, and one recording's JSON.
- Bare `/record` shows what is recording and a panel to start, stop or list.

The model has the same verbs as one tool, `record`, with `action` `start`, `stop`, `list`, `show` or `step` (and a `note` for a line of narration mid-recording). After a recording exists, ask for it: "turn the checkout recording into a skill". The model reads the JSON, drops the noise, maps each `{SECRET:name}` to a stored secret or a named environment variable, and writes a skill that navigates, then finds each element by role and visible text — using the recorded selector only as a hint.

What a recording holds: `click` (the selector and the element's own words), `input` (the value on *change*, not per keystroke), `enter`, `navigate` (the top frame) and `manual`. One tab at a time.

## What the rewrite keeps, and what it changes

The recording model is the original's: the same step types, the same `{SECRET:name}` placeholders, the same `<home>/recordings/<slug>-<stamp>.json` file, the same "one tab, values on change, top frame only" limits. In Nodecode it is reached through a tool and a slash command instead of a `record` subcommand, and the skill it wrote for Hermes's `/learn` is the recipe in `(help :record)`, because a Nodecode skill is written by the model that reads the recording. It attaches to the page target `/json/list` names rather than to the browser target with `Target.attachToTarget`; both see the same clicks, and the page target needs fewer commands.

## License

MIT, Copyright (c) 2026 Nous Research: see `LICENSE` and `NOTICE`.
