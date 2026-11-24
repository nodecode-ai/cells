# nodecode-comfyrizz

Comfyrizz: run a film pipeline in chat over your own loopback Comfyrizz engine
- brief, plan, spend card, explicit acceptance, generation, CPU assembly,
export and a single clip's retake. A Nodecode rewrite of
[mojomast/comfyrizz-hermes](https://github.com/mojomast/comfyrizz-hermes) by
[mojomast](https://github.com/mojomast).

The model gets the plugin's twelve tools, the ones the original's MCP server
answered, as Nodecode tools of their own: `comfyrizz_status`,
`comfyrizz_create_project`, `comfyrizz_list_projects`, `comfyrizz_plan`,
`comfyrizz_get_plan`, `comfyrizz_get_spend_card`,
`comfyrizz_accept_generation`, `comfyrizz_job_status`,
`comfyrizz_list_takes`, `comfyrizz_assemble`, `comfyrizz_export` and
`comfyrizz_retake`. Every one of them talks to the Comfyrizz engine on your
own machine, over the routes the original used. `/comfyrizz` is yours: with
no argument it asks whether the engine answers, with a brief it proposes a
free plan. `(help :comfyrizz)` is the model's page, and it carries the
original skill's rules: no silent spend, no self-started Colab, no claim
without the tool result that says so.

## Get it

Tell the agent "add the comfyrizz cell", or run `nodecode add nc://comfyrizz`.

## Set it up

Start the Comfyrizz engine on the machine Nodecode runs on, then point the
cell at it -- its default port is 4319, so the section's default is usually
right:

```jsonc
{
  "comfyrizz": {
    "engine_url": "http://127.0.0.1:4319",   // loopback only: 127.0.0.1, localhost or ::1
    "token_env": "COMFYRIZZ_TOKEN",          // only when the engine asks for a token
    "timeout_seconds": 120,
    "answer_chars": 6000
  }
}
```

`engine_url` is checked at start: a URL with another host, another scheme or
a password in it refuses, with the reason on the cells line and in the
model's notices. The token is read from the variable `token_env` names and
rides both headers the engine already accepts; it never appears in a tool's
answer.

## Use it

Ask for a film. The loop is the original's, and `(help :comfyrizz)` says it
in full:

1. `comfyrizz_status` first -- if the engine is not connected, the tools
   answer `stub: true` with the reason, and nothing is invented.
2. `comfyrizz_list_projects`, then `comfyrizz_create_project` with the
   agreed brief and title.
3. `comfyrizz_plan` and `comfyrizz_get_plan`: planning is free.
4. `comfyrizz_get_spend_card`, shown in chat, before any paid work.
5. Only an unambiguous yes: `comfyrizz_accept_generation` with the
   `approvalId` the spend card carried. Without one it refuses with
   `APPROVAL_REQUIRED`, and no engine call is made.
6. `comfyrizz_job_status` while the shots generate.
7. `comfyrizz_assemble`, then `comfyrizz_export`; the export path is quoted
   exactly as the engine returned it.
8. `comfyrizz_list_takes`, then `comfyrizz_retake` for one clip -- a paid
   retake needs a fresh spend card and a fresh yes.

The engine's own answers come back as JSON, clipped to `answer_chars`; a
route the running engine does not expose yet reads as "engine_route_not_wired"
rather than as an empty success.

Run the tests with `.github/test-cell.sh tools/nodecode-comfyrizz` from the
cells repository: every HTTP call is stubbed, so no engine and no network are
needed.
