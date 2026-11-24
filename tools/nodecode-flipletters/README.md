# nodecode-flipletters

FlipLetters: split-flap and flip-dot board videos, designed in flipcode and
rendered to MP4, with team and social sharing. A Nodecode rewrite of
[Terahertz-Inc/flipletters-mcp](https://github.com/Terahertz-Inc/flipletters-mcp)
by [Terahertz-Inc](https://github.com/Terahertz-Inc).

The model gets FlipLetters' own thirteen tools — read the flipcode cheat
sheet, take an example board, validate a draft, build a Studio link, render,
poll, and share to a team or through Postiz — plus `flipletters_tools`, which
lists what the live server offers. `/flipletters` is the operator's way in,
and `(help :flipletters)` is the model's page.

## Get it

Tell the agent "add the flipletters cell", or run `nodecode add nc://flipletters`.

## Set it up

Every call on the hosted server needs a FlipLetters key: open
[Studio](https://app.flipletters.com) → **Account → API keys & agents** and
create one (it starts with `flk_`; creating one is free). Put it in the
environment:

```sh
echo 'FLIPLETTERS_API_KEY=flk_…' >> ~/.nodecode/.env
```

or write it in the cell's section, which `/settings` writes:

```jsonc
{
  "flipletters": {
    "api_key_env": "FLIPLETTERS_API_KEY",   // or "api_key" written inline
    "url": "https://app.flipletters.com/mcp/v1",
    "timeout_seconds": 60
  }
}
```

A cell with no key anywhere refuses at start, and the notice names the
variable; fix it and `restart-cells` tries again.

| Setting | Default | What it does |
| --- | --- | --- |
| `api_key` / `api_key_env` | none (`FLIPLETTERS_API_KEY`) | the `flk_` key, inline or by environment variable |
| `url` | `https://app.flipletters.com/mcp/v1` | the server's Streamable HTTP endpoint |
| `timeout_seconds` | `60` | one exchange's ceiling; a render polls inside it |

## Use it

Ask for a board — *"make a trainboard that flips through NOW BOARDING, GATE 12,
ON TIME"* — and the model reads the reference, drafts, validates until the
warnings are gone, and gives you the Studio share link. It renders, shares or
posts only when you ask.

| Tool | What it does |
| --- | --- |
| `flipletters_flipcode_reference` | the flipcode cheat sheet: directives, board types, size limits, transitions |
| `flipletters_example_board {template}` | a complete valid board of one type, to edit rather than write |
| `flipletters_validate_board {code}` | a draft parsed: valid or not, warnings, normalized code, summary, share link |
| `flipletters_template_url {board?, grid?, title?, accent?, out?, sound?, speed?, slides?, flip?, hold?}` | a readable Studio URL, one query parameter per flipcode line |
| `flipletters_create_animation {text, template?}` | text to a rendered MP4 in one call |
| `flipletters_render_board {code, repeat?, wait?}` | a cloud render of a flipcode document |
| `flipletters_render_status {jobId}` | one render job polled, and its MP4 URL once done |
| `flipletters_list_teams` / `flipletters_share_to_team {teamId, name, code}` | the teams the key belongs to, and a board into a team library |
| `flipletters_list_workspaces` | the key owner's agency projects |
| `flipletters_list_postiz_destinations {workspaceId}` | a project's Postiz destinations |
| `flipletters_list_postiz_integrations {workspaceId, destinationId}` | the social channels on one destination |
| `flipletters_share_to_postiz {workspaceId, destinationId, integrationId, date, type?, content?, mediaUrl?, fileName?, providerType?}` | schedule, draft or post the MP4 through your own connected channels |
| `flipletters_tools {refresh?}` | what the server offers this key, with the arguments each requires |

`/flipletters` says where the cell points and whether it has a key;
`/flipletters tools` lists the server's tools; `/flipletters refresh` asks
again; `/flipletters reference` prints the cheat sheet.

Two things cost money or publish, so the primer says to do them only when
asked: a render spends the key owner's export quota (free renders carry a
watermark), and a Postiz share goes out to the accounts they connected.
`create_animation` is not idempotent — every call starts a new render.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the package, named after the folder |
| `wire.lisp` | MCP over Streamable HTTP: the session, tools/list, tools/call, and the answers' text |
| `cell.lisp` | the section, the fourteen tools, `/flipletters` and `(help :flipletters)` |
| `test/cell-test.lisp` | lifecycle, the handshake, the tools' arguments and answers, the refusals — every request a stubbed `nlk:http` |
| `config.example.jsonc` | the section, commented |

## Why the tools are the server's

The original is a manifest and a skill over a hosted server, so its tool set
and their schemas live at the endpoint, not in the repository. The cell keeps
them there: each tool passes its documented arguments through, a JSON-RPC
error or a bare status comes back as `ERROR:` with the server's words, and
`flipletters_tools` answers the live list when a name is refused. The local
`npx` stdio variant the README mentions is not bundled — the cell dials the
hosted endpoint directly and installs nothing.

## Test

From the cells repository's root, offline:

```sh
.github/test-cell.sh tools/nodecode-flipletters
```

## License

MIT. Copyright (c) 2026 Terahertz, Inc.: see `LICENSE` and `NOTICE`.
