# nodecode-super-artifacts

Super Artifacts: publish a page, form or tracker as a live private URL, and read back what visitors submitted. A Nodecode rewrite of [Super-Artifacts/plugins](https://github.com/Super-Artifacts/plugins) by [Super-Artifacts](https://github.com/Super-Artifacts).

The model gets five tools. `super_whoami` names the Super Artifacts account
it would publish as; `super_publish` takes a slug, a title and a whole
standalone HTML page and answers the URL that page opens at; `super_data`
reads back the rows visitors submitted to one artifact's form;
`super_artifacts` lists what is published, or reads one of them; and
`super_tool` reaches every other tool the server offers by its own name --
uploads, templates, comments, totals, shares, `super_help`. `/super-artifacts`
says what is ready and signs a machine in.

It is for what a chat cannot carry: a page someone opens on a phone, a form
that collects answers, a tracker, the proof of a change. Publish again at the
same slug and it is a new version at the same URL, and every earlier one can
be put back. Private unless you say otherwise.

## Get it

Tell the agent "add the super-artifacts cell", or run `nodecode add nc://super-artifacts`.

## Set it up

The `super-artifacts` section of `~/.nodecode/config.jsonc` (or `/settings`)
needs a key, in either of two ways:

- **Sign in.** `/super-artifacts signin` registers this client with the
  product, prints a page and a short code, and polls in the background until
  you approve it. The token it mints is kept at 0600 in
  `<home>/super-artifacts/token.json`, and refreshed there when it ages out.
  `/super-artifacts status` says whether it landed; `/super-artifacts cancel`
  stops waiting.
- **Name a key.** Set `key_env` to the variable holding a key from the
  dashboard's API keys page (by default `SUPER_ARTIFACTS_KEY`), or put the
  key in a file and name it in `key_file`. A key named either way wins over a
  saved sign-in.

`base_url` is the MCP server, `https://superartifacts.app/mcp` unless you
point it at another one; it must be `https`. Nothing is published until the
model publishes something.

## Use it

- "Publish a one-page supper-club reservation form and give me the link."
- "Show me what people have submitted to my reservation page."
- "Add a vegetarian option to my reservation form and publish a new version."

The cell carries the original's rules as `(help :super-artifacts)`, one line
of which rides every request: check which account it publishes as, publish
private, never widen who can open an artifact unless you asked, one stable
slug per subject so every update is a version, and report the rows that are
there. Never publish a secret: a private link is still a link.

## What is in it

| File | What it is |
| --- | --- |
| `mcp.lisp` | The transport: the handshake, the session id, JSON and event-stream answers, the key, and one `tools/call`. |
| `signin.lisp` | The OAuth device sign-in: the registration, the code, the poll, the token at 0600, and `/super-artifacts status`. |
| `cell.lisp` | The five tools' schemas and answers, the section, `/super-artifacts`, and the manual topic. |
| `config.example.jsonc` | The section, commented: documentation, not config. |

## License

MIT, Copyright (c) 2026 Super Artifacts: the original's `LICENSE`, unchanged,
with what was rewritten in `NOTICE`.
