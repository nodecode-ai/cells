# nodecode-birta

Birta: publish websites, reports and decks at a shareable link, collect on-page feedback and have the agent apply it. A Nodecode rewrite of [birtadev/plugins](https://github.com/birtadev/plugins) by [birtadev](https://github.com/birtadev).

## Get it

Tell the agent "add the birta cell", or run `nodecode add nc://birta`.

## Set it up

Birta is a hosted workspace: a page, site, report or deck is published at
[birta.dev](https://birta.dev), and every tool call acts on the Birta projects
the account can open. The cell talks to Birta's own MCP server,
`https://birta.dev/mcp`, and speaks that protocol itself, so nothing else is
installed.

Birta signs in with OAuth in the browser. Put the bearer token that sign-in
issues for the MCP server in the `birta` section (or in `/settings`, or on the
page's Settings tab), or export it:

```sh
export BIRTA_TOKEN=...
```

```jsonc
{
  "birta": {
    "token": "...",                 // or name the variable holding it
    "token_env": "BIRTA_TOKEN",
    "url": "https://birta.dev/mcp"  // only for a proxy of your own
  }
}
```

A token pasted into the panel is kept in a 0600 file under
`~/.nodecode/secrets/`; the folder holds no secret. The cell reads the token at
start: after a refresh, run `(restart-cells "nodecode-birta")`.

`/birta` with no argument shows the endpoint and whether a token is there.
`(help :birta)` is the model's guide.

## Use it

Ask for what you want; the model picks the tool:

| Tool | Use |
| --- | --- |
| `birta_tools` | what the connected server offers: every tool's name, its one-line description and the arguments it requires |
| `birta_call` | call one of them by name with its arguments object |

The server's own tools are the work: `list_projects` and `create_project`,
`list_versions`, `publish_version` and `rollback`, `list_version_comments` and
`reply_to_thread`, `project_settings`, `set_project_access`, `share_project`,
`change_role` and `revoke_access`, the lead forms (`create_form`, `list_forms`,
`list_leads`), `connect_domain` and `verify_domain`, `stats`, `set_site_mark`.
This cell keeps that surface whole rather than hard-coding a subset, so a tool
Birta adds is callable the day it appears.

From the keyboard:

```
/birta                       the connection
/birta tools                 the tools the server offers (optionally: /birta tools leads)
/birta list_projects {}      one call, its answer
```

### The loop it is for

Publish a version, share the project address, let people leave remarks on the
page, then ask the agent to read them (`list_version_comments`), apply them and
publish again. Threads keep their numbers across versions, and the project
address stays the same across publications and rollbacks.

### What of the original is here

The plugin was an MCP server entry plus sixteen skills. The entry is the two
tools above; the skills are `(help :birta)`: publishing and review, access and
lead forms, and how the page itself should read and look (decks, reports,
proposals, landing pages, portfolios, event pages, specs, guides, interactive
tools, diagrams, charts, typography, writing that sounds like a person).

Left out: the OAuth browser step (Nodecode has no MCP sign-in flow, so the
section carries the token), and the sixteen skill files as separate texts —
they are one manual topic here.

## License

MIT, Copyright (c) 2026 SP Krou Tech: see `LICENSE` and `NOTICE`.
