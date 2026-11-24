# nodecode-twenty-crm

Twenty CRM: its records over REST and GraphQL, and its live schema, as four tools the model calls. A Nodecode rewrite of [TREE-Ind/hermes-twenty-crm](https://github.com/TREE-Ind/hermes-twenty-crm) by [TREE-Ind](https://github.com/TREE-Ind).

## Get it

Tell the agent "add the twenty-crm cell", or run `nodecode add nc://twenty-crm`.

## Set it up

You need a Twenty workspace, and an API key from it: in Twenty, **Settings, Developers, API keys**. Give the cell the key's environment variable and the workspace's base URL, in the `twenty-crm` section of your config:

```jsonc
{
  "twenty-crm": {
    "base_url": "https://api.twenty.com",   // or your own instance, e.g. "http://127.0.0.1:3000"
    "api_key_env": "TWENTY_API_KEY"
  }
}
```

`api_key_file` may name a file holding the key instead. `timeout` (30 seconds) and `max_types` (80) are the other two settings. Ask for the cell to be set up in the conversation, or edit the section in `/settings`.

## Use it

Ask for what you want in the CRM: "which companies did we add this week", "create a note on the Acme opportunity", "add a field to the Company object". The agent reads the schema first, then calls:

| Tool | What it does |
| --- | --- |
| `twenty_describe_workspace` | The base URL, the REST, GraphQL and metadata endpoints, and whether the key is configured. |
| `twenty_rest` | One REST endpoint: `/rest/companies`, `/rest/people`, `/rest/opportunities`, ... with query parameters and a JSON body. |
| `twenty_graphql` | Twenty's GraphQL API: `api=core` for records and relations, `api=metadata` for objects, fields, views, dashboards and workflows. |
| `twenty_schema` | The live schema: the type names, or one type's fields with their exact types. Twenty schemas differ per workspace, so this is how the agent learns yours before writing anything. |

`/twenty status` says which workspace the cell is pointed at and whether it answers; `/twenty doctor` prints the whole connection.

A person can call the same functions from eval, in the `twenty-crm` package.

## What the rewrite leaves out

The original plugin can also run a self-hosted Twenty stack for you: it bootstrapped a Docker Compose project, started and stopped it on session hooks, and read its logs with `/twenty start|stop|logs`. This cell connects to a Twenty that already runs, and leaves the stack to you. Its OAuth client-credentials flow is left out too: the API key is the one credential. See `NOTICE`.

## Test

```sh
.github/test-cell.sh tools/nodecode-twenty-crm
```

The tests stub every HTTP call and the key environment: nothing reaches a workspace.

## License

MIT. The original plugin is Copyright (c) 2026 TREE Industries: see `LICENSE` and `NOTICE`.
