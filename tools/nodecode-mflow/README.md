# nodecode-mflow

mFlow: boards, tasks, Knowledge Base and coding agent. A Nodecode rewrite of
[vladyslavpatrick/mflow-hermes](https://github.com/vladyslavpatrick/mflow-hermes)
by [vladyslavpatrick](https://github.com/vladyslavpatrick).

The model gets ten tools that talk to mFlow's own hosted MCP server, one call
each: `mflow_projects`, `mflow_tasks`, `mflow_create_tasks`,
`mflow_update_task`, `mflow_comment`, `mflow_docs`, `mflow_update_doc`,
`mflow_delegate`, `mflow_agent_status` and `mflow_call` for anything the
ten do not cover. `(help :mflow)` is the model's page, `/mflow QUERY` asks
mFlow across the boards, and `/mflow` alone says where the cell points.

## Get it

Tell the agent "add the mflow cell", or run `nodecode add nc://mflow`.

## Set it up

An mFlow Personal Access Token is required. Put it in the environment, and
name the variable in the cell's section:

```sh
echo 'MFLOW_TOKEN=…' >> ~/.nodecode/.env
```

```jsonc
{
  "mflow": {
    "token_env": "MFLOW_TOKEN",              // or "token" written inline
    "endpoint": "https://mcp.m-flow.io/mcp",  // or your own proxy
    "timeout_seconds": 120                     // one call's ceiling
  }
}
```

| Setting | Default | What it does |
| --- | --- | --- |
| `token` / `token_env` | none (one is required) | the mFlow Personal Access Token, inline or by environment variable |
| `endpoint` | `https://mcp.m-flow.io/mcp` | mFlow's MCP endpoint |
| `timeout_seconds` | `120` | one call's ceiling, in seconds |

## Use it

- **`mflow_projects`** — `{project?}`. Every project the token can see, or one
  by its id, with its statuses, labels and custom columns. Most other tools
  want a project id, so this is the first call.
- **`mflow_tasks`** — `{project, key?, status?, limit?}`. One task by its
  readable key (`MVPB-123`), or a project's tasks, filtered by status.
- **`mflow_create_tasks`** — `{project, tasks}`. Up to 50 tasks in one
  all-or-nothing call, each task an object of mFlow's own fields, nested with
  `localRef` and `parentRef`.
- **`mflow_update_task`** — `{task, fields}`. A task's fields, as mFlow names
  them; `labelIds` is a full replace.
- **`mflow_comment`** — `{task, content}`. A comment on one task.
- **`mflow_docs`** — `{project?, doc_type?}`. The Knowledge Base, global when
  no project is named; `convention`, `workflow`, `metric`, `guide` or `adr`.
- **`mflow_update_doc`** — `{project, doc?, title, content, doc_type?,
  adr_status?, superseded_by?}`. Update a doc rather than duplicate it.
- **`mflow_delegate`** — `{task, tier?, repo_url?, no_repo_output?,
  extra_prompt?, ai_provider?}`. Hand a task to mFlow's coding agent, which
  answers at once; `light` is 2GB and 1 CPU, `mid` the default, `heavy` 7GB
  and 4 CPUs.
- **`mflow_agent_status`** — `{task}`. Where that delegation stands.
- **`mflow_call`** — `{tool, arguments?}`. Any mFlow tool by its own name,
  arguments as written: `delete_*`, `transfer_knowledge_base`, `ask_mflow`.
- **`/mflow QUERY`** — mFlow's own semantic search, for the operator. With no
  argument it says where the cell points and how long a call may take.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the section, the JSON-RPC client, the ten tools, `/mflow` and `(help :mflow)` |
| `test/cell-test.lisp` | lifecycle, the exchange, the tools and the refusals, every request a stubbed `nlk:http` |
| `config.example.jsonc` | the section, commented |

## What the original did, and what this does

The plugin was a pointer and a skill: an `mcp.json` naming mFlow's hosted MCP
server behind one bearer token, and a skill teaching the model the server's
tools, with the pitfalls written out: ids not names, `labelIds` a full
replace, list before creating a doc, an ADR's three sections, supersede
rather than delete, confirm `delete_*`, poll the coding agent and never in a
tight loop. That skill is `(help :mflow)` here, and the tools it taught are
the cell's own, so nothing has to be configured as an MCP server first.

The client is this cell's: a streamable-HTTP JSON-RPC `initialize` once, the
`Mcp-Session-Id` echoed after it, one `tools/call` per verb, a JSON or
`data:` framed answer read by its reply's id, and a session the server forgot
taken once more. A token refused is a refusal that names the variable to put a
fresh one in.

The plugin's OAuth sign-in (authorization code with PKCE, no refresh grant)
was left out: it needs a browser callback this cell does not open, so the
token, the plugin's own headless option, is the one way in. The plugin's
`MFLOW_TOKEN` becomes the cell's `token_env`.

## Test

From the cells repository's root, offline:

```sh
.github/test-cell.sh tools/nodecode-mflow
```

## License

MIT. Copyright (c) 2025 mFlow: see `LICENSE` and `NOTICE`.
