# nodecode-pethost

Pethost: deploy and look after Docker Compose projects on your own Pethost machine. A Nodecode rewrite of [pethost-dev/plugin](https://github.com/pethost-dev/plugin) by [pethost-dev](https://github.com/pethost-dev).

[Pethost](https://pethost.dev) is hosting for what an agent builds: one rented Linux machine that runs Docker Compose projects at a flat monthly price, each with HTTPS, backups, request logs and a panel. The original plugin is a skill plus Pethost's hosted MCP server. Here the playbook is the manual the model reads, and the server's tools are three verbs in the same session.

| What | Does |
| --- | --- |
| `pethost_machine` | The machine and every project on it, with each address and its problems. The call that comes first. |
| `pethost_call` | One tool of Pethost's MCP server, by name, with its arguments: deploy, logs, traffic, files, restarts, rollbacks. |
| `pethost_tools` | The server's tool catalog, one line a tool, optionally filtered. |
| `/pethost` | What the cell talks to, a ping of the machine, or the tool list. |
| `(help :pethost)` | The playbook: when Pethost is the answer, how a deploy is written, and what not to do to a running site. |

Say "deploy this to Pethost and send me the link" and the model calls `pethost_machine`, writes a compose file if the project has none, uploads it, deploys it and answers with the address.

## Get it

Tell the agent "add the pethost cell", or run `nodecode add nc://pethost`.

## Set it up

Pethost needs a subscription (a machine is set up when you subscribe) and the cell needs an access token for Pethost's MCP server, which signs in by OAuth:

1. Subscribe at [console.pethost.dev](https://console.pethost.dev/).
2. Add the server `https://mcp.pethost.dev/mcp` to any MCP client that can sign in by OAuth (the original plugin's install does it with `hermes mcp add pethost --url https://mcp.pethost.dev/mcp --auth oauth`), and approve it in the browser.
3. Take the access token that sign-in issued and put it where this cell reads it: the `PETHOST_TOKEN` environment variable in the shell that starts Nodecode, or a file named in `token_file`.

```sh
export PETHOST_TOKEN=...
```

Then, in `/settings` (or in the `pethost` section of `config.jsonc`):

```jsonc
"pethost": {
  "token_env": "PETHOST_TOKEN",
  "url": "https://mcp.pethost.dev/mcp",
  "timeout": 120
}
```

| Setting | What it does |
| --- | --- |
| `token_env` | The variable the access token is read from. `PETHOST_TOKEN` when unset. |
| `token_file` | A file whose first line is the token, instead of the variable. |
| `url` | The server's address. Pethost's own unless you run one. |
| `timeout` | How long one request may take, in seconds. A deploy call waits for the deploy, so it is generous. |

A section with no token refuses at start and says which variable it looked for; `/pethost` then reports the same.

## Use it

`/pethost ping` asks the server for the machine, so a wrong token or a dead network shows up before the model needs it, and `/pethost tools` lists what the server can do. The model reaches the same tools by calling `pethost_machine`, `pethost_call` and `pethost_tools`, and reads the playbook at `(help :pethost)`.

From eval, in the `pethost` package:

```lisp
(pethost:machine)
(pethost:call-tool "GetProject" :project_id "recipes")
(pethost:list-tools "deploy")
```

They are the tools' own bodies: the same contract, the same server, the same answer.

### What it does not do

The original is one package for several clients. Its skill's prose is here as the manual, condensed to what a model needs while it works. Its transfers stay shell commands the model writes itself: `CreateTransfer` answers a URL under the machine's own name, and the model runs `tar` and `curl` after checking it, exactly as the playbook says. The sign-in is not done here: the cell reads a token rather than opening a browser, so the OAuth flow stays with the client that ran it. And the original's views in hosts that render MCP apps have no counterpart: the answers are text.
