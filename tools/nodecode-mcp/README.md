# nodecode-mcp

The MCP client. It reads `mcp.servers` from the config, connects each server
(a command over stdio, or a URL over streamable HTTP) and turns every remote
tool into a function in the `mcp:` package, called through `eval`. Each one is
generated from the server's `tools/list` at connect time, named `SERVER/TOOL`
in kebab-case, with keyword arguments from its input schema and the tool's
description as its docstring.

```lisp
(mcp:files/read-file :path "/etc/hosts")   ; one tool
(mcp:call "files" "read_file" :path "x")   ; the same by name; use this inside kept definitions
(mcp:tools)  (mcp:tools "files")           ; every tool and its arguments
(mcp:schema "files" "read_file")           ; a tool's input schema
(mcp:status)                               ; each server's state, tools, transport, last error
(mcp:restart "files")                      ; reconnect one server
```

Keyword arguments fold to the schema's names (`:per-page` is `per_page`).
`t` is true, `:false` false, `nil` absent, a list an array and a plist an
object. A result is the content blocks as text, capped near 7000 characters
(`:limit` changes it). A call waits up to the server's timeout, 30 s by
default (`:timeout` changes it).

`MCP-TOOL-ERROR` is the tool's own refusal. `MCP-OFFLINE` is a server still
down after one reconnect, and names its last failure and its stderr log.
`MCP-TIMEOUT` is a deadline that passed; the connection stays up.

The model learns all this from `(help :mcp)`, which adds a live catalog of the
tools. `/mcp status | on NAME | off NAME | restart NAME | tools NAME` answers
one line. The web page's Control > MCP tab adds, removes, restarts and tests
servers through `/api/mcp`, and its Catalog fills the form from
`catalog.json`, a list of known servers whose recipes come from each
project's own docs.

## Config

```jsonc
"mcp": {
  "servers": {
    "files":  {"command": "mcp-files", "args": ["--root", "/tmp"], "env": {"K": "v"}, "timeout_ms": 30000},
    "remote": {"url": "https://host/mcp", "headers": {"Authorization": "Bearer ..."}},
    "off":    {"command": "x", "enabled": false}
  },
  "inherit_env_allowlist": ["PATH", "HOME"]
}
```

A url without a command is HTTP and anything else is stdio; `transport`
overrides. Header values never appear in a status or an error. An entry that
fails to validate shows as `refused` while the others run. Not supported:
`cwd`, `${VAR}` expansion, per-tool allow and deny lists, OAuth, and the old
two-endpoint SSE transport.

## No reader thread

Every read happens on the calling thread under that call's deadline, which is
also how a call is cancelled. A server's `ping` or `tools/list_changed` is
seen only while a call is reading. One short-lived thread per server connects
at start, so the gateway never waits.

Tests: `just mcp-check` in nodecode, over an in-memory transport, the Python
fixture server in `test/` and an HTTP fixture.
