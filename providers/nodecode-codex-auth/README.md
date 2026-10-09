# nodecode-codex-auth

A Nodecode cell that lets a ChatGPT subscription serve OpenAI-family
models. When a lane points at the OpenAI API and your saved login is a
ChatGPT one, the cell answers Nodecode's credential lookup with that login.
It also sends the requests to the Codex backend with the headers that login
needs.

The login itself lives in Nodecode's shared `auth.json`, written by
`codex login` or the Nodecode cloud gateway. This cell stores nothing.

## Install

```sh
nodecode add nc://codex-auth
```

Or in Nodecode, run `/setup` → **Choose** and pick **codex-auth**, or copy
this folder into `~/.nodecode/cells/nodecode-codex-auth/` and restart
Nodecode.

## Configure

Optional. Having the section enables the cell, and `"enabled": false`
turns it off:

```jsonc
{
  "codex-auth": {
    // Where a saved ChatGPT login is served. A relay's address goes here.
    // Used only when the provider's own endpoint is api.openai.com.
    "endpoint": "https://chatgpt.com/backend-api/codex/responses"
  }
}
```

See [`config.example.jsonc`](config.example.jsonc).

## Test

From a Nodecode checkout's `src/`:

```sh
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(push #p"/path/to/cells/providers/nodecode-codex-auth/" asdf:*central-registry*)' \
  --eval '(asdf:test-system :nodecode-codex-auth)'
```

MIT licensed.
