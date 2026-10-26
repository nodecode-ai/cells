# nodecode-opencode-go

A Nodecode cell for [OpenCode Go](https://opencode.ai/docs/go/), OpenCode's
model subscription. models.dev already lists Go, but Go refuses every request
that does not carry an `x-opencode-session` header (HTTP 400
`MissingSessionID`), before it even reads the key. With this cell a turn on
`opencode-go/<model>` goes out the way Go asks for it:

- every request carries `x-opencode-session`: an opaque digest of the
  Nodecode session id, the same on every round of one session (Go routes and
  caches by it), never the session id itself
- each model rides the wire Go serves it on (chat completions, Responses,
  Anthropic Messages or Google), all under one base,
  `https://opencode.ai/inference/go/v1`: models.dev's per-model package
  decides, and the models omp found models.dev misroutes are pinned

The core already sends its own user agent, which Go also asks for. No core
code names OpenCode: the cell is four hooks over seams the core already has.

Follows [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `opencode-go`
provider. See [NOTICE](NOTICE).

## Key

Subscribe to Go and make a key at <https://opencode.ai/auth>. Save it with
`/connect`, or set `OPENCODE_API_KEY`. A saved key wins over the variable.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-opencode-go/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "opencode-go": {
    // where Go is served; every lane appends its own path
    "base_url": "https://opencode.ai/inference/go/v1"
  }
}
```

A wire named in `providers.opencode-go` (an `sdk` on the provider, or on one
of its `models[]`) outranks the cell's choice.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-opencode-go
```

## Gaps

- Only what Go answers before the key is proven without a key: every wire
  reaches Go's key check (HTTP 401) instead of the session refusal (400). What
  omp carries for individual models past that point (tool_choice off for
  DeepSeek and MiMo, reasoning text replayed on tool-call turns) is not
  carried until a real Go key shows Nodecode needs it.

MIT licensed.
