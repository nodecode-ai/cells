# nodecode-kimi-code-delegate

Kimi Code Delegate: ask the locally signed-in Kimi Code CLI for read-only advice, as one tool the model calls. A Nodecode rewrite of [s-a-s-k-i-a/hermes-kimi-code](https://github.com/s-a-s-k-i-a/hermes-kimi-code) by [s-a-s-k-i-a](https://github.com/s-a-s-k-i-a).

The agent doing the work stays the agent doing the work: this cell hands a
second, different model one self-contained question and brings back what it
says. Kimi is not a model route here — it never replaces this session's
model, never takes a turn, and cannot be chosen as a fallback — and the call
is read-only by construction:

- one fresh `kimi acp` process and one ACP session per call, then it is gone;
- the CLI's own `kimi login` is the credential, and the child is handed an
  environment narrowed to the names a CLI needs, so no other key of this
  process reaches it;
- a permission request is answered with the agent's own `reject_once`
  option, and a filesystem or terminal request with a JSON-RPC error;
- an empty answer is a failure, not a turn: the cell says the provider
  status is unknown rather than guessing quota.

## Get it

Tell the agent "add the kimi-code-delegate cell", or run `nodecode add nc://kimi-code-delegate`.

## Set it up

Install the [Kimi Code CLI](https://github.com/MoonshotAI/kimi-cli) and sign in
once, in a terminal:

```sh
kimi login
```

The cell finds `kimi` on `PATH`, or the CLI's own `~/.kimi-code/bin/kimi`.
Nothing else is needed; there is no key of its own. The section is optional
and lives in `~/.nodecode/config.jsonc` (`/settings`, or the file;
`config.example.jsonc` has the shape):

```jsonc
"kimi-code-delegate": {
  "command": "kimi",                 // a name on PATH, or a path
  "working_directory": "",           // where a call runs; empty = Nodecode's own
  "model": "k3",                     // or "k3-256k"
  "thinking": "auto",                // or an option id the CLI offers
  "timeout_seconds": 300,
  "startup_timeout_seconds": 10
}
```

Check the wiring before spending a real call: `/kimi-code doctor` runs the
handshake alone and says who answered, the version, the models the CLI
offers and its current effort.

## Use it

The model calls `kimi_code_delegate` with a `prompt` — work it cannot see
any of this conversation, so the prompt must stand alone — and optionally a
`model` (`k3`, `k3-256k`) and a `thinking` effort. The answer is JSON: the
agent's text, the model and effort it ran on, and `delegate_only`.

Ask for it in words and the agent decides; ask explicitly when you want the
second opinion:

> Ask Kimi to review this migration plan for anything that would break on
> rollback.

It is most useful where a second model is worth a call: a design about to be
committed to, a review of a diff or a plan, a second reading of a rule. It
cannot edit files, run commands or answer with tool calls, so a task that
needs any of that is the worst fit.

`/kimi-code` with anything but `doctor` prints its usage.
