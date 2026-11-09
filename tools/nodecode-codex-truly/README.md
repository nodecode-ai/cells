# nodecode-codex-truly

Codex Truly: the real context window of a Codex subscription, measured and written. A Nodecode rewrite of [kyssta-exe/hermes-codex-truly](https://github.com/kyssta-exe/hermes-codex-truly) by [kyssta-exe](https://github.com/kyssta-exe).

The Codex catalog a subscription is served understates what the backend takes: it advertises 272K or 500K per model and publishes an 872K ceiling, while a request carrying ~920K input tokens completes. Nodecode reads `providers.<id>.models[].context_window` before anything it learned, so one scoped row per model hands you the window you pay for.

| What | Does |
| --- | --- |
| `codex_truly` | The model's verb: `status`, `apply`, `revert`, `probe`. |
| `/codex-truly` | The same from the prompt: `status`, `full`, `apply [dry-run]`, `revert`, `probe [MODEL]`. |
| a manual topic | `(help :codex-truly)`, the window and the trade it makes, in words. |

## Get it

Tell the agent "add the codex-truly cell", or run `nodecode add nc://codex-truly`.

## Set it up

Sign in to the subscription once, which is what puts the token in Nodecode's shared `auth.json` under `oauth_tokens.openai-codex`:

```
/openai-codex → Sign in
```

Then, in `/settings` (or in the `codex-truly` section of `config.jsonc`):

```jsonc
"codex-truly": {
  "provider": "openai-codex",
  "target_context_window": 900000,
  "models": [],
  "timeout": 60
}
```

| Setting | What it does |
| --- | --- |
| `provider` | The provider id whose sign-in and model rows are read. `openai-codex` unless a custom provider speaks the Codex wire. |
| `token_env` | A variable holding an access token, used before the saved sign-in. Empty when unset. |
| `auth_file` | The auth store to read, when it is not this home's. Empty means the organism's own. |
| `target_context_window` | The window `apply` grants each model, 272000 to 918000. `918000` is the highest value backed by measurement. |
| `models` | Model ids to manage. Empty manages every model the account's catalog exposes. |
| `timeout` | How long one request to the backend may take, in seconds. |

## Use it

```
/codex-truly status          # the one-line picture, free
/codex-truly full            # the table: advertised, ceiling, configured, measured
/codex-truly apply dry-run   # what a write would change
/codex-truly apply           # write it; a new session reads the wider window
/codex-truly revert          # take back exactly those values
/codex-truly probe gpt-6.1-sol   # re-measure the true ceiling live
```

From eval, in the `codex-truly` package:

```lisp
(codex-truly:status)            ; the table, as text
(codex-truly:apply-windows)     ; the tool's own body
(codex-truly:probe "gpt-6.1-sol")
```

`probe` bisects the live backend: a refused request is free, an accepted one bills its input to the subscription, so it costs about one accepted request and is never run by itself. There is no full 1M of input on this route — about 1.05M total context shares a 128K output reserve, leaving ~920K — and a window past ~925K fails late, mid-stream, as `context_length_exceeded`.

### What it does not do

The original also selected a Codex-backed image provider and generated images through the subscription. That is Nodecode's own `image_gen` choice and its own image cells, so this cell leaves it alone and only reports the window. The interactive settings wizard and the `/codex-truly` desktop panel are gone with the harness that had them: the section is the settings, and the command is the panel.
