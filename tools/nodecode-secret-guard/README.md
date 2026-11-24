# nodecode-secret-guard

Secret Guard: keeps API keys out of the conversation — a key is stored under the name a tool reads it by, and all the model ever sees is a fingerprint and a length. A Nodecode rewrite of [ChasClogston/hermes-secret-guard](https://github.com/ChasClogston/hermes-secret-guard) by [ChasClogston](https://github.com/ChasClogston).

## Get it

Tell the agent "add the secret-guard cell", or run `nodecode add nc://secret-guard`.

## Set it up

Nothing has to be set. Keys are kept in one private file, `secret-guard/secrets.json` under the Nodecode home, written mode 600 and replaced atomically. The section is there if you want it:

- `secret-guard.store_file` — where the keys are kept, if not the default file.
- `secret-guard.redact_tool_results` — scrub every stored value out of every tool result (default `true`).
- `secret-guard.block_secret_reads` — refuse a call that reads a credential store (default `true`).
- `secret-guard.allow` — regular expressions a call may match to pass the gate anyway.
- `secret-guard.export_env` — put each stored key into the process's environment, when the variable is not already set, so a shell command reads `$NAME` (default `true`).

Add a key without pasting it into the chat:

```
/secret add OPENAI_API_KEY OPENAI_API_KEY      # the value is in that environment variable
/secret add GROQ_API_KEY ~/.keys/groq          # or in a file, mode 600
```

The command reads the value itself, so it never appears in the transcript; what comes back is the fingerprint and the length.

## Use it

- `secret_request` — get a key stored. You never see the value: it answers the name, a fingerprint and the length. Never ask a person to paste a key into chat.
- `secret_list` — the names held, with fingerprints, lengths, sources and dates.
- `secret_check` — CLEAR, or LEAKED with the names a draft carries and the draft with them scrubbed, before it is pasted or sent.
- `/secret` — what is held, and a panel to add, forget or verify; `/secret add NAME SOURCE`, `/secret remove NAME`, `/secret verify [NAME]`, `/secret list [NAME]`.
- `(help :secret-guard)` — the manual, for the model.

Three layers come off one store:

1. **The gate** runs at the `:tool` point before a call: a call that reads a secret-shaped file (`.env`, `*credential*`, `*token*`, `*.pem`, `id_rsa`, `auth.json`), prints a keychain or password-manager value (`security find-*-password -w`, `dump-keychain`, `secret-tool lookup`, `pass show`), or dumps the whole environment (`printenv`, `env`, `export -p`) is refused, the file is named, and the call does not run. The argument names are not consulted, only their text, so a built-in tool, a cell and an MCP server get the same check. `allow` exempts a call by pattern.
2. **The scrub** runs after every call: each stored value in the result comes back as `[secret:NAME redacted]`, longest value first. A value under six characters is held but left alone, since it would be found in unrelated text.
3. **The reader** is for using a key without pasting it: `(secret "NAME")` inside a form, or `$NAME` in a shell command while `export_env` is on. Anything a form prints back is scrubbed on its way out, so the value stays in the image.

Everything is local: one file under the Nodecode home, string matching in this folder, no network, no service, no subprocess.

## Tests

```sh
.github/test-cell.sh tools/nodecode-secret-guard
```

Offline, in a scratch home: lifecycle, the store and its mode, the fingerprint and length a name answers with, the scrub, the short-value rule, the gate and its switch and allow list, the draft check, and the command.
