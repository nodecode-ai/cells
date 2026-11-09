# nodecode-vault

Vault: a local secret store sealed with AES-256-GCM, its items named by `hv:<item>[:<field>]` refs, and a command run with the mapped variables in its environment. A Nodecode rewrite of [Guerilla-ops/floorvault](https://github.com/Guerilla-ops/floorvault) by [Guerilla-ops](https://github.com/Guerilla-ops).

One file holds every item, encrypted as a whole under a 32-byte key the file
does not carry: the key is a file of its own (written 0600), or the
environment variable `key_env` names. Nothing is created by a read — a
missing key or store is a refusal that says how to make it.

## Get it

Tell the agent "add the vault cell", or run `nodecode add nc://vault`.

## Set it up

1. `/vault setup` once: it writes the key file and an empty store (both under
   the cell's default folder in the home) and creates nothing that exists.
2. In `/settings`, `vault.key_file` names the key file if it is not the
   default, or `vault.key_env` names an environment variable holding the key
   as 64 hex characters — for a key a service manager hands the process.
3. `vault.env` maps variables to refs, one `NAME=hv:<item>[:<field>]` per
   entry: that is what `vault_run` resolves.
4. `/vault doctor` prints the store, the custody and how many items there are,
   and never a value.

Without a key the cell still installs and answers; every verb says what is
missing, and nothing is dialled or prompted for.

## Use it

The model has five tools:

| Tool | What it does |
| --- | --- |
| `vault_list` | items, labels and field names; never a value |
| `vault_get` | one secret by its `hv:` reference |
| `vault_put` | store one field, creating the item |
| `vault_remove` | delete an item |
| `vault_run` | run a shell command with the mapped variables in its environment |

`vault_run` is the one to reach for when a command needs a secret: the value
goes into that command's environment and never into the transcript.

By hand, and from eval:

| Command | What it does |
| --- | --- |
| `/vault setup` | make the key file and an empty store |
| `/vault list` | what is in the store |
| `/vault show REF` | one item's fields, masked |
| `/vault get REF` | one value, in clear |
| `/vault put ITEM FIELD VALUE` | store a field |
| `/vault rm ITEM` | delete an item |
| `/vault env ITEM NAME [FIELD]` | the `vault.env` entry to paste |
| `/vault doctor` | the paths, the custody, the count |
| `/vault run CMD` | a command under the mapped variables |

From eval: `(vault:secret "hv:openai:key")`, `(vault:put "openai" "key" "…")`,
`(vault:items)`, `(vault:mapping)`, `(vault:run "printenv OPENAI_API_KEY")`.
`(help :vault)` is the primer.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the package, its one condition, `(vault:…)` |
| `store.lisp` | the seal, the key custody, the items and refs, `vault_run` |
| `cell.lisp` | the section, the five tools, `/vault`, the manual |

## What it does not do

The original seals with AES-256-SIV through a Python library and resolves
variables into the harness's own process at startup; this cell seals with
AES-256-GCM (ironclad, already in the image) and applies its mapping at the
moment a command runs, since a Nodecode cell cannot reach back into a process
that already started. The OS key-store custody (Secret Service, the macOS
keychain) and the `systemd-creds` custody are not carried over: the key is a
file or an environment variable. See `NOTICE`.

## License

MIT, Scott Lee's: see `LICENSE` and `NOTICE`.
