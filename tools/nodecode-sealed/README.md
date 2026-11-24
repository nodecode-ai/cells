# nodecode-sealed

Sealed: an encrypted store of environment variables, opened by a key file of its own and loaded into the running process at start. A Nodecode rewrite of [kailashrajh/hermes-sealed](https://github.com/kailashrajh/hermes-sealed) by [kailashrajh](https://github.com/kailashrajh).

One file holds every variable, encrypted as a whole with AES-256-GCM under a
32-byte key the file does not carry: the key is a file of its own, and both are
0600. At every start the store is read once and its variables are put into
Nodecode's own environment, so a provider, a cell or a command that names one
finds it, and the plaintext is in no `.env`. Nothing is created by a read: a
missing store or key is a refusal that says how to make it.

The format is the original's, byte for byte — the `SEALED1` header, the
base64url payload of nonce, ciphertext and tag, and a dotenv body inside — so a
store the original wrote opens here, and one written here opens there.

## Get it

Tell the agent "add the sealed cell", or run `nodecode add nc://sealed`.

## Set it up

1. `/sealed init` once: it writes the key file and an empty store under the
   home, both 0600, and creates nothing that already exists.
2. `/sealed set NAME VALUE` for each variable you keep, or let the model call
   `sealed_set`.
3. In `/settings`, `sealed.store` and `sealed.key_file` name the two files when
   they are not the defaults (`sealed/sealed.store` and `sealed/sealed.key`
   under the home). `sealed.strict_permissions` makes an over-permissive file a
   refusal instead of a warning, and `sealed.override_existing` false leaves a
   variable the process already has standing.

```jsonc
"sealed": { "strict_permissions": true, "override_existing": true }
```

## Use it

The model has four tools:

| Tool | What it does |
| --- | --- |
| `sealed_status` | both paths, their modes, the entry count, what is loaded; never a value |
| `sealed_list` | the names, and how many characters each value holds; a prefix narrows it |
| `sealed_set` | store one variable, encrypted at rest and live at once |
| `sealed_unset` | remove one |

By hand, and from eval:

| Command | What it does |
| --- | --- |
| `/sealed init [force]` | make the key and the store; `force` makes a new key and keeps the entries |
| `/sealed set NAME VALUE` | store one variable |
| `/sealed unset NAME` | remove one |
| `/sealed list` | names and value lengths |
| `/sealed value NAME` | one value, in clear |
| `/sealed status` | the paths, the modes, the count, what is loaded |
| `/sealed rotate` | re-encrypt under a new key, the previous key kept as `.bak` |
| `/sealed load` | read the store into this environment now |
| `/sealed unload` | put back what was there before |

From eval: `(sealed:names)`, `(sealed:value "NAME")`,
`(sealed:set-value "NAME" "…")`, `(sealed:status)`,
`(sealed:load-secrets)`, `(sealed:unload-secrets)`. `(help :sealed)` is the
primer.

A start puts the store's variables into the environment and says in one notice
how many; the stop that follows puts back exactly what each variable held
before, so stopping the cell leaves the environment as it found it.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the package, its one condition, `(sealed:…)` |
| `store.lisp` | the format, the cipher, the dotenv body, the key custody, the environment |
| `cell.lisp` | the section, the four tools, `/sealed`, the manual, the load at start |

## What it does not do

A value reaches a transcript two ways here, and the original avoided both: a
`/sealed set NAME VALUE` line, and a `sealed_set` call the model made. The
original read the value from the terminal without echo, or from stdin, so it
never touched argv, history or a transcript; `/sealed value NAME` prints one in
clear for the same reason a person asked for it.
The original's verbs (`init`, `set`, `unset`, `list`, `status`, `rotate`) all
survive as `/sealed` verbs; its configuration keys `enabled`,
`strict_permissions` and `override_existing` keep their meaning, and the key
file is the original's raw 32 bytes, not a hex spelling. Nothing is dialled: the
cell talks to no service, and a store on another machine is that machine's.

## Test it

```sh
.github/test-cell.sh tools/nodecode-sealed
```

The tests run in a scratch image with no network: a store in a scratch folder,
a stubbed environment, and the original's own store — a fixed key, nonce and
ciphertext — read back here.
