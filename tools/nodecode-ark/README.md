# nodecode-ark

Ark: encrypted, deduplicated snapshots of the brain, restorable one file at a time. A Nodecode rewrite of [medomohamed22/hermes-ark](https://github.com/medomohamed22/hermes-ark) by [medomohamed22](https://github.com/medomohamed22).

What a Nodecode home cannot recompute is `config.jsonc` and the cells folder:
the layer, the memory, the skills, every installed cell. Ark keeps snapshots
of exactly those in a repository of your own, and hands one file back when
something is lost, mangled or overwritten.

- **Deduplicated.** A file's content is stored once, under its own address, however many snapshots hold it. A second snapshot of an unchanged home writes nothing at all.
- **Encrypted.** With a passphrase, every object is sealed before it touches the disk, and the object's address is a keyed HMAC, so the folder does not even say which files are equal. Without one, the repository is a plain folder — your choice.
- **Restorable by item.** `/ark restore cells/you/note.lisp` puts one file back, preview first, and refuses a path that would leave the home.
- **Pruned by policy.** The newest snapshots are always kept, plus one an hour, one a day and one a month; everything else and the data none of them refers to is dropped.

Credentials are never captured: `.env`, `auth.json`, `vault.key`, pairing
tokens, any `*.key` or `*.pem`, and the cells folder's own git storage are
skipped, so a snapshot is brain, not secrets.

## Get it

Tell the agent "add the ark cell", or run `nodecode add nc://ark`.

## Set it up

Nothing is required. With no section the repository is a plain folder at
`ark/` under this home, and snapshots are unencrypted.

To encrypt, name the environment variable that holds your passphrase in
`/settings` or in the `ark` section of the config:

```jsonc
"ark": {
  "passphrase_env": "ARK_PASSPHRASE",   // holds the passphrase; empty = unencrypted
  "repository": "",                       // empty = ark/ under this home; a USB drive or a synced folder otherwise
  "roots": ["config.jsonc", "cells/"],    // what a snapshot holds
  "keep_last": 10, "keep_hourly": 24, "keep_daily": 7, "keep_monthly": 12
}
```

The passphrase itself is never written down: the repository keeps a salt, the
KDF cost and a verifier, and a wrong passphrase is refused before anything is
read or written. There is no passphrase recovery — keep it in your password
manager.

## Use it

The model takes snapshots with the `ark_checkpoint` tool, before anything
risky touches memory or skills; you take them from `/ark`:

```
/ark                       the repository, the snapshots, and what changed since the last one
/ark snap [label]          take a snapshot now
/ark list                  every snapshot, newest first
/ark show [id]             what a snapshot holds
/ark diff [a] [b]          what changed between two snapshots (default: the last two)
/ark verify                prove every snapshot's files are present and open
/ark restore PATH [id] [yes]   put one file back (preview unless yes)
/ark prune [dry]           apply the retention policy and drop unreferenced data
```

`/ark status` also counts the drift since the last snapshot and says so
loudly when the brain has shrunk to under half the files it held — the case
where nobody noticed the loss yet.

The same from Lisp:

```lisp
(nodecode-ark:create-snapshot :label "before the merge")  ; the manifest, oldest-kept order
(nodecode-ark:snapshot-manifests)                       ; newest first
(nodecode-ark:restore-item nil "cells/you/note.lisp")    ; newest snapshot, one file
(nodecode-ark:status-text)
```

Two homes, two repositories: a `$NODECODE_HOME` profile keeps its own `ark/`
under its own folder, so a profile can be snapshotted without touching the
default one.

## What it does not do

Ark is not the whole-home archive Nodecode's own `/backup` makes: it holds the
brain, not project files, transcripts or caches, and it restores one file at
a time rather than a profile beside the home. For a whole home as one file,
use `/backup`; for the brain, versioned and encrypted, use this.

## The seal

AES is not in the image's own toolkit, so the cipher here is a keyed
HMAC-SHA-256 stream in counter mode with an encrypt-then-MAC tag over
`nonce || ciphertext`, the keys stretched from the passphrase with
PBKDF2-HMAC-SHA-256 (120,000 rounds by default, adjustable). The tag is
checked before a byte is written back. That is a standard construction,
written small and readable in `cell.lisp`, on top of the one digest the image
already carries.

## Licence

MIT, the original's. See `LICENSE` and `NOTICE`.
