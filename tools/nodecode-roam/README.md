# nodecode-roam

Roam: an encrypted bundle of your Nodecode home in storage you own, so a second machine can be set up from it. A Nodecode rewrite of [vcruz305/hermes-roam](https://github.com/vcruz305/hermes-roam) by [vcruz305](https://github.com/vcruz305).

`config.jsonc` and the cells you keep go into one sealed file, written
where you say: a folder on a drive you already sync, or one HTTPS URL you
can PUT to. The bundle is AES-256-GCM under a passphrase that only the
config points at, and its head -- the version, the date, the counts, one
digest of the contents -- is HMAC-signed, so a wrong passphrase or an
edited store is refused before anything is written.

## Get it

Tell the agent "add the roam cell", or run `nodecode add nc://roam`.

## Set it up

One required setting, the store, and one passphrase the config names but
never holds:

```jsonc
{
  "roam": {
    "store": "~/Sync/nodecode-roam/",         // a folder, or an https URL
    "passphrase_env": "NODECODE_ROAM_PASSPHRASE",
    "profile": "default",                     // one bundle per profile
    "include": ["config.jsonc", "cells/"],    // what a bundle holds
    "exclude": [".cache/", ".git/", "store/", ".roam/", "node_modules/"],
    "allow_agent_apply": false,               // may a pull write files?
    "backup_keep": 5
  }
}
```

| Member | Default | What it does |
| --- | --- | --- |
| `store` | none (required) | the folder, or the `https://` URL, this profile's bundle lives at |
| `passphrase_env` | `NODECODE_ROAM_PASSPHRASE` | the variable holding the passphrase |
| `passphrase_file` | none | a file holding it instead |
| `profile` | `default` | which bundle in the store: one name per set of machines |
| `include` | `config.jsonc`, `cells/` | the paths under the home a bundle holds |
| `exclude` | `.cache/` `.git/` `store/` `.roam/` `node_modules/` | prefixes left out, `*.fasl` always |
| `authorization_env` | none | a variable holding an `Authorization` header, sent verbatim to an HTTPS store |
| `operation_timeout` | `60` | seconds one store read or write may take |
| `allow_agent_apply` | `false` | let `roam_pull` write files without your yes |
| `backup_keep` | `5` | how many pre-restore backups of this home are kept |

An S3 or R2 bucket is reached through a presigned URL, a WebDAV endpoint or
a small signing proxy in front of it: the cell carries no vendor signer, and
`authorization_env` names the header value, never the value itself.

## Use it

On the first machine:

> Push this home to my roam store.

On the second machine, with the same section and the same passphrase:

> What would a pull from roam change? Then restore it.

| Tool | Argument | What it does |
| --- | --- | --- |
| `roam_status` | none | what this home holds, what the store holds, and whether they differ |
| `roam_push` | `force?`, `note?` | seal the files in scope and write the next version; `force` replaces a store this home is behind |
| `roam_pull` | `apply?` | the plan -- added, replaced, unchanged -- and, with `apply` and `allow_agent_apply` on, the restore |

From the keyboard, `/roam` shows where things stand, `/roam push [force]`
writes a version, `/roam diff` is the plan, `/roam pull apply` restores,
`/roam backups` lists what each restore kept, and `/roam restore NAME` puts
an earlier backup back.

Every restore copies each file it replaces into `.roam/backups/<stamp>/`
first, with a journal, and takes the files it does not carry along. A store
another machine has moved past this home's last version is refused on push
unless it is forced, so two machines do not quietly overwrite each other.

`(help :roam)` is the same, for the model.
