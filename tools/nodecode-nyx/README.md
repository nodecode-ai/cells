# nodecode-nyx

Nyx: the user's own long-term memory, in a file on their disk. A Nodecode rewrite of [sixgodgit/nyx](https://github.com/sixgodgit/nyx) by [sixgodgit](https://github.com/sixgodgit).

The model gets four tools — `nyx_remember`, `nyx_recall`, `nyx_forget`,
`nyx_restore` — that keep a dated journal entry for what the user said and
find it again by its words, and `/nyx` to show the store or search it. No
model runs on the write path and nothing leaves the machine: an entry is a
line of a text file, written as it was said, with the time it was said.

## Get it

Tell the agent "add the nyx cell", or run `nodecode add nc://nyx`.

## Set it up

It works out of the box: memory is a folder, and with no `nyx` section it
sits under the nodecode home. To choose the folder, and to bound recall and
the quarantine:

```jsonc
{
  "nyx": {
    "data_dir": "~/.neurobase",   // or NEXSANDBASE_HOME; empty means <nodecode home>/memory
    "recall_limit": 8,             // entries one recall answers
    "forget_days": 7,              // how long a forgotten entry can be restored
    "filter_secrets": true         // replace tokens and keys before writing
  }
}
```

| Member | Default | What it does |
| --- | --- | --- |
| `data_dir` | unset | the folder the journal lives in; `NEXSANDBASE_HOME` is read too |
| `recall_limit` | `8` | how many entries one recall answers unless the call says otherwise |
| `forget_days` | `7` | how long a forgotten entry stays restorable before it is erased |
| `filter_secrets` | `true` | replace tokens, keys and passwords with a placeholder |

A `data_dir` the cell cannot write refuses at the start, naming the member;
so does an integer under its floor.

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that I drink oolong and that the deploy token is ghp_…

The model calls `nyx_remember`, and the token reaches the disk as
`[hidden: GitHub token]` while the rest is kept verbatim. Later,
"what do I drink?" makes it call `nyx_recall`, which answers every entry
whose words all appear, newest first, each with its date and its id. Ask it to
forget one and `nyx_forget` moves that entry to the quarantine; `nyx_restore`
puts it back until `forget_days` have passed, after which it is erased from
the disk. `/nyx` prints the store, its folder and what is in quarantine;
`/nyx oolong` searches it from the shell.

Memory is one line an entry, in the original's shape:

```
2026-11-09 07:12:33 | user | I drink oolong, never coffee
```

## The store

`journal.txt` holds the live entries; `quarantine.jsonl` holds the
forgotten ones, each with the time it expires. Both are plain text in
`data_dir`, and a folder the Nyx command line already wrote reads here when
`data_dir` names it.

## Not ported

- **Semantic retrieval.** The original reaches it through an embedding model
  or an external embedding API; this cell does no model call on either path,
  so recall is by the words of the query.
- **The bi-temporal facts, knowledge graph and L3 profile** built on the
  journal, which need the Python package's own pipeline.
- **The optional encrypted vault** and its companion reveal plugin, whose
  keystore and approval seam belong to the host.
- **The MCP server**, the `hermes memory setup` commands and the Chinese
  localisation; the section, `/settings` and `/nyx` are the same surface.

MIT, as the original.
