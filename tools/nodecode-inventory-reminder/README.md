# nodecode-inventory-reminder

Inventory Reminder: "do I already have X?" answered from a cached, read-only
digest of this host — its repositories and which are cloned here, its skills,
its MCP servers, the CLIs on PATH, the model files on disk and the packages in
the harness's venv. A Nodecode rewrite of [ether-btc/inventory-reminder](https://github.com/ether-btc/inventory-reminder) by [ether-btc](https://github.com/ether-btc).

An agent that has to re-derive what it already has spends a turn doing it, and
one that guesses gets it wrong. This cell answers the question from a digest
somebody else already wrote, in the shape the original plugin answered it: hits
grouped by section with the detail that makes each one usable (where the clone
is, whether the MCP server is enabled and what runs it, the model's size and
path), and one line for a session bootstrap.

Read-only, and no rescan per turn: the digest is read when a tool is called,
and nothing here writes it.

## Get it

Tell the agent "add the inventory-reminder cell", or run `nodecode add nc://inventory-reminder`.

## Set it up

Point the cell at a digest:

```jsonc
"inventory-reminder": {
  "inventory_json": "~/.hermes/inventory.json",
  "max_hits": 40
}
```

`inventory_json` is the file to read — a JSON object of `repos`, `skills`,
`tools`, `apps`, `models` and `services`, which is what the original plugin's
scanners write (`repo-inventory.py` and `system-inventory.py` in its
repository, or anything else that writes the same JSON). Left out, the cell
reads `$INVENTORY_JSON`, then `~/.hermes/inventory.json`, which is where those
scanners put it by default.

`max_hits` is how long one report may get before it says how many hits it left
out. A value that is not a positive number refuses at start, and the folder
says so on `/cells` and in the notices block.

## Use it

- `/inventory-reminder aider` searches every section for `aider`;
  `/inventory-reminder` alone answers the one-line digest.
- `(inventory-reminder:lookup "aider")` and `(inventory-reminder:digest)`
  answer the same two things through eval.
- `inventory_where` is the model's tool, with `term` and `json`; the original
  plugin's `--json` shape — `{"query": ..., "hits": [{"section", "item",
  "detail"}]}` — is what `json` answers.
- `inventory_summary` is the model's one-line digest.

```
3 hits for "aider" in ~/.hermes/inventory.json

  SKILLS
    aider    (2)

  CLI TOOLS
    aider    (/opt/bin/aider)

  KEY PACKAGES
    aider-chat    (0.50.1)
```

Nothing matching says `MISS: <term> — not found in any section`, and a digest
that is not there refuses naming the file it looked for, so a miss is never
confused with a missing digest.

The section is `inventory-reminder`; the manual topic is
`(help :inventory-reminder)`.

## What the rewrite leaves out

The scanners themselves. The original plugin ships two Python scanners plus a
refresh script, an installer and a rollback script that keep the digest
current, and a weekly cron job; this cell reads the digest they produce and
never rescans, which is exactly what the original's two tools did per turn.
See [NOTICE](NOTICE).
