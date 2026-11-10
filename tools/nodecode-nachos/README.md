# nodecode-nachos

Nachos: durable memory in one local markdown file, its manifest in every request and the entries a turn needs inlined. A Nodecode rewrite of [Nacho-Labs-LLC/hermes-plugin-nachos](https://github.com/Nacho-Labs-LLC/hermes-plugin-nachos) by [Nacho-Labs-LLC](https://github.com/Nacho-Labs-LLC).

## Get it

Tell the agent "add the nachos cell", or run `nodecode add nc://nachos`.

## Set it up

Nothing is required: with no `nachos` section the memory is `nachos/memory.md` under the Nodecode home, and the cell keeps it from the first turn.

A `nachos` section in `~/.nodecode/config.jsonc` moves the store and bounds what a request carries:

```jsonc
{
  "nachos": {
    "store_file": "",        // absolute, or relative to the Nodecode home; empty is nachos/memory.md
    "prefetch_top_n": 5,    // entries whose bodies one turn may inline
    "prefetch_chars": 1500, // what those bodies may spend, heading included
    "manifest_chars": 1200  // the manifest's soft budget; over it summaries shorten, entries stay
  }
}
```

`config.example.jsonc` in this folder is the same section, commented.

## Use it

The model keeps the memory; you read the file. `(help :nachos)` carries the manual, and `/nachos` says where the store is and what it holds.

- `nachos_memory_put` — add or replace an entry: `title` (its key is the slug of it), `body`, and optionally `summary` and `category`. Write the body summary-first: an omitted summary is its first non-empty line, and the category groups the entry in the manifest.
- `nachos_memory_recall` — an entry by `key`, or `query` for every entry whose title, summary or body contains it.
- `nachos_memory_remove` — delete an entry by `key` or `title`.
- `/nachos` — the store's path, its entry count and the last prefetch; `/nachos list`, `/nachos recall KEY`, `/nachos remove KEY`.

Every request carries the manifest: one line per entry, grouped by category, its summary cut only when the budget is tight. Entries are never dropped from it, so the model always knows one exists. At each round the entries that the turn's own words rank highest -- smoothed TF-IDF over titles and summaries, computed on this disk, no network and no model call -- are inlined under `Recalled memory`, and marked `►` in the manifest.

The store is a plain markdown file, hand-editable and grep-able:

```markdown
# ops

## Deploy notes
Run the deploy script after the tests.
```

A `# category` opens a group, a `## title` an entry, and the first non-blank line under it is the summary the manifest shows. Your edit is read back at the next turn, with no import step; an entry is one heading, so a heading line inside a body is refused when the model writes one.

Run `.github/test-cell.sh tools/nodecode-nachos` from the cells repository: the tests start the cell on a scratch store and never touch your own.
