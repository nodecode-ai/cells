# nodecode-optmem

OptMem Memory: an append-only memory on your own disk, with a decay tree that keeps recent memories verbatim and compresses old ones into summaries. A Nodecode rewrite of [rarf/optmem-hermes-plugin](https://github.com/rarf/optmem-hermes-plugin) by [rarf](https://github.com/rarf).

The model gets eight tools — `optmem_note`, `optmem_recall`, `optmem_wake`,
`optmem_nap`, `optmem_zoom`, `optmem_forget`, `optmem_config` and
`optmem_import` — and a `/optmem` command. Every request carries the store's
wake digest as one `optmem` block: the newest memories verbatim, older ones
as their one-line compressions. No key, no service, no network: keeping a
memory costs no tokens.

## Get it

Tell the agent "add the optmem cell", or run `nodecode add nc://optmem`.

## Set it up

Nothing is required — the store is `~/.nodecode/optmem/`, created when the
cell starts. The `optmem` section of `config.jsonc` (or `/settings`) only
changes where it lives and how it reads:

```jsonc
{
  "optmem": {
    "store": "~/notes/optmem",   // optional: the folder holding LOG.txt and TREE/
    "recall_mode": "auto",       // auto, regex or bm25
    "wake_lines": 96,            // how much the digest every request carries prints
    "auto_nap": false            // compress one block every ten finished turns
  }
}
```

A member of the wrong shape is refused at start, naming the member, and
`(restart-cells "nodecode-optmem")` tries again after you fix it.

## Use it

> Remember that the deploy window is Thursday nights.

The model calls `optmem_note`, and the note is on disk for good — one line,
one atomic fact, older notes never overwritten. Later:

> What did we decide about the deploy window?

It calls `optmem_recall`, which searches every memory ever written: the
original `memo` regex for pattern-like queries, ranked BM25 for prose, and
auto to choose between them. `optmem_zoom` opens a compressed block back into
its halves, down to the raw memories; `optmem_forget` drops a summary, never
a memory.

`/optmem` prints the store, how many memories it holds and what compression
is pending; `/optmem deploy window` searches it from the shell.

### The decay tree

Memories are never rewritten. Each block of two, four, eight… consecutive
memories can be compressed into one line of at most 280 bytes — a *nap* — and
a block is built from its two halves, smallest first. The digest that rides
your requests is a cover of that tree, so the last memories arrive verbatim
while the oldest arrive as their compressions, within the `wake_lines`
budget. A digest is complete only when every block it names has a summary;
when one is missing, `optmem_wake` says so and hands the model the next nap's
prompt.

Nothing runs in the background by default: the model compresses when the
digest asks. `auto_nap` compresses one pending block every ten finished
turns with a local extractive summary — no model call, nothing invented.

### The store

`LOG.txt` holds one 320-byte record per memory (`#<id> <date> <text>`, space
padded), and `TREE/<size>` holds the summaries of the blocks of that size at
288 bytes each. That is the format Victor Taelin's
[OptMem](https://github.com/VictorTaelin/OptMem) `memo` tool writes, so a log
written here stays readable by it.

Upstream design: [VictorTaelin/OptMem](https://github.com/VictorTaelin/OptMem).
This cell is the Hermes integration by [rarf](https://github.com/rarf)
rewritten for Nodecode; see [NOTICE](NOTICE). MIT, see [LICENSE](LICENSE).
