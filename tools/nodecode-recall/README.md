# nodecode-recall

Recall: a private, on-device index of the documents on your machine, so "where is that Acme invoice" is answered from the file instead of a guess. A Nodecode rewrite of [memeshee/hermes-recall](https://github.com/memeshee/hermes-recall) by [memeshee](https://github.com/memeshee).

## Get it

Tell the agent "add the recall cell", or run `nodecode add nc://recall`.

## Set it up

Nothing is required. The index is `~/Documents` alone until you name more:

```jsonc
"recall": {
  "roots": ["~/Documents", "~/Downloads"],
  "excludes": ["*.zip"],
  "ambient": true
}
```

`~/Downloads` and `~/Desktop` are opt-in on purpose: they are full of screenshots, installers and exported keys. Then say "index my documents", or run `/recall index`.

A PDF is read through `pdftotext` and a Word, Excel, PowerPoint or OpenDocument file through `unzip`, when your machine has them; without either, the file is still indexed by the words in its name and `recall_open` says what it could not read.

## Use it

The model gets five tools, and the same verbs are callable from eval:

| Tool | What it does |
| --- | --- |
| `recall_index` | Builds or refreshes the index over the roots (or the folders the call names). Incremental: only new or changed files are re-extracted. `rebuild` re-reads everything. |
| `recall_search` | Ranks the files under a query, with the matched line in brackets, a date and a score. Filter by `path` substring or by `ext`. |
| `recall_open` | One file as text, with `offset` and `max_chars` to page through a long one. |
| `recall_stats` | What is indexed, where, when, and what was skipped. |
| `recall_forget` | Drops a file, a folder or the whole index. It never touches the file on disk. |

`/recall <query>` searches, `/recall index [path]` builds, `/recall stats` and `/recall forget all` do what they say. `(help :recall)` is the manual, and `(recall:search "powder coating")` works from eval too.

With `ambient` on, a message that reads like it is about a document ("send the lease to legal") gets a short block before the model answers: up to `max_hints` paths, each with a date and one bracketed line, and never the file itself -- the model still has to read it before quoting it. A file already shown to that session is not shown again.

## What it never reads

One policy decides, in `guard.lisp`, judged on the **resolved** path so a symlink named `notes.txt` cannot point at a key:

- Nodecode's own home, where its cells, secrets and store live;
- virtual filesystems (`/proc`, `/sys`, `/dev`, `/run/user`) -- `/proc/self/environ` is a readable file full of keys;
- the credential-store denylist: `.env`, `*.pem`, `id_rsa*`, `*.kdbx`, `.netrc`, `*.gpg`, `*secret*`, `*recovery*code*`, `.aws`, `.ssh`, `.gnupg`, `.kube`, `.config` and the rest of it in `guard.lisp`.

The rules are the cell's: `excludes` adds patterns, it never removes one. Nothing leaves the machine -- there is no network call anywhere in the cell, and no embedding service.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | The package, the index structures, the one index file under the organism's home. |
| `guard.lisp` | The read policy: the home, the virtual filesystems, the credential denylist. |
| `extract.lisp` | Text, HTML/XML, PDF and Office extraction, and the chunker. |
| `index.lisp` | The walk, the incremental run, the postings, BM25 and the snippet. |
| `cell.lisp` | The five tools, `/recall`, the manual, the ambient hint and the background refresh. |

## License

MIT. Recall is Copyright (c) 2026 memeshee: see `LICENSE` and `NOTICE` for what was rewritten and what was left out.
