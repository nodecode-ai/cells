# nodecode-model-tiers

The Claude model hierarchy, and the leaked prompt archive behind it, quoted with line numbers.

The fact the archive states and no vendor page does: above Opus sits a **Mythos** tier, and **Claude Fable 5.2** is a Mythos-tier model with extra safety measures for biology and cybersecurity. It shares one underlying model with **Mythos 5.2**. The generation before says the same of 5.1, in its own file - which is the point: one document can be a fake, two that agree a generation apart are harder to fake.

## Get it

Tell the agent "add the model tiers cell", or run `nodecode add nc://model-tiers`.

## Use it

Ask in your own words: *what is above Opus*, *is Fable the same model as Mythos*, *what did the export controls do*, *what does Cursor's prompt say*, *how does Codex describe itself*. The model reads the same archive you do.

- `model_tiers_files` - the archive: thirteen documents, what each one is, its vendor, size and digest. Filter by vendor (`Anthropic`, `OpenAI`, `Z.ai`, `Meta`, `Cursor`, `Cline`) or a word.
- `model_tiers_list` - the Claude ladder, tier by tier (`mythos`, `opus`, `sonnet`, `haiku`).
- `model_tiers_get` - one model whole: where it sits, whether a person can have it, its API string, and the archive's sentences with document and line.
- `model_tiers_search` - any word, over the models, the tiers and the documents' own summaries.
- `model_tiers_quote` - one document's own lines: every line carrying a word, numbered, or a range as `116-122`. This is the one verb that fetches, once per document.

In chat: `/model-tiers` for the ladder and the timeline, `/model-tiers files` for the archive, `/model-tiers fable` for one model, `/model-tiers source` for the archive's head and a digest, `/model-tiers lines 116-122` for the prompt's own words.

From Lisp, with the cell on:

```lisp
(nodecode-model-tiers:models-matching :tier "mythos")
(nodecode-model-tiers:files-matching :vendor "OpenAI")
(nodecode-model-tiers::quote-text :file "cursor-2" :lines-text "1-12")
```

## What it does not do

It does not ask a vendor anything, and it does not crawl. The hierarchy and the index are answered from `data.lisp` with no request, no key and no account. Only `model_tiers_quote` reaches the network: it fetches one document from the archive once, caches it under `~/.cache/nodecode-model-tiers/`, and reads the cache after that.

## Read it with care

A snapshot: [elder-plinius/CL4R1T4S](https://github.com/elder-plinius/CL4R1T4S) at `93a58476`, 9 November 2026, read 11 November. Every document carries its size, its line count and its sha256, and every quote carries a document key and a line number, so a person can check any sentence against the file it came from - that is what `model_tiers_quote` is for.

Where the archive and a vendor page disagree, this cell gives the archive's reading of itself, and says so in every answer.

None of it was announced. It was read out of files.

## Set it up

Nothing is required.

## Credit

The documents are [elder-plinius](https://github.com/elder-plinius)'s collection, in [CL4R1T4S](https://github.com/elder-plinius/CL4R1T4S); the words in them are each vendor's. The code is MIT, ours. See `NOTICE`.
