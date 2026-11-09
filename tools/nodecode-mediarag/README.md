# nodecode-mediarag

MediaRAG: local search over your documents, notes, images and audio. A Nodecode rewrite of [dyiapanis/mediarag](https://github.com/dyiapanis/mediarag) by [dyiapanis](https://github.com/dyiapanis).

The original is a Hermes plugin that indexes a corpus into SQLite and embeds every chunk with an EmbeddingGemma ONNX tower. This cell keeps the corpus, the five tools and the hybrid ranking, in Lisp and in one readable file: a keyword lane (BM25) fused, by reciprocal rank, with a fuzzy lane over hashed character 4-grams. No model is downloaded and nothing is uploaded.

## Get it

Tell the agent "add the mediarag cell", or run `nodecode add nc://mediarag`.

## Set it up

One member is required: the folder the corpus lives in. It is created on first start, and the section refuses at start until it is named.

```jsonc
{
  "mediarag": { "corpus_root": "~/documents/corpus" }
}
```

Then `(restart-cells "nodecode-mediarag")`, or set it in `/settings`. Ask `/mediarag` to see the corpus the cell is on.

| Member | Default | Notes |
| --- | --- | --- |
| `corpus_root` | none, required | the folder the corpus lives in; `corpus.sexp` inside it is the index |
| `max_chunk_chars` | `1500` | longest indexed row, the original's chunk size |
| `default_results` | `5` | rows one search answers when the call names no `k` |

## Use it

The model gets the original's five verbs, by the same names:

| Tool | What it answers |
| --- | --- |
| `mediarag_search` | ranked rows for a query: file, kind, span and snippet (`query`, `k`, `kind`) |
| `mediarag_add_note` | stores a note; searchable at once (`text`) |
| `mediarag_ingest_dir` | indexes a folder: markdown, text, html, csv, subtitles, images, audio (`path`) |
| `mediarag_add_source` | one file, one folder, or an http(s) URL downloaded into the corpus cache (`source`) |
| `mediarag_status` | rows, sources and the corpus root |

`/mediarag` says what the corpus holds; `/mediarag ferry times` searches it. `(help :mediarag)` has the same table while the cell runs, and from eval the same functions answer: `(nodecode-mediarag:search-text "ferry")`, `(nodecode-mediarag:ingest-dir "~/notes")`.

Audio and image rows are what the original could not produce without its model: a WAV file's rows carry the same 11.2-second spans the original's audio lane cut them into, and every other media file is one row named by its file name. Subtitles (`.srt`, `.vtt`) are grouped the way the original grouped them, with their real cue times.
