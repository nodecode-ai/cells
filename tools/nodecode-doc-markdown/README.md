# nodecode-doc-markdown

Document Markdown: turns one local file — PDF, DOCX, PPTX, XLSX, HTML, CSV, JSON, TXT or MD — into Markdown the model can read. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-doc-markdown](https://github.com/TakeshiTGAL/hermes-plugin-doc-markdown) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

## Get it

Tell the agent "add the doc-markdown cell", or run `nodecode add nc://doc-markdown`.

## Set it up

Nothing is required: enabled, the tool reads files at once. The `doc-markdown` section of the config refines it — `max_file_bytes` (the largest file read, 20971520 by default), `max_output_chars` (the longest Markdown kept, 200000) and `refuse_hidden` (refuse a path with a component that starts with ".", on by default). `config.example.jsonc` is the section written out.

## Use it

Ask for the contents of a document and the model calls `doc_convert` with the file's absolute path: it answers the document as Markdown — headings with their `#`s, a table row as one line of cells, a slide per `## Slide N`, a sheet per `## Name` — and says which file, of what type and how many bytes it read. `/doc PATH` does the same from the shell:

```
/doc ~/Documents/quarterly.pdf
```

The guard is the original plugin's, and it is worth knowing before you hit it. Every symbolic link is followed first; then the tool refuses any path with a component that starts with `.`, any path inside the Nodecode home folder, a relative path, an `http`, `https` or `file` URL, a missing file, a directory, an extension outside `pdf, docx, pptx, xlsx, html, htm, csv, json, txt, md`, and a file larger than `max_file_bytes`. What it reads is what this process can open: it is a guard, not a sandbox, and a hostile document is still a document this process parsed.

A `.docx`, `.xlsx` or `.pptx` is read as the zip of OOXML parts it is, with a DEFLATE decoder in the cell, so no Python, MarkItDown, node or npm is involved. A PDF is best-effort: its streams are inflated where they say zlib and the text operators are read out, so a scanned page — or a PDF whose text lives in compressed object streams — answers nothing and says so instead of answering noise. An `.xls` workbook is not read at all: save it as `.xlsx` or `.csv` first.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the path guard, the DEFLATE and zip readers, the OOXML, HTML, CSV and PDF readers, the `doc_convert` tool, `/doc` and the manual. |
| `nodecode-doc-markdown.asd` | The system and its test system. |
| `test/` | The offline tests: inflate and zip, every reader over a fixture this process writes, the guard's refusals, the tool through the wire, `/doc`. |
| `config.example.jsonc` | The section, written out as documentation. |

## License

MIT. The original is Copyright (c) 2026 TakeshiTGAL: see `LICENSE` and `NOTICE`.
