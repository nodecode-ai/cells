# nodecode-tabella

Tabella: read, convert and create office documents with pandoc. A Nodecode rewrite of [dyiapanis/tabella](https://github.com/dyiapanis/tabella) by [dyiapanis](https://github.com/dyiapanis).

The tools are the original plugin's, with the same names and arguments:

| Tool | What it does |
| --- | --- |
| `doc_read` | A document's text as Markdown: DOCX, ODT, EPUB, PPTX, HTML, LaTeX, RST and Markdown through pandoc; a PDF through pdftotext; a plain text file read directly. |
| `doc_read_meta` | What the filesystem knows about a document: format, size, modification time. |
| `doc_convert` | pandoc from one format to another: markdown to PDF, DOCX, HTML, ODT or EPUB, and any pair of built-in pandoc formats. |
| `doc_create_document` | Content written as a document, through pandoc. |

Ask "turn this report into a PDF", "what does contract.docx say?", "give me the minutes as an EPUB" — the model reads with `doc_read` and writes with `doc_convert` or `doc_create_document`.

## Get it

Tell the agent "add the tabella cell", or run `nodecode add nc://tabella`.

## Set it up

The cell runs the programs you already have and needs two of them: **pandoc** (the converter; `apt install pandoc`, `brew install pandoc`) and **pdftotext** from poppler (`apt install poppler-utils`, `brew install poppler`). A **PDF as output** needs a LaTeX engine as well: `xelatex` or `pdflatex` (`apt install texlive-xetex`).

Then set the one folder documents are written into. In `/settings`, or in the `tabella` section of `config.jsonc`:

```jsonc
"tabella": {
  "output_dir": "~/Documents",   // required: where created and converted files go
  "pandoc": "pandoc",
  "pdftotext": "pdftotext",
  "pdf_engine": "",              // empty: xelatex if it is there, else pdflatex
  "max_chars": 200000
}
```

`output_dir` is the cell's one required member, and a start without it says so by name. Everything else has a default. A program named in the section may be a name on `PATH` or a path of its own.

## Use it

You do not call the tools; the model does. A person can call the same verbs from eval, on a plain path, in the `tabella` package:

```lisp
(tabella:convert-file "~/Documents/report.md" "pdf")
(tabella:create-file "docx" "## Agenda\n\n1. Budget\n2. Hiring" :title "Board meeting")
(tabella:read-text "~/Documents/notes.docx" :max-chars 4000)
```

`(help :tabella)` is the model's manual for the tools, kept while the cell runs. `/tabella` says where documents are written and which program is missing, and its panel reads or converts one.

### What the guards are

pandoc can run code: `--filter`, `--lua-filter` and a format name ending in `.lua` all execute a file. The cell keeps the original plugin's two gates on that:

- **Format names** must be built-in pandoc names — letters, digits and underscores, with `+ext`/`-ext` modifiers — so nothing that looks like a path reaches `-f` or `-t`.
- **`extra_args`** takes only `--toc`, `--number-sections` and a one-token `-Vkey=value`. Anything else is refused, naming the flag.
- **The category** is a subfolder of `output_dir`; a `category` that climbs out of it (`..`, a leading slash) is refused.

A file written outside `output_dir`, when a call asks for one by name, is allowed and the answer says so: `(outside ~/Documents)`.

### What the rewrite leaves out

The original also built spreadsheets and slide decks (`xlsx`, `xlsb`, `ods`, `odp`, `pptx`) with python-docx, openpyxl, python-pptx and odfpy, wrote PDFs with WeasyPrint's HTML renderer, OCR'd images and scanned pages through anydoc and LiteParse, and redacted and re-inserted PDF text through PyMuPDF (`doc_edit_pdf`). This cell has none of those libraries: it writes documents with pandoc (markdown, docx, pdf, html, odt, epub, rst, latex), writes PDFs through LaTeX, and refuses an image or a scanned page by name instead of OCR'ing it. `doc_read_meta` answers what the filesystem knows rather than what a parser extracted. The plugin's MCP server, its loopback HTTP lane, its virtualenv bootstrap and its `documents.convert`/`documents.create` module layout are packaging for another harness, and are not here.

## License

MIT. The tools, their schemas and their guards are dyiapanis's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
