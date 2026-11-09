# nodecode-iwork

iWork Studio: Apple Numbers, Keynote and Pages files - read a document, set a cell, edit slides, export, undo. A Nodecode rewrite of [Arkanji/iwork-studio](https://github.com/Arkanji/iwork-studio) by [Arkanji](https://github.com/Arkanji).

## Get it

Tell the agent "add the iwork cell", or run `nodecode add nc://iwork`.

## Set it up

Put the folders this cell may touch in `~/.nodecode/config.jsonc`:

```jsonc
{
  "iwork": {
    "folders": ["~/Documents", "~/Desktop"],
    "backups": 10,
    "timeout": 180
  }
}
```

The same fields are in `/settings`. `folders` is required: the cell reads and writes inside those folders and refuses everything else, so nothing else on disk can be reached through it.

It needs a Mac with Numbers, Keynote or Pages installed - the cell drives the real apps through the `osascript` macOS ships. No key, no network, no extra program.

## Use it

Ask the agent for what you want done, or call the tools from eval:

- `iwork_read` - what a document says: a Numbers table's cells, a Keynote deck's slides with their titles and bodies, a Pages document's body text.
- `iwork_set_cell` - set one Numbers cell (`ref` like `B2`), in the app. The cell is read back afterwards; when the app did not keep what was written, the file is put back and the tool says so. `dry_run` touches nothing.
- `iwork_slides` - list a Keynote deck's slides, add one, set a slide's title and body, or delete one (never the last).
- `iwork_export` - PDF, and xlsx from Numbers, pptx from Keynote, docx from Pages. A new file is never overwritten unless you say so.
- `iwork_backups` - every write with its backup; `action: restore` puts a file back byte for byte.

`/iwork` shows whether this is a Mac, which of the three apps are installed, the folders in force and the last writes. `(help :iwork)` is the same primer the model reads.

Every write goes: the fence check -> a byte-exact backup -> the app's own edit -> the file read back and compared with what was asked -> a receipt. A refusal touches nothing.

## What is in it

| File | What it is |
| --- | --- |
| `jxa.lisp` | The JavaScript for Automation each app runs, and the one seam that runs it: `osascript -l JavaScript`, parameters as one JSON object in, one JSON object back. |
| `cell.lisp` | The five tools, their contracts, the fence, backups, receipts, `/iwork` and the manual. |

## What it does not do

No formulas, number formats or cell styling, no design kits or brand extraction, no CSV import, no thumbnails, no rendered-slide review, no Slideshow control, and no reading of an iWork file without its app. Arabic text round-trips as the app stores it, without the original's byte-exact guarantee - see `NOTICE`.

## License

MIT, Copyright (c) 2026 Arkanji: see `LICENSE` and `NOTICE`.
