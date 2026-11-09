# nodecode-office

Office Desk: guarded Word, PowerPoint and Excel edits - tracked Word changes, text replacements with a backup and a revertible receipt for every write. A Nodecode rewrite of [DECRUX9812/hermes-office](https://github.com/DECRUX9812/hermes-office) by [DECRUX9812](https://github.com/DECRUX9812).

## Get it

Tell the agent "add the office cell", or run `nodecode add nc://office`.

## Set it up

Put the folders you let it edit on the desk. In `~/.nodecode/config.jsonc`:

```jsonc
{
  "office": {
    "folders": ["~/Documents", "/Users/me/Work"],
    "author": "Nodecode",   // the name Word tracked changes carry
    "preview": 12          // how many blocks office_inspect shows
  }
}
```

The same fields are in `/settings`. Nothing else is needed: no key, no Office install, no LibreOffice, no network.

## Use it

Ask the agent for what you want done, or call the tools from eval:

- `office_inspect` - what a .docx, .pptx or .xlsx says: its paragraphs, slides or cells, with the Word revision count.
- `office_search` - where a phrase is, by paragraph, slide or cell reference.
- `office_replace` - replace text in Word paragraphs, PowerPoint text frames or Excel shared strings, in place or into a new file.
- `docx_tracked_edit` - replace a stretch of Word text as a tracked change: a `w:del` and a `w:ins` under your author name, so a reviewer accepts or rejects it in Word.
- `office_receipt` - every write with its digests and backup; `action: revert` puts a file back byte for byte.

`/office` shows the folders on the desk and the last writes. `(help :office)` is the same primer the model reads.

Every write goes: the desk check (the file must be inside one of your folders) -> a byte-exact backup -> the edit, written to a temporary file and renamed -> a receipt. A refusal touches nothing.

## What is in it

| File | What it is |
| --- | --- |
| `zip.lisp` | The ZIP container a .docx, .pptx or .xlsx is: read (chipz inflates), repack (untouched parts keep their bytes, edited ones are stored). |
| `office.lisp` | The XML, read as text: paragraphs, runs, slides, cells, shared strings, replacement, and the tracked-change splice. |
| `cell.lisp` | The five tools, their contracts, the desk guard, backups, receipts, `/office` and the manual. |

## What it does not do

No comments, no accept/reject of existing revisions, no slide composition or diff, no Excel cell/formula writes, no LibreOffice recalculation, and no Desk page with workstreams - see `NOTICE`.

## License

MIT, Copyright (c) 2026 DECRUX9812 and hermes-office contributors: see `LICENSE` and `NOTICE`.
