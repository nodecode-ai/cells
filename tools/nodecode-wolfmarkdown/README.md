# nodecode-wolfmarkdown

WolfMarkDown: checks, cleans and proves Markdown files, and shows what a rewrite kept. A Nodecode rewrite of [WolfMarkTools/WolfMarkDown](https://github.com/WolfMarkTools/WolfMarkDown) by [WolfMarkTools](https://github.com/WolfMarkTools).

A document is checked three ways. **Shape**: an unclosed fence, a heading that skips a level, a table without a delimiter row, trailing whitespace, three blank lines in a row, a list marker the document does not use throughout, an adjacent citation pair (`[6][26]`, which GFM reads as one reference link), and whether the file already formats to itself. **Integrity**, against an untouched snapshot you name: every URL, version, hash, ISO date, percent, money amount, path, environment name and code span the source carried is still in the rewrite, no value changed into another of its class, every ordered item kept the position it had, and every ledger record (`[12] https://...`) came through on a line of its own, in order. **Form**: a canonical rewrite — one blank line at most, blank lines where GFM needs them, one unordered marker, a space between adjacent citations, one newline at the end — with fenced code kept byte for byte.

A PASS is Markdown quality proven against a source. It is not a claim that the content is true, and nothing here says so.

## Get it

Tell the agent "add the wolfmarkdown cell", or run `nodecode add nc://wolfmarkdown`.

## Set it up

Nothing is required: the cell reads the document and needs no key, no network and no package. The three settings are optional.

```jsonc
"wolfmarkdown": {
  "list_style": "-",        // the unordered marker clean writes and verify expects: -, * or +
  "max_issues": 20,         // how many findings one report prints before it counts the rest
  "write_by_default": false // whether markdown_clean writes the file without being told to
}
```

## Use it

- `markdown_verify` — a report: PASS or FAIL, one line per check, then each finding with its line and how to fix it. Pass `source` to prove content integrity against the untouched snapshot.
- `markdown_clean` — the canonical form, and with `source` it writes nothing when a protected token would be lost. `write_file: true` puts it on disk; without it the cleaned text comes back.
- `markdown_outline` — the source map a rewrite is planned from: headings in order, fenced blocks, table lines, ordered lists, record runs, and the protected tokens each class carries.
- `/markdown verify PATH`, `/markdown clean PATH`, `/markdown outline PATH`, or `/markdown PATH` to check that file.

Calling one: name the file the user asked about, and when the file is a rewrite of something
else, name the snapshot as `source` — that is the check that catches a lost URL or a renumbered list.

## What it does not do

There is no GFM parser, no Prettier and no markdownlint here: the checks are line-oriented and say so. Setext headings, HTML blocks and a fence inside a deep list item are left alone rather than guessed at, the canonical form is this cell's own rather than Prettier's byte for byte, and the token classes are the ones the original protected, not every number in the text.

## License

MIT. WolfMarkDown is Copyright (c) 2026 WolfMark Tools: see `LICENSE` and `NOTICE`.
