# nodecode-foundry

Foundry: one sentence in, a working Nodecode cell out — a folder watched for one file type, or a JSON URL polled for new items. A Nodecode rewrite of [dolepee/hermes-plugin-foundry](https://github.com/dolepee/hermes-plugin-foundry) by [dolepee](https://github.com/dolepee).

Say what you want watched or polled and the foundry writes the cell:

> watch ~/Invoices for new PDF files every 30 minutes

> poll https://example.com/items.json every 15 minutes

What it writes is a cell like any other, working rather than a placeholder:

| File | What it is |
| --- | --- |
| `nodecode-<slug>.asd` | the system, and the test system its own slice runs |
| `cell.lisp` | the section, a tool, a `/`command, a worker and the state file of what it has already reported |
| `test/cell-test.lisp` | the lifecycle every cell's folder carries, and one answer |
| `README.md` | what the cell is, and how to use it |
| `config.example.jsonc` | the section, as an example |
| `CATALOG.md` | the draft that says what it is |

Both archetypes report what is new since the last report and remember it, so
nothing is reported twice: the watcher lists the folder for one extension, the
poller reads one JSON URL and reads `items`, `results` or `data` — the array
itself, or the array under one of those names.

## Get it

Tell the agent "add the foundry cell", or run `nodecode add nc://foundry`.

## Set it up

Nothing is required.

- `generated_dir` — where a scaffolded cell is written; empty means this
  home's `foundry/generated/`.
- `max_items` — how many new items a generated tool reports in one answer
  (default 20).

## Use it

The model calls `foundry_plan`, `foundry_build`, `foundry_test`,
`foundry_list` and `foundry_catalog`; you type `/foundry`:

| Verb | What it does |
| --- | --- |
| `/foundry plan <sentence>` | the spec the sentence reads as, writing nothing |
| `/foundry build <sentence>` | write `nodecode-<slug>/` under the root, scan it, read it |
| `/foundry list` | what is under the root |
| `/foundry test <slug>` | scan and read one again |
| `/foundry catalog <slug>` | its catalog draft |

One directory is the whole write surface: the root, symlinks followed, so a
path outside it refuses before anything is written. A second build into the
same folder refuses rather than overwrite.

The cell is **written, not installed**: read `cell.lisp`, then put the folder
under `~/.nodecode/cells/` and it loads at the next boot, or
`(restart-cells "nodecode-<slug>")` starts it now.

## What the check is

`foundry_test` reads the generated text for markers — `#.` and a process
launch — and runs the Common Lisp reader over every file with evaluation off.
Nothing generated is executed, here or at build time, so neither the scan nor
the read is proof; a person reads the code before installing it.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the request reader, the write, the check, the five tools and `/foundry` |
| `templates.lisp` | the two archetypes, as text |
| `test/` | the slice: the parser, the build, the fence, the verbs |

## License

MIT. The original is Copyright (c) 2026 Iyanda Abdulqodir Adedolapo: see `LICENSE` and `NOTICE`.
