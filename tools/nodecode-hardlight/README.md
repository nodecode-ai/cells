# nodecode-hardlight

Hardlight UI: frontend design guidance with original style grammars, and local tools that measure contrast, a page graph's navigation and a project's own source. A Nodecode rewrite of [nosliwhtes/hardlight-ui](https://github.com/nosliwhtes/hardlight-ui) by [nosliwhtes](https://github.com/nosliwhtes).

## Get it

Tell the agent "add the hardlight cell", or run `nodecode add nc://hardlight`.

## Set it up

Nothing is required: the guides, the style atlas and all five tools work as they arrive, and nothing leaves the machine.

Two optional settings, in the `hardlight` section of `~/.nodecode/config.jsonc` or from `/settings`:

```jsonc
"hardlight": {
  "root": "",        // the frontend project hardlight_inspect reads when a call names none
  "max_files": 120   // how many files one inspection reads before it stops (1..500)
}
```

## Use it

Ask for design work, or call the tools directly:

- `hardlight_reference` reads one of the shipped guides (`workflow`, `navigation`, `states`, `typography`, `color`, `motion`, `review`, `systems`, `browser`, `layout`, `platform`, `performance`), the whole style atlas, or one exact style grammar by ID.
- `hardlight_direction` turns a brief into a deterministic direction packet: three candidate grammars for the surface (or the exact IDs you name), the facts still missing, an unanswered design worksheet and a verification matrix.
- `hardlight_contrast` measures opaque foreground/background pairs against the `text`, `large_text` or `ui` threshold.
- `hardlight_navigation` analyzes a declared page graph: shortest paths per task in your own link order, unknown targets, one label reaching several pages, unreachable pages and dead ends.
- `hardlight_inspect` walks a frontend project, read-only and bounded: the files it read, the CSS custom properties it found, and advisory findings for missing image alt text, placeholder links and removed focus outlines.

`/hardlight help | styles | version | TOPIC` runs the same reading from the prompt, and `(help :hardlight)` carries the seven-step workflow the cell is meant to be used with.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The workflow, the section, the five tools and `/hardlight`. |
| `data.lisp` | The shipped guides and the style atlas, read from the cell's own folder. |
| `contrast.lisp`, `navigation.lisp`, `direction.lisp`, `inspect.lisp` | The four checks, each behind a declared contract. |
| `data/references/` | The original's twelve guides, unchanged. |
| `data/atlas.json`, `data/platform-features.json` | The original's style atlas and platform ledger, unchanged. |

## What the checks do not prove

A contrast ratio is one painted pair on a solid surface. A navigation analysis is one declared graph, not a browser. An inspection is a bounded heuristic sweep that executes nothing. None of them certifies usability, beauty or WCAG conformance, and each answer says so.

## License

MIT. Copyright (c) 2026 Seth Wilson and Hardlight UI contributors: see `LICENSE` and `NOTICE`.
