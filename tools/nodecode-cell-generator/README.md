# nodecode-cell-generator

Cell Generator: scaffold a complete Nodecode cell — system, declaration, handlers, tests — from a short description, and add a tool or a command to one it wrote. A Nodecode rewrite of [iruzen-dono/hermes-plugin-gen](https://github.com/iruzen-dono/hermes-plugin-gen) by [iruzen-dono](https://github.com/iruzen-dono).

## Get it

Tell the agent "add the cell generator cell", or run `nodecode add nc://cell-generator`.

## Set it up

Nothing is required.

- `directory` — where a scaffolded folder goes. Empty means this home's `cells/`, the folder the next boot loads.
- `max_tools` — how many tools one scaffold may declare (default 8).

## Use it

Ask for a cell: *"make a cell called weather with a lookup tool that takes a city"*. The agent calls `cell_scaffold`, and the folder it writes is a cell like any other:

| File | What it is |
| --- | --- |
| `nodecode-<name>.asd` | the system, and the test system its own slice runs |
| `cell.lisp` | the package, a schema and a handler for each tool, and the `nle:define-cell` declaration: section, tools, `/name` command, manual line |
| `README.md` | what the cell is, and how to use it |
| `test/` | the slice, and the lifecycle tests every cell's folder carries |

- `cell_add_tool` splices one more tool — schema, handler, `:tool` clause — into a cell it wrote.
- `cell_add_command` splices one more slash command into one.

The handlers a fresh cell gets answer a placeholder: edit `cell.lisp` to make them answer for real, then `(restart-cells "nodecode-<name>")` loads it now instead of at the next boot.

A name is letters, digits and dashes — no dot, no slash — so a scaffolded folder is always inside the cells folder, and a folder already there is refused, never overwritten. A tool or command added later goes in at a marker line the scaffold writes, so a hand-written cell with no markers refuses by name instead of being half-patched.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The three verbs, the templates they write, and the one declaration. |
| `package.lisp` | The package and the cell's condition. |
| `test/` | What is proved: a scaffolded folder loads, starts, its tool answers, its command is there; a name that would leave the cells folder refuses; a patch lands. |

## License

MIT. The original is Copyright (c) 2026 Iruzen: see `LICENSE` and `NOTICE`.
