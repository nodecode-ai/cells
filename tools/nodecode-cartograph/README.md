# nodecode-cartograph

Cartograph: maps a local repository and returns the source excerpts a coding task needs. A Nodecode rewrite of [anthony-maio/cartograph](https://github.com/anthony-maio/cartograph) by [anthony-maio](https://github.com/anthony-maio).

## Get it

Tell the agent "add the cartograph cell", or run `nodecode add nc://cartograph`.

## Set it up

Nothing to install and no key: the cell reads the files itself, in Lisp. The
optional `cartograph` section of the config bounds what it may read:

```jsonc
{
  "cartograph": {
    "allowed_roots": ["/absolute/path/to/code"],  // absent = any directory; [] = none
    "ignore": ["fixtures"],                // extra directories to skip
    "max_files": 2000,                     // code files read before the walk stops
    "source_chars": 8000,                  // the longest excerpt of one file
    "max_output_chars": 40000              // the ceiling on one answer
  }
}
```

## Use it

Ask for a map of a repository, or for the code a task touches:

```text
Map ~/code/my-project with cartograph_analyze; then cartograph_context on the
same repository for "validate provider names and require API keys", and read
the files it names before changing anything.
```

The model gets two tools; the operator gets `/cartograph`:

- `/cartograph /path/to/repo` — the map: how many code files, how many import
  edges resolved inside the repository, the ranked files and the dependency
  hubs.
- `/cartograph /path/to/repo validate provider` — the excerpts to read for
  that task, each with its line range.
- `/cartograph` alone maps the session's own directory, and
  `/cartograph <task>` selects from it.

From eval the same two are `(cartograph:analyze "/path/to/repo")` and
`(cartograph:context "/path/to/repo" "validate provider")`, and
`(help :cartograph)` carries the primer while the cell runs.

## What it does

- Walks the repository, skipping `.git`, `node_modules`, `dist`, `target` and
  the other package stores and build output.
- Counts lines and reads each file's import forms — Python, JavaScript,
  TypeScript, Go, Rust, Ruby, Java, Kotlin, C, C++, C#, Swift, Elixir and Lisp
  — and resolves them to files of the same repository, relatively or by a
  module path.
- Ranks the files by a PageRank over those edges, so the modules everything
  imports come first, and lists them as dependency hubs.
- For a task, matches its words against paths and bodies, then adds the tests
  that name a chosen file, and returns an excerpt from the line that carries
  the most of them, with the line range it came from.

Static and local: nothing is executed, no model is called, nothing is sent
anywhere. The import reading is lexical, so an edge it misses is a miss and
not a fact, and an excerpt can be truncated — read the file before editing it.

## What the rewrite leaves out

The original plugin ran an installed Node CLI (`@anthony-maio/cartograph`) and
so also offered its LLM-backed wiki, its prepare-a-packet command, its MCP
server and its cache of artifacts. Here the analysis is the cell's own Lisp, so
there is nothing to install, and the answer is the map and the excerpts only.

## License

MIT. Copyright (c) 2026 Anthony Maio: see `LICENSE` and `NOTICE`.
