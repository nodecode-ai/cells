# nodecode-myswarm

MySwarm: list, diagram, export (with best-effort PII pruning) and import `agents.md` swarm configurations, keeping the file an import replaces. A Nodecode rewrite of [tomkubik/hermes-myswarm](https://github.com/tomkubik/hermes-myswarm) by [tomkubik](https://github.com/tomkubik).

## Get it

Tell the agent "add the myswarm cell", or run `nodecode add nc://myswarm`.

## Set it up

Nothing is required to read or diagram an `agents.md`. To take a shared one in, the cell needs a folder of its own for the file it replaces:

```jsonc
"myswarm": {
  "data_dir": "~/.nodecode/myswarm"
}
```

`workspace` names the folder holding `agents.md`; left empty, the cell uses the working directory of the session that calls it. `default_export_filename` and `default_style` (`unicode` or `ascii`) have defaults.

## Use it

Ask, or type it:

- *"List my agents and what they do"* → `myswarm_list`
- *"Draw my agent setup"* → `myswarm_ascii`: the box-and-arrow topology, plus a pure 7-bit ASCII block you can paste into a chat
- *"Export my agents.md so I can share it"* → `myswarm_export`: a copy with emails, tokens, home paths, phone numbers, IP addresses and owner fields redacted, and how many of each it took out
- *"Import shared-agents.md and keep my old one"* → `myswarm_import`: shows the whole file, warns about what an `agents.md` can do to a model, keeps the one it replaces
- *"Put my old agents.md back"* → `myswarm_revert`

From a shell: `/myswarm list`, `/myswarm ascii`, `/myswarm export FILE.md`, `/myswarm import [--preview] [--allow] FILE`, `/myswarm revert`, `/myswarm status`. Through eval: `(myswarm:list-agents)`, `(myswarm:topology)`, `(myswarm:export-copy)`, `(myswarm:import-file "shared.md")`, `(myswarm:revert)`, `(myswarm:status)`; `(help :myswarm)` is the manual.

What it holds to:

- **PII pruning is best effort.** It redacts what looks like a secret or a person; it cannot promise it found them all. Read anything you share before you share it.
- **An import is a copy with a backup, never a merge.** The file it replaces is kept in `data_dir`, and a revert puts it back. A file the import created is taken away again.
- **The import's security reading is best effort too.** A high-severity warning — a prompt injection, a piped `curl`, a destructive command, a hunt for `~/.ssh` or `~/.aws`, shell-rc persistence, zero-width characters — stops an import until you allow it with `--allow`.

It writes in two places only: the workspace it was told to work in, and its own `data_dir`. No key, no network, no model call.
