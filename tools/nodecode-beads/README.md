# nodecode-beads

Beads: read, claim and update a bd (Beads) work graph from the agent, six tools over the CLI. A Nodecode rewrite of [jacobhausler/hermes-beads](https://github.com/jacobhausler/hermes-beads) by [jacobhausler](https://github.com/jacobhausler).

[bd](https://github.com/steveyegge/beads) keeps one work graph per project in a
store directory, read and written by one CLI: beads, their blockers, and who
holds what. This cell gives the model the same six verbs the plugin gave it,
each of them one fixed argv call to that CLI, so an agent and a person read
the same state and the agent can take a bead without taking someone else's.

## Get it

Tell the agent "add the beads cell", or run `nodecode add nc://beads`.

## Set it up

1. Install the bd CLI, 1.3 or newer: `brew install beads`, or a release
   tarball from [steveyegge/beads](https://github.com/steveyegge/beads/releases).
2. Run `bd init` in a project, which makes the store there (the directory
   holding `.beads/`).
3. Point the cell at it, in `/settings` or the `beads` section of
   `~/.nodecode/config.jsonc`:

```jsonc
{
  "beads": {
    "bd": "bd",                       // its name on PATH, or a path to it
    "store": "/home/you/code/myproj", // the store a call that names none uses
    "actor": "you"                     // the name claims and writes are recorded under
  }
}
```

Nothing there is a secret. `store` and `actor` are conveniences: every tool
takes a `workspace` (and the writes an `actor`) of its own, so one session
can work several stores.

## Use it

The model gets `beads_frontier` (what is ready now: blocked and claimed rows
out, epics out), `beads_show`, `beads_claim` (claimed, then read back -- a
bead another actor holds is refused with their name), `beads_update` (a
guarded write over priority, notes, due, estimate, external_ref or defer,
under an `if_assignee`/`if_status` pair: a guard that no longer holds writes
nothing and answers STALE), `beads_comment` (append-only) and `beads_smoke`
(which bd, which store, how much is ready).

/beads shows the operator the frontier, `/beads LABEL` the one label's. From
eval:

```lisp
(beads:frontier "/home/you/code/myproj")
(beads:show "/home/you/code/myproj" "bd-3")
(beads:claim "/home/you/code/myproj" "bd-3" :actor "ada")
(beads:update "/home/you/code/myproj" "bd-3" :actor "ada"
              :if-assignee "ada" :if-status "in_progress"
              :fields (nlk:make-json-object "priority" 2))
```

Closing a bead stays with plain `bd close`: the escape hatch is the product.

## What it does not do

The plugin's Hermes Desktop pane (a task tree with blocker cards) is an
Electron surface, not part of this cell. The two unguarded description/title
replacement verbs the plugin shipped disabled are left out as well; edit
content with `bd` itself.
