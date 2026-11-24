# nodecode-rex

Rex: panes for long commands, read back, and this session's window named
and marked. A Nodecode rewrite of [kshitijk4poor/hermes-rex-plugin](https://github.com/kshitijk4poor/hermes-rex-plugin) by [kshitijk4poor](https://github.com/kshitijk4poor).

[Rex](https://github.com/kshitijk4poor/hermes-rex-plugin) is a terminal
multiplexer with a `rex` CLI. Run Nodecode inside one of its panes and the
shell's `REX_SESSION` and `REX_BLOCK` are there, so a session knows which pane
it lives in — and the model can put a dev server, a watcher or a long build in
a pane beside it instead of hiding it in the background, read what it printed,
and name its window after the task, with one glyph for what it is doing.

## Get it

Tell the agent "add the rex cell", or run `nodecode add nc://rex`.

## Set it up

The cell drives the `rex` CLI, so Rex has to be installed (it is found on
`PATH`; a cell that cannot find it refuses to start and says so). If it lives
somewhere else, give its path in the cell's `binary` setting (`/settings`).

Nothing else is needed inside a Rex pane: `session` and `block` default to
`REX_SESSION` and `REX_BLOCK`. Started from somewhere else — a `serve`, a
desktop launcher — name them:

```jsonc
"rex": {
  "binary": "rex",
  "session": "s-1a2b3c",   // empty reads REX_SESSION
  "block": "b-4d5e6f",     // empty reads REX_BLOCK
  "split": "below",        // where a new pane goes: below or right
  "label_words": 4,
  "timeout_seconds": 3
}
```

## Use it

| Tool | What it does |
| --- | --- |
| `rex_run` | Run a command in a new pane beside this one: `command`, a `label`, `split` (below or right), `keep_open`. It never takes focus, and the answer names the new pane. |
| `rex_capture` | The newest `lines` of a pane's output — this session's own, or one `pane` id from `rex_panes`. |
| `rex_panes` | Rex's sessions, and this session's windows and panes, with its own window and pane marked. |
| `rex_label` | Name this session's window in two to four lowercase words, and set one status mark: `busy` ●, `waiting` ?, `done` ✓, `stopped` ■, `failed` ✗, or `clear`. |

The same four from the shell:

```
/rex                                  # where this shell is, and a panel of rows
/rex run npm run dev                  # a pane for it, beside this session
/rex capture 40                       # the newest 40 lines of this pane
/rex panes                            # sessions, windows, panes
/rex mark done                        # ✓ on this window's label
/rex rename fix-login                 # and a bare word renames, as /title does
```

And through eval, where an ordinary command can call it:

```lisp
(rex:run "npm run dev" :label "dev-server")
(rex:capture :lines 40)
(rex:panes)
(rex:label "fix-login" :mark "done")
```

Every refusal is `ERROR: REX-ERROR` with the reason: no Rex pane (and which
variable to set), a rex call that failed (with what rex said), a command or a
label with no word in it.

## How it is put together

The CLI is one seam, `nodecode-rex::*run*`, which every verb goes through:
`rex --autostart=false …`, the session and pane passed in its environment, a
`timeout_seconds` deadline with the process killed past it. The status glyph
is a prefix on the window label read back from `rex window inspect` each time,
so a rename by the operator or the model keeps the rest of the label; only the
glyph is the cell's.

Run `.github/test-cell.sh tools/nodecode-rex` from the cells repository.
