# nodecode-plan-canvas

The agent's plan as boxes, whose status comes from its real work. A Nodecode rewrite of [0xbyt4/hermes-canvas](https://github.com/0xbyt4/hermes-canvas) by [0xbyt4](https://github.com/0xbyt4).

A canvas is one plan for one chat: boxes, each naming the files it writes and the
command that proves it. The agent draws it, and then the cell keeps it honest --
a write into a box's files makes that box `doing`, and the exact check command's
exit code makes it `done` or `failed`. The agent never sets a status that the
work can set. A file written that no box planned shows up as a drift box, which
the operator can adopt as planned work or ask the agent to undo.

## Get it

Tell the agent "add the plan-canvas cell", or run `nodecode add nc://plan-canvas`.

## Set it up

Nothing has to be set. Canvases live under the home in `plan-canvas/`; set the
section's `folder` to keep them elsewhere, and `keep_edits` to change how many of
the operator's edits one canvas holds.

The cell has four tools the agent calls and one command the operator types:

| tool | what it does |
| --- | --- |
| `canvas_draw` | draw the plan: a title, a project root, and boxes (id, label, files, check, after, options) |
| `canvas_update` | change the plan with ops applied together: add, remove, restore, rename, set_files, set_check, set_after, set_status, answer, move, adopt, link, unlink |
| `canvas_get` | the whole canvas, plus the operator's edits the agent has not read yet |
| `canvas_next` | the boxes that can start now, each with a ready `delegate_task` goal |

## Use it

Ask for a plan before the code: "draw the plan for this payments change, with a
check for each box". The agent calls `canvas_draw` and puts the returned card
line in its reply.

Then, in the shell:

```
/plan-canvas                  the plan: every box, its status, its check
/plan-canvas next             the boxes that can start now
/plan-canvas note api keep the write and read in one module
/plan-canvas answer cache redis
/plan-canvas adopt drift-1    keep a file the agent wrote outside the plan
/plan-canvas remove ui        remove a box; writes into its files are refused
/plan-canvas restore ui       put it back
/plan-canvas drop             delete this chat's canvas
```

A note, an answer, an adopt and a removal reach the agent as the operator's own
words: it reads them in `canvas_get`, and hears them at its next request.

The cell reads what the harness already reports -- the files a tool call wrote, and
the shell command it ran with its exit -- so nothing has to be installed for a box
to change status. A check is run by the agent as the box names it: `(sh "npm test
-- payments")` inside an `eval` call, the exact command, nothing appended.
