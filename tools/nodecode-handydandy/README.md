# nodecode-handydandy

HandyDandy: warns when a chat session grows too long and asks the agent to write a structured handoff for a fresh one. A Nodecode rewrite of [percyhendrux123-prog/hermes-handydandy](https://github.com/percyhendrux123-prog/hermes-handydandy) by [percyhendrux123-prog](https://github.com/percyhendrux123-prog).

## Get it

Tell the agent "add the handydandy cell", or run `nodecode add nc://handydandy`.

## Set it up

No key and no service: the cell reads the turn's own meter. Its `handydandy` section is optional, and the defaults are the whole setup for most people:

```jsonc
"handydandy": {
  "mode": "advise",            // advise: one nudge. handoff: the agent writes a handoff with the tool.
  "max_input_tokens": 100000,  // 0 turns this check off
  "context_percent": 50,       // share of the model's context window; 0 turns it off
  "max_rounds": 40,            // 0 turns this check off
  "handoff_dir": "",           // empty: the home's handoffs folder
  "session_log_csv": ""        // empty: no log
}
```

## Use it

Once a session passes its limit, the session is told once, in the prompt of the round about to be asked, and the operator gets one notice naming the file its handoff goes to. In `advise` mode the line is a nudge; in `handoff` mode it asks for the handoff, again at each turn's first round until the file is there.

The `handoff` tool writes that file: one markdown page with the orders and approvals still in force, the state of each workstream with paths and ids, the open items and who owns each, the next action and what was not touched, for a fresh session to read first. It lands at `<handoff_dir>/HANDOFF-<date>-<session>.md`, and the next request says where it is.

`/handydandy` shows where the session stands and offers to forget the notice; `/handydandy path` names the file a handoff would go to, `/handydandy reset` forgets the notice so the limits are checked again from the next request. In eval, `(handydandy:status)` answers the same line, and `(help :handydandy)` is the manual.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The whole cell: the size limits, the one line a long session is told, the handoff tool and `/handydandy`. |
| `package.lisp` | The `nodecode-handydandy` package, nicknamed `handydandy`, and its one condition. |
| `config.example.jsonc` | The section, commented. |

## License

MIT. HandyDandy is Copyright (c) 2026 Deploy Axiom: see `LICENSE` and `NOTICE`.
