# nodecode-identity

Identity: view and edit SOUL.md, MEMORY.md, USER.md and AGENT.md, switch profiles, and see which line the model last leaned on. A Nodecode rewrite of [indigokarasu/hermes-identity-plugin](https://github.com/indigokarasu/hermes-identity-plugin) by [indigokarasu](https://github.com/indigokarasu).

## Get it

Tell the agent "add the identity cell", or run `nodecode add nc://identity`.

## Set it up

Nothing is required. The cell reads the four files from `identity/` under the Nodecode home, and `dir` names another folder:

```jsonc
{
  "identity": {
    "dir": "~/notes/me",     // where the four files live
    "stale_days": 21,        // a line nothing repeated this long is listed stale
    "track": true,           // stamp a line when the model's words or a tool call repeat it
    "prompt": true           // the files ride every turn's standing context
  }
}
```

## Use it

Ask the agent to read or change one of the files, or type `/identity` in a shell:

- `/identity` — the dashboard: how much of each file the model has used, and what has gone stale.
- `/identity SOUL.md` — one file, a line a row, each with when it was last used.
- `/identity all` — every line, used or not.
- `/identity profiles`, `/identity switch NAME` — the roster, and the profile a new launch boots into.
- `/identity reset` — clear every stamp.

The same is callable from eval: `(identity:view "SOUL.md")`, `(identity:edit "SOUL.md" :mode "append" :content "Name: ...")`, `(identity:dashboard t)`, `(identity:profiles)`, `(identity:switch-profile "work")`, `(identity:reset-usage)`. `(help :identity)` carries the whole manual while the cell runs.

Three tools take the same work from the model, so it never has to write a path: `identity_view`, `identity_edit`, `identity_dashboard`, `identity_list_profiles`, `identity_switch_profile` and `identity_reset_usage`.

**How a stamp is earned.** The files ride the prompt every turn, so a stamp cannot mean "was in context". It means "the model said it": after each assistant message, and after a tool call whose arguments name one of the four files, every line whose first 60 characters — or whose `key: value` value, when that value is at least 12 characters — appears in that text is stamped. It is a heuristic, and it reads the model's words, never the operator's.

## Files

| File | What it is |
| --- | --- |
| `cell.lisp` | The cell: the section, the six tools, `/identity`, the manual, the standing section, the two stamping hooks. |
| `test/cell-test.lisp` | The lifecycle every cell shares, and the tools, stamps, restart and section over a scratch folder. |

## License

MIT, as the original. See `LICENSE` and `NOTICE`.
