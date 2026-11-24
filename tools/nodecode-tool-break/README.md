# nodecode-tool-break

Intelligent Tool Break: stop a stalled tool call with `/break`, steer it with `/break {message}`, or reissue it once with `/again`. A Nodecode rewrite of [apoapostolov/hermes-agent-awesome-plugins](https://github.com/apoapostolov/hermes-agent-awesome-plugins) by [apoapostolov](https://github.com/apoapostolov).

## Get it

Tell the agent "add the tool break cell", or run `nodecode add nc://tool-break`.

## Set it up

No key and no required setting: the cell runs on its defaults. Two numbers in its `tool-break` section are yours if you want them: `again_limit` (how many times `/again` may reissue one call, 2 by default) and `salvage_chars` (how much of a broken call's own output the note keeps, 8000 by default; 0 keeps none).

## Use it

While a tool call is taking longer than it is worth, type one of these in the shell. The turn is not cancelled: the call keeps running unless you cancel it, and what changes is what the model reads when the call comes back.

- `/break` — mark the newest call in flight. Its result reaches the model as a break note instead of its own output: what was broken, how long it ran, `Do not retry the same call`, and the tail of whatever the call did return, so the turn continues on what it has.
- `/break {message}` — the same, and your words ride the note as the steer: the model reads them ahead of anything else queued.
- `/again` — the note says reissue that exact call once, and carries its name and arguments so the model does not have to reconstruct them. `/again {hint}` adds a tweak. Two `/again` on the same call is the budget; a third is refused, so a model cannot loop on a call that will not finish.
- `/break --id ID` picks a call by its id, as `/break-status` spells it.
- `/break-status` — the calls in flight in this session as one JSON object: id, name, a label of what it runs, how long it has run, and how much of its `/again` budget is spent.

A call belongs to the session waiting on it: another session's `/break` leaves it alone. With nothing in flight, `/break` says so and hands a message back to you to send as an ordinary prompt.

```
/break-status
{"inflight":true,"count":1,"name":"eval","elapsed_ms":41210,
 "tools":[{"id":"c3","name":"eval","label":"eval (sh \"make test\")",
            "elapsed_ms":41210,"again_count":0,"again_disabled":false}]}
```

The model also has the vocabulary in its manual: `(help :break)`.

## How it works

One `:TOOL` hook around every dispatch, and three slash commands. The hook opens a row before the call runs — that row is what `/break-status` lists and what `/break` marks — and reads the mark after the call returns, where the note replaces the tool's own text. An unwind (Esc, a cancel) closes the row and takes any mark with it. There is no thread, no timer and no network: the tables are this image's, and a stop clears them.

What it does not do: it never signals a process and never interrupts a running body, so a call that never returns is still the shell's cancel — the same limit the original's public edition states. The `/again` budget is kept while the cell runs, not reset at each session start.

Run `.github/test-cell.sh tools/nodecode-tool-break` from the cells repository.

## License

MIT. A rewrite of apoapostolov's plugin: see `NOTICE` and `LICENSE`.
