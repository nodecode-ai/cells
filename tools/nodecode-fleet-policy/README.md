# nodecode-fleet-policy

Fleet Policy: wake a scheduled turn only when an MCP read tool has something new. A Nodecode rewrite of [Julientalbot/hermes-fleet-policy](https://github.com/Julientalbot/hermes-fleet-policy) by [Julientalbot](https://github.com/Julientalbot).

The original asks one question of a Hermes agent: what did it actually send to
its provider, what does each cron job cost, and is there anything new to look
at? It answered from Hermes's own hooks, its managed scope and its cron store.
Nodecode already reports its own turns and its own cron spend, so this rewrite
keeps the piece no cell had: the **wake gate**. `fleet_news` calls one read
tool on one MCP server, remembers the ids it has answered before, and reports
only the items nobody has seen -- `{"wakeAgent": false}` when there are none.
A scheduled prompt that begins with that answer spends no model call on "no".

## Get it

Tell the agent "add the fleet-policy cell", or run

```sh
nodecode add nc://fleet-policy
```

## Set it up

One thing is required: the MCP endpoint every watch calls.

```jsonc
{
  "fleet-policy": {
    "server_url": "http://127.0.0.1:8787/mcp",
    "token_env": "MY_MCP_TOKEN"          // optional: the NAME of an env var, never the token
  }
}
```

Set it in `/settings`, on the page's Settings tab, or by hand in
`~/.nodecode/config.jsonc`, then `(restart-cells "nodecode-fleet-policy")`.
An endpoint that is not an `http://` or `https://` address is refused at the
start, in the loader's own words.

## Use it

The model calls the tool; you can call the same verb from `eval`:

```lisp
(fleet-policy:watch-new-items :tool "LIST_MAIL" :id-field "id"
                               :show-fields '("subject" "from.name"))
```

- The first call for a watch records a **baseline** and reports nothing.
- Every later call reports the items whose id was never seen, one short line
  each, then marks them seen. Nothing is reported twice.
- Nothing new answers `{"wakeAgent": false}` -- the stock wake gate.
- `:dry-run t` prints what a call would answer and records nothing.
- The state is one small file per watch under `fleet-policy/` in the Nodecode
  home (`~/.nodecode/fleet-policy/` by default, `state_dir` moves it). A
  corrupt state file is a refusal, not a silent fresh start, and a read that
  failed records nothing, so a broken endpoint never marks items as seen.

`/fleet-policy` lists what each watch remembers; `/fleet-policy LIST_MAIL`
prints what that tool would report now, as a dry run.

### What counts as an item

Every outermost object in the answer that carries a scalar `id_field`, at any
depth -- including a JSON document a relay wrapped in a string. Inside an item
nothing is searched, so a nested id (an item's author, say) is an attribute,
not an item of its own. An answer that says `successful: false`, or carries a
non-empty `error`, is a failed read: the ids are left alone and the call
refuses in words.

### In a scheduled prompt

```
First run (fleet-policy:watch-new-items :tool "LIST_MAIL")
through eval. If it answers {"wakeAgent": false}, stop here and say nothing.
Otherwise summarise the new items and what they need.
```

That is the whole gate: no new items, no model work.

## Credit

A rewrite of Julientalbot's hermes-fleet-policy; see `NOTICE`. MIT, the
original's licence; see `LICENSE`.
