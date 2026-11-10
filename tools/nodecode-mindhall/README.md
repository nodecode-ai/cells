# nodecode-mindhall

Mindhall: one shared mind of about-you notes, work, dates and a signed change log, read at every turn. A Nodecode rewrite of [jai-srinivas/mindhall](https://github.com/jai-srinivas/mindhall) by [jai-srinivas](https://github.com/jai-srinivas).

Every AI a person uses remembers a different version of them: one was told the launch
moved, another still has the old date. Mindhall is one small file they all read and
write instead. It has four rooms -- **About you** (lasting facts and rules), **Work**
(a project, client or course and where it stands), **Dates** (a day and what happens
on it) and a **Change Log** where every write is signed with the name the writing agent
runs as. Before each turn the model reads a short block of it: about-you notes, the
work not done, the next two weeks of dates and the latest changes.

## Get it

Tell the agent "add the mindhall cell", or run `nodecode add nc://mindhall`.

## Set it up

Nothing has to be set: the cell starts on `~/.mindhall/mindhall.json`, signs changes
with the name `Nodecode`, and needs no key and no service. The `mindhall` section of
`config.jsonc` (or `/settings`) is there when the defaults do not fit.

| Member | Default | What it does |
| --- | --- | --- |
| `store` | `~/.mindhall/mindhall.json` | the mind's file: point another AI or harness at the same path and it reads the same mind |
| `agent` | `Nodecode` | the name every change is signed with, as the Change Log shows it |
| `inject` | `true` | put the mind's short block in front of every turn |
| `max_chars` | `1800` | the character ceiling on that block |
| `block_changes` | `5` | how many recent changes the block carries |
| `days_ahead` | `14` | how many days ahead its dates reach |
| `max_text` | `500` | the longest note, work name, next step or date label |

## Use it

Say what changed and the model saves it in the same turn:

> The Mindhall launch moved to the 22nd, and I want shorter answers from now on.

`mindhall_set_date` moves the date it already has rather than adding a conflicting
second one, and `mindhall_add_note` keeps the preference in About you, signed. A later
session -- or another agent on the same file -- starts with:

```
About the user:
  - Wants shorter answers
Work:
  - Mindhall [Active, next: port it]
Upcoming dates:
  2026-11-22  Launch
Latest changes:
  2026-11-08  Nodecode: Moved Launch from 2026-11-20 to 2026-11-22
```

The model has five tools: `mindhall_read` (one room, or all of it), `mindhall_add_note`,
`mindhall_update_work`, `mindhall_set_date` and `mindhall_remove`. The operator has
`/mindhall`, and `/mindhall about|work|dates|changes` for one room. The same verbs are
callable from an eval form, since they answer strings:

```lisp
(mindhall:add-note "Ships on Fridays")
(mindhall:set-date "Demo" "2026-12-04")
(mindhall:read-mind "dates")
```

## The mind

One JSON document, indented so a person can read it and an outside tool can write it:

```json
{
  "about": [{"id": 1, "note": "Wants shorter answers"}],
  "work": [{"id": 1, "name": "Mindhall", "status": "Active", "step": "port it"}],
  "dates": [{"id": 1, "day": "2026-11-22", "what": "Launch", "work": ""}],
  "changes": [{"id": 1, "at": "2026-11-08T09:30:00Z", "by": "Nodecode",
               "room": "Dates", "summary": "Moved Launch from 2026-11-20 to 2026-11-22"}]
}
```

A write reads the file, changes one record and replaces the file in one atomic move, so a
refused request writes nothing and a crash mid-write leaves the old mind. Each tool call
is also recorded in the working context, and a mind that cannot be read costs a turn
nothing: the block is simply left out.

## Credit

The four rooms, the five tools, the block in front of a turn and the signed change log
are jai-srinivas's, from the Hermes plugin this cell rewrites. See NOTICE for what
changed and what was left out.
