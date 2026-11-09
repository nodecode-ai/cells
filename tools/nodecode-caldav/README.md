# nodecode-caldav

CalDAV Calendar: list and read the events of one calendar collection you run, read-only, as two model tools. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-event-read](https://github.com/TakeshiTGAL/hermes-plugin-event-read) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

## Get it

Tell the agent "add the caldav cell", or run `nodecode add nc://caldav`.

## Set it up

The cell reads one CalDAV collection — the calendar your own server already
holds, such as Nextcloud, Radicale or Baikal. Three settings, and the server
running is the fourth thing:

```jsonc
{ "caldav": {
    "url": "http://127.0.0.1:5232/user/calendar/",
    "user": "your-account",
    "password": "your-password",
    "days": 7,
    "max_events": 50,
    "timeout_seconds": 20
} }
```

| Member | Default | Notes |
| --- | --- | --- |
| `url` | — | the collection's own URL, http or https, with a host and a non-empty path; no user name, query or fragment |
| `user` | — | the account name, sent as HTTP Basic on every request |
| `password` | — | the password; left empty, the variable `password_env` names is read |
| `password_env` | `CALDAV_PASSWORD` | the variable holding the password |
| `days` | `7` | the window a `list_events` call with no arguments reads |
| `max_events` | `50` | how many rows one answer shows before it says the list is cut |
| `timeout_seconds` | `20` | seconds one request may take |

A section naming no collection, no user or no password refuses the start and
says which one is missing, so neither tool is offered on a calendar the cell
cannot read; set it in `/settings` and `(restart-cells "nodecode-caldav")`.
Over plain `http` the password travels unencrypted — use `https`, or a server
on this machine. A server that answers `404`, `401`, or HTML instead of a
multistatus says so in the message: the status is named.

## Use it

Ask what is on the calendar and the model calls `list_events`, which sends one
`calendar-query` REPORT with `Depth: 1` and answers one block an event: the
summary, the start and end as the calendar writes them, a `tzid` when the
value carries one, whether it repeats, and the uid. `read_event` takes a uid
and answers that one event; a CalDAV text match is a substring match, so the
exact uid is picked out of the answer and a uid that merely contains it is not
returned.

From eval, the same calls:

```lisp
(caldav:list-events)                                    ; the section's days
(caldav:list-events "20261108T000000Z" "20261115T000000Z")
(caldav:read-event "uid-from-a-list")                   ; one event by uid
```

`/caldav` reads the section's days; `/caldav 3` three days; `/caldav today`,
`/caldav tomorrow` and `/caldav week` the days they name. `(help :caldav)` is
the same guidance the model reads, and `*primer*` is that text, so a layer can
reword it.

Nothing is converted and nothing is expanded: a DATE stays a date, a value
ending in `Z` has no `tzid`, a missing `DTEND` is an end that is not stated,
and a recurring event is one row whose start is the master's with `repeats` on
it. An empty window is `No events in ...`, not an error.

## What it changes, and what it does not

It is read-only: no event is created, edited or deleted, no scheduling reply
is sent, and no file is written. The single network request is the REPORT to
the `url` you set — redirects are not followed by the image's HTTP call and no
other host is named — and the credential goes nowhere but that collection.
