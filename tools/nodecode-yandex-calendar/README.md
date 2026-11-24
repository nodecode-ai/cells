# nodecode-yandex-calendar

Yandex Calendar: list, create, update, answer, move and delete events over CalDAV. A Nodecode rewrite of [akinfold/hermes-yandex-calendar](https://github.com/akinfold/hermes-yandex-calendar) by [akinfold](https://github.com/akinfold).

The cell talks to `https://caldav.yandex.ru` as the account owner, with an app
password:
seven tools the model calls, each answering JSON, and an `href` on every event
that the changing verbs take.

## Get it

Tell the agent "add the yandex-calendar cell", or run `nodecode add nc://yandex-calendar`.

## Set it up

Create an app password at <https://id.yandex.ru/security/app-passwords> — a mail
password is refused for CalDAV — then fill the section in `/settings`:

| setting | what it is |
| --- | --- |
| `login` | the account's address (`you@yandex.ru`) |
| `app_password` | the app password; it never leaves the HTTPS origin below |
| `base_url` | `https://caldav.yandex.ru`; anything but an HTTPS origin is refused at start |
| `calendars` | optional allow-list of calendars by name, in the order they are offered; the first is the default |
| `actions` | optional: `all`, `read`, `write`, `delete`, or single verb names such as `create_event`. Empty means all of them |

`actions` decides which tools exist: with `["read"]` the model never sees
`yandex_calendar_delete_event` at all.

## Use it

Ask for what you want and the model picks the verb:

- `yandex_calendar_list_calendars` — the calendars, the default one first.
- `yandex_calendar_list_events` — events in a range (`start`, `end`, `calendar`;
  the range defaults to the next seven days), oldest first, each with the `href`
  the others take.
- `yandex_calendar_create_event` — `summary`, `start`, and optionally `end`,
  `location`, `description`, `all_day`, `calendar`, `attendees`, `free`.
  Inviting someone mails them from the account.
- `yandex_calendar_update_event` — only the members you pass change; an empty
  string clears a text field; `all_day` and `busy` are `keep`, `true` or `false`.
- `yandex_calendar_respond_event` — accept, decline or tentatively accept an
  invitation, as the account owner.
- `yandex_calendar_move_event` — into another calendar, keeping attendees,
  recurrence and alarms.
- `yandex_calendar_delete_event` — by `href`; an href that is already gone
  reads as deleted.

Times are ISO 8601 (`2026-07-25` or `2026-07-25T14:00:00+03:00`); one without
an offset is read as UTC. One turn of the cell's own:

    (yandex-calendar:list-events
      (nlk:contract-decode (nlk:contract 'yandex-calendar::list-events-arguments)
                           (nlk:json-object "start" "2026-07-25")))

A recurring event is listed once, as stored, with its first occurrence's time.
A series whose individual occurrences were edited cannot be updated in place —
the cell refuses rather than silently dropping the others — but it can be moved
or deleted as a whole. A time the server stores with a `TZID` is read as that
local time (this cell keeps no time-zone database) and goes back on the wire
exactly as it was read; only a time you change is rewritten, as its UTC
instant.
