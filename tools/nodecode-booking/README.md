# nodecode-booking

Booking: free time and real bookings on your own Google Calendar. A Nodecode rewrite of [kyle-bartlett/booking-bridge](https://github.com/kyle-bartlett/booking-bridge) by [kyle-bartlett](https://github.com/kyle-bartlett).

Ask "what is free on Thursday afternoon", "book a strategy consult tomorrow at 10", "move that to Friday", "cancel it". Availability is Google's own FreeBusy answer cut into slots, never a guess, and every write re-checks it, so a time that filled since the last look is refused instead of double booked.

## Get it

Tell the agent "add the booking cell", or run `nodecode add nc://booking`.

## Set it up

You need a Google Cloud project with the Calendar API enabled and an OAuth client of type **Desktop app**, plus a refresh token for that client. Then export the two secrets in the shell that starts Nodecode, under the names the cell's section reads:

```sh
export GOOGLE_CALENDAR_CLIENT_SECRET=...     # the Desktop app client's secret
export GOOGLE_CALENDAR_REFRESH_TOKEN=...     # a refresh token for calendar.events and calendar.freebusy
```

The refresh token is the one Google hands an OAuth client once you have consented; any OAuth client you already use for your own calendar can mint one, and the scopes to grant are `https://www.googleapis.com/auth/calendar.events` and `https://www.googleapis.com/auth/calendar.freebusy`.

Tell the agent "set up the booking cell" to write the section, or write it yourself:

```jsonc
"booking": {
  "calendar": "primary",
  "client_id": "<the Desktop app client id>", // public, so it lives in the section
  "client_secret_env": "GOOGLE_CALENDAR_CLIENT_SECRET",
  "refresh_token_env": "GOOGLE_CALENDAR_REFRESH_TOKEN",
  "utc_offset": "-06:00",   // empty reads this machine's own zone
  "duration_minutes": 30,
  "timeout": 20
}
```

The cell refuses to start until `client_id` and both variables answer, and says which one is missing. `/booking` then proves the grant with one call and lists the next five events.

## Use it

| Tool | Does |
| --- | --- |
| `booking_list_availability` | genuinely free slots in a window of at most 14 days, cut to the consult length |
| `booking_list_upcoming` | the next events with their ids, titles, times, links and status |
| `booking_book_event` | books one consult; refuses on a conflict, and the same booking twice is one event |
| `booking_reschedule_event` | moves an event to a free time, keeping its length when `end` is left out |
| `booking_cancel_event` | deletes one event |

The three writes need `confirm: true`, which the model sets only after you have agreed to that exact time; without it the answer is a refusal and nothing changes. Read-only tools never need it.

Times are RFC3339 (`2026-11-17T09:00:00-06:00`) or a bare `2026-11-17T09:00`, read at the call's `utc_offset`, else the section's, else this machine's zone, and every answer writes times back at the same offset. A booked event carries the private property `bookingBridgeKey`, the same idempotency mark the original plugin wrote, and attendees you list are emailed an invitation.

`/booking` shows whether the grant answers and what is coming; `/booking help` prints the manual, and `(help :booking)` has it too.

## Where it goes

Only `oauth2.googleapis.com` (the refresh grant) and `www.googleapis.com` (Calendar v3). The access token lives in memory for the process. No shell, no files, no telemetry.
