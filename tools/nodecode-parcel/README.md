# nodecode-parcel

Parcel Deliveries: what is on the way, from the Parcel app, and a tracking
number added to it. A Nodecode rewrite of
[promptclickrun/hermes-parcel](https://github.com/promptclickrun/hermes-parcel)
by [promptclickrun](https://github.com/promptclickrun).

## Get it

Tell the agent "add the parcel cell", or run `nodecode add nc://nodecode-parcel`.

## Set it up

Parcel's API is a **Parcel Premium** feature: generate a key at
[web.parcelapp.net](https://web.parcelapp.net) and put it in the `parcel`
section, inline or in the variable `api_key_env` names. Nothing else is
required, and the cell refuses to start until a key is readable.

| Key | Default | Purpose |
| --- | --- | --- |
| `api_key` | empty | the Parcel API key; left empty, the variable `api_key_env` names is read |
| `api_key_env` | `PARCEL_API_KEY` | the environment variable holding that key |
| `api_base` | `https://api.parcel.app/external` | Parcel's external API; keep the default |
| `cache_seconds` | `600` | how long a delivery list is reused before a new read |
| `request_timeout` | `15` | seconds one request may take (at most 60) |
| `reads_per_hour` | `20` | Parcel's own hourly read limit, counted locally |
| `adds_per_day` | `20` | Parcel's own daily add limit, failed adds included |

## Use it

Ask in ordinary words — "what is arriving today?", "where is my UPS
package?", "track this number, it is the running shoes" — or use the command:

```
/parcel
/parcel recent
```

Three tools answer the model:

- `parcel_deliveries` lists what is arriving: status, expected date, latest
  event; `filter: recent` adds what was already delivered, `tracking_number`
  narrows it to one package, `include_events` adds the history.
- `parcel_add_delivery` starts tracking a package: a tracking number, a
  Parcel carrier code (look one up with `parcel_carriers`), a short
  description, and a postcode or order email where the carrier needs one.
- `parcel_carriers` finds a carrier code by name; it costs no request.

Parcel's limits are strict: 20 reads an hour and 20 adds a day, failed adds
included, and its deliveries endpoint serves Parcel's own cache, so polling
harder never freshens a list. The cell reuses a list for ten minutes, keeps a
local count and refuses the request past the limit instead of letting Parcel
reject it, and answers the last list marked `stale` when the limit is spent or
Parcel is unreachable. A typo in an add — an unknown carrier, a tracking
number of two characters, a carrier that needs a postcode — is caught before
it is sent, so it cannot burn one of the twenty.

The `parcel` section's counts cover this process only; other apps using the
same key share Parcel's limit.

## What leaves the machine

Only Parcel: `deliveries/` (GET), `add-delivery/` (POST) and
`supported_carriers.json` (GET, sent without the key) on
`https://api.parcel.app/external`. The key rides as the `api-key` header and
is never printed, logged or answered back. Nothing is written to disk: the
cache and the request counts live in the running process. There is no shell
command, no background thread and no telemetry.

Unofficial: the cell is not affiliated with or endorsed by Parcel.

## Run its tests

```sh
.github/test-cell.sh tools/nodecode-parcel
```

The tests stub the cell's one HTTP seam and never dial.
