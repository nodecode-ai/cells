# nodecode-phone-call

Phone Call: an approved outbound phone call with an AI disclosure, and what came of it. A Nodecode rewrite of [winchxyz/hermes-phone](https://github.com/winchxyz/hermes-phone) by [winchxyz](https://github.com/winchxyz).

The agent writes a brief, you approve it, and only then is anything dialed. The
callee hears an AI disclosure first and then the message you approved. You get
the brief back, the state of the call, the thing it said, and — when the call
was recorded — a link to what the other side answered.

## Get it

Tell the agent "add the phone-call cell", or run `nodecode add nc://phone-call`.

## Set it up

A real call needs a [Twilio](https://console.twilio.com) account and a number
of your own. In `/settings`, or in the `phone-call` section of `config.jsonc`:

```jsonc
"phone-call": {
  "account_sid": "AC...",              // console.twilio.com
  "auth_token": "...",                  // the account's auth token
  "from_number": "+14155550100",       // a number on that account
  "allowed_numbers": ["+14155550123"], // what may be dialed; empty dials nothing
  "user_name": "Ada",                  // who the call is on behalf of
  "daily_call_limit": 10,
  "max_duration_s": 300
}
```

`/phone-call check` says which settings are set (names only, never a value),
how the allowlist stands, and whether calls are real or dry runs. With no
Twilio account every call is a dry run: it is prepared, approved and reported,
nothing is dialed, and the cell says so.

The allowlist is the safety valve and it fails closed: `allowed_numbers` empty
means no number may be dialed at all, and `allow_all: true` is the developer's
explicit way past it. `NODECODE_PHONE_CALL_ALLOWLIST`, a comma-separated list
in the environment, is read too.

Quiet hours are off by default, as in the original. Turn them on with
`enforce_quiet_hours: true` and `callee_utc_offset` (hours from UTC): calls
outside 08:00 to 21:00 callee time are then refused unless the brief says
`override_quiet_hours`.

## Use it

> "Call Bella Napoli on +14155550123 and book a table for four on Friday at
> eight; my name is on the booking."

The agent prepares the brief with `phone_prepare` and shows you the card: who,
which number and where it came from, the goal, the exact opening line, what the
call will and will not share, the limits, and the one command that dials.

| Command | What it does |
| --- | --- |
| `/phone-call approve <id>` | approve this call and place it: one use, fifteen minutes |
| `/phone-call cancel <id>` | cancel an unapproved card |
| `/phone-call status [<id>]` | where the call stands, or the recent log |
| `/phone-call transcript <id>` | what the call said, and the recording link |
| `/phone-call consent <id> <field>` | allow one sensitive field on one call |
| `/phone-call log [<n>]` | the recent calls |
| `/phone-call check` | which settings are set, and how calls go out |

| Tool | Use |
| --- | --- |
| `phone_prepare` | write the brief and the approval card; never dials |
| `phone_call` | dial a call you approved, by its id |
| `phone_status` | one call's state and result, or the log |
| `phone_transcript` | what was said, and the recording of the answer |

`(help :phone-call)` is the manual the model reads before it reaches for any of
them.

### What the original taught, kept here

Nothing is dialed without the operator's approval for that exact call, valid
fifteen minutes and usable once. Emergency numbers (911, 112, 999, 000, 110,
119, 100 to 103 and the rest of the original's list), premium-rate numbers and
short codes are refused before and again at dial time. One call is live at a
time, the daily limit and the sixty-second gap to one number hold, and an
unanswered or failed call is never redialed on its own. A fact under a
sensitive name — card, address, ID number, bank, email, phone — is withheld
from the brief unless the operator grants that field for that call, and a cvv,
a password or a one-time code is never shared, consent or not. The AI
disclosure is the first sentence of every call, because Twilio speaks it first.

## Not ported

The original's FastAPI bridge: Twilio Media Streams carrying a realtime voice
session, the live transcript of both legs, and the mid-call question card that
put the callee on hold and asked the operator in chat. A cell has no public URL
to give Twilio, so the call speaks a TwiML document — the disclosure, the
approved message, an optional recording of the answer, goodbye — and the other
side's words are heard through Twilio's recording rather than transcribed.
The seven settings, the safety rules and the four tools are the original's; the
SQLite file shared by two processes is a table in this one.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-phone-call
```

Every request is a stubbed `nlk:http` and the clock is a constant, so nothing
reaches Twilio, needs an account or touches the network.

MIT licensed, as the original is.
