# nodecode-gatekeeper

Gatekeeper: consent, opt-out, quiet-hours and wording rules for the agent's outbound messages. A Nodecode rewrite of [percyhendrux123-prog/hermes-gatekeeper](https://github.com/percyhendrux123-prog/hermes-gatekeeper) by [percyhendrux123-prog](https://github.com/percyhendrux123-prog).

It holds the rules a person set for being messaged -- who agreed to be messaged on which channel, who said STOP, what hours are theirs, which wording was agreed -- and answers PASS, APPROVE or BLOCK for one message before it goes out. It sends nothing itself and reaches no network: the ledger is one file under the home.

## Get it

Tell the agent "add the gatekeeper cell", or run `nodecode add nc://gatekeeper`.

## Set it up

One setting is required: `gatekeeper.owner_offset`, your own UTC offset as `+HH:MM` (or `-HH:MM`), used to date a consent record and as its default offset. Set it in `/settings` or on the Settings tab.

The rest have defaults:

```jsonc
"gatekeeper": {
  "owner_offset": "+00:00",      // required: your own UTC offset
  "require_consent": true,        // a consent record for this exact recipient and channel
  "start": "08:00",               // permitted window, recipient-local, inclusive
  "end": "21:00",                 // exclusive
  "min_gap_seconds": 0,           // least gap between two sends in one thread
  "templates": [],                // approved routine wording; empty asks for every message
  "stop_keywords": ["STOP"]       // the opt-out words you record
}
```

## Use it

The model has two tools, so a message is checked before it is sent and noted after:

- `gatekeeper_check channel recipient message [thread]` answers **PASS** (routine: send it), **APPROVE** (ask the person first) or **BLOCK** (do not send), with the rule that decided.
- `gatekeeper_record channel recipient outcome [thread]` says what happened: `sent`, `failed`, or `reply` when they answered, which clears the thread.

You have `/gatekeeper`:

```
/gatekeeper                                  the ledger's state and the usage
/gatekeeper log [1-50]                       the decisions taken, newest first
/gatekeeper consent <channel> <recipient> [where and how they agreed]
/gatekeeper optout <recipient>              an absolute suppression
/gatekeeper revoke <channel> <recipient>    take the consent back
/gatekeeper reply <channel> <recipient> [thread]
/gatekeeper variables <channel> <recipient> name=Ada;city=Oslo
```

The rules, in the order they decide:

1. An **opt-out** blocks absolutely: a new consent and an approval do not override it.
2. A **missing consent** for that exact recipient and channel blocks.
3. **Quiet hours** outside the window block -- the window is the recipient's local time, so a consent record carries their UTC offset.
4. An **unanswered thread** (a send with no reply noted), a send inside `min_gap_seconds`, and **wording** that matches none of your templates ask for approval rather than blocking.

A template is compared exactly, after `$name` or `${name}` is filled from the variables you stored for that recipient: `Hello $name, the build finished.`

The ledger keeps a SHA-256 of each recipient and each message body, with the channel, the thread, the decision and its time -- no address book and no message text in clear. It is one file, `gatekeeper/ledger.sexp` under the Nodecode home, written owner-only.

## What it does not do

It gates what the model brings to `gatekeeper_check`; it cannot stop a tool that sends without asking. A message sent by a room or a channel cell on its own, or by a scheduled run, is only gated if something calls the tool first. `stop_keywords` documents the words you record; this cell does not read inbound traffic to observe them.

Run `.github/test-cell.sh tools/nodecode-gatekeeper` from the cells repository.

## License

MIT, the original's. See `LICENSE` and `NOTICE`.
