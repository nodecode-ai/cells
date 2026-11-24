# nodecode-reaction-ledger

Reaction Ledger: the emoji reactions left in your chats, by emoji, chat and day. A Nodecode rewrite of [olric3015/hermes-plugins](https://github.com/olric3015/hermes-plugins) by [olric3015](https://github.com/olric3015).

A reaction is the cheapest thing anyone says to a bot, and nothing remembered
it: the thumbs up on an answer, the heart on a summary, the cross on a
command that failed. This cell keeps one row per reaction left on a message
in a chat room your bot serves - discord, telegram, slack - and answers
`/reaction-log`, so you can see which emoji people actually use, on how many
messages, in how many chats, and how recently.

## Get it

Tell the agent "add the reaction-ledger cell", or run `nodecode add nc://reaction-ledger`.

It reads the channel kit, so it needs a room of yours on - the Discord or
Telegram bot, or any other kit room. With no room on, the ledger is simply
empty and the command says so.

## Set it up

Nothing to set: no key, no endpoint, no dependency. The one setting is
optional, and `/settings` holds it:

```jsonc
{
  "reaction-ledger": {
    "rows": 5000        // how many reaction changes the ledger keeps
  }
}
```

## Use it

```
/reaction-log             last 24 hours, by emoji
/reaction-log all         everything in the ledger
/reaction-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
/reaction-log days        add to any of the above to group by day, newest first
/reaction-log recent      add to any of the above to list the newest 10 reaction changes instead
/reaction-log recent 25   the newest 25 instead (1 to 100)
/reaction-log clear       delete the ledger
```

Each message counts once, with the reactions it has now, so adding a thumbs
up and taking it back again cancels out:

```
Reactions, last 24 hours:

emoji  messages  chats  last
👍            6      2  4m ago
❤             3      1  1h ago
all           9      2  4m ago

Each message counts once, with the reactions it has now.
```

A report is also one eval call, for a summary you want to paste somewhere:

```lisp
(reaction-ledger:summary :window "all" :by "day")   ; the table as a string
(reaction-ledger:recent :window "7d" :limit 25)        ; the newest reaction changes
(reaction-ledger:clear)                                 ; delete the ledger
```

## What it keeps

The ledger is `reaction-ledger/reactions.jsonl` under the home, one JSON
object a line: the room, the emoji, whether it was set or taken back, the
time, and 12 hex digits of a SHA-256 over the chat and over the message.
Never the chat or message id itself - a Telegram chat id names a person or a
group - never who reacted, and never anything said in the chat. The oldest
rows are dropped past the section's `rows`.

Nothing leaves this machine: no network call, no telemetry, no account. The
cell only watches the rooms: it cannot answer a reaction, and a row it fails
to write is a row it loses, never a message that fails.

MIT licensed; see [LICENSE](LICENSE) and [NOTICE](NOTICE).
