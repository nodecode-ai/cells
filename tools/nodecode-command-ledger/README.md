# nodecode-command-ledger

Command Ledger: which slash commands you run, how often and when last. A Nodecode rewrite of [olric3015/hermes-plugins](https://github.com/olric3015/hermes-plugins) by [olric3015](https://github.com/olric3015).

Nodecode has dozens of slash commands - `/model`, `/new`, `/context`, `/think`,
`/cells` - and nothing remembers which ones you actually use. This cell keeps
one row per command run and answers `/command-log`, so you can see that you
switch models ten times a day, that `/compress` never gets used, or which
commands you run from a room rather than the terminal.

## Get it

Tell the agent "add the command-ledger cell", or run `nodecode add nc://command-ledger`.

## Set it up

Nothing to set: no key, no endpoint, no dependency. The one setting is
optional, and `/settings` holds it:

```jsonc
{
  "command-ledger": {
    "rows": 5000        // how many commands the ledger keeps, newest first
  }
}
```

## Use it

```
/command-log             last 24 hours, by command
/command-log session     the most recent session that ran one
/command-log all         everything in the ledger
/command-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
/command-log surfaces    add to any of the above to group by surface (cli, gateway)
/command-log days        add to any of the above to group by day, newest first
/command-log recent      add to any of the above to list the newest 10 commands instead
/command-log recent 25   the newest 25 instead (1 to 100)
/command-log clear       delete the ledger
```

Each line names the command, how often it ran, its share, in how many
sessions, and when last:

```
Slash commands, last 24 hours:

command       uses  share  sessions  last used
/model           9    53%         3    4m ago
/context         4    24%         2    1h ago
/think           4    24%         2    2h ago
total           17   100%         3    4m ago
```

A report is also one eval call, for a summary you want to paste somewhere:

```lisp
(cmdlog:summary :window "all" :by "surface")   ; the table as a string
(cmdlog:recent :window "7d" :limit 25)          ; the newest commands
(cmdlog:clear)                                  ; delete the ledger
```

The cli surface is a command typed in the terminal; gateway is one run by
eval, a room or the web page. `/command-log` itself is not counted.

## What it keeps

The ledger is `command-ledger/commands.jsonl` under the home, one JSON object
a line: the command name, the surface, the time, and 12 hex digits of a
SHA-256 over the session id. Never the arguments typed after the command - a
model name, a path, a prompt - and never the session id itself, which on a
channel names the chat and the person. The oldest rows are dropped past the
section's `rows`.

Nothing leaves this machine: no network call, no telemetry, no account. The
cell only watches - it cannot block a command or change what one answers, and
a row it fails to write is a row it loses, never a command that fails.

MIT licensed; see [LICENSE](LICENSE) and [NOTICE](NOTICE).
