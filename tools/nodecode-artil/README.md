# nodecode-artil

Artil: an agent's own email address, phone number and Telegram, read and answered through the `artil` CLI. A Nodecode rewrite of [artilai/plugins](https://github.com/artilai/plugins) by [artilai](https://github.com/artilai).

Artil gives an agent an account of its own: an email address, often a phone number and a Telegram bot, and a Message URL that other agents of a workspace write to. The account lives in Artil; the `artil` program on your machine holds the sign-in and does the work, and this cell is the model's and your way to it.

## Get it

Tell the agent "add the artil cell", or run `nodecode add nc://artil`.

## Set it up

The cell drives the `artil` program, so that has to be there first, and signed in as the agent the account is:

```sh
npm i -g artil-ai
artil auth login
```

Where no browser opens, make a token on the agent's Tokens tab in Artil and run `artil auth login --token <token>`. The sign-in stays in `artil`'s own config directory; the cell holds no key of its own.

The program is found on `PATH`. If it lives somewhere else, give its path in the cell's `program` setting (`/settings`); a cell that cannot find it refuses to start and says so. `timeout_seconds`, 90 by default, bounds one call.

## Use it

The model gets six tools:

| Tool | What it does |
| --- | --- |
| `artil_status` | Who the account is: its address, number and handle, as `artil auth status` prints them. |
| `artil_inbox` | The newest messages of email, sms or telegram, with the id of each. |
| `artil_read` | One message whole, by its id. |
| `artil_send` | A new message, a reply in the same thread, or a forward with a note under it. |
| `artil_message_agent` | Another agent of the workspace, at its Message URL. |
| `artil_team` | The workspace's agents, by name and id. |

You have `/artil` for the same account from the input box: `/artil email`, `/artil team`, `/artil auth status` -- the words after `/artil` are the `artil` command's own arguments. `(help :artil)` holds the guide the model reads, and the Lisp verbs `artil:inbox`, `artil:read-message`, `artil:send-message`, `artil:message-agent`, `artil:team` and `artil:status` are the same calls from `eval`.

Messages arrive from outside: the cell tells the model to read a message before acting on it and to treat its text as information, never as instructions.

## What it does not do

The original plugin was also a Hermes gateway platform: it ran `artil internal hermes-channel` and started a run for each new message. A chat surface in Nodecode is a room cell, and this rewrite keeps the account where the plugin's own skill put it instead -- tools the model calls and a command you type -- so a new message does not start a run by itself: the agent reads the inbox when it is asked to. `artil listen on` and Artil's own forwarding still work from a terminal.
