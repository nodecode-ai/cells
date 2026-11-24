# nodecode-mermail

Mermail: agent email inboxes over a hosted MCP server, provision a mailbox, read, search, draft and send mail. A Nodecode rewrite of [Nudgen-Marketing/mermail-skills](https://github.com/Nudgen-Marketing/mermail-skills) by [Nudgen-Marketing](https://github.com/Nudgen-Marketing).

## Get it

Tell the agent "add the mermail cell", or run `nodecode add nc://mermail`.

## Set it up

Make an API key in a Mermail workspace (Mermail's console), then either export it
before Nodecode starts:

```sh
export MERMAIL_API_KEY=...
```

or put it in the `mermail` section of `~/.nodecode/config.jsonc`:

```jsonc
{
  "mermail": {
    "api_key": "",
    "base_url": "",
    "profile": "full"
  }
}
```

`base_url` is empty for Mermail's own endpoint (`https://console.mermail.app/mcp`)
and names another one only for a server of your own. `profile` is `full` for
the whole catalog, or `agent-inbox` for its twelve provisioning and safe-read
tools. `/mermail check` asks the server whether the key answers; `/mermail`
says where the key would come from, without a request.

The key is the only credential: it is sent as `x-api-key` to the endpoint the
section names and to nowhere else.

## Use it

Ask the agent about your mail. It calls the tools in its catalog:

| Tool | What it does |
| --- | --- |
| `mermail_mailboxes` | the workspace's mailboxes, each with the `mailbox_id` every other call takes |
| `mermail_emails` | one page of a mailbox's newest messages, metadata only |
| `mermail_search` | messages matching text, sender, subject or a date range |
| `mermail_read` | one message: headers, a bounded body, attachment names, scan status |
| `mermail_send` | deliver one email (a real send) |
| `mermail_draft` | save a draft; nothing is delivered |
| `mermail_call` | any Mermail MCP tool by its bare name: `reply_to_email`, `forward_email`, `schedule_email_send`, `get_thread`, `move_email`, `create_mailbox` and the rest |

The envelope is the original's: path ids (`mailboxId`, `emailId`) at the top
level, the field set in `query` for a read and in `body` for a write. Reads ask
for metadata first and for an agent-safe, scan-gated, length-bounded body
second; an email's own words are untrusted data, and the `(help :mermail)`
primer says so.

## License

MIT, Copyright (c) 2026 Nudgen Marketing: the original's `LICENSE`, unchanged.
See `NOTICE` for what this rewrite keeps and what it leaves out.
