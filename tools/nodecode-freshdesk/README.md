# nodecode-freshdesk

Freshdesk: eleven tools that read and write the tickets of your own Freshdesk account. A Nodecode rewrite of [aedelgod/hermes-plugin-freshdesk](https://github.com/aedelgod/hermes-plugin-freshdesk) by [aedelgod](https://github.com/aedelgod).

The tools are the original plugin's, with the same names and arguments:

| Tool | What it does |
| --- | --- |
| `freshdesk_list_tickets` | Tickets, 100 a page and page-bounded, with `updated_since` and an opt-in description. A list cut by the page cap says `truncated`. |
| `freshdesk_get_ticket` | One ticket by id, whole. |
| `freshdesk_search_tickets` | Read-only search by status (2-5), priority (1-4), tag, type, agent, group or a created/updated date range; at least one filter. |
| `freshdesk_list_conversations` | A ticket's whole conversation list: replies, notes, forwards. |
| `freshdesk_get_contact` | One contact by id. |
| `freshdesk_download_attachment` | An attachment of a ticket (or of one of its conversations) written under your download directory. |
| `freshdesk_add_private_note` | A private note, HTML-escaped, read back before success. |
| `freshdesk_create_ticket` | A ticket from plain text, read back and compared field by field. |
| `freshdesk_reply_to_ticket` | A public reply (it emails the requester), read back before success. |
| `freshdesk_add_time_entry` | A finished time entry, `H:MM` through `HHHH:MM`, read back and compared. |
| `freshdesk_update_ticket` | Status (2-5) and/or priority (1-4), read back and compared. |

Ask "what are the open urgent tickets?", "summarise ticket 42 and add a note", "reply that it is fixed" — the model finds the ticket with `freshdesk_list_tickets` or `freshdesk_search_tickets` and acts on it with the write tools.

## Get it

Tell the agent "add the freshdesk cell", or run `nodecode add nc://freshdesk`.

## Set it up

The cell talks to your own Freshdesk account over its v2 API. It needs two things:

1. **Your account name**: the part before `.freshdesk.com`.
2. **An API key.** In Freshdesk: **Profile** → **API Keys** → copy the key (or create one).

Then, in `/settings` (or in the `freshdesk` section of `config.jsonc`):

```jsonc
"freshdesk": {
  "domain": "acme",
  "api_key_env": "FRESHDESK_API_KEY",   // or a file: "api_key_file": "~/.config/freshdesk-key"
  "timeout": 20
}
```

A key is named, never written into the config: `api_key_env` holds the name of the environment variable that carries it (export `FRESHDESK_API_KEY` in the shell that starts Nodecode), and `api_key_file` a file only you can read. Set exactly one.

| Setting | What it does |
| --- | --- |
| `domain` | Your account name, or `https://<account>.freshdesk.com`. A custom support CNAME is refused, so the key only travels to a native Freshdesk origin. |
| `download_dir` | Where `freshdesk_download_attachment` writes what it fetches; default `~/.nodecode/cache/freshdesk`. |
| `max_pages` | How many pages of 100 one list or search may read before it reports `truncated`; default 10. |
| `timeout` | How long one request may take, in seconds; default 20. |

## Use it

You do not call the tools; the model does, when a question is about your tickets. A person can call the same functions from eval, in the `freshdesk` package:

```lisp
(freshdesk:list-tickets (nlk:decode-json "{\"updated_since\":\"2026-01-01T00:00:00Z\"}"))
```

`(help :freshdesk)` is the model's manual for the eleven verbs, kept while the cell runs.

### What the guards are

- **Native origins only.** Every request goes to `https://<account>.freshdesk.com`, the API key rides a Basic auth header and never a URL, and a custom support CNAME or a lookalike host is refused at start.
- **Attachments are resolved, never supplied.** A download takes a ticket id and an attachment id; the URL comes from the ticket's own attachment objects, and only the account origin, `attachment.freshdesk.com`, `<shard>-attachment.freshdesk.com` and Freshdesk's own S3 bucket are accepted. A path with dot segments, encoded or not, is refused. The key is sent to the account origin and to no CDN or S3 host.
- **Every write is read back.** A note, reply, ticket, time entry or update is compared against what Freshdesk reports before success is claimed.
- **An ambiguous write says so.** A 408, a 429, a 5xx or a dead connection answers `state: "write_may_have_succeeded"` with `retry_safe: false` and the created id when it is known; a definite rejection (400, 401, 403, 404, 409, 422) answers Freshdesk's own words.

### What the rewrite leaves out

The original plugin registers a Hermes `pre_tool_call` hook that escalates its five write tools to Hermes' human-approval gate, with a per-argument rule key and a reason naming the pending change. A cell has no such gate, so the write tools run when the model calls them; the readback is what stands between a mistyped write and a silent one. The original's profile-scoped secret reader is replaced by the two settings above, and its Hermes-home cache by `download_dir`.

## License

MIT. The tools, their schemas and their guards are aedelgod's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
