# nodecode-sequenzy

Sequenzy: email marketing for SaaS and ecommerce teams. A Nodecode rewrite of [Sequenzy/skills](https://github.com/Sequenzy/skills) by [Sequenzy](https://github.com/Sequenzy).

The agent works the operator's own Sequenzy account: it looks up and adds subscribers, reads the lists, tags and segments it can target, lists campaigns and their state, reads delivery and engagement numbers, and sends one transactional email. Every call goes to `https://api.sequenzy.com` with the operator's API key.

## Get it

Tell the agent "add the sequenzy cell", or run `nodecode add nc://sequenzy`.

## Set it up

Paste a key from the Sequenzy dashboard, **Settings → API keys**, into the cell's `api_key`, or put it in the environment variable `SEQUENZY_API_KEY` and leave `api_key_env` at its default:

```jsonc
{
  "sequenzy": {
    "api_key_env": "SEQUENZY_API_KEY",
    "max_rows": 25
  }
}
```

Two more members are optional: `company_id` pins one workspace when the key may reach several (it rides as `x-company-id`, as the vendor's own CLI sends it), and `base_url` points the cell at another address than the vendor's.

The key's own scopes decide what works: a key with only `subscribers:read` answers a campaign list with HTTP 403, and one without the send scopes cannot send. `sequenzy_account` names the scopes and whether live sends are allowed, so the agent can ask before it tries.

## Use it

The model gets seven tools:

| Tool | What it does |
| --- | --- |
| `sequenzy_account` | The companies the key may act in, the current one, its scope count and whether it may send live. |
| `sequenzy_subscribers` | One subscriber by email (profile: tags, attributes, list names, notes), or a list filtered by status, tag, list, segment or free text. |
| `sequenzy_add_subscriber` | Add or merge a contact, with a `duplicate_strategy` of `skip`, `merge` or `overwrite`. |
| `sequenzy_campaigns` | Campaigns with their state and dashboard addresses, or one by id. Read-only. |
| `sequenzy_stats` | Sent, delivered, opened, clicked, bounced, replies and opt-outs, with rates: the account's for 7d, 30d or 90d, or one campaign's. |
| `sequenzy_audience` | Every list, tag and segment, with the ids and names the other tools take. |
| `sequenzy_send` | One email: a saved template `slug`, or `subject` and HTML `body`. Answers an accepted send id, not a delivery. |

The operator gets `/sequenzy` — no argument shows the account and the week's numbers, an argument searches subscribers — and five read-only verbs for eval: `(sequenzy:account)`, `(sequenzy:stats :period "30d")`, `(sequenzy:campaigns :status "draft")`, `(sequenzy:campaign "camp_123")`, `(sequenzy:subscriber "someone@example.com")`, `(sequenzy:audience)`, `(sequenzy:subscribers :query "gmail.com")`.

Sending is real: mail goes to real recipients and counts against the plan. A send answers as soon as Sequenzy accepts it, so pass one `idempotency_key` per logical email and reuse it on every retry — a retry with the same key and the same request returns the original send instead of sending twice.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the seven tools, the verbs and `/sequenzy`. |
| `test/cell-test.lisp` | The lifecycle, each tool and verb against a stubbed wire. |

## License

MIT. The plugin it rewrites is Copyright (c) 2026 Sequenzy: see `LICENSE` and `NOTICE`.
