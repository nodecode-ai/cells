# nodecode-bento

Bento: subscribers, tags, fields, events, statistics, broadcasts and transactional email as fifteen model tools. A Nodecode rewrite of [shagghiesuperstar/hermes-plugin-bento](https://github.com/shagghiesuperstar/hermes-plugin-bento) by [shagghiesuperstar](https://github.com/shagghiesuperstar).

## Get it

Tell the agent "add the bento cell", or run `nodecode add nc://bento`.

## Set it up

Three values from your Bento account (Settings, API Keys, at
[app.bentonow.com/account/teams](https://app.bentonow.com/account/teams)): the
publishable key (`pk_...`), the secret key (`sk_...`) and the site UUID. Paste
them into `publishable_key`, `secret_key` and `site_uuid`, or leave each empty
and the cell reads the variable the original read -- `BENTO_PUBLISHABLE_KEY`,
`BENTO_SECRET_KEY`, `BENTO_SITE_UUID`.

Then name the verified Bento Author addresses that may send in `allowed_senders`;
with that list empty, every scheduled broadcast and every transactional email is
refused. A section naming no credentials refuses the start and says so, and no
tool is registered until they are set. Then `(restart-cells "nodecode-bento")`,
or set them in `/settings`.

```jsonc
{ "bento": { "publishable_key": "", "secret_key": "", "site_uuid": "",
             "allowed_senders": ["news@yourbrand.com"] } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `publishable_key` | — | `pk_...`; empty reads `BENTO_PUBLISHABLE_KEY` |
| `secret_key` | — | `sk_...`; empty reads `BENTO_SECRET_KEY` |
| `site_uuid` | — | the site every call targets; empty reads `BENTO_SITE_UUID` |
| `base_url` | Bento's | only `https://*.bentonow.com` or `http://localhost` is accepted |
| `allowed_senders` | `[]` | verified Author addresses allowed as From; empty refuses every send |
| `environment` | `production` | `disabled` refuses every Tier 2 and Tier 3 call; `staging` behaves like production and still emails real people |
| `bulk_threshold` | `25` | an import larger than this previews first (Tier 2) |
| `min_schedule_lead_minutes` | `30` | a scheduled broadcast must start at least this far ahead |
| `max_batch_size_per_hour` | `5000` | the upper bound for a broadcast's pacing |
| `max_recipients_per_call` | `10` | the most messages one `bento_email_send` carries (Bento's own cap is 60) |
| `allow_transactional_override` | `false` | permit `transactional=true`, which sends even to unsubscribed people |
| `timeout_seconds` | `30` | seconds one request may take |

Bento keys are user-level and reach every team and site: treat them like a root
credential, and use one Nodecode profile per Bento site.

## Use it

Ask the model to look someone up, tag them, import a list, list the broadcasts,
read the stats, or draft a broadcast. In the conversation and from eval:

```lisp
(bento:subscriber "ada@example.test")                  ; one person: fields, tags, unsubscribed_at
(bento:tags)                                           ; every tag, cached five minutes
(bento:upsert '(("email" . "ada@example.test") ("tags" . ("vip"))))
(bento:tag "ada@example.test" "add" "vip")            ; Tier 1, immediate
(bento:stats :kind "site")
(bento:broadcast "October news" "Hello" "<p>...</p>" "Brand" "news@yourbrand.com"
                 :inclusive-tags '("vip"))
```

The fifteen tools, by tier:

| Tool | Tier | What it does |
| --- | --- | --- |
| `bento_subscriber_get` | T0 | one subscriber by email or uuid |
| `bento_tag_list`, `bento_field_list` | T0 | every tag / field, cached five minutes |
| `bento_broadcast_list` | T0 | broadcasts by status (no status answers only sent ones) |
| `bento_stats` | T0 | site, segment, report and ads statistics |
| `bento_subscriber_upsert` | T1, T2 above `bulk_threshold` | import or upsert 1-1000 people; async, no Flows |
| `bento_subscriber_tag` | T1 | add or remove one tag on one person |
| `bento_tag_create`, `bento_field_create` | T1 | a definition, refusing one that exists |
| `bento_broadcast_create` | T1 | a draft only; a person sends it from the dashboard |
| `bento_subscriber_consent` | T2 | subscribe or unsubscribe one person |
| `bento_event_track` | T2 | custom events; can fire Flows that email people |
| `bento_purchase_track` | T2 | one `$purchase`, keyed by the real order id |
| `bento_broadcast_schedule` | T3 | create and approve a broadcast for `send_at` |
| `bento_email_send` | T3 | transactional email through Bento |

Every Tier 2 and Tier 3 call previews first: `dry_run` defaults to true and
answers the exact request body plus a `confirm_token`. The real call repeats the
same parameters with `dry_run=false` and that token; change anything and it is
refused, so a token cannot send something nobody previewed. A dry run performs no
POST and costs no quota.

`/bento` prints the status (site, base, allowed senders) without a request;
`/bento tags`, `/bento fields`, `/bento broadcasts` and `/bento stats` make that
one read. `(help :bento)` answers the guidance the model reads.

## What it does not do

It sends real email: `bento_broadcast_schedule` and `bento_email_send` cause
Bento to mail real people, and the two event tools can trigger Flows that do. The
documented API has no cancel endpoint for a scheduled broadcast, so cancelling is
dashboard-only. Bento is the processor: the subscriber data you read or write is
exchanged with Bento. There is no telemetry and no self-update; the only files
written are `bento/audit.jsonl` under the home. The original's human-approval
hook and its cross-process claim lock did not come across; see NOTICE.
