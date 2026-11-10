# nodecode-redaction-pack

Redaction Pack: masks 71 more vendor credential formats in the text you send and in tool output. A Nodecode rewrite of [pierrenode/hermes-redaction-pack](https://github.com/pierrenode/hermes-redaction-pack) by [pierrenode](https://github.com/pierrenode).

A token from a vendor nobody taught the harness to recognise goes out in clear
text: a deploy script that prints its Doppler token, a failing `curl` that echoes
a Vault token, a `docker login` line with a Docker Hub PAT. This cell adds 71
vendor-prefixed shapes to what gets masked on the way to the model, so those land
as a mask instead. It is a table of patterns, compiled locally, and nothing else:
no service, no model, no network, no key.

## Get it

Tell the agent "add the redaction-pack cell", or run `nodecode add nc://redaction-pack`.

## Set it up

Nothing has to be set: with an empty section all 71 formats are masked in the
user and tool text of every request, and the cell is on as soon as its folder
is. The section narrows it:

- `redaction-pack.vendors` — mask only these vendors' formats, as the table
  spells them (`HashiCorp Vault`, `Doppler`, `Tailscale`). Empty masks all 71.
- `redaction-pack.style` — how a mask reads in your own text of a request:
  `ends` (the default) keeps the first six and last four characters,
  `sentinel` names the vendor and nothing of the token.

```jsonc
"redaction-pack": { "vendors": ["HashiCorp Vault", "Doppler"], "style": "ends" }
```

A vendor name the table does not carry is refused at start, so a typo shows up
as the folder's `REFUSED` line instead of silently masking nothing.

## Use it

Nothing to type. Every request masks, on its way out:

- the **user text** of every message, keeping the first six and last four
  characters, so you can still tell which credential it was:
  `deploy failed: 401 Unauthorized for PMAK-6...e3b9 (retrying)`;
- every **tool result**, with the non-reusable sentinel
  `<<redacted: HashiCorp Vault service token>>`, because a file's content is
  what a model tends to copy back into a config file.

The log keeps the originals, so `/sessions` and every transcript still show
what was written. The system prompt and the assistant's own turns are never
read. A matcher that throws passes the text through and says so once.

Three ways to ask for it directly:

- `redact_credentials` — the tool: hand it text the model has in hand and it
  answers the masked text and what it found (`style` takes `ends` or
  `sentinel`).
- `/redaction-pack` — what is masked, how many messages so far, the last
  failure; `/redaction-pack <text>` previews any text.
- `(redact-text "…")` from eval, and `(help :redaction-pack)` for the manual.

### What is in the table

Secrets managers and keys: 1Password account Secret Key, HashiCorp Vault
service and batch tokens, Doppler (CLI, personal, service-account, SCIM, audit,
service), Amazon Bedrock long- and short-term keys. Cloud and infrastructure:
Alibaba Cloud AccessKey, DigitalOcean OAuth refresh token, Fly.io access and
macaroon tokens, Heroku API key, OpenShift user token, Scalingo API token,
Cloudflare `cfk_` key, Pulumi, Infracost, Databricks, PlanetScale, Supabase,
Tailscale keys. Model and ML platforms: Hugging Face organization token,
LangSmith, Pinecone, Weights & Biases. Observability: Grafana Cloud and service
account tokens, Sentry user and organization tokens, Dynatrace, New Relic (user,
insert, Insights), PostHog, Rootly. Developer tools and registries: Postman,
Sourcegraph, Prefect, ReadMe, RubyGems, Clojars, JFrog Artifactory, Typeform,
Frame.io, Docker Hub personal and organization tokens, Figma, Slack app
configuration refresh token, Atlassian (Jira/Confluence and organization admin),
Bitbucket Data Center, CircleCI, Buildkite, SonarCloud, Sourcegraph Cody,
Contentful, Apify, Adobe client secret. Commerce and messaging: Shopify, Square,
Shippo, Duffel, EasyPost, Brevo, HubSpot private app token, Flutterwave live and
test keys, Ramp client secret.

Each row carries the source its shape was taken from — a
[gitleaks](https://github.com/gitleaks/gitleaks) rule, a
[TruffleHog](https://github.com/trufflesecurity/trufflehog) detector, or the
vendor's own documentation — in `formats.lisp`, alongside the pattern unchanged
from the original.

### What it does not do

Formats the original left out on purpose stay out: what the harness already
masks, the ones with an open pull request upstream, and shapes no source pins
down (Supabase's newer `sb_secret_` keys have a published prefix and no published
length, so a pattern would be a guess; its `sbp_` personal and OAuth tokens are
in). A mask can only hide more text than nothing: a false positive shows up as a
masked string, never as a leaked one.

## Test it

```sh
.github/test-cell.sh tools/nodecode-redaction-pack
```

The tests run in a scratch image with no network. Every credential in them is
built at run time from a prefix and a deterministic fill, so the repository holds
no credential-shaped literal.
