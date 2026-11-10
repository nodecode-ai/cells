# nodecode-lowrouter

LowRouter: its EU-hosted, OpenAI-compatible routes as a Nodecode provider, filtered to the ones a turn can run on, with each route's real context window and your credit balance in euros. A Nodecode rewrite of [carboniferio/hermes-lowrouter-provider](https://github.com/carboniferio/hermes-lowrouter-provider) by [carboniferio](https://github.com/carboniferio).

## Get it

Tell the agent "add the lowrouter cell", or run `nodecode add nc://lowrouter`.

## Set it up

Make a key at <https://lowrouter.ai/app/keys> and save it with `/connect`
(pick LowRouter), or leave it in `LOWROUTER_API_KEY`. Without a key the
picker still fills — `/v1/models` answers anonymously — and the balance
waits for one.

Optional, and rarely needed — the section is on unless it says
`"enabled": false`:

```jsonc
{
  "lowrouter": {
    // where the API is served, appended /chat/completions, and where
    // /models and /credits are read. LowRouter's own address; a relay's
    // goes here.
    "base_url": "https://api.lowrouter.ai/v1"
  }
}
```

## Use it

`/models` lists the routes an `auto/` or `alias/` id names, and a turn on
one goes to `https://api.lowrouter.ai/v1/chat/completions` with the key as
its bearer.

The picker is the filtered catalog, not the raw `/v1/models` list: an
`auto/<creator>/<model>` id (the one that fails over between European and
global hosts) or the account's own `alias/<name>`, a chat model, one that
takes tool calls, with at least 64,000 tokens of context. Pinned
`<host>/<creator>/<model>` routes are left out because the `auto/` id
already covers them, as are embeddings and realtime models. Each route
carries the window the catalog publishes for it, so compression runs when
the model's own window fills rather than at one number assumed for the whole
endpoint.

`alias/coder` (or whichever alias you made) is usually the good default:
an alias picks a model once, for every session.

`/lowrouter` shows your credit balance in the currency LowRouter bills in
(euros, for an EU account) and how many routes the picker holds;
`/lowrouter models` reads the live catalog, lists the routes that take tool
calls and takes them into the picker at once.

LowRouter bills in euros and reports the amount of each response in
`usage.cost`. Nodecode counts cost in US dollars, so a LowRouter session
reads with an unknown cost, and the balance above is the amount left.

## What this rewrite carries, and what it leaves to Nodecode

The plugin registers one Hermes model provider: a base, a key variable, a
filter over `/v1/models`, context windows read from the same answer and
cached an hour per key and base, `/v1/credits` on `/usage`, four fallback
models, and an auxiliary model.

- **The plugin's model filter** is the cell's, unchanged: `auto/` and
  `alias/` ids, the chat modality, `tools` among the supported parameters,
  and a 64,000-token floor with an unpublished window passing.
- **The window lookup.** Nodecode keeps a catalog model's window beside
  the model, so the live answer's `context_length` becomes the row's
  window; there is no per-key cache to keep, because the worker refetches
  the catalog hourly and a restart fetches again.
- **`/usage`.** Nodecode's cost accounting is a dashboard's, not a
  command's; the balance is `/lowrouter` here, read from the same
  `/v1/credits` answer and formatted as the plugin formats it.
- **The auxiliary model.** The plugin points Hermes' background work (context
  compression, summaries) at `auto/mistralai/mistral-small-2603`; Nodecode's
  is `general.auxiliary_model`, which the README here can only suggest.
- **The fallbacks.** The plugin's four models stay as the picker's rows
  until the live catalog answers, and again whenever it does not.
- **Nothing is converted.** The plugin does not convert EUR to USD, and
  neither does this cell.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-lowrouter
```

Every request is a stubbed `nlk:http` and every key variable a stubbed
`nle::credential-env`: nothing reaches LowRouter.

Apache-2.0 licensed.
