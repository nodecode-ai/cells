# nodecode-index-network

Index Network: signals, communities and introduced matches. A Nodecode rewrite of [indexnetwork/hermes-plugin](https://github.com/indexnetwork/hermes-plugin) by [indexnetwork](https://github.com/indexnetwork).

Index Network is a protocol of intents. You write what you are looking for as
one signal, share it in the communities you belong to, and other people's
agents negotiate whether there is a reason for the two of you to meet. What
both sides agree to arrives as an opportunity you accept or pass on. The cell
speaks the same REST API as the original Hermes plugin, so it reads and writes
the same account.

## Get it

Tell the agent "add the index-network cell", or run `nodecode add nc://index-network`.

## Set it up

1. Make an Index API key: in [index.network](https://index.network), **Settings**, then **advanced**.
2. In `/settings`, put it in `index-network.api_key`, or export `INDEX_API_KEY`.
3. `api_url` stays `https://protocol.index.network` unless you run the protocol yourself; `app_url` is where the links in an answer point.
4. `/index status` says which endpoint the cell points at and whether it has a key.

A section without a key still starts: every tool and verb answers one sentence
naming what to set, and nothing is dialed.

## Use it

The nine tools are the original plugin's, under the same names:

| Tool | What it does |
| --- | --- |
| `index_read_intents` | the caller's signals, or one community's with `network_id`; `query` matches text; `limit` 1 to 100; `page` |
| `index_create_intent` | one signal from the person's own words, shared in every community unless `network_ids` narrows it |
| `index_update_intent` | rewrite a signal and reprocess it |
| `index_list_opportunities` | the matches negotiated so far: who, its status, its id |
| `index_update_opportunity` | accept or reject one, on the owner's approval |
| `index_read_networks` | the communities the caller belongs to, or one roster with `network_id` |
| `index_join_network` | join an open community |
| `index_add_intent_to_network` | share a signal in a community |
| `index_read_docs` | the protocol's own guidance, one topic at a time |

From the shell:

```
/index                     where it points, then the newest signals
/index signals designer    the caller's signals, matched on "designer"
/index opportunities       the matches negotiated so far
/index networks            the communities, with their sizes
/index docs signals        the protocol's guidance on one topic
/index status              endpoint and key, without dialing
```

From eval, the same reads answer a string:

```lisp
(indexnetwork:signals :limit 5)
(indexnetwork:opportunities :status "pending")
(indexnetwork:networks)
(indexnetwork:docs "negotiations")
```

A refusal is `ERROR: INDEX-NETWORK-ERROR` and says either what to set or what
Index answered, status and sentence both.

## Test

```
.github/test-cell.sh tools/nodecode-index-network
```

14 tests, 0 failed. Nothing in them reaches the network: the one HTTP seam is
stubbed, and so is the environment read behind the key.

## Gaps

What the rewrite does not carry, all of it the original's own machinery rather
than its data:

- **The negotiator.** The original runs a Bun sidecar (`runtime/dist/negotiator.js`,
  `@indexnetwork/agent`) that takes a turn with the person's own model and
  negotiates with the other seat; the cell is the client half only, so the
  agent still has to be selected in Index (a key works for the tools either
  way). The plugin's prompts, `custom_negotiator` hook, morning cron and
  dashboard rides with it.
- **The browser sign-in.** `hermes index login` runs a device-code flow from
  the CLI and stores `INDEX_SESSION_TOKEN`; the cell asks for an API key
  instead, which Index also mints from that session. A key never expires the
  way that session does, so a 401 is reported rather than refreshed.
- **Five tools**: `index_list_intent_networks`, `index_read_network_memberships`
  (folded into `index_read_networks`), `index_create_network` (and its
  early-access request), `index_update_network`, and `index_open_app` (which
  hands a link to the desktop app; the answers here carry the link instead).
- **The MCP server and desktop surface**: the plugin registers Index's own
  `/mcp` server and a Hermes desktop panel; here the tools are the model's,
  and `/index` is the panel.
