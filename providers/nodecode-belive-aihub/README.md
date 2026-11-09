# nodecode-belive-aihub

BeLive AI Hub: an OpenAI-compatible model gateway as a Nodecode provider, its
model picker filled from the hub's own catalog. A Nodecode rewrite of
[belivetech/hermes-belive-aihub](https://github.com/belivetech/hermes-belive-aihub)
by [belivetech](https://github.com/belivetech).

## Get it

Tell the agent "add the belive-aihub cell", or run `nodecode add nc://belive-aihub`.

## Set it up

Make a workspace inference key in BeLive AI Hub at <https://aihub.belive.sg/>.
Save it with `/connect` (pick BeLive AI Hub), or set
`BELIVE_TOKENHUB_API_KEY` in the environment; a saved key answers first. The
plugin's own advice holds: use a workspace inference key, not a console login
or management key.

Optional, and rarely needed — the section is on unless it says
`"enabled": false`:

```jsonc
{
  "belive-aihub": {
    // where the gateway is served; the chat lane appends /chat/completions
    "base_url": "https://tokenhub-api.belive.sg/api/v1"
  }
}
```

## Use it

`/models` lists BeLive AI Hub's chat models, and a turn on one goes to
`https://tokenhub-api.belive.sg/api/v1/chat/completions`.

The hub is one endpoint in front of many upstream models, so the cell reads
the hub's own model list (`<base_url>/models`, with your key) and keeps only
the entries a turn can actually run on: text out, tool calls in, and a ready
`chat_completions` route that takes tools. The hub's exact `author/slug` ids
are preserved, and a model's context window is read off the same row.

When that catalog is unreachable, the picker falls back to the three models
the original plugin curates, so an offline start still fills it. A successful
empty catalog is an answer, not a failure: it empties the picker, as the
original leaves it.

A `base_url` other than the hub's own — a proxy or a self-hosted relay — is
not the hub's catalog to interpret, so the curated list answers there. The
hub's staging base, `https://api-stg-tokenhub.belive.sg/api/v1`, is the one
exception: it is accepted, and lists staging's models.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-belive-aihub
```

MIT licensed — see [LICENSE](LICENSE). Ported from the original plugin; see
[NOTICE](NOTICE).
