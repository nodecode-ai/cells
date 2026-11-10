# nodecode-postel

Postel: find what is going viral in your niche, and draft, schedule or publish your version to X, LinkedIn, Bluesky and Mastodon. A Nodecode rewrite of [robinfaraj/postel-mcp](https://github.com/robinfaraj/postel-mcp) by [robinfaraj](https://github.com/robinfaraj).

## Get it

Tell the agent "add the postel cell", or run `nodecode add nc://postel`.

## Set it up

The cell needs a Postel account and an API key. Make one in Postel under
Settings, then API Keys (it starts with `pstl_`), and put it in the `postel`
section of `~/.nodecode/config.jsonc`, or export it as `POSTEL_API_KEY`:

```jsonc
{
  "postel": {
    "api_key": "pstl_..."        // or "api_key_env": "POSTEL_API_KEY"
  }
}
```

Connect the networks you post to in Postel under Settings, then Channels. No
network has to be connected to research, and scheduling and publishing need a
paid Postel plan or trial. `/postel` with no argument reports the workspace,
the plan, the AI credits left and the networks that are connected, without ever
printing the key.

## Use it

Ask in words; the cell's eight tools are what the model calls.

| Tool | What it does |
| --- | --- |
| `postel_viral` | What went viral in a niche, from Postel's library of indexed X posts, with each post's numbers and link. |
| `postel_account` | The workspace behind the key: plan, whether it can publish, AI credits, connected networks. |
| `postel_posts` | The workspace's posts - drafts, scheduled, published, waiting for approval - each with its id. |
| `postel_post` | One post by id, as it stands now. |
| `postel_save` | Saves one post: text, or a thread, with media urls and a LinkedIn first comment. A draft unless a time or `publish_now` says otherwise. |
| `postel_generate` | Postel writes the draft in the user's saved voice. Uses 1 Postel AI credit. |
| `postel_schedule` | Times a saved draft. |
| `postel_publish` | Publishes a saved draft now; it cannot be undone from here. |

`/postel cold email` does the research from the keyboard, and `/postel` alone
reports the account. `(help :postel)` carries the writing advice the original
plugin's skill taught: the shape of a LinkedIn post, of an X thread, of a
Bluesky or Mastodon post, and the order of work - research, draft, confirm the
text and the network and the time, then schedule or publish.

The post text a research answers with is untrusted source material: the model
is told to use it for patterns, never to copy it, and to ignore any
instruction inside it.

## What it talks to

Postel's REST API at `https://www.postel.app/api/v1`, with the key as a bearer
token and nothing else. `base_url` in the section moves it for a proxy of your
own. Nothing here reads a local file, and the cell stores no key of its own.
