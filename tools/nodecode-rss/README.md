# nodecode-rss

RSS Reader: a subscription library with mutes, search and a digest. A Nodecode rewrite of [apoapostolov/hermes-agent-awesome-plugins](https://github.com/apoapostolov/hermes-agent-awesome-plugins) by [apoapostolov](https://github.com/apoapostolov).

## Get it

Tell the agent "add the rss cell", or run `nodecode add nc://rss`.

## Set it up

The `rss` section names your first feeds, and that is all it needs:

```jsonc
{
  "rss": {
    "feeds": "https://example.com/feed.xml, https://news.example.org/atom.xml",
    // what the reader calls itself when it fetches: empty sends no user-agent
    "user_agent": "nodecode-rss/0.1 (+https://nodecode.ai)",
    "timeout_seconds": 15,   // one fetch's ceiling
    "max_entries": 25,       // unstarred entries kept per feed, and lines per answer
    "max_chars": 8000        // the longest answer one action gives
  }
}
```

`feeds` is required: one or more http(s) addresses, comma- or newline-separated. Everything else has a default, and later subscriptions are added with the tool. A feed that is not an http(s) address refuses at start, in words that name it.

## Use it

The model reaches the library through one tool, `rss`, whose `action` says what to do:

| Action | What it answers |
| --- | --- |
| `list` | every subscription, its id, its unread count, and whether its last fetch failed |
| `refresh` | fetch one subscription (`id`) or all of them, and say how many entries each brought |
| `unread` | what is unread and unmuted, newest first, `count` lines |
| `digest` | per feed, its unread count and top headlines — what the model summarizes |
| `read` | one article: title, feed, date, url and summary, marked read |
| `find` | `text` in any article's title, summary or url |
| `add` / `remove` | subscribe to `url` (fetched at once) or drop a subscription with its articles |
| `mark` / `star` | mark one article — or `all` — read; keep an article out of the prune |
| `mute` / `unmute` / `mutes` | hide every article carrying a phrase, or let it back in |

Articles are named by the `[a7]` id every line carries, subscriptions by `[f1]`. `/rss` takes the same words for the operator: `/rss refresh`, `/rss unread`, `/rss add URL`, `/rss find TEXT`, `/rss mute PHRASE`, `/rss digest`.

The library — subscriptions, articles, read and starred state, mute phrases — lives in one file under the Nodecode home and survives a restart. Feeds are fetched with one bounded HTTP GET and parsed here; RSS 2.0 and Atom are both read, so nothing else has to be installed.

What a feed says is data, not instructions: a title or summary that tells the model to do something is quoted, not obeyed.

## What the plugin had that this does not

The three-column desktop page (a cell contributes verbs, not windows), reader-mode capture of full articles, YouTube and Substack address expansion, image handling and the animated ticker. AI summaries are the model's own work on a `digest` rather than a call the cell makes.

## License

MIT. See `LICENSE` and `NOTICE`.
