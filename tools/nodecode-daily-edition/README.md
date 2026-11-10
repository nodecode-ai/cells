# nodecode-daily-edition

Daily Edition: a dated front page compiled from interest phrases, with one story once, cross-posted stories merged and earlier days' stories withheld. A Nodecode rewrite of [ousiaresearch/daily-edition](https://github.com/ousiaresearch/daily-edition) by [ousiaresearch](https://github.com/ousiaresearch).

## Get it

Tell the agent "add the daily edition cell", or run `nodecode add nc://daily-edition`.

## Set it up

The `daily-edition` section names what you want to read about — a phrase per interest, never a
feed address:

```jsonc
{
  "daily-edition": {
    "interests": ["fusion energy", "housing in Malaga", "the port authority"],
    "max_stories": 20,        // how many stories one edition prints, 1 to 40
    "muted": ["sports"],     // interest phrases left out of the edition
    "watchwords": ["ferry"], // words that star a story and keep it at the top
    "timeout_seconds": 12,   // one interest's fetch ceiling
    "provider": "https://news.google.com/rss/search?q={query}&hl=en-US&gl=US&ceid=US:en",
    "max_chars": 12000       // the longest answer one action gives
  }
}
```

`interests` is the one required member. `provider` is the query feed an interest is expanded
into: `{query}` is replaced by the phrase, and the default is Google News RSS, which is
public and keyless. Naming an interest sends that phrase to the provider when an edition is
compiled; nothing else leaves the machine, and no key is needed.

## Use it

Ask for the paper, call the tool, or type `/daily-edition`:

```
daily_edition {action: "print"}
daily_edition {action: "print", interests: ["tidal power"], max_stories: 8}
daily_edition {action: "diff"}
daily_edition {action: "sources"}
/daily-edition
/daily-edition archive
```

| Action | What it answers |
| --- | --- |
| `print` | today's front page as text: masthead, standfirst, lead, a section per interest, and what could not be read; the edition is filed as a back issue |
| `diff` | what changed since the last edition: new, still running, gone |
| `sources` | the interests, the provider, the mutes and watchwords, and how many back issues are kept |
| `archive` | the seven back issues, newest first |

What makes it an edition rather than a feed:

- **One story, once.** The same story from several mastheads is clustered by canonical link
  and then by headline evidence, so it prints as one line crediting every publisher.
- **A lead.** Consensus wins the top of the page: more mastheads outrank fewer, recency
  breaks the tie, and a watchword stars a story to the top.
- **No reprints.** Anything the newest edition from an earlier day printed is withheld.
  Printing twice on one day replaces that day's page.
- **Back issues.** The last seven editions are kept under the Nodecode home, so `diff` and
  `archive` have something to compare against.

No model is involved: the edition is composed deterministically, from the same rules the
plugin's page used.

## Tests

```sh
.github/test-cell.sh tools/nodecode-daily-edition
```
