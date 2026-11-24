# nodecode-dexio

Dexio wiki memory: the pages that match each turn in front of the model, and six tools to search, read and write a wiki your other agents and your team share. A Nodecode rewrite of [dexio-wiki/hermes-plugin](https://github.com/dexio-wiki/hermes-plugin) by [dexio-wiki](https://github.com/dexio-wiki).

Dexio (https://dexio.wiki) is one wiki that all your agents read and write over MCP: the agent you run here, your other agents, and the people on your team work from the same pages, so what one learns the others can use. Nothing is captured automatically — no transcripts, no turn logs. The agent files what is worth keeping, the way a person adds to a team wiki.

The model gets the same six tools a Hermes user got:

| Tool | What it answers |
| --- | --- |
| `dexio_search` | Pages matching a question's words, best first, each with its title, path and up to three matching lines. |
| `dexio_read` | One page's markdown, with its version and the pages it links to and from. |
| `dexio_list` | The wiki's pages, or one folder's, with titles and descriptions. |
| `dexio_write` | Creates a page or replaces its whole text; an optional `base_version` refuses a page that moved underneath it. |
| `dexio_edit` | Replaces one exact, unique piece of a page — an empty `new_text` deletes it. |
| `dexio_append` | Adds text to the end of a page, creating it if needed. |

Before each model request the pages this turn's message matches arrive as a `## From your Dexio wiki` block, capped by `dexio.recall_chars` and asking for `dexio.max_results` pages; a message shorter than three characters, or `auto_recall: false`, leaves the recall to the tools. Every request also carries a paragraph saying where the wiki is and when to file into it.

## Get it

Tell the agent "add the dexio cell", or run `nodecode add nc://dexio`.

## Set it up

You need a Dexio wiki and an API key from it — on the hosted wiki, under **Settings > Agents** (it starts with `dxk_`). Put the key in the `dexio` section of `config.jsonc` or in `/settings`:

```jsonc
{
  "dexio": {
    "api_key": "dxk_…",    // or api_key_env naming the variable, or just DEXIO_API_KEY in the environment
    "url": "https://app.dexio.wiki"   // your own server, if you self-host Dexio
  }
}
```

A self-hosted Dexio answers the same wire: point `url` at it. Nothing else is needed — `agent` (the name your writes carry, `nodecode` by default), `max_results`, `recall_chars`, `auto_recall` and `timeout_seconds` all have defaults, and `/dexio` with no argument says whether the cell reached the wiki and how many pages it holds.

**What leaves the machine:** only what each tool call sends to the wiki you configured, and — before each request, unless `auto_recall` is off — the first 300 characters of your message, as a search query. Transcripts, tool results, files and the built-in memory are never sent.

## Use it

Ask the agent to file what matters ("put the pricing decision in the wiki"), or say `/dexio pricing` to search the wiki yourself. The pages are ordinary pages on your wiki: open them in its web view, edit them there, and the agent sees the change at its next request.

Run `.github/test-cell.sh tools/nodecode-dexio` from the cells repository.
