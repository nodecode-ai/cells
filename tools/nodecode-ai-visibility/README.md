# nodecode-ai-visibility

AI visibility: whether AI crawlers can read a website, how ready it is for AI search, and its latest public report of brand mentions and citations in ChatGPT, Gemini and Perplexity. A Nodecode rewrite of [meelis545/qqq-ai-visibility-mcp](https://github.com/meelis545/qqq-ai-visibility-mcp) by [meelis545](https://github.com/meelis545).

The original is a portable manifest: no code of its own, only the remote MCP server at `https://qqq.ee/mcp` and a skill saying how to call it. This cell talks to the same server with the same three tools, from a turn, from `/ai-visibility` and from eval.

## Get it

Tell the agent "add the ai-visibility cell", or run:

```sh
nodecode add nc://ai-visibility
```

## Use it

Nothing to set up: the checker is free, read-only and needs no key, and the cell answers from the moment it is on.

Ask in words — *"can ChatGPT read our site?"*, *"does anything cite example.com in AI answers?"*, *"is our site ready for AI search?"* — and the model calls one of:

```text
check_ai_visibility domain="example.com" language="en"
get_public_report domain="example.com"
get_tool_info
```

- `check_ai_visibility` is the technical readiness check: whether the AI crawlers (GPTBot, OAI-SearchBot, ClaudeBot, PerplexityBot, Google-Extended) may read the site, and its robots.txt, sitemap, title, meta description, one H1, canonical, noindex, structured data, content in HTML, key pages and `llms.txt`, with the three most important fixes. It asks no AI model.
- `get_public_report` reads the site's stored public report: in how many search-grounded AI answers the brand was named, how often its own site was cited, the questions asked, the sources used and the other companies named, with the report's date. It fetches nothing.
- `get_tool_info` says what the three measure and what their limits are.

Each answers the server's own text, links included, so it reads the way the page reads. Mentions, citations and readiness are three separate results: nothing here adds them into one score.

The same three, from the operator's line:

```text
/ai-visibility example.com            the readiness check
/ai-visibility report example.com     the stored AI mention report
/ai-visibility info                  what is measured
```

and from eval, as strings:

```lisp
(ai-visibility:check "example.com")
(ai-visibility:report "example.com" :language "et")
(ai-visibility:info)
```

A domain is taken as written: a pasted URL is cut to its host, so `https://www.example.com/pricing` is checked as `www.example.com`. A name with no dot, an IP address and `localhost` are refused before anything is sent, because the checker reads public websites only.

### Settings

`/settings` (or the Settings tab, or the `ai-visibility` section of `~/.nodecode/config.jsonc`):

| Setting | Default | What it does |
| --- | --- | --- |
| `endpoint` | `https://qqq.ee/mcp` | the `tools/call` endpoint; the hosted checker |
| `language` | `en` | the language of the readings: `en`, or `et` for an Estonian site |
| `timeout_seconds` | 20 | how long one call may wait for the checker |

### What it reaches

`https://qqq.ee/mcp` over HTTPS, one POST per call, with nothing in it but the tool name, the domain, the language and a user-agent naming this cell. No key, no account, no file written. The server sees the domain asked about and the calling address, which is how it caps the readiness check at five a day for one address; its answer says so when they are spent, with a link to the full check.

### Gaps

The cell speaks one `tools/call` per question and skips the MCP session handshake (`initialize` and the `Mcp-Session-Id` header), exactly as the original's own documented calls do. A server that starts to require a session is a refusal naming the server, not a silent retry.

## Credits

MIT, Copyright (c) 2026 QQQ OÜ. The checker, its three tools and the shape of their answers are the original's; the rewrite is against this repository's cell contract. See [NOTICE](NOTICE) for what was left out.
