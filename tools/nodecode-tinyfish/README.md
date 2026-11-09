# nodecode-tinyfish

TinyFish: web search, page fetch, and a goal-based web agent that clicks, fills forms and reads across a real site. A Nodecode rewrite of [tinyfish-io/tinyfish-web-agent-integrations](https://github.com/tinyfish-io/tinyfish-web-agent-integrations) by [tinyfish-io](https://github.com/tinyfish-io).

## Get it

Tell the agent "add the tinyfish cell", or run `nodecode add nc://tinyfish`.

## Set it up

A TinyFish API key is all it takes; make one free at
[agent.tinyfish.ai/api-keys](https://agent.tinyfish.ai/api-keys). Either set it in
the cell's settings (`tinyfish.api_key`) or export `TINYFISH_API_KEY` — the cell
reads the setting first, then the variable, as the original did.

```jsonc
{
  "tinyfish": {
    "api_key": "tf-..."
  }
}
```

Nothing else is required. Search and Fetch are free on TinyFish; a web agent
run spends the account's credits. The hosts can be pointed elsewhere
(`search_url`, `fetch_url`, `agent_url`) for a proxy, and the ceilings are in
the settings: `timeout_seconds` for one call, `agent_timeout_seconds` for how
long a run is waited on, `poll_seconds` for how often it is asked,
`max_results` and `max_chars` for how much an answer carries.

## Use it

The model gets five tools, the original plugin's names kept where they mean the
same thing:

| Tool | What it does | Cost |
| --- | --- | --- |
| `tinyfish_search` | web results for one query: title, url and snippet each | free |
| `tinyfish_fetch` | the text of one or more pages, as markdown, text or the document tree | free |
| `tinyfish_agent` | a plain-language goal carried out on one URL — navigate, click, fill a form, read several pages — and the result it found | credits |
| `tinyfish_list_profiles` | the saved website sign-ins (Browser Context Profiles) a run can use | free |
| `tinyfish_create_profile` | a new profile, and the link the operator opens to sign in to the site themselves | free until opened |

The same verbs are Lisp functions the operator can call from eval, each
answering a string:

```lisp
(tinyfish:search-text "tinyfish web agent" :limit 3)
(tinyfish:fetch-text (list "https://example.com/") "markdown")
(tinyfish:agent-text "https://shop.example/search" "Return the first 5 products and prices as JSON")
(tinyfish:profiles-text)
```

`/tinyfish` reports the key, the hosts and the credits left; `/tinyfish usage`
reads the wallet by itself, and `/tinyfish profiles` lists the saved sign-ins.
`(help :tinyfish)` teaches the model the same in one topic.

A refusal answers `ERROR: TINYFISH-ERROR: ...` with what the API said, so the
model reads what to fix; a missing key says where to make one.

### Saved sign-ins and credits

A Browser Context Profile is a saved website sign-in. `tinyfish_create_profile`
answers a `setup_url`; the operator opens it, signs in to the site and clicks
Save profile — the cell never sees a password. Pass the profile's `id` to
`tinyfish_agent` as `profile_id` and the run starts signed in.

A run spends TinyFish credits and can take minutes: `tinyfish_agent` starts the
run, asks for its state every `poll_seconds`, and answers the result, or the
`run_id` when `agent_timeout_seconds` pass first. Before a run that buys, posts,
sends or changes an account, ask the operator.

## Not ported

- The Hermes web-provider and browser-provider seams (its `web_search`,
  `web_extract` and browser tools routed to TinyFish). Nodecode has web search
  of its own; here the same API is a set of verbs, plus the agent and profiles
  that are TinyFish's alone.
- The credit-approval hook and the `hermes tinyfish` setup CLI, which change a
  harness's config and prompt per billed session. A run is one tool call here.
- The opt-in setup telemetry, the wallet auto-reload view, and the remaining
  profile verbs (sign-in link for an existing profile, rename, delete):
  `tinyfish_create_profile` returns the sign-in link, and the others are one
  API call each away.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-tinyfish
```

Every TinyFish call is a stubbed `nlk:http`, the poll's clock a stubbed sleep,
so nothing reaches the network and no test waits.

MIT licensed; the original is MIT, see [NOTICE](NOTICE).
