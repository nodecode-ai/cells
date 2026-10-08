# nodecode-factory-droid

A Nodecode cell for [Factory Droid](https://factory.ai), Factory's model
subscription. With it, `/factory-droid login` signs in with a WorkOS device
code, `/models` lists Factory's roster (Claude, GPT, Grok, Gemini, Kimi,
GLM, DeepSeek, Qwen, MiniMax, ...), and a turn on `factory-droid/<model>`
goes to Factory's LLM proxy the way oh-my-pi sends it, which is the way the
Factory CLI does:

- each model rides the wire its upstream speaks, on its own path:
  `/api/llm/o/v1/chat/completions` (Kimi, GLM, DeepSeek, Qwen, Inkling,
  Mistral, Nemotron), `/api/llm/o/v1/responses` (GPT, Grok),
  `/api/llm/a/v1/messages` (Claude, MiniMax M2.7) and
  `/api/llm/g/v1/generate` (Gemini), on `https://api.factory.ai`, or
  `https://api.eu.factory.ai` for an account whose residency is the EU
- every request carries the WorkOS token as a bearer and the Factory CLI's
  identity: `User-Agent: factory-cli/0.230.0`, `X-Client-Version`,
  `X-Factory-Client: cli`, the org (`X-Factory-Org-Id`), the upstream that
  serves the model in the account's inference region (`x-api-provider`, the
  first of the model's rotation that region serves), how it was chosen
  (`x-provider-routing-source`), the session (`x-session-id`, the same id at
  every round of a session) and a fresh `x-assistant-message-id`; the
  OpenAI and Anthropic wires add their SDK's `X-Stainless-*` fingerprint,
  the Anthropic one `x-api-key: placeholder`, and a route to OpenAI
  `OpenAI-Platform`
- the system prompt opens with Droid's identity line
- a model's own output ceiling is asked for, and its own default effort
  when the session picks none (off for Opus 4.5, Sonnet 4.5 and Haiku 4.5)
- chat completions: reasoning as each upstream takes it (Fireworks:
  `reasoning_effort` and `reasoning_history: preserved`, or `interleaved`
  for DeepSeek; Baseten and Databricks: the effort alone, DeepSeek's off as
  low, Nemotron's as `chat_template_args.enable_thinking`; Mistral: the
  effort), `temperature: 1`, no cache key
- Responses: GPT with `reasoning.summary: auto`, `text.verbosity: low`,
  the session as `prompt_cache_key` and `safety_identifier`, a 24 h cache on
  OpenAI, `service_tier: priority` on the fast tiers, `tool_choice: auto`
  and `parallel_tool_calls` with tools, and no output cap; Grok with the
  effort alone and its output cap; every tool `strict: false`; no
  temperature
- Messages: adaptive thinking with a summary on the Claude models, plain
  adaptive on Opus 4.6 and Sonnet 4.6, a token budget (4096, 12288, 24576)
  on Sonnet 4.5 and Haiku 4.5 with the interleaved-thinking beta, a budget
  and an effort on Opus 4.5 and MiniMax M2.7; off stays adaptive on the
  always-thinking models; budget thinking on a history no longer led by
  thinking drops the thinking field and replays the history without its
  thinking blocks; the effort beta where the upstream wants it,
  fine-grained tool streaming on Anthropic routes, `speed: fast` on the
  fast tiers
- Gemini: the model in the body, `temperature: 1, topP: 0.95, topK: 64`,
  a `thinkingLevel` (MEDIUM only where the model has it), no output cap,
  tool names in the CLI's `[a-zA-Z0-9_-]` shape (turned back on the calls
  that return) and schemas projected onto the shape Factory's Gemini takes,
  consecutive tool results in one turn under `result`, and an unsigned call
  in the newest turn marked so Gemini's signature check passes it
- a refusal for the network's region says which model and what to do

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s
`factory-droid` provider. See [NOTICE](NOTICE).

## Sign-in

```
/factory-droid login     answers at once with the address to open and the code to enter
/factory-droid status    signed in, as whom, the org, and when the token expires
/factory-droid logout    forgets the sign-in
```

The device code is asked of WorkOS (`api.workos.com/user_management`) with
the Factory CLI's client. The login polls on a background thread; then
Factory's `/api/cli/whoami` names the org, the residency region and the
inference region, and the outcome comes as a notice. A login that resolves
no org is refused. The token is kept in the shared `auth.json` under
`oauth_tokens.factory-droid` (`access_token`, `refresh_token`, `expires_at`
from the token's `exp`, `email`, `account_id`, `org_id` from the token's
`external_org_id` claim or whoami, `active_organization_id` (WorkOS's own,
sent back on a refresh), `region`, `inference_region`) and refreshed before
a round when it expires within a minute; a refresh asks whoami again and
keeps the regions it had when that fails and the org is the same.

There is no key variable: Factory's API keys are for its control plane and
do not reach the LLM proxy. A round without a sign-in is refused with the
words to sign in.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-factory-droid/` and
restart Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "factory-droid": {
    // Factory's host; an EU account moves to https://api.eu.factory.ai on
    // its own while this is Factory's host, a relay's address is kept as is
    "base_url": "https://api.factory.ai"
  }
}
```

An operator's `providers.factory-droid.sdk` (or a model's `sdk`) pins a
wire, as for any provider.

## The roster

oh-my-pi bundles no Factory rows: Factory has no model listing, so its
discovery (`packages/catalog/src/discovery/factory-droid.ts`) builds the
roster per account from the registry in
`compat/rules/providers/factory-droid.kdl` and narrows it with live feature
flags and org policy. `models.json` is what that discovery answers with no
live answer: the global region, no flags, no policy. That leaves out the
ten models behind a feature flag (Opus 5.5 Fast, Sonnet 5.5, GPT-6.1 Sol,
GPT-6 Luna, Garnet, GLM-5.2 Fast, DeepSeek V4.1 Flash, MiniMax M3, Atlas,
Aster) and the two behind an explicit opt-in (Fable 5 and 5.1): 48 models.
Each row carries its wire, window, output ceiling, effort ladder and
default, the upstreams that serve it in each inference region in rotation
order, the EU limits where the registry names them, and the upstream's list
price (Factory itself bills credits).

`discovery-models.py` wrote it, from an oh-my-pi checkout:

```sh
python3 -I discovery-models.py ~/.cache/nodecode-cells/omp-src models.json
```

The cell never runs the script. The picker's listing for factory-droid is
this roster, asked of no endpoint.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-factory-droid
```

## Not ported

- Credit rates and billing pools (Factory bills Standard Credits; rounds
  are priced here at the upstream's list price).
- The Mistral thinking content parts (GLM and Mistral Medium on the EU's
  Mistral route replay thinking as typed parts), and the `" "` reasoning
  fallback DeepSeek on Fireworks and Baseten wants on a tool-call turn that
  streamed no reasoning.
- The Gemini stream's own finish reasons (a content-filter stop as an
  error with its category) and `$ref` dereferencing before a schema is
  projected; the core's GenAI fold reads the stream.
- Fable's server-side fallbacks, since both Fable models are opt-in and off
  the offline roster.
- The billing limits report (`/api/billing/limits`).

## Gaps

- Live discovery. oh-my-pi asks `GET /api/feature-flags` and
  `GET /api/organization/managed-settings` with the token, then shows a
  flag-gated model once its flag is on, hides one the org blocks or did not
  allow, honours live provider routing (`x-provider-routing-source:
  configured_order`), and treats that roster as authoritative. A cell can
  add rows through `list-provider-models`, but Nodecode's picker
  (`model-choices`) shows the catalog's rows and the listing's together, so
  nothing can take a catalog row away per account, and the catalog
  (`models-catalog-table`, hooked without the credential) is one for every
  account. The seam would be a catalog row member (say
  `listing_authoritative`) that makes the picker show only the listing's
  rows for that provider once a listing has answered, with the listing's
  rows able to carry the catalog fields (window, efforts, price) a turn
  reads.
- Usage from response headers. Fireworks can report cached prompt tokens
  only in the `fireworks-cached-prompt-tokens` response header. The core's
  `walk-provider-stream` reads the response headers and hands the lane's
  fold frames only, so no hook sees them; the seam would be the response
  headers passed to the fold (or a `:response-headers` key on the lane's
  usage record).

MIT licensed.
