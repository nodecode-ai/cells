# nodecode-ai-usage

AI usage: how much of each AI subscription is left, read from the plan itself. A Nodecode rewrite of [lvabarajithan/hermes-ai-usage-tracker](https://github.com/lvabarajithan/hermes-ai-usage-tracker) by [lvabarajithan](https://github.com/lvabarajithan).

One GET per provider, to the provider that owns the key: GitHub Copilot's
premium requests, chat and completions; Z.AI / GLM's coding-plan session,
weekly and tool quotas; Kimi's coding plan; MiniMax's 5-hour and weekly
windows; DeepSeek's balance; OpenCode Go's rolling, weekly and monthly
windows; OpenCode Zen's credits. Read-only, and nothing goes anywhere else.

## Get it

Tell the agent "add the ai-usage cell", or run `nodecode add nc://ai-usage`.

## Set it up

Each provider's key is read from an environment variable, and the section
names which variable for each:

| Provider | Member | Usual variable |
| --- | --- | --- |
| GitHub Copilot | `copilot_token_env` | `COPILOT_GITHUB_TOKEN` |
| Z.AI / GLM | `zai_key_env` | `GLM_API_KEY` |
| Kimi coding plan | `kimi_key_env` | `KIMI_API_KEY` |
| MiniMax coding plan | `minimax_key_env` | `MINIMAX_API_KEY` |
| DeepSeek | `deepseek_key_env` | `DEEPSEEK_API_KEY` |
| OpenCode Go | `opencode_go_key_env` | `OPENCODE_GO_API_KEY` |
| OpenCode Zen | `opencode_zen_key_env` | `OPENCODE_ZEN_API_KEY` |

Export the ones you pay for. With none set the cell refuses at start, naming
the variables it looked for; with one, it installs and reports the plans it
has a key for, saying what to set for the ones it does not. `cache_seconds`
(60) bounds how long an answer is reused and `timeout_seconds` (12) bounds
one probe.

## Use it

Ask, and the model calls the tool:

| Tool | What it does |
| --- | --- |
| `ai_usage` | every provider whose key this home has, or one named: each quota window's used and left percentage, its reset and the plan's own detail |

By hand, from the shell: `/quotas` prints the same report, `/quotas refresh`
asks the plans again, `/quotas zai` reads one provider. From eval:

```lisp
(ai-usage:report)                          ; the text /quotas shows
(ai-usage:report :provider "copilot" :refresh t)
(ai-usage:windows "zai")                   ; :windows, :plan, :reason as a plist
```

Provider ids are the original plugin's: `copilot`, `zai`, `kimi-coding`,
`minimax`, `deepseek`, `opencode-go`, `opencode-zen`.

A plan that rejects the key, an endpoint its tier does not offer, a provider
with no quota API and a missing key are each one line of the report, not a
failure. A refusal reads `ERROR: AI-USAGE-ERROR` and says what to set.

The section, documented, is in `config.example.jsonc`.
