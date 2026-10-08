# nodecode-alibaba-coding-plan

A Nodecode cell for the [Alibaba Coding Plan](https://modelstudio.console.alibabacloud.com/),
Alibaba Cloud Model Studio's coding subscription (Qwen, GLM, Kimi and MiniMax
models for a flat fee). With it, `/connect` offers Alibaba Coding Plan,
`/models` lists the plan's models, and a turn on
`alibaba-coding-plan/<model>` goes to the plan's endpoint in your region over
the OpenAI chat wire, asking a reasoning model to think the Qwen way.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s
`alibaba-coding-plan` provider. See [NOTICE](NOTICE).

## What omp's login does, and how this cell carries it

omp's login for this provider is a custom one, but it signs nothing in. It
asks three things:

1. Which endpoint: 1 International (the default,
   `https://coding-intl.dashscope.aliyuncs.com/v1`), 2 China
   (`https://coding.dashscope.aliyuncs.com/v1`), or 3 a custom base URL.
   The two regions are separate products and their keys do not cross.
2. The key, pasted from the console of that region.
3. Nothing more: it checks the key with a one-token chat request to
   `qwen3.5-plus` (or with `GET <base>/models` for a custom base) and keeps
   key and base together, so a turn goes to the chosen base.

Here the endpoint choice is the section's `region` (and `base_url` for a
custom base), and the key is a plain key:

- **Region.** `"region": "international"` (the default) or `"china"`.
- **Custom base.** `"base_url"`, when set, wins over the region, as omp's
  option 3 does.
- **Key.** Save it with `/connect`, or set `ALIBABA_CODING_PLAN_API_KEY`. A
  saved key wins over the variable. `/connect` checks a key by asking the
  region's `/models` listing with it, before it saves it.

Where to make a key: <https://modelstudio.console.alibabacloud.com/> for the
international plan, <https://bailian.console.aliyun.com/?tab=model#/api-key>
for the China one.

## Thinking

omp's rule for this provider is the Qwen thinking dialect for every model.
For a reasoning model, an effort sends `enable_thinking: true` and no
`reasoning_effort`, and `off` sends `enable_thinking: false`. With no effort
set, nothing is sent, so the plan's default applies, as the chat lane does
for every provider. A model that does not reason is told nothing.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-alibaba-coding-plan/` and
restart Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "alibaba-coding-plan": {
    // where the plan was bought: international or china
    "region": "international"
    // a proxy, or another endpoint; set, it wins over region
    // "base_url": "https://your-proxy.example/v1"
  }
}
```

## Not ported

- The login's key check against `qwen3.5-plus`. `/connect` checks a key
  against the listing instead.
- omp's provider-wide 600 s stream-idle timeout. Set
  `providers.alibaba-coding-plan.stream_idle_timeout_ms` in the operator's
  config for the same.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-alibaba-coding-plan
```

MIT licensed.
