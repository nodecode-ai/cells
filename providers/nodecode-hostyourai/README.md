# nodecode-hostyourai

HostYourAI: its `hyai/auto` router as a Nodecode provider, with a task and a policy hint for each request. A Nodecode rewrite of [cogitor/hermes-hostyourai](https://github.com/cogitor/hermes-hostyourai) by [cogitor](https://github.com/cogitor).

HostYourAI is an inference router: the model `hyai/auto` picks a warm, often EU-hosted, open-weight model for each request, and a request may say what kind of work it is and what to optimize for.

## Get it

Tell the agent "add the hostyourai cell", or run `nodecode add nc://hostyourai`.

## Set it up

Make a key at <https://hostyourai.com/app/docs>. Save it with `/connect`, or set `HOSTYOURAI_API_KEY`; a saved key wins over the variable. Then pick the provider HostYourAI and the model `hyai/auto` in `/models`.

The section is optional; the defaults are the ones below.

## Use it

```
/hostyourai                        what is set, and the words it takes
/hostyourai task code              chat, code, summarise, classify, extract, rag, legal or reason
/hostyourai policy speed           balanced, speed, cost or quality
/hostyourai code speed             shorthand: a task and a policy in any order
/hostyourai task auto              a small model picks the task for each message
/hostyourai off                    no hint: hyai/auto decides alone
/hostyourai usage                  the account's balance and this month's spend
```

The same two words are the cell's section, so they can be set there instead:

```jsonc
{
  "hostyourai": {
    "task": "code",
    "policy": "speed",
    "transport": "body",     // or "headers": X-HYAI-Task and X-HYAI-Policy
    "usage_warn_eur": 5       // say the balance is low below this many euros; 0 never says it
  }
}
```

A hint only reaches a round that runs on `hyai/auto`; every other provider's rounds ignore it. With `task: "auto"`, the last user message (200 characters of it) goes to HostYourAI's System One model, and the task it answers with at 0.8 confidence or better is the hint — anything less is `chat`.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-hostyourai
```

MIT licensed. Ported from `cogitor/hermes-hostyourai`; see [NOTICE](NOTICE).
