# nodecode-xiaomi

A Nodecode cell for [Xiaomi MiMo](https://platform.xiaomimimo.com), Xiaomi's
model platform. With it, `/connect` offers Xiaomi MiMo, `/models` lists the
MiMo models, and a turn on `xiaomi/<model>` goes over the OpenAI chat wire to
wherever your key is served: `https://api.xiaomimimo.com/v1` for a
pay-as-you-go key, the Token Plan's regional cluster for a Token Plan key.
A reasoning model is asked to think the way MiMo takes it.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `xiaomi`
provider. See [NOTICE](NOTICE).

## What omp's login does, and how this cell carries it

omp's login for this provider is a custom one, but it signs nothing in. It
opens the console's key page, asks for a key, and checks it with a one-token
chat request to `mimo-v2.5`. It takes two kinds of key:

- a pay-as-you-go key (`sk-...`), checked at `https://api.xiaomimimo.com/v1`
- a Token Plan key (`tp-...`), which only the plan's regional clusters
  serve. omp tries Singapore, then Amsterdam, then China, and its model
  discovery does the same, so the models it lists carry the cluster that
  took the key and a turn goes there.

Here the key is a plain key, and the cluster rule is the cell's:

- **Key.** Save it with `/connect`, or set `XIAOMI_API_KEY`. A saved key
  wins over the variable.
- **A tp- key's cluster.** The section's `token_plan_region`. At `"auto"`
  (the default) the cell does what omp does: the first of
  `token-plan-sgp`, `token-plan-ams` and `token-plan-cn` whose `/models`
  takes the key serves it, for as long as the process runs. A listing finds
  it as it lists; a round that comes first asks the clusters itself. `"sgp"`,
  `"ams"` or `"cn"` pins one and asks nothing. If no cluster takes the key,
  the round goes to Singapore and shows its refusal.
- **A sk- key** goes to `base_url`, `https://api.xiaomimimo.com/v1` unless
  the section says otherwise. A tp- key never does, as in omp.

Where to make a key: <https://platform.xiaomimimo.com/#/console/api-keys>;
a Token Plan key is on the plan page,
<https://platform.xiaomimimo.com/console/plan-manage>.

## Thinking

omp's rule for the MiMo family is the zai dialect. For a reasoning model, an
effort sends `thinking: {"type": "enabled"}` and `off` sends
`thinking: {"type": "disabled"}`; `reasoning_effort` is never sent, since
MiMo does not take it. With no effort set, nothing is sent, so the
platform's default applies, as the chat lane does for every provider.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-xiaomi/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "xiaomi": {
    // where a pay-as-you-go (sk-) key is served
    "base_url": "https://api.xiaomimimo.com/v1",
    // where a Token Plan (tp-) key is served: auto, sgp, ams or cn
    "token_plan_region": "auto"
  }
}
```

## Not ported

- The login's key check against `mimo-v2.5`. `/connect` checks a key against
  the base's `/models` listing instead, which for a tp- key is the cluster
  the listing hook finds.
- omp's replay rules for MiMo (`reasoning_content` on every assistant turn,
  no synthetic placeholder). The chat lane already replays the
  `reasoning_content` a model streamed, on every assistant turn once one
  carries it.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-xiaomi
```

Every round is a stubbed `dex:post`, every cluster probe a stubbed `dex:get`,
every listing a stubbed `nle::http-fetch`: nothing reaches Xiaomi.

MIT licensed.
