# nodecode-pushary

Pushary: push notifications, questions and scope approvals on the operator's phone. A Nodecode rewrite of [Pushary/hermes-plugin-pushary](https://github.com/Pushary/hermes-plugin-pushary) by [Pushary](https://github.com/Pushary).

## Get it

Tell the agent "add the pushary cell", or run `nodecode add nc://pushary`.

## Set it up

1. Make an account at <https://pushary.com> and copy its API key into the environment
   Nodecode runs in, as `PUSHARY_API_KEY`. The key stays in the environment, never in
   the config.
2. Pair the phone app with the same account; that is the phone the notifications and
   the questions go to.
3. Optionally, tune the section in `~/.nodecode/config.jsonc`:

```jsonc
"pushary": {
  "api_key_env": "PUSHARY_API_KEY",   // the variable holding the key
  "agent_name": "Nodecode",           // the name the phone shows
  "base_url": "https://pushary.com"   // only a self-hosted copy changes it
}
```

4. Say `/pushary test`: one notification goes to the phone and the answer comes back
   to the terminal. Nothing runs.

## Use it

Once it is on, the model has six tools and a person has the same ones from eval:

| Tool | What it does |
| --- | --- |
| `pushary_notify` | A notification on the phone: a fact, and optionally a detail page behind it. |
| `pushary_ask` | A question on the phone — yes/no, one of a few choices, or free text — and the answer, waited for. |
| `pushary_wait` | One more poll for a question asked with `wait: false`. |
| `pushary_cancel` | A pending question that stopped being relevant. |
| `pushary_remind` | A reminder saved on Pushary, so it reaches the phone after the session ends. |
| `pushary_propose_scope` | The boundary of a run — the paths to change, the ones to leave alone, the definition of done — agreed in one tap. |

From eval: `(pushary:notify "the build is green" :title "Build")`,
`(pushary:ask "ship it?")`, `(pushary:remind "water the plants" :in-minutes 30)`.

`/pushary` says what the tools would use; `/pushary test` sends one notification;
`(help :pushary)` is the model's own primer.

A question blocks for at most 55 seconds per call, which is Pushary's own ceiling: when
the person has not answered by then, the answer says so and carries the `correlationId`,
and `pushary_wait` continues the wait in a later turn. A refused call — a bad key, no
network, an empty body — is a failed tool call carrying Pushary's own words.
