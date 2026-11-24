# nodecode-ms-decision

Microsoft-Decision-1: typed decisions (yes/no, choice, score) over Azure AI Foundry, as tools. A Nodecode rewrite of [guglxni/ms-decision-provider](https://github.com/guglxni/ms-decision-provider) by [guglxni](https://github.com/guglxni).

Microsoft's `Microsoft-Decision-1` (Azure AI Foundry's System One) is a
classifier, not a chat model: one request asks a map of named, typed
questions about one state, and each comes back as a probability
distribution rather than prose. The cell gives the model four tools:

| Tool | Asks | Answers |
| --- | --- | --- |
| `decision_noul` | a yes/no question about a state | `{"noul": 0.82}`: the probability of yes |
| `decision_choice` | which of some options this state is | the most probable option, every option's probability, a confidence |
| `decision_score` | where this state sits on ordered levels | a probability-weighted score and the whole distribution |
| `decide` | several of the above about one state | one answer per question, keyed by id |

The whole distribution survives the call: the model picks the threshold, so a
low probability is an abstention, not a failure. Use it to route (which team,
which queue), to gauge severity, or to gate something on "is this safe".

While the cell runs, `(help :ms-decision)` answers the manual, every
request's help section carries one line naming the tools, and the same four
questions are Lisp verbs through eval: `(ms-decision:noul STATE QUESTION)`,
`(ms-decision:choice STATE QUESTION OPTIONS)`, `(ms-decision:score STATE
QUESTION LEVELS)` and `(ms-decision:decide STATE QUESTIONS)`.

## Get it

Tell the agent "add the ms-decision cell", or run `nodecode add nc://ms-decision`.

## Set it up

1. An Azure AI Foundry resource with a `Microsoft-Decision-1` deployment.
2. `AZURE_FOUNDRY_API_KEY` and `AZURE_FOUNDRY_BASE_URL`, set in the
   environment or in `~/.nodecode/auth.json`:

```json
{"api_keys": {"ms-decision": {"provider": "ms-decision", "key": "..."}}}
```

   The saved key wins over the variable. The endpoint is
   `https://<resource>.openai.azure.com`: the cell refuses any host that is
   not an https Azure OpenAI resource (another domain, a port, userinfo, a
   query, or a path beyond `/openai/v1`), so a misconfiguration cannot send
   the key or the state anywhere else. With no endpoint or no key, nothing is
   sent and the tool answers `{"error": ...}` saying which is missing.
3. `/ms-decision` says where the key comes from; its panel checks the
   endpoint with one yes/no question.

## Use it

The model calls the tools; `/ms-decision` shows the state:

```
/ms-decision                where the key comes from, the endpoint, the model, the gate
/ms-decision check          ask the endpoint one yes/no question
/ms-decision gate on|off    watch risky tool calls, and ask before they run
```

### The gate (optional)

With `"gate": true` the cell watches known-risk tool calls (Nodecode's
`eval`, `write_file` and `patch`, plus any you list in `watch`) at the `:tool`
point. It sends the endpoint a projection of the call, never the call: a
fixed tool label and one allowlisted operation word (`sh`, `write-file`,
`delete-file`, ...), so no argument, path or message leaves the machine. The
answer is advisory. In `approve` mode the operator is asked Allow once or
Deny and the call runs only on Allow once; in `block` mode a call scored
below `block_threshold` (or not scored at all) is refused outright. A refusal
is an ordinary failed tool result, so the model reads why.

This is a speed bump, not a security boundary: the guard cell reads shell
commands, and `eval` has full host authority.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "ms-decision": {
    "base_url": "",          // empty reads AZURE_FOUNDRY_BASE_URL
    "model": "",             // empty reads MS_DECISION_MODEL, else ms-decision-1
    "timeout_seconds": 20,
    "gate": false,
    "gate_mode": "approve",  // or block
    "block_threshold": "0.5",
    "watch": [],
    "ask_seconds": 120
  }
}
```

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-ms-decision
```

No network and no key: every exchange is a stubbed `dex:request`.

## Gaps

- **Which hosts count.** Only `<resource>.openai.azure.com` is accepted,
  which is what the plugin required. A Foundry resource whose host is another
  shape needs the endpoint to be one of these.
- **The gate's watched names.** The plugin watched Hermes's tool names
  (`terminal`, `execute_code`, `write_file`, ...); Nodecode has fewer tool
  names, so the default set is `eval`, `write_file` and `patch`, and `watch`
  adds the rest.
- **Approval caching.** Hermes could keep an approval rule for one exact call
  under an HMAC key; here every watched call is asked again, so the key and
  the on-disk cache are gone.

MIT licensed. See [NOTICE](NOTICE).
