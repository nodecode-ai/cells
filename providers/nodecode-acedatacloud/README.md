# nodecode-acedatacloud

Ace Data Cloud: its GPT-4.1 models, over the OpenAI chat wire, as a Nodecode provider. A Nodecode rewrite of [AceDataCloud/AceDataCloudModelProviderHermes](https://github.com/AceDataCloud/AceDataCloudModelProviderHermes) by [AceDataCloud](https://github.com/AceDataCloud).

It puts **Ace Data Cloud** in the model picker with the two models the
original verified, `gpt-4.1-mini` and `gpt-4.1`, and nothing else out of the
account's shared catalog. A turn's messages, tool schemas and tool results go
to `https://api.acedata.cloud/openai/chat/completions` with your Ace key; the
model listing asks the same base's `/models`.

## Get it

Tell the agent "add the acedatacloud cell", or run `nodecode add nc://acedatacloud`.

## Set it up

Make an API key in the Ace Data Cloud console, under Applications -- a General
application's API Key, or one made for this integration under Manage Keys.
Then either save it with `/connect` (pick Ace Data Cloud) or export it as
`ACEDATACLOUD_API_KEY` in the environment Nodecode starts in. Check that the
application has the chat service enabled and enough balance for a first call.

The section has two members, both optional:

```jsonc
"acedatacloud": {
  "base_url": "https://api.acedata.cloud/openai",  // where a chat round goes
  "timeout_seconds": 10                             // one model-listing fetch
}
```

`base_url` is for a relay in front of the service; leave it alone otherwise.

## Use it

Pick Ace Data Cloud in `/setup`, `/models` or with `nodecode chat --provider
acedatacloud --model gpt-4.1-mini`, and talk. Both models take tool calls, so
the agent's evals, shell and file verbs work as they do on any other chat
provider. Every call spends the Ace Data Cloud balance of the account the key
belongs to; the cell shows no price, because Ace's own billing is the
authority and the original's picker never claimed one either.

On a box with no key yet, the picker still offers the two models: it does not
ask the endpoint until there is a key to ask with. `/connect`'s check does
send the key it is testing, once, and reads a refusal as a refused key.

## Test it

```sh
.github/test-cell.sh providers/nodecode-acedatacloud
```

Offline: every listing is a stubbed fetch, every round a stubbed POST, every
key variable a stub.

## Gaps

Nothing of the original's runtime is missing: it was a provider profile and
a model filter, and both are here. Two things are deliberately not carried:
the signup link with its referral parameters (Nodecode's picker opens no
signup page), and the original's `requires_hermes` version floor, which the
cell contract does not express.
