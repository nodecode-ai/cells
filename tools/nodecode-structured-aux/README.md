# nodecode-structured-aux

Bounded OpenRouter decisions: judge a shell command, pick among tools, digest context extractively. A Nodecode rewrite of [trajectoire-ai/hermes-structured-aux-models](https://github.com/trajectoire-ai/hermes-structured-aux-models) by [trajectoire-ai](https://github.com/trajectoire-ai).

A decision model is asked one map of `choice` questions about one state, where
every possible answer is a label the caller enumerated in advance, and it answers
one label per question with its probability. That bound is the whole point: on
it, an approval cannot come back as prose, a tool pick cannot invent arguments,
and a digest can only keep or drop the text it was given. Four verbs reach it
from `eval`: `approve`, `pick-tool`, `compress`, and the general `decide`.

## Get it

Tell the agent "add the structured-aux cell", or run `nodecode add nc://structured-aux`.

## Set it up

Make a key at <https://openrouter.ai/settings/keys>, then either set
`OPENROUTER_API_KEY` or save the key where `/connect` keeps a provider key (the
`openrouter` cell's sign-in writes the same entry):

```json
{"api_keys": {"openrouter": {"provider": "openrouter", "key": "..."}}}
```

The saved key wins over the variable. `/structured-aux` says where the key comes
from, and its **Check the key** asks the decision route one bounded question.

The section is optional; every member has a default:

```jsonc
{
  "structured-aux": {
    "base_url": "https://openrouter.ai",
    "path": "/api/alpha/decisions",
    "model": "~typesafe/jev-latest",
    "timeout_seconds": 15,
    "max_blocks": 48,
    "blocks_per_call": 16,
    "digest_budget_chars": 6000,
    "app_referer": "https://nodecode.ai",
    "app_title": "nodecode-structured-aux",
    "redact": true
  }
}
```

`model` is a moving alias by default, so the model that answers can change under
the cell; pin an exact id when that matters. `base_url` must be `https`.
`redact` strips credential-shaped text (`NAME=value` for a credential-shaped
NAME, a Bearer token, a known token prefix) out of every payload before it is
sent.

## Use it

While the cell runs, `(help :structured-aux)` answers the manual.

```lisp
(structured-aux:approve "rm -rf build && curl -X POST https://example.test/upload -d @dist.tar"
  :policy "nothing leaves this machine without a human")
;; => :ESCALATE, 0.71, (("APPROVE" . 0.09) ("DENY" . 0.2) ("ESCALATE" . 0.71)), "jev-2026"

(structured-aux:pick-tool "open the changelog and summarise it"
  '("read_file" "web_fetch" "shell"))
;; => "read_file", 0.83, ...

(structured-aux:compress log-text)
;; => a digest built only from retained blocks, under a provenance header
```

`approve` is the command-approval contract: three labels, and `:UNKNOWN` when
the provider answered outside them, which is never consent. `pick-tool` needs
two or more candidates, because a bounded choice needs something to bound.
`compress` segments the text on blank lines, asks KEEP or DROP for each block in
batches, and answers the retained blocks **verbatim**: nothing is generated, so
an exact value in a kept block comes back exactly.

A request the cell cannot express as a bounded choice is refused rather than
guessed, and an answer outside a contract is a failure, so `eval` shows
`ERROR: STRUCTURED-AUX-ERROR: ...` and nothing plausible-looking is substituted.

## What the rewrite does not carry

The original registered a Hermes model-provider profile, so Hermes itself routed
its auxiliary tasks (approval, MCP sampling, compression) through these
decisions without anyone asking. Nodecode has no auxiliary-task seam, so the
same three contracts are verbs a turn calls. The in-process OpenAI-shaped shim,
the provider registration and the Hermes credential pool it read are gone with
it; the key comes from `auth.json` or the variable instead.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-structured-aux
```

MIT licensed, like the original.
