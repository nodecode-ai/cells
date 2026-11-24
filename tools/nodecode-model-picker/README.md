# nodecode-model-picker

Model Picker: each turn runs the cheapest of three model tiers that can do the job. A Nodecode rewrite of [aean0x/hermes-model-picker](https://github.com/aean0x/hermes-model-picker) by [aean0x](https://github.com/aean0x).

`low`, `default` and `high` are three model ids on one provider, labelled
Quick, Standard and Expert. Every prompt is classified into one of them and
the turn that follows runs on it; `high` is meant for money, an irreversible
or published action, or a security decision, and a classifier that cannot
answer falls back to `low` rather than failing the turn. Repeated failed tool
calls unlock `escalate_model`, which climbs one tier and no further than
`high`.

## Get it

Tell the agent "add the model-picker cell", or run `nodecode add nc://model-picker`.

## Set it up

Give each tier a model id in the `model-picker` section of
`~/.nodecode/config.jsonc` (or in `/settings`), three ids on the same
provider, plus that provider's id:

```jsonc
{
  "model-picker": {
    "provider": "openai",
    "low_model": "gpt-5-mini",
    "default_model": "gpt-5",
    "high_model": "gpt-5-pro",
    "escalate_after": 3
  }
}
```

A tier with no model refuses to start, and the refusal names the key. The
other settings are optional: `classifier_model` sends the classification to
a cheaper model (the provider must be named too, since a side call may not
move hosts on its own), `keep_previous` (default true) keeps the last tier
when the classifier cannot answer, and each tier's `best_for` list is what
the classifier is told the tier is for — that list, not the code, is the
routing policy. `config.example.jsonc` shows the whole section.

## Use it

- Say what you want. A prompt that reads like a lookup runs on the low tier;
  ordinary work on the default one; anything about money, a publish or
  security on the high tier.
- `/model-picker` says where the session stands and offers the picks;
  `/model-picker low|default|high` pins a tier for the session, and
  `/model-picker auto` resumes classifying. A pinned session is never
  classified.
- Ask the model to `escalate_model` after a call has failed `escalate_after`
  times in a row: the next turn runs one tier higher.

What it writes is one durable session fact, the same one `/models` writes: the
tier's model becomes the session's own selection, so the next turn reads it
with nothing patched. A new fact lands only when the tier changes.

## Gaps against the original

The original compacts the transcript before a classifier-driven switch, through
a subclass of Hermes's context compressor; Nodecode's context is the model's
own to edit (see `(help :context)`), so nothing is compacted here.
`escalate_model` counts consecutive failed tool calls from the `:TOOL` point
and clears the count on a call that worked, rather than staging a checkpoint.
The `/medium` alias of the original is carried; its `MODEL_PICKER_*`
environment variables and its adjacent `config.json` are not — settings live
in the section, where the setup panel reads them. A tier on another provider
is still refused by construction: the section names one provider.

## Test

```sh
NODECODE_SRC=<a nodecode checkout> .github/test-cell.sh tools/nodecode-model-picker
```

The slice is offline: the classifier call is stubbed, and a session's
selection is the real fact, in a temp store. 16 tests, 0 failed.
