# nodecode-prompt-optimizer

Prompt Optimizer: rewrites a prompt shorter and clearer for the model that will read it, and scores the change. A Nodecode rewrite of [Sahil-SS9/hermes-multichannel-prompt-optimizer](https://github.com/Sahil-SS9/hermes-multichannel-prompt-optimizer) by [Sahil-SS9](https://github.com/Sahil-SS9).

A prompt is expensive before the model reads it. `hey could you maybe explain to me very kindly what python generators are please when you get a chance` costs eighteen words and says one thing; `Explain Python generators.` says the same thing, sharper, for three. The original did that to every message behind the operator's back. This cell does it when it is asked to: the same rewrite, the same five-dimension score, the same model-family tailoring, on two surfaces, the operator's `/optimize` and the model's `optimize_prompt` tool.

## Get it

Tell the agent "add the prompt-optimizer cell", or run `nodecode add nc://nodecode-prompt-optimizer`.

## Set it up

Nothing is required. The rewrite runs on the auxiliary model (`general.auxiliary_model`, else the session's own), so it uses the provider already configured -- no second key. To pin it, put the key in the shared config's `general` section, or ask the agent to.

The cell's own section tunes it; leave it absent and the defaults below stand:

```jsonc
{
  "prompt-optimizer": {
    "family": "auto",     // auto | claude | openai | google | deepseek | qwen
    "max_tokens": 400,     // the most the rewriting model may answer
    "min_words": 3,        // a prompt of fewer words stands as it is
    "record": true         // keep a local record, which /prompt-stats reads
  }
}
```

## Use it

```
/optimize hey could you maybe explain python generators when you get a chance

prompt (claude family): 34 -> 82 points (+48)
  clarity 10->25  specificity 9->12  structure 14->20  actionability 8->25
  18 -> 4 words, about 22 -> 6 tokens

Explain Python generators.
```

- `/optimize PROMPT` rewrites what you type after it; typed alone it opens a box that asks for the prompt.
- `/prompt-stats` says how many rewrites are recorded, what they gained, and the last three.
- The model calls `optimize_prompt` when you ask it to polish or tighten a prompt, and gets the same page back.
- `(nodecode-prompt-optimizer:rewrite-prompt "..." :target "claude-opus-4-7")` does the same through eval; `(help :prompt-optimizer)` is the manual.

**The target picks the tactics.** The family is read off the target model's name when the section says `auto`: a Claude target is asked for tagged structure, an OpenAI one for the instruction first, a Gemini one for the task before its context, DeepSeek for its constraints set apart, Qwen for its role in one clause. An unknown model gets the plain rule, without made-up guidance.

**The score** is the original's, in Lisp: clarity (how much is filler), specificity (how much names something exact), structure (whether it is laid out as more than one thought), actionability (whether it opens with a verb that names the task), each 0-25, 100 together -- and an ordinary four-characters-to-a-token estimate of what it costs.

**Nothing leaves the machine** but the prompt itself, to the provider you already run. The record is one JSON line a rewrite under `~/.nodecode/prompt-optimizer/rewrites.jsonl`; delete the file to forget it.

## What the rewrite does not do

The original intercepted every message of five words or more before it reached the model, asked for approval in an overlay, kept its history in SQLite, wrote HTML reports, detected the prompt's language with a Python package, and carried tactics for twenty-one vendor families. Here the family is read from five, a non-English prompt is preserved by instruction rather than by detection, and the history is one JSONL file under the home.
