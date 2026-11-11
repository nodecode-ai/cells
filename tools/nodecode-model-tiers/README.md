# nodecode-model-tiers

The Claude model hierarchy as Claude Code's own prompt describes it, quoted with line numbers. Above Opus sits a **Mythos** tier, and the model most people are talking to, **Fable 5.2**, is a Mythos-tier model with extra safety measures for biology and cybersecurity. It shares one underlying model with **Mythos 5.2**.

The source is one file: [`ANTHROPIC/CLAUDE-CODE-FABLE-5.2.txt`](https://github.com/elder-plinius/CL4R1T4S/blob/main/ANTHROPIC/CLAUDE-CODE-FABLE-5.2.txt) in [elder-plinius/CL4R1T4S](https://github.com/elder-plinius/CL4R1T4S), 957,315 bytes, 7,832 lines, committed 9 November 2026. Nothing here is a vendor announcement: it is a leaked system prompt, read.

## Get it

Tell the agent "add the model tiers cell", or run `nodecode add nc://model-tiers`.

## Use it

Ask in your own words: *what is above Opus*, *is Fable the same model as Mythos*, *what did the export controls do*, *what does the prompt say about kernel optimization*. The model reads the same hierarchy you do.

- `model_tiers_list` - the ladder, tier by tier (`mythos`, `opus`, `sonnet`, `haiku`), with what each tier is and which models sit on it.
- `model_tiers_get` - one model whole: where it sits, whether a person can have it, its API string, and the file's own sentences with the line each stands on. A fragment of the name is enough.
- `model_tiers_search` - any word, over names, tiers, availability, notes and quoted sentences.
- `model_tiers_quote` - the file itself: every line carrying a word, numbered, or a line range as `116-122`. This is the one verb that fetches, once.

In chat: `/model-tiers` for the ladder and the timeline, `/model-tiers mythos` for one tier, `/model-tiers fable` for one model, `/model-tiers source` for the file's digest and link, `/model-tiers lines 116-122` for the prompt's own words, `/model-tiers kernel` for every line carrying a word.

From Lisp, with the cell on:

```lisp
(nodecode-model-tiers:model-by-name "fable")
(nodecode-model-tiers:models-matching :tier "mythos")
(nodecode-model-tiers::quote-text :lines-text "116-122")
```

## What it does not do

It does not ask Anthropic anything, and it does not crawl. The hierarchy answers from `data.lisp` with no request, no key and no account. Only `model_tiers_quote` reaches the network: it fetches the file once, caches it under `~/.cache/nodecode-model-tiers/`, and reads the cache after that.

## Read it with care

The file is a snapshot: it says what it said on 9 November 2026, and no more. Line numbers are given so a person can check every quote against the file itself - that is what `model_tiers_quote` is for. Where the file and a vendor page disagree, this cell gives the file's reading of itself, and it says so in every answer.

The hierarchy is the file's own words, not an announcement: nobody published "Mythos is above Opus"; it was read out of a 7,832-line system prompt.

## Set it up

Nothing is required.

## Credit

The file is [elder-plinius](https://github.com/elder-plinius)'s leak, in [CL4R1T4S](https://github.com/elder-plinius/CL4R1T4S); the words quoted are Anthropic's. The code is MIT, ours. See `NOTICE`.
