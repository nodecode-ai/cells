# nodecode-jailbreaks

A working collection of jailbreak prompts, indexed: **51 files in 19 folders**, and every one of them quotable by line.

The collection is [togg53192-cmd/jailbreaks](https://github.com/togg53192-cmd/jailbreaks) - 16,208 stars, prompt files for Claude, GPT, Gemini, Grok, Kimi, Qwen, GLM, DeepSeek, Gemma, Mistral, Muse, and the free stealth models. What this cell adds is the part a clone does not give you: **what each file is**, and the ability to read any of it without cloning anything.

## Get it

Tell the agent "add the jailbreaks cell", or run `nodecode add nc://jailbreaks`.

## Use it

Ask in your own words: *what jailbreaks exist for Gemini*, *which of these are real system prompts*, *what does the Jack register actually say*, *where does "UNRESTRICTED" appear in the collection*. The model reads the same index you do.

- `jailbreaks_list` - the catalogue: every prompt with its target, its kind, its folder and its size. Narrow by folder (`Anthropic`, `Zhipu AI`, `0x8D jailbreaks`), by kind, or by a word.
- `jailbreaks_get` - one prompt whole: what it targets, its digest, and its opening lines quoted. A fragment of the key is enough.
- `jailbreaks_search` - a word over the catalogue, and any question the collection answers as a whole.
- `jailbreaks_grep` - a word *inside* the prompts: every line carrying it, with file and line, across all 51 or one.
- `jailbreaks_quote` - one prompt's own lines: a range (`1-40`) or every line carrying a word, numbered.

In chat: `/jailbreaks` for the collection by kind, `/jailbreaks kinds` for the legend, `/jailbreaks persona` for one kind, `/jailbreaks Anthropic` for one folder, `/jailbreaks jack` for one prompt, `/jailbreaks lines jack 1-40` to read it, `/jailbreaks grep UNRESTRICTED` to search inside them.

## The kinds, which are the real finding

The same word covers four different things, and the collection does not distinguish them. This cell does:

| kind | n | what it is |
|---|---|---|
| `workspace-policy` | 22 | a deployment's own operating specification - the biggest family, one template adapted per model |
| `persona` | 13 | a named voice or posture, written to be adopted |
| `content-policy` | 4 | a rewrite of what the model will produce |
| `system-prompt` | 8 | **a model's own prompt, as it was run** - evidence, not a technique |
| `modelfile` | 2 | an Ollama Modelfile wrapping a prompt |
| `withdrawn` | 1 | pulled by its author, kept as a notice |
| `empty` | 1 | a placeholder with no prompt in it |

That distinction is worth having before you quote one in an argument: a `system-prompt` tells you what a vendor did; a `workspace-policy` tells you what someone tried.

## What the digests found

- **`jack`** - one 302-line register, four files, byte-identical: filed for ChatGPT free tier, Gemini chat, Grok fast and Kimi standard. One prompt, four targets, unadapted for any of them.
- **`unified`** - one scope note filed twice, once under `DeepSeek/` and once under `Stealth Models/`.

Both are in the index, so `jailbreaks_get jack` says the file is one of four rather than letting a reader assume four people wrote it.

## What it does not do

It does not run the prompts, test them, or claim they work. It does not clone the collection: a file is fetched from the author's own repository when you ask for it, and cached. And it carries **no licence** - the collection has no licence file at all, so the prompts are quoted with their file and line and redistributed by nobody but their author. See NOTICE.

## Read it with care

These are prompts written to get past a model's guardrails. Some are aimed at free tiers, some at agents with tools and files. Read one before you paste it into anything that can act on your machine - and note that the collection itself withdrew one after a security report.
