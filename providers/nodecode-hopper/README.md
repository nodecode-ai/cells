# nodecode-hopper

Hopper: a small OpenRouter catalog of your own, offered as a provider. A Nodecode rewrite of [mykeura/hopper](https://github.com/mykeura/hopper) by [mykeura](https://github.com/mykeura).

OpenRouter's own provider in Nodecode lists everything OpenRouter serves. Hopper
lists only the model ids you wrote down: a free model that is available for a
while, a model a release added this morning, a shortlist for a week's work. The
picker offers `hopper/<id>` for each of them and nothing else, and a turn on one
goes to `https://openrouter.ai/api/v1` with the id exactly as the line wrote it.

The list is one text file:

```text
~/.nodecode/hopper/models.txt
```

one OpenRouter model id a line, `#` starting a comment, blank lines skipped. A
fresh file holds the original plugin's default model:

```text
inclusionai/ling-3.0-flash-vl:free
```

A line is a model id the way OpenRouter spells it, `provider/model`, with any
routing or price suffix the id carries (`:free`, `:nitro`). Model availability
and prices are OpenRouter's and may change; a line that is not `provider/model`
is skipped rather than offered.

## Get it

Tell the agent "add the hopper cell", or run `nodecode add nc://hopper`.

## Set it up

The key is OpenRouter's own: make one at
<https://openrouter.ai/settings/keys>, then save it with `/connect` (provider
`hopper`) or set `OPENROUTER_API_KEY`. A saved key wins over the variable, and
one key serves Hopper and OpenRouter alike.

Nothing else is required. The section is on unless it says `"enabled": false`:

```jsonc
{
  "hopper": {
    // where OpenRouter is served; the chat lane appends /chat/completions
    "base_url": "https://openrouter.ai/api/v1",
    // the list; empty is hopper/models.txt in the organism's home
    "models_file": ""
  }
}
```

## Use it

```
/hopper                 the list as it stands
/hopper add ID...       add model ids
/hopper remove ID...    take them out
/hopper reset           back to the default model
/hopper file            which file the list lives in
```

`/hopper add anthropic/claude-sonnet-4.5` adds a line and the next `/models`
open offers it, without a restart: the cell drops the catalog it built, so the
row the picker reads is built again from the file. Editing the file by hand does
the same once the picker opens again.

`(help :hopper)` has the same words while the cell runs.

A key typed at `/connect` is checked the way it will be spent: the cell asks
OpenRouter's own listing with it, and keeps none of that listing's rows.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-hopper
```

## Gaps

The original also shipped a small command-line helper (`manage.py`) and, before
it was stripped for catalog intake, a desktop half. Here the same verbs are
`/hopper`'s, and the file is the one interface both halves shared, so nothing
else is carried.

The original read `HERMES_HOME` and `HOPPER_MODELS_FILE` from the environment;
this cell reads the `hopper` section instead, so the same list is a setting the
operator can see in `/settings` rather than a variable only a shell remembers.

MIT licensed.
