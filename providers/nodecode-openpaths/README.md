# nodecode-openpaths

OpenPaths: auto task-tier routing and hosted models, as a Nodecode provider. A Nodecode rewrite of [lee101/hermes-openpaths](https://github.com/lee101/hermes-openpaths) by [lee101](https://github.com/lee101).

OpenPaths serves an OpenAI-compatible wire at `https://openpaths.io/v1`. With
this cell, `/connect` offers OpenPaths, `/models` lists its models, and a turn
on one of them goes out in the form OpenPaths takes:

- the roster is led by its task-tier routes — `auto`, `auto-easy-task`,
  `auto-medium-task`, `auto-hard-task`, `auto-think` — which pick a vendor
  model per request; the vendor ids it fronts follow
- `reasoning_effort` is sent in OpenPaths' own vocabulary,
  `none`/`low`/`medium`/`high`/`auto`: a pick it does not take is lowered to
  the nearest level it does (`minimal` becomes `low`, `xhigh` and `max`
  become `high`), turning reasoning off sends `none`, and an `auto-think`
  route with no pick at all is sent `auto`
- nothing is sent for a model when no effort was picked, so OpenPaths' own
  default applies

The model list is OpenPaths' own `/models`, read once at start and refreshed
hourly, with models.dev's rows for context and prices where it publishes
them. Before either has been read, the list is the curated one the original
named.

## Get it

Tell the agent "add the openpaths cell", or run `nodecode add nc://openpaths`.

## Set it up

Make an API key at [openpaths.io](https://openpaths.io/), then pick OpenPaths
in `/models` and paste the key when the picker asks, or set
`OPENPATHS_API_KEY` in the environment. A saved key wins over the variable.
The address is optional, for a relay or a proxy:

```jsonc
{
  "openpaths": {
    "base_url": "https://openpaths.io/v1"
  }
}
```

## Use it

Ask for a model — `/models` lists what OpenPaths currently serves — then talk
as usual. `/think` offers the rungs an OpenPaths route takes; a pick outside
them is lowered to one it does before the request leaves.

Defaults, if you want them up front: `auto-medium-task` is the general
route, `auto-easy-task` the cheap one for side work (compression,
summaries), `auto-hard-task` for the difficult turns, and `auto-think` for a
route that reasons before it answers.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-openpaths
```

MIT licensed. See [NOTICE](NOTICE).
