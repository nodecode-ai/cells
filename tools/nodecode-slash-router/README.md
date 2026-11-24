# nodecode-slash-router

Slash Router: sends a misspelled or shortened slash command to the live command it meant, through TypeSafe's Jev. A Nodecode rewrite of [raitoxlol/hermes-slash-router](https://github.com/raitoxlol/hermes-slash-router) by [raitoxlol](https://github.com/raitoxlol).

A user types `/modles` and nothing happens. The agent calls

```lisp
(slash-router:route "/modles")
;; => "/modles -> /models (confidence 0.93, model jev-1.13.0). Run /models; the
;;     arguments the user typed still belong to it."
```

and runs the right command. A command whose name is already right, including
its native aliases, answers without a lookup, so `/help` and `/new` cost
nothing. Everything else is one paid TypeSafe API call against Nodecode's own
live slash catalog -- every command this image can run, the ones cells
registered included -- asking Jev one choice question: same request, same
rubric and the same 0.85 confidence floor as the original. Under that floor,
or when Jev names nothing, the answer says so and asks the user what they
meant instead of guessing.

When the user explains, keep it:

```lisp
(slash-router:remember "modles" "models" "open the model picker")
```

The correction is advice: later lookups of that token carry it as a prior
decision and lookups of other tokens carry it as one of up to eight examples,
and Jev still decides every use. `(slash-router:forget "/modles")` drops it,
with no argument the most recent one, and `(slash-router:status)` says where
the key comes from, the base, the model and how many corrections are saved
without asking anyone.

What leaves the machine: the token, the command names and descriptions, a
prior decision and those examples -- never a message, a file or a
conversation. Arguments are the caller's to keep; the router routes the name.

## Get it

Tell the agent "add the slash-router cell", or run `nodecode add nc://slash-router`.

## Set it up

Make a key at <https://console.typesafe.ai/>, then set `TYPESAFE_API_KEY` or
keep it in `~/.nodecode/auth.json` the way `/connect` keeps a provider key:

```json
{"api_keys": {"typesafe": {"key": "..."}}}
```

Every non-exact lookup uses paid TypeSafe API usage. The section is optional:

```jsonc
{
  "slash-router": {
    // empty: TYPESAFE_BASE_URL, else https://api.typesafe.ai
    "base_url": "",
    // empty: TYPESAFE_DEFAULT_MODEL, else jev-1.13.0
    "model": ""
  }
}
```

A `base_url` of your own speaks the same `POST /v1/systemone` -- a local
[DJev](https://github.com/mmastrac/djev-spark) server, say -- and takes no key
when none is set. `/slash-router` prints the status and opens its panel;
`/slash-router check` spends one lookup to prove the key works, and
`/slash-router forget` drops the most recent correction.

## Use it

Ask the agent to send a typed shortcut to the command it meant, or call the
verbs yourself from eval:

```lisp
(slash-router:route "/hlep")                       ; one lookup
(slash-router:route "/widgts" :commands
  '(("widgets" . "list the widgets") ("gadgets" . "list the gadgets")))
(slash-router:route "/modles" :explanation "open the model picker")
(slash-router:remember "/mdl" "models" "pick a model")
```

`:commands` routes onto a list of your own instead of the live catalog;
`:explanation` is what the user said they wanted, which Jev weighs first.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-slash-router
```

Every TypeSafe exchange in the tests is a stubbed `dex:request`, every key
store a scratch `auth.json`, every correction file a scratch JSON file: the
run needs no network and touches none of the operator's files.

## Not carried over

The desktop half of the original plugin: the composer middleware that
rewrote a draft in place, the "What did you mean?" field under it, the status
bar indicator, the command-palette actions to copy routing history or forget
the last reminder, and the installer. Reminders live in one JSON file under
the Nodecode home instead of Desktop plugin storage, and the catalog is
Nodecode's own. See [NOTICE](NOTICE).

MIT licensed.
