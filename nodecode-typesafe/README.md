# nodecode-typesafe

A Nodecode cell for [TypeSafe](https://typesafe.ai)'s System One judgments:
typed questions about one state, answered as probabilities. With it the model
can call, through `eval`,

```lisp
(typesafe:judge "The patch renames the public function parse-config."
                '(:risky (:type "bool" :instructions "Does this change risk breaking callers?")
                  :kind  (:type "choice" :instructions "What kind of change is this?"
                          :criteria (:fix nil :feature nil :refactor "restructures without changing behaviour"))
                  :size  (:type "score" :instructions "How large is the change?"
                          :criteria ("trivial" "small" "medium" "large"))))
;; => (:RISKY (:TYPE :BOOL :BOOL 0.82)
;;     :KIND (:TYPE :CHOICE :CHOICE "refactor" :PROBABILITIES (("fix" . 0.1) ...) :CONFIDENCE 0.6)
;;     :SIZE (:TYPE :SCORE :SCORE 1.4 :PROBABILITIES (("0" . 0.1) ...) :CONFIDENCE 0.4)),
;;    "typesafe/jev-latest", (:INPUT-TOKENS 212 :OUTPUT-TOKENS 0)
```

and `(typesafe:models)` lists the judgment models the key may use. While the
cell runs, `(help :typesafe)` answers the manual, and every request's help
section carries one line naming the two verbs.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `typesafe`
provider. See [NOTICE](NOTICE).

## Why functions, not a chat lane

omp catalogues TypeSafe's `jev-latest` under the `judge` kind, not as a chat
model: its api, `typesafe`, is one `POST /v1/systemone` that takes `{state,
model, questions}` and answers `{model, answers, usage}`. There is no
conversation, no tool call and no stream, so no Nodecode lane could carry a
turn on it, and a catalog row would put `jev-latest` in `/models` as a model a
turn could pick and never run. omp itself reaches it the way this cell does:
its eval runtimes expose `judge()` (`coding-agent/src/eval/judgment-bridge.ts`),
which checks the questions and calls `TypeSafeJudge`. In Nodecode, `eval` is
the model's one tool and a cell's Lisp functions are its vocabulary (as
`web:search` is the websearch cell's), so the port is a pair of functions in
the `typesafe:` package with a help topic.

What is carried:

- the request `{"state", "model", "questions"}` to `<base>/v1/systemone` with
  the bearer key, `Accept` and `Content-Type: application/json`
- the questions checked before anything goes, as the eval bridge checks them:
  `choice` needs two or more options, each a rubric or nil (sent as null);
  `bool` (the wire's `noul`) may carry `true` and `false` descriptions;
  `score` needs two or more levels, lowest first; instructions are non-empty
  text; a state is non-empty text, or a JSON object or array
- questions and state as Lisp (keyword plists, lists) or as JSON (hash tables,
  or a JSON text for the questions, which keeps labels exactly as written)
- three attempts at 10 s each; a 408, 429, 5xx or a dropped connection is
  tried again after what `Retry-After` says (at most 5 s), else 0.5 s, 1 s;
  any other status is said with TypeSafe's own words
- every question must come back with an answer of its type, else the
  judgment fails naming it; `noul` comes back as `bool`, as the eval bridge
  shows it
- the key never reaches the model: a refusal's text is cleaned of it

## Key

Make a key at <https://console.typesafe.ai/>. Set `TYPESAFE_API_KEY`, or keep
it in `~/.nodecode/auth.json` the way `/connect` keeps a provider key:

```json
{"api_keys": {"typesafe": {"provider": "typesafe", "key": "..."}}}
```

The saved key wins over the variable. `/typesafe status` says where the key
comes from (asking no one); `/typesafe check` asks TypeSafe's model listing
with it, as omp validates a key, and says how it answered.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-typesafe/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "typesafe": {
    // empty: TYPESAFE_BASE_URL, else https://api.typesafe.ai
    "base_url": "",
    // empty: TYPESAFE_DEFAULT_MODEL, else jev-latest; a call's :model wins
    "model": ""
  }
}
```

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-typesafe
```

## Gaps

- **`/connect` for a key.** `/connect` offers only catalog providers a lane
  drives with at least one turn model (`connect-picker-rows`,
  `provider-id-refusal`), so it cannot save a key for a provider that is no
  lane. The cell reads the same `api_keys.typesafe` entry `/connect` would
  write; saving one from a shell needs a core seam that lets a cell offer a
  key-only provider to `/connect` (a catalog row flag, or a `:connect` hook
  point answering the providers it keys).

## Not ported

- omp's `judge` model role chain: when no TypeSafe key is configured omp
  answers the same questions with an ordinary chat model through keyword
  prompts (`judgment/text.ts`, `chat.ts`), and with a tiny local model; here a
  judgment needs TypeSafe.
- `judge_batch()` (many states, one question set) and omp's judgment cache;
  call `typesafe:judge` in a loop.
- OpenRouter's `decisions` route, which shares the wire under another path.
- Billing on the session ledger: the usage is returned, not journalled.

MIT licensed.
