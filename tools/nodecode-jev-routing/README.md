# nodecode-jev-routing

Jev Routing: per-prompt model routing from TypeSafe Jev. A Nodecode rewrite of [apoapostolov/hermes-agent-awesome-plugins](https://github.com/apoapostolov/hermes-agent-awesome-plugins) by [apoapostolov](https://github.com/apoapostolov).

Type normally. When a prompt is worth thinking about, this cell asks TypeSafe
Jev four typed questions about it -- what kind of work it is, how hard it is,
how much model capability it deserves, and whether it needs deep reasoning --
and composes the answers into a capability tier. Then it names the model
*your own routing table* holds for that tier: the fast free model for a
one-line fix, the frontier model for an architectural change.

The original does this inside every Hermes turn and rewrites the request.
Nodecode hands a cell no per-turn model hook, so this one is the decision
engine itself: the model calls it through `eval` and acts on the answer --
run the subtask on that model, switch with `/model`, or just know what the
turn is worth.

## Get it

Tell the agent "add the jev-routing cell", or run `nodecode add nc://nodecode-jev-routing`.

## Set it up

Two things: a TypeSafe key, and a routing table.

Make a key at <https://console.typesafe.ai/>. Set `TYPESAFE_API_KEY`, or keep
it in `~/.nodecode/auth.json` the way `/connect` keeps a provider key -- the
same `api_keys.typesafe` entry the typesafe cell reads, so one key serves
both:

```json
{"api_keys": {"typesafe": {"provider": "typesafe", "key": "..."}}}
```

The routing table is a JSON file you write, pointed at by `config_path`:

```jsonc
{
  "jev-routing": {
    "config_path": "~/routing.json",   // empty leaves the cell inert
    "mode": "shadow",                  // shadow, notify, or off
    "base_url": "",                     // empty: TYPESAFE_BASE_URL, else https://api.typesafe.ai
    "model": "",                        // empty: TYPESAFE_DEFAULT_MODEL, else jev-latest
    "timeout_ms": 3500
  }
}
```

A `base_url` of your own speaks the same `POST /v1/systemone` (a local DJev
server, say); with no key saved or set, the request goes without
`Authorization`. `/jev-routing` says where the table is, which chains it holds and
what the ledger says, asking no one.

## The routing table

```jsonc
{
  "routes": {
    "quick":    [{"provider": "opencode-go", "model": "beam-free"}],
    "standard": [{"provider": "opencode-go", "model": "glm-5.3"}],
    "high":     [{"provider": "openai-codex", "model": "gpt-6-sol", "thinkingLevel": "medium"}],
    "premium":  [{"provider": "openai-codex", "model": "gpt-6-sol"}],
    "xpremium": []
  },
  "kindMinimumTier": {"plan": "high", "review": "high", "implement": "standard"},
  "kindModels": {"implement": [{"provider": "openai-codex", "model": "gpt-6-sol", "minTier": "standard", "priority": 20}]},
  "confidenceThreshold": 0.34,
  "minPromptChars": 12,
  "budget": {"dailyUsd": 10, "monthlyUsd": 150, "softRatio": 0.7, "hardRatio": 0.9},
  "free": {"enabled": true, "policy": "fallback-only", "pool": [{"provider": "opencode-go", "model": "beam-free"}]},
  "pricing": {"opencode-go/glm-5.3": {"input": 1.4, "output": 4.4}}
}
```

A tier the file leaves empty stays empty; the loader never invents a model
list. `kindModels` names a model per kind of work, from a tier up;
`kindMinimumTier` is a floor a protected kind never routes below. Over the
soft budget ratio the pick drops one tier, over the hard ratio it drops to
quick unless the demand is already premium, and an empty `free` pool changes
nothing. A `pricing` row is per million tokens, in dollars.

## Use it

Through `eval`, while the cell runs:

```lisp
(jev-routing:route "Refactor the parser into two modules.")
;; => (:TIER "premium" :DESIRED-TIER "premium" :PROVIDER "openai-codex" :MODEL "gpt-6-sol"
;;     :THINKING NIL :KIND "implement" :DEMAND 3.0 :CONFIDENCE 0.81
;;     :BUDGET-PRESSURE 0.0 :LOW-CONFIDENCE NIL :DOWNGRADED NIL :NOTES NIL
;;     :GLYPH "." :LINE "tier premium · openai-codex/gpt-6-sol · kind implement · ..."
;;     :LATENCY-MS 96)

(jev-routing:route "ok")                     ; => (:SKIPPED "acknowledgement") -- nothing sent
(jev-routing:judge "Review this diff.")       ; the four answers alone, for the why
(jev-routing:spend)                            ; today, this month, the caps, the pressure
(jev-routing:record "gpt-6-sol" :provider "openai-codex" :input 12000 :output 800)
```

`HISTORY` (the recent conversation, at most 4000 characters go),
`ACTIVE-MODEL` and `CONTEXT-TOKENS` are optional, and they make a short
follow-up route on what it means rather than on its eleven characters.

`/jev-routing` says where the table is, its chains and the ledger. `/jev-routing route TEXT`
names the tier a text deserves; `/jev-routing why` re-asks Jev about the last prompt
and shows the full judgment, probabilities and all; `/jev-routing spend` is the
ledger. `(help :jev-routing)` answers this manual.

**What leaves the machine.** The prompt, up to 4000 characters of the
conversation you pass as history, the working directory, the active model,
an estimate of the context in use, and the spend snapshot, to TypeSafe alone
(`https://api.typesafe.ai` unless you set `base_url`). Nothing else, and
nothing at all for a prompt that does not re-route. A missing key, a timeout
or a bad answer signals `JEV-ERROR` and changes nothing.

**The ledger** is `<table-stem>-state.json` beside the routing table: days
and months, split by model, with the original's shape. A reported USD total
wins; otherwise your `pricing` rows price the tokens; an unpriced model
records tokens and no dollars rather than inventing spend. Only what you
`record` is in it -- no turn is recorded for you.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-jev-routing
```

## Not ported

- **The turn.** The original rewrites the provider request per turn, offers
  an approve row for guarded tiers, and moves the session through the app's
  model switch. A cell has no per-turn model hook, so this one answers and
  the caller acts.
- **The catalogue, quota and cache hold.** `decide.py` filters a live model
  list, reads Codex quota windows and holds a warm cache. A cell reads no
  catalogue, so every configured row is eligible -- the catalog edition's
  own behaviour -- and no pick is held for the cache.
- **`/jev-routing suggest`, the ranking and the generated layer**, and the
  desktop half in JavaScript.
- **Confirm clicks, pinning and stickiness** (`/jev-routing revert`, the
  `jev-confirm:` marker, `stickiness`), which belong to a turn stream.
- **`TYPESAFE_API_URL`**: a base is set with the section's `base_url` or
  `TYPESAFE_BASE_URL`.
