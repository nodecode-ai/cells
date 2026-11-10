# nodecode-vram

Coffee VRAM: whether a local Ollama model fits the GPU room that is left, and what to unload when it does not. A Nodecode rewrite of [jimmy77733/coffee-vram](https://github.com/jimmy77733/coffee-vram) by [jimmy77733](https://github.com/jimmy77733).

The original guards the local Ollama calls a harness makes: before each one
it estimates whether the model fits next to what is already loaded, holds the
call while an idle model is unloaded to make room, extends `keep_alive` on
models it wants warm, and shows a chip with all of it. A cell cannot stand
between the turns of a provider lane, so this one answers the same questions
and offers the same action: `vram_status` (the GPU and what `/api/ps`
holds), `vram_fit` (the estimator and the decision), `vram_unload` (the
`keep_alive: 0` the original's evict sent), and `/vram` for the operator.

## Get it

Tell the agent "add the vram cell", or run:

```sh
nodecode add nc://vram
```

## Set it up

Nothing is required. Ollama on the default `http://127.0.0.1:11434` and an
`nvidia-smi` on `PATH` are read as they are; without an NVIDIA program the
cell falls back to Ollama's own figures, as the original did.

```jsonc
{
  "vram": {
    "ollama_url": "http://127.0.0.1:11434", // the Ollama to read and unload on
    "smi_path": "nvidia-smi",               // empty or missing: Ollama's own numbers
    "gpu_total_mb": 0,                       // the pool by hand on a driver that misreports it
    "reserve_mb": 2048,                      // left for the desktop
    "warn_pct": 85,
    "hold_pct": 95,                          // the fit check budgets up to here
    "kv_cache_type": "f16",                  // match OLLAMA_KV_CACHE_TYPE
    "estimate_overhead_pct": 10,
    "pinned_models": []                      // never suggested, never unloaded
  }
}
```

A section member outside its range, or an `ollama_url` without `http://` or
`https://`, refuses at start, and the refusal names the key.

## Use it

The model has three tools while the cell runs:

- `vram_status {full?}` — the pool, its use, the level (`ok`, `warn`,
  `hold`), every loaded model with the room it takes, what spilled into
  system RAM, its context and when it goes, and the room left before the
  hold line.
- `vram_fit {model, context?}` — the estimate for a model at that context
  (weights, KV cache from the model's own shape, margin), the room now, and
  either "Fits" or the models worth unloading and the tool that unloads one.
- `vram_unload {model}` — one idle model out of Ollama. It loads again on
  its next call; a pinned model and one Ollama does not list are refused.

`(help :vram)` is the primer behind them, and `(vram:status)`,
`(vram:fit "qwen3:8b")`, `(vram:unload "llama3:8b")` are the same three
from eval. `/vram` answers the status; `/vram models`, `/vram settings`,
`/vram fit <model> [context]` and `/vram evict <model>` are the rest.

Read it before switching to another local model: loading one that does not
fit unloads another, or runs partly on the CPU.

## What changed

- The estimator, the budget cap, the pinned list and the eviction order are
  the original's; the GPU pool is read with `nvidia-smi` instead of the
  NVIDIA driver library, and Ollama's figures are the fallback.
- The call queue is gone: nothing here holds a live local call. The model
  asks `vram_fit` first and unloads with `vram_unload` when it wants the
  room, and the operator does the same through `/vram`.
- Keep-warm, the background sampler, the desktop chip and pane, and the
  original's own CLI are the harness's surfaces and are left out (NOTICE).

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-vram
```

Every request is a stubbed `NLK:HTTP` and the GPU reader a bound `*RUN*`, so
nothing reaches Ollama, the network or a GPU.

MIT licensed.
