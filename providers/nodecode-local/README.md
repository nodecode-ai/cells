# nodecode-local

A Nodecode cell for oh-my-pi's `local` provider: small text models that run
on the machine, in the worker processes omp runs them in. With it,
`/model-aux local lfm2.5-230m` makes one of them the auxiliary model, the
one Nodecode runs side work on (naming sessions, recaps), and a round on it
asks the model's worker over its Unix socket.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `local`
provider (api `local-inference`, base `local://inference`). See
[NOTICE](NOTICE).

## What omp runs, and what of it a cell can drive

omp's `local` provider is no endpoint. Each tiny model is served by one
worker process on the machine, which owns a Unix socket named after the model
(`<runtime dir>/<model>-<backend>.sock`), serves every process that dials it,
and exits after fifteen idle minutes. It speaks newline-delimited JSON
(`coding-agent/src/tiny/title-protocol.ts`): a `ping` answered with `pong`, a
`chat` request (`messages` of `system` and `user` turns, `maxNewTokens`)
answered with `progress` events and then the generated `text`, or an
`error`. omp ships two workers:

- **ONNX** (every platform): the omp binary itself, re-entered as `omp
  __omp_worker_tiny_inference`, running transformers.js over
  onnxruntime-node inside omp's Bun runtime. Nothing of it can run from
  Lisp; the cell drives the same binary when omp is installed (`omp_command`),
  with the environment omp gives it (`OMP_TINY_WORKER_SOCKET`,
  `OMP_TINY_WORKER_MODEL`, `OMP_TINY_WORKER_TAG`).
- **MLX** (Apple silicon): `mlx-server.py`, a Python script run by a venv
  holding mlx-lm 0.31.3. The cell carries the script verbatim
  (`worker/mlx-server.py`) and starts it as omp does, under omp's venv when it
  is there or the `python` the section names.

So the cell is a client of omp's workers:

- a round first dials the model's socket in omp's own runtime directory
  (`$XDG_STATE_HOME/omp/run/tiny/` when that `omp` directory exists, else
  `~/.omp/run/tiny/`), so a worker omp already started serves Nodecode too;
  any worker that answers a ping is used
- when none answers, it starts one (unless `spawn` is false), detached, its
  output in `<model>-<backend>.log` beside the socket, and waits up to 120 s
  for it to bind; a worker that exits says the tail of its log
- the conversation is flattened as omp's transport flattens it
  (`local-inference-api.ts`): the system prompt and history system messages
  as `system`, each assistant turn's text folded into the user turn after
  it, a tool result as the user's, a trailing assistant turn as a user turn
- `maxNewTokens` is the configured output ceiling, else 256, held within 1
  and 1024
- the answer is the round's text; an empty one is omp's `Local inference
  returned no output.`, and a worker's `error` is said with its words

The eight text models are listed (`lfm2.5-230m`, `lfm2.5-350m`,
`falcon-h1-90m`, `qwen3-1.7b`, `llama3.2:3b`, `gemma-3-1b`, `qwen2.5-1.5b`,
`lfm2-1.2b`), with no window, no price and no tool calls, as omp seeds them.
Because they call no tools, `/models` (which lists turn models) does not offer
them; type `/model-aux local <model>`. `qwen3-1.7b` runs on MLX only, as
omp's ONNX backend refuses it.

## What cannot run from a cell

- Without omp installed there is no ONNX worker: its runtime is omp's Bun
  binary with transformers.js and onnxruntime-node, which nothing in
  Nodecode carries. Set `omp_command` to omp's path, or let omp start the
  worker (any omp session that generates a title does).
- The MLX worker needs Apple silicon and a Python with mlx-lm. omp installs
  that venv itself (`uv` or `python3 -m venv`, then `pip install
  mlx-lm==0.31.3`); the cell does not install anything, it uses omp's venv or
  the `python` you name.
- Either worker downloads its model's weights from the Hugging Face Hub the
  first time it loads it (into omp's `tiny-models` cache), as it does for
  omp.
- Kokoro (speech) and Parakeet and Whisper (transcription), the other five
  rows of omp's `local` provider, run in other omp workers and are not
  carried: they are not text models.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-local/` and restart
Nodecode, or run `(restart-cells)`. Then `/model-aux local lfm2.5-230m`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "local": {
    "backend": "auto",        // auto, onnx or mlx
    "omp_command": "omp",     // the omp binary an ONNX worker starts from
    "python": "",             // empty: omp's mlx-lm venv, else python3
    "runtime_dir": "",        // empty: omp's run/tiny directory
    "spawn": true             // false: only use a worker already up
  }
}
```

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-local
```

The worker is a stand-in on a real Unix socket in `/tmp`, answering from a
thread of the test process; starting one is a stubbed `spawn-worker` that
records the command and puts the stand-in up. No process is started and no
weight is downloaded.

## Not ported

- omp's connection reuse: it keeps one socket per worker and multiplexes
  requests by id; a round here dials, asks once, and closes.
- omp's launch tags: omp replaces a worker whose tag is not its own build's;
  this cell uses any worker that answers, and omp replaces one this cell
  started the next time it looks.
- the title, memory and completion prompts omp builds around a chat
  (`<title>` prefill, stop strings): Nodecode's side calls bring their own
  prompts.
- the device and dtype choices of the ONNX worker (`PI_TINY_DEVICE`,
  `PI_TINY_DTYPE`) pass through the environment to the worker unchanged.

MIT licensed.
