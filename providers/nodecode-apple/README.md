# nodecode-apple

A Nodecode cell for Apple Foundation Models, the language model macOS 27
runs on the machine (Apple silicon, Apple Intelligence on). On such a Mac,
`/models` lists `apple/on-device` once the model says it is available, and a
turn on it runs on the machine, tool calls included. On any other machine the
cell says once that it does nothing there, and registers nothing.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `apple`
provider (api `apple-foundation-models`). See [NOTICE](NOTICE).

## The bridge

omp reaches the model through a Swift bridge,
`crates/pi-natives/src/applefm/bridge.swift`: built into a dylib with
`build-bridge.sh`, embedded in omp's native addon, and `dlopen`ed on macOS 27
behind a four-function C ABI (`omp_applefm_availability`,
`omp_applefm_generate`, `omp_applefm_cancel`, `omp_applefm_free`). Every
request lowers the whole conversation into a Foundation Models transcript,
and the bridge streams exactly one model turn back as JSON events on Swift's
own threads.

The cell drives the same bridge. It carries `bridge.swift` verbatim and a
`main.swift` of its own that wraps the C ABI in a small executable, and
builds the two into one helper with `bridge/build.sh` (omp's toolchain
detection: Swift 6.4+ with the macOS 27 SDK, from `$OMP_APPLEFM_SWIFTC`, the
selected Xcode, or the Command Line Tools). A Lisp image cannot take the
bridge's callbacks on Swift's threads, so the helper runs as a child, one
request per process:

- `helper availability` prints the availability event
- `helper generate` reads the request on stdin and prints each event as a
  line; ending the process cancels the generation

The helper is built on first use into the user cache
(`~/.cache/nodecode-apple/nodecode-apple-bridge-<hash of the sources>`), or
named with `helper`. The lane (`wire.lisp`) ports omp's transport:

- the request: the system prompt as `instructions`; user and history system
  messages as `prompt` entries, an assistant turn as a `response` (its text)
  and a `toolCalls` entry (each call's id, name and arguments as a JSON text),
  a tool result as a `toolOutput` naming its call and tool; images as
  base64 parts labelled `image-1`, `image-2`, ... across the conversation (a
  placeholder for a model without vision); `temperature`, `topP`,
  `maxTokens`; `greedy` for temperature 0 with no bounds; `toolChoice` none
  or required when there are tools; `reasoningLevel` (minimal and low light,
  medium moderate, high and above deep) for a model that reasons
- tool parameters lowered into the dialect `GenerationSchema` decodes
  (omp's `foundation-models.ts`): every object titled, ordered
  (`x-order`), closed and listing what it requires; string literals as an
  enum, a nullable type as its one type, local `$ref`s inlined; what the
  decoder cannot express (a free-form map, an open schema, a tuple, a
  mixed enum) asked for as a JSON-encoded string and parsed back out of the
  call's arguments
- the events: `text` and `reasoning` stream, `toolCall` fragments gather
  per call id, `usage` splits the cached part of the prompt, `done` ends the
  turn; a turn with calls is a tool round, one whose output reached
  `maxTokens` is a length finish
- a bridge `error` is said with its code: a guardrail or a refusal is final,
  a context overflow is the one the core evicts on, `rate_limited` waits, an
  unavailable model says why

The model is offered as omp offers it: `on-device`, named `Apple <variant>`,
its window the bridge's `contextSize` (else 128,000), its output ceiling the
smaller of that and 32,768, images and reasoning as the bridge reports them.
While the bridge says the model is unavailable (not eligible, Apple
Intelligence off, assets not ready, macOS older than 27), the row lists no
model and a notice names the reason.

## Install

On a Mac with Apple silicon and macOS 27, with Xcode 27 or its Command Line
Tools: copy this folder into `~/.nodecode/cells/nodecode-apple/` and restart
Nodecode, or run `(restart-cells)`. The first start builds the helper (a
minute or so) and asks the model whether it is available.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "apple": {
    // a helper already built (sh bridge/build.sh OUT); empty builds one
    "helper": ""
  }
}
```

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-apple
```

The tests run on Linux: the Mac is a stubbed `mac-p`, and the helper a
`/bin/sh` script that keeps the request it was sent and prints canned bridge
events, so the request bytes, the fold and the process plumbing are the real
ones. The Swift (`bridge.swift`, `main.swift`) is not compiled here; it is
compiled on the Mac at first use.

## Gaps

- **`main.swift` is untested.** Nothing in this repository can compile
  against the macOS 27 SDK; the shim is small (it calls the bridge's own C
  functions and prints what they emit), but its first compile is on the
  operator's Mac.

## Not ported

- omp's in-process load (the dylib behind the addon) and its handle-based
  cancel: here a generation is one helper process, cancelled by ending it.
- `topK` (the core's config carries no top-k), and a named tool choice (the
  core's tool choice is a mode, not a tool).
- the tool-result error flag omp prefixes with `Tool call failed:` (a
  Nodecode tool result carries no such flag).
- omp's message normalization (`transform-messages.ts`).

MIT licensed.
