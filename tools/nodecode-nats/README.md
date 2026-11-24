# nodecode-nats

NATS JetStream: a session's door to an event bus. A Nodecode rewrite of [atbrace/hermes-jetstream](https://github.com/atbrace/hermes-jetstream) by [atbrace](https://github.com/atbrace).

The agent publishes one thin event to a subject on your NATS server and pulls a batch back, through the same gate the original kept: nothing leaves unless the subject is on an allowlist.

## Get it

Tell the agent "add the nats cell", or run `nodecode add nc://nodecode-nats`.

## Set it up

It needs a NATS server with JetStream enabled and a stream bound to the subjects you care about — the cell uses one, it never creates one. In the `nats` section of `~/.nodecode/config.jsonc`:

```jsonc
{
  "nats": {
    "servers": ["nats://127.0.0.1:4222"],   // the first one is the one used
    "stream": "hermes",                       // the stream that binds your subjects
    "publish_allowed": ["demo.>"],            // empty denies every publish
    "subscribe_allowed": ["demo.>"],          // empty denies every pull
    "timeout_seconds": 10,
    "max_payload_bytes": 32768
  }
}
```

No credentials live in the section: point it at a server that accepts this machine, or put the token in the URL. Then `/nats ping` says whether the server answers.

## Use it

From a session, in eval or through the tools:

```lisp
(nats:publish "demo.build" '(:ok t) :kind "ci-result")   ; => "Published 132 bytes to demo.build [stream=hermes seq=7]"
(nats:fetch "demo.>" :batch 5)                            ; => the events, or "No events ..."
```

or the tools `nats_publish` and `nats_fetch`, which take the same arguments as a JSON object. `/nats` reports the section; `/nats ping` probes the server.

What the gates are:

- **A publish subject is literal.** No `*` and no `>`, and it must match `publish_allowed`. An empty allowlist denies.
- **The payload is scanned.** A string shaped like an API token, a private key or an AWS key is refused before it goes on the wire — publish a reference to the data, not the secret.
- **The envelope is thin:** schema, kind, time, a per-process nonce, and your data. The nonce is what tells two processes apart.
- **A pull acknowledges as it reads**, newest batch in the answer, and waits at most the timeout: one fetch, not a poll loop.

The wire is the NATS client protocol itself, over the socket support already in the image — no `nats` CLI and no client library to install.

## What it does not do

The original also woke sessions on matching subjects through durable consumers, mirrored Hermes lifecycle events onto `hermes.>`, and watched a subject from inside a live session. Here a session publishes and pulls; waking is a durable consumer you run in your own tooling against the same stream.
