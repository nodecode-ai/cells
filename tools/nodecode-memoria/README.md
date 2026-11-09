# nodecode-memoria

Memoria Cloud memory: cross-session recall, plus explicit save, correct, forget and profile tools over the Memoria service. A Nodecode rewrite of [matrixorigin/memoria](https://github.com/matrixorigin/memoria) by [matrixorigin](https://github.com/matrixorigin).

Memoria is a memory service: facts about a person, kept across sessions and
recalled by meaning. This cell is its client. The model gets six tools —
`memoria_search`, `memoria_store`, `memoria_update`, `memoria_forget`,
`memoria_profile`, `memoria_feedback` — the operator gets `/memoria`, and
(auto recall on, the default) each turn carries what the person's own words
recall, in an untrusted-data block the model is told to verify.

## Get it

Tell the agent "add the memoria cell", or run `nodecode add nc://memoria`.

## Set it up

You need a Memoria account and a **memory-service API key**: sign in at
[thememoria.ai](https://thememoria.ai), create a key, and paste it into the
cell's section — in `/settings`, or by hand:

```jsonc
{
  "memoria": {
    "api_key": "sk-...",            // the memory-service key, not the website's session token
    "branch": "main",               // the memory branch this profile reads and writes
    "top_k": 5,                     // how many memories one recall may carry
    "auto_recall": true            // false: recall only when the model asks
  }
}
```

`api_url` points at a self-hosted Memoria instead of the free Cloud service
(`https://api.thememoria.ai`); it is an origin with no path, HTTPS unless it
is a loopback address. `context_chars` (6000) bounds what a turn carries and
`timeout_seconds` (15) bounds one exchange.

Without a key the tools answer one refusal naming `memoria.api_key`, and
nothing leaves the machine. The subject every request carries is a hash of
this home, so no path is sent and a second profile is a second subject on the
same account.

## Use it

Ask the model to remember something, or to recall it:

> Remember that I take my coffee black, with no sugar.
> What do I take in my coffee?

`(help :memoria)` teaches the model the six tools while the cell runs.
`/memoria` shows the state, `/memoria search WORDS` asks the service, and
`/memoria profile` reads the standing profile. A correction and a deletion
need an exact id, so the model searches first; every id a search answers is
a whole memory.

What the rewrite left out — the local SQLite outbox and its retry ladder, the
deduplicated turn capture and its background worker — is in [NOTICE](NOTICE).

## Tests

```sh
.github/test-cell.sh tools/nodecode-memoria
```

They run offline: the cell's one network seam, `*REQUEST*`, answers canned
objects, so the tests prove the binding each request carries as well as the
answer the model read.
