# nodecode-sabi

Sabi: the local routing proxy as a provider, with its attribution headers. A Nodecode rewrite of [vizuh/sabi](https://github.com/vizuh/sabi) by [vizuh](https://github.com/vizuh).

[Sabi](https://github.com/vizuh/sabi) is a proxy that runs on your own machine,
speaks the OpenAI Chat Completions wire, and decides per request which model,
effort and provider a turn goes to. A client sends one alias, `sabi-code`, and
gets Sabi's routing; the client keeps its own loop, tools and permissions.

With this cell, `/models` lists `Sabi` with the one alias, and a turn on
`sabi/sabi-code` goes to the address in its section — `http://127.0.0.1:8787/v1`
by default — carrying the attribution Sabi groups a trajectory by:

- `X-Sabi-Client: nodecode` — who is asking, so Sabi can tell its clients apart
- `X-Sabi-Session: <session id>` — the Nodecode session, when its id is a short
  opaque string; anything else is left off rather than rewritten

Nothing else about the request changes: the same history, tools and arguments
go out, and no stale copy of those headers survives. Only a round whose
provider is `sabi` is touched, and no key is asked for — the upstream
credential belongs to the proxy.

## Get it

Tell the agent "add the sabi cell", or run `nodecode add nc://sabi`.

## Set it up

Run the Sabi proxy on the same machine (`hermes proxy start` in the original
recipe, or Sabi's own setup) and leave its base as it is. There is no key in
this cell. If the proxy listens somewhere else, say so in the section:

```jsonc
{
  "sabi": {
    "base_url": "http://127.0.0.1:9911/v1",
    "context_tokens": 65536
  }
}
```

`context_tokens` is what the picker reports for the alias and what a turn is
sized against; Sabi spends it per upstream model, so keep it at or below what
the models behind it actually offer.

## Use it

Pick `Sabi` in `/models` — or say "use sabi" — and work as usual. Sabi decides
what each request runs on; this cell only says who is asking. `/settings` has
the section, and turning the folder off in `/cells` puts every provider back
exactly as it was.

## Test

From the cells repository's root:

```sh
.github/test-cell.sh providers/nodecode-sabi
```

MIT licensed. Rewritten from `vizuh/sabi` at commit
`b59bd21b89fca091bf9ebcfb3082a4191e511da7`; see [NOTICE](NOTICE).
