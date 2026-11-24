# nodecode-reachmap

ReachMap: what this machine can reach right now. A Nodecode rewrite of
[MindSproutStudios/hermes-reachmap](https://github.com/MindSproutStudios/hermes-reachmap)
by [MindSproutStudios](https://github.com/MindSproutStudios).

## Get it

Tell the agent "add the reachmap cell", or run `nodecode add nc://reachmap`.

## Set it up

Nothing. The cell starts on the defaults: the usual DevTools ports
(9222-9226, 9229, 9230, 9333), the usual local-service ports (Ollama, LM
Studio, ComfyUI, n8n, Postgres, Redis, the Nodecode dashboard and more) and
the CLIs on PATH. The `reachmap` section in `/settings` adds what is
particular to this machine:

| Member | What it does |
| --- | --- |
| `cdp_ports` | DevTools ports to probe instead of the usual eight |
| `extra_ports` | more local ports to check |
| `scanners` | any of `browser`, `mcp`, `service`, `cli`, `drive`; empty runs all five |
| `auth_probes` | tools whose sign-in to verify by running them (gh, aws, gcloud, az, vercel, netlify, wrangler, railway, flyctl, hf, xurl, supabase); empty probes none |
| `hide_identities` | leave the addresses out of the map; by default a session says who it is signed in as |
| `cache_seconds` | how long one scan is reused (90 by default) |
| `timeout_seconds` | the ceiling on one probe |

A `scanners` value the cell does not have, an `auth_probes` tool it cannot
probe, or a port that is not a number refuses at start, naming it.

## Use it

Ask the model whether it can do something, or ask it to look before it says
it cannot; it calls `reachmap` with `action: find` and the thing's name.

```
/reachmap                    the whole map
/reachmap find gmail         what that name reaches here
/reachmap check <a reply>    what a reply denies that is live
/reachmap rescan             forget the cache and scan again
```

Through eval, the same: `(reachmap:report)`,
`(reachmap:find-capability "docker")`, `(reachmap:check-text "...")`.

The map rides each turn as a prompt section, so a model that has never run
the tool still knows there is a browser with Shopify open, an Ollama on
:11434 and a `gh` on PATH — and is told to check `find` before denying
anything.

## What it reads

Everything is on this machine and read-only: `http://127.0.0.1:<port>/json/version`
and `/json/list` on the DevTools ports, one TCP connect per service port,
one PATH lookup per tool (and one command run per `auth_probes` entry), the
`mcp` section of this home's config, and the mount roots. Nothing is sent
anywhere, and nothing it finds is started.

## Licence

MIT, as the original. See LICENSE and NOTICE.
