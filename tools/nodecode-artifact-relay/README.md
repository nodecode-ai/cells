# nodecode-artifact-relay

Artifact Relay: long results as private links. A Nodecode rewrite of [eloktev/hermes-artifact-relay](https://github.com/eloktev/hermes-artifact-relay) by [eloktev](https://github.com/eloktev).

The model gets two tools, `artifact_publish` and `artifact_read`. The first
takes a title and a whole document -- Markdown, or one standalone HTML page --
and answers a viewer URL on your relay; the second takes that URL, or its bare
id, and hands the original source back. `/artifact-relay` says what is
configured, and signs a machine in.

It is for what does not belong in a transcript: a report, a comparison, a
rendered page, anything longer than a reply. The chat stays a few lines and a
link. Nothing else about the session goes with it -- no title, no platform, no
history -- unless `include_provenance` is on, and then only the session id.

## Get it

Tell the agent "add the artifact relay cell", or run `nodecode add nc://artifact-relay`.

## Set it up

The `artifact-relay` section of `~/.nodecode/config.jsonc` (or `/settings`)
needs a service and a token, in either of two ways:

- **Sign in.** `/artifact-relay setup` asks the hosted control plane
  (`https://relay.lok-labs.com`, the original's default) for a device code,
  prints the activation page and the code, and polls in the background until
  you approve it. The token it mints is kept at 0600 in
  `<home>/artifact-relay/token`, the service it belongs to beside it, and one
  proof artifact is published so you can see the link work. `/artifact-relay
  status` says whether it landed; `/artifact-relay cancel` stops waiting.
  `/artifact-relay setup https://my-relay.example` starts from another
  control plane.
- **Point it at a relay yourself.** Set `base_url` to the relay's origin and
  name the environment variable holding its token in `token_env` (default
  `ARTIFACT_RELAY_API_TOKEN`), or put the token in a file and name it in
  `token_file`. A relay of your own is the [service's own
  repository](https://github.com/eloktev/artifact-relay), self-hosted.

The service must be `https`, or plain `http` on loopback. A viewer URL the
relay answers with that is not on that same origin, under `/a/`, is refused,
so a link can never walk the token to another host. `expires_days` runs from 0
to 3650 and defaults to 30; one artifact may be at most 5 MiB.

## Use it

Ask for a report, or for "a link to it". The agent calls `artifact_publish`
with the whole document and answers you the URL; `artifact_read` fetches the
source of one back. `/artifact-relay` alone, or with `status`, prints the
service, where the token comes from, and whether a sign-in ever saved one.

Never publish a secret: a private link is still a link.

## What is in it

| File | What it is |
| --- | --- |
| `client.lisp` | The origin rules, the token, the multipart publish and the read. |
| `setup.lisp` | The device-code sign-in: the control plane, the poll, the token at 0600, the proof artifact, and `/artifact-relay status`. |
| `cell.lisp` | The two tools' schemas and answers, the section, `/artifact-relay`, and the manual topic. |
| `config.example.jsonc` | The section, commented: documentation, not config. |

## License

MIT, Copyright (c) 2026 Egor Loktev: the original's `LICENSE`, unchanged, with
what was rewritten in `NOTICE`.
