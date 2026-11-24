# nodecode-artifacts

Artifacts: a private library for finished documents, with guest links that expire. A Nodecode rewrite of [dmmeteo/hermes-artifacts](https://github.com/dmmeteo/hermes-artifacts) by [dmmeteo](https://github.com/dmmeteo).

The model gets six tools: `artifact_publish` saves a document -- a
self-contained HTML page, Markdown, SVG, a PNG or a WebP -- from a file or
from inline text; `artifact_list` and `artifact_get` find it again;
`artifact_download` writes its original bytes out; `artifact_share` makes one
guest link for one artifact; `artifact_revoke_share` kills that link.
`/artifacts` says what is configured and what the library holds.

Publishing is private and creates no link, so the chat stays the chat and the
document lives where it can be linked when someone asks. A guest link is a
URL with a token in it, shown once, good for a week unless you say otherwise,
and revocable by id; other links to the same artifact keep working. Artifacts
are immutable: changing one means publishing a new one.

## Get it

Tell the agent "add the artifacts cell", or run `nodecode add nc://artifacts`.

## Set it up

The cell is a client. It needs an artifact service of your own -- the
original's own application, in the [same
repository](https://github.com/dmmeteo/hermes-artifacts), is one: it serves the
`/internal/v1` API this cell speaks. Then the `artifacts` section of
`~/.nodecode/config.jsonc` (or `/settings`) needs two things:

- `base_url` -- the service's origin, `https://`, or plain `http://` on this
  machine, e.g. `http://127.0.0.1:8080`.
- the machine credential -- the name of the environment variable holding it in
  `token_env` (default `HERMES_ARTIFACTS_TOKEN`), or a file in `token_file`.
  The token travels as a bearer header and nowhere else, never in a URL.

Optional: `source_profile` is the attribution label a publish carries (a label
and a filter, never an owner), and `timeout_seconds` bounds one request.

Until both are set the tools answer a refusal naming the member to set, and
`/artifacts status` says the same.

## Use it

Ask for what you want: "publish this report", "what's in the library",
"give me a link to the quarterly page for two days", "revoke that link". Or
type `/artifacts` to see the section in force and the newest artifacts, and
`/artifacts quarterly` to search titles.

The model reads `(help :artifacts)` for the rules: publish privately, share
only when asked, hand the URL back as it came, never build one by hand, and
treat a guest URL as the grant it is.
