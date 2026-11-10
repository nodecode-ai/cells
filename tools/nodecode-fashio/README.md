# nodecode-fashio

Fashio: on-model fashion photos, virtual try-on, flat lays, background removal and short product videos from a garment photo, on your own Fashio key and credits. A Nodecode rewrite of [fashiolabs/fashio-agent-plugin](https://github.com/fashiolabs/fashio-agent-plugin) by [fashiolabs](https://github.com/fashiolabs).

The original is configuration only: an Agent Plugins v1 package that points an
MCP client at Fashio's hosted server and bundles one skill. That server is a
wrapper over the public REST API at `api.fashiolabs.com`, so this cell speaks
that API directly with `nlk:http` -- the same account, the same key, the same
diamonds, none of the MCP handshake -- and offers the same jobs as tools the
model calls.

## Get it

Tell the agent "add the fashio cell", or run:

```sh
nodecode add nc://fashio
```

## Set it up

One key: a Fashio API key, `fio_live_...`, from
<https://fashiolabs.com> -> Profile -> API Keys (it is shown once).

```jsonc
{
  "fashio": {
    // the variable the key is read from; or write the key here as "api_key"
    "api_key_env": "FASHIO_API_KEY",
    // "base_url": "https://api.fashiolabs.com",   // the host, unless you proxy it
    "timeout_seconds": 60,
    "list_limit": 20
  }
}
```

A section that names a variable with nothing in it, or a base URL with no
scheme, refuses at start, and the refusal says which member and where a key
comes from.

## Use it

Seven tools while the cell runs:

- `fashio_account` -- the diamond balance and plan. Read it before spending.
- `fashio_catalog {kind}` -- `tools` (each `toolId` with what it needs and its
  fallback price), `fashion_models` (with `gender`), `venues`, `poses` (with
  `pose_type` for the jewelry sets) or `ready_sets`, Fashio's curated
  photoshoot library with its shot counts.
- `fashio_upload {path}` -- one local image (JPEG, PNG or WebP, up to 8 MB,
  read by signature) to a URL a generation's image fields take.
- `fashio_generate {tool_id, images, ...}` -- a garment on an AI model, a
  try-on, jewelry on a model, a flat lay, background removal, an upscale, a
  reframe, an instruction edit, a product video, or a saved look replayed with
  `template_set_id`. Spend.
- `fashio_generation {id}` -- a job's status, its result URLs, and for a
  photoshoot pack how many shots have landed.
- `fashio_templates {action}` -- list, save or delete the account's saved
  presets (a model, venue, pose and ratio kept from one generation).
- `fashio_catalog_job {ready_template_set_id, image}` -- one curated ready set
  run on a product, one image per shot. Spend, per shot.

Everything that spends diamonds, and the delete, needs `confirm: true`: the
first call says what it costs and the model has to tell the user before it can
call again. That is the original skill's "say the price and wait for a yes",
as a refusal the model cannot forget. `(help :fashio)` is the same text for
the model; `/fashio GENERATION-ID` polls a job from the operator's side, and
with no argument it says which host the cell asks and what it offers.

From eval the same verbs are ordinary functions, exported and answering a
string: `(fashio:account)`, `(fashio:catalog "tools")`, `(fashio:upload
"dress.png")`, `(fashio:generate "virtual_model" '(("garment_photo" . "https://...")) :confirm t)`, `(fashio:generation "AbCdEf123")`.

## What changed

- The endpoints, the request bodies and the answer shapes are the original's
  server's: `/v1/account`, `/v1/models`, `/v1/fashion-models`, `/v1/venues`,
  `/v1/poses`, `/v1/uploads`, `/v1/generations`, `/v1/template-sets`,
  `/v1/ready-template-sets`, `/v1/catalog-jobs`, and the same bearer key.
- The original's 14 MCP tools are these seven: the three that read Fashio's
  curated ready sets are `fashio_catalog kind:"ready_sets"` plus
  `fashio_catalog_job`, and the four template-set tools are
  `fashio_templates`.
- The list endpoints wrap their rows under a name of Fashio's own, so the rows
  are read from whichever array member the answer carries rather than from a
  guessed key: a catalogue that gains or renames a wrapper still lists.
- The original sent uploads as base64 bytes; here `fashio_upload` takes a local
  path and sends the file's own bytes with the type its signature spells.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-fashio
```

Every request is a stubbed `NLK:HTTP` and the key a stubbed
`NLE::CREDENTIAL-ENV`, so nothing reaches Fashio or the network.

MIT licensed.
