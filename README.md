# nodecode-cline

A Nodecode add-on for Cline's API (`api.cline.bot`). It does two things
Nodecode can't do on its own:

- It sends the `X-CLIENT-TYPE` header on the Cline lane's requests. Cline
  serves its free models only to a request that names its client. This
  add-on names Nodecode, never Cline.
- It adds the models Cline serves by plan (the Pass and the free ones) to
  the lane's model list. Cline's `/models` listing leaves them out, so the
  add-on reads them from Cline's public feed.

Your Cline key lives in Nodecode's shared `auth.json`, where `/connect`
writes it. This add-on stores nothing.

## Install

In Nodecode, run `/setup` → **Choose** and pick **cline**. Or clone this
repository into `~/.nodecode/addons/nodecode-cline/`.

## Configure

Optional. Having the section enables the add-on, and `"enabled": false`
turns it off:

```jsonc
{
  "cline": {
    "provider": "cline-pass",       // the lane a Cline key serves
    "client_type": "nodecode",      // what X-CLIENT-TYPE names
    "feed": "https://api.cline.bot/api/v1/ai/cline/recommended-models",
    "buckets": ["clinePass", "free"]
  }
}
```

See [`config.example.jsonc`](config.example.jsonc).

## Test

From a Nodecode checkout's `src/`:

```sh
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(push #p"/path/to/nodecode-cline/" asdf:*central-registry*)' \
  --eval '(asdf:test-system :nodecode-cline)'
```

MIT licensed.
