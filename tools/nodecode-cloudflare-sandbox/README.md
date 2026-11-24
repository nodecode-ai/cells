# nodecode-cloudflare-sandbox

Cloudflare Sandbox: run commands in a Cloudflare Container, through a Worker
you deploy to your own account, and keep the workspace's files as a snapshot
when it goes idle. A Nodecode rewrite of
[kiyo-e/hermes-cloudflare-sandbox](https://github.com/kiyo-e/hermes-cloudflare-sandbox)
by [kiyo-e](https://github.com/kiyo-e).

In Hermes the plugin swaps the terminal the agent's tools run in, so every
`sh`, file and `execute_code` call lands in a Cloudflare Container. Here it is
one tool, `cf_sandbox`, and one set of verbs: your own shell is untouched,
and what you send to the container is the work you meant to isolate.

## Get it

Tell the agent "add the cloudflare sandbox cell", or run `nodecode add nc://cloudflare-sandbox`.

## Set it up

The cell is the client half. The other half is the Worker, which is in the
original repository: deploy it to your own Cloudflare account (its
`wrangler.jsonc` names the images and instance types, and `SANDBOX_API_TOKEN`
is the secret the Worker checks), then tell the cell its origin and token.

- `endpoint` — the Worker's origin, `https://something.workers.dev`
  (`http://` only for a Worker on loopback).
- `token_env` — the name of an environment variable holding the same value
  you set as the Worker's `SANDBOX_API_TOKEN`; or `token`, pasted into
  `/settings`, which keeps it in a 0600 file.
- `namespace`, `workspace` — what the workspace name is hashed from. The
  identity is hashed before it leaves: a path of yours never travels to the
  Worker.
- `image`, `instance` — which image `wrangler.jsonc` deploys, and the
  container size (`lite`, `standard-1` … `standard-4`); unset, the Worker's
  own.
- `persistent` — default on: the workspace is snapshotted when idle, so files
  survive between commands. Off, every start of the cell is a fresh one.
- `cwd`, `timeout_seconds` — where commands start, and how long one may run.

Running containers are billed by Cloudflare, to your account. The container
has outbound internet; your environment variables, skills and credentials are
not copied into it.

## Use it

The model calls `cf_sandbox(command, cwd, stdin, timeout)` and reads the
command's output and exit status together. From eval:

```lisp
(cf-sandbox:run "uname -a; python3 -V")   ; (:exit-code 0 :output "...")
(cf-sandbox:read-file "/workspace/notes.txt")
(cf-sandbox:write-file "/workspace/notes.txt" "text goes through stdin, nothing quoted")
(cf-sandbox:health)
(cf-sandbox:release)   ; snapshot, stop the container; the files stay
(cf-sandbox:reset)     ; drop the workspace; the next command starts a clean one
```

`/cloudflare-sandbox` answers the Worker's health, the workspace name and the
image; `release` and `reset` are its other two words. `(help
:cloudflare-sandbox)` teaches the model the same, while the cell runs.

A command's answer can be lost — a dropped stream, a timeout, an HTTP error.
The Worker does not replay requests, so every refusal says the command may
already have run: read the workspace before repeating it.

## What it needs

The Worker from the original repository, deployed to your Cloudflare account.
Nothing else: the cell runs on what Nodecode already carries.

## License

MIT, as the original. See [LICENSE](LICENSE) and [NOTICE](NOTICE).
