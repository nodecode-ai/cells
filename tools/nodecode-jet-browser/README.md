# nodecode-jet-browser

Jet Browser: its own acceptance check, run in a network-isolated container. A Nodecode rewrite of [masakaai/jet-browser](https://github.com/masakaai/jet-browser) by [masakaai](https://github.com/masakaai).

Jet Browser is a browser built on WPE WebKit whose project publishes its runtime as one digest-pinned container. The check this cell runs is the project's own: the container starts, the built-in fixture page loads, the cell types into it through native pointer and text events, reads the two fields back out of the DOM, captures a PNG, and the container is removed. The answer is evidence — the title, the URL, the text that was typed, the PNG's octet count and sha256, the milliseconds it took — not a claim that a browser works.

## Get it

Tell the agent "add the jet-browser cell", or run `nodecode add nc://nodecode-jet-browser`.

Then run `/jetbrowser run`, or ask for the check: the model calls the tool `jet_browser_verify`.

## Set it up

The cell runs one container, so it asks which image, once. In `/settings` (or the page's Settings tab), under `jet-browser`:

- `image` — required. The image to run, pinned by digest. The project publishes one; paste it from the cell's own guide or from `config.example.jsonc`:

  `ghcr.io/masakaai/jet-browser@sha256:540a1fec531bf71b62c2c2d030c033249955ffc192ab7e0e7a1ac8d8412a661b`

- `docker` — the CLI to run; `docker` by default, as `PATH` finds it.
- `screenshot_file` — where to keep the PNG the check captures; empty keeps only its sha256.

Until `image` is set the folder is `REFUSED`, with the reason on the `/cells` line and in the model's notices: nothing of its tools is registered while it refuses.

**Docker is required**, and Docker access is root-equivalent on the host. That is the price of the check, and it is why the image is named by the operator rather than defaulted.

## Use it

- `/jetbrowser` typed alone shows what it would run and a row that runs it; `/jetbrowser run` runs the check and answers one line of evidence.
- `jet_browser_verify` is the same as a tool. It takes no arguments: what it runs is the section's. Its answer is JSON — `{"success":true,"network":"disabled","image":…,"title":…,"url":…,"input":…,"screenshot_bytes":…,"screenshot_sha256":…,"duration_ms":…,"screenshot_file":…}` — or, for a check that failed, `{"success":null,"error":"…"}`.
- `(help :jet-browser)` is the manual a model reads.

## What runs, and what does not touch you

One `docker run`, a fixed argument vector, no shell and nothing mounted:

```
docker run --rm -i --name nodecode-jet-browser-<8 hex>
  --platform=linux/amd64 --network=none
  --cap-drop=ALL --cap-add=SETUID --cap-add=SETGID
  --security-opt=no-new-privileges --security-opt=systempaths=unconfined
  --security-opt=apparmor=unconfined
  --security-opt=seccomp=<the cell's seccomp_profile.json>
  --memory=1g --cpus=2 --pids-limit=256 --shm-size=256m
  --env=JET_BROWSER_SMOKE=1 <image>
```

- The container has **no network**; only the page the image serves itself (`http://127.0.0.1:8080/`) is loaded. Docker itself reaches the registry the first time the image is not local.
- AppArmor and `systempaths` are unconfined and the checked-in seccomp profile allows the user-namespace and mount syscalls WPE WebKit's own bubblewrap sandbox needs. The profile ships beside the cell (`seccomp_profile.json`, from the original repository); `seccomp_profile` names another, and an empty name falls back to Docker's own.
- Nothing of yours is mounted: no home, no profile, no files, no conversation. The tool takes no arguments, so nothing of the turn's enters the container either.
- On timeout, or when the client exits badly, only the uniquely named container this run created is force-removed. The docker client's own words are kept in a log file under the system temp directory, deleted when a run succeeds and named in the error when it does not.

## Layout

- `cell.lisp` — the section, the tool, the command, the manual and the transport.
- `seccomp_profile.json` — the container's syscall profile, carried over from the original.
- `test/` — the slice: the lifecycle, the protocol through a scripted container, the command, a failed check, the screenshot file.

`.github/test-cell.sh tools/nodecode-jet-browser` runs it offline: no test needs Docker.
