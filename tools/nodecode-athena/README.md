# nodecode-athena

Athena: the local AI app store. A Nodecode rewrite of [athenaforhermes/athena](https://github.com/athenaforhermes/athena) by [athenaforhermes](https://github.com/athenaforhermes).

It gives the model (and you) the whole loop of a small app store: search a
catalogue of AI apps, install one by cloning its repository, run it in the
background, read its log, and stop it. The reading of the log is the part worth
having: an app that dies says why — the card is out of memory, the port is
taken, its install step never ran — and the tool answers that cause with the
step that fixes it.

## Get it

Tell the agent "add the athena cell", or run:

```sh
nodecode add nc://athena
```

## Set it up

One setting is required: where apps live.

```jsonc
{
  "athena": {
    "apps_dir": "~/nodecode-apps",              // required: apps install and run here
    "catalog_url": "https://api.pinokio.co/apps", // the store's JSON API
    "git": "git",                                 // the git athena_install clones with
    "timeout": 30                                 // seconds per catalogue request
  }
}
```

Set it in `/settings` or on the page's Settings tab; a section naming no
`apps_dir` is refused at start, and the refusal says so. Nothing else needs an
account or a key: the catalogue is public, and an app that wants a credential
is labelled so by its own log.

Apps are run with what the machine already has: `git` to clone, `sh` to start,
and `node` or `python3` when the app's own start script needs them.

## Use it

Seven tools, which you can also call from eval in the `athena` package:

| Tool | What it does |
| --- | --- |
| `athena_search` | Search the catalogue: name, repo, version, description, tags, and the `install_with` handle to install. |
| `athena_apps` | What is installed here, whether each runs, its pid, and the URL its web UI answers on. |
| `athena_install` | Clone an app into `apps_dir` from a git URL, an `owner/repo`, or a catalogue name. |
| `athena_launch` | Run the app's own start script in its folder, in the background, output to its log. |
| `athena_stop` | End the process this cell started for an app. |
| `athena_logs` | The tail of the app's log, and the named cause with its fix; `why` answers only the reading. |
| `athena_status` | Where the store reads, and how many apps are installed and running. |

And `/athena` with no argument lists the installed apps, with an argument
searches the catalogue — one line each, for a person reading along.

From eval, the same verbs:

```lisp
(athena:list-apps nil)          ; what is installed, and what runs
```

Or in the conversation:

```text
> find an image upscaler, install it and start it
```

Each app's log and pid live beside the apps, in `apps_dir/.athena/`, so a
failure can be read after the app stopped, and a pid from an earlier Nodecode
process is still known.

**The app's own code runs as you**, with your files and your privileges:
nothing here sandboxes it. An install only clones — when an app needs its own
install step (`npm install`, `pip install -r requirements.txt`), its log says so
when the start fails, and the reading names it.

## What the rewrite leaves as it was

The catalogue API, the shape of each search row, the install-by-clone, the
launch-stop-log loop, the reading of a failing log, and the ids of the tools.
What changed is what drives it: the Hermes plugin ran its own Node engine and
its own HTTP UI; here the verbs are native, and no Node process of the cell's
own exists. `athena_publish`, `athena_files`, `athena_machines` and the
desktop panels are not part of this cell: see NOTICE.
