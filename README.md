# Nodecode add-ons

The index of add-ons for [Nodecode](https://nodecode.ai) that don't ship with
it. `/setup` → **Choose** reads [`index.json`](index.json) and lists these
after the add-ons Nodecode ships.

Nodecode's own add-ons of this kind live here, one folder each:

| Folder | What it does |
| --- | --- |
| [`nodecode-channel-slack`](nodecode-channel-slack) | Talk to Nodecode through a Slack app. |
| [`nodecode-claude-code`](nodecode-claude-code) | Claude models through your own logged-in Claude Code CLI. Linux and macOS. |
| [`nodecode-cline`](nodecode-cline) | Cline's API: the client header its free models need, and its model feed. |
| [`nodecode-codex-auth`](nodecode-codex-auth) | A ChatGPT subscription's login on OpenAI-family lanes. |

**What an entry is.** A folder in a public git repository, pinned at one
commit that a person here read before merging. The folder is the whole
repository, or one folder of it named by `path`, the way this repository's
own add-ons are listed. Installing one clones that
commit and nothing moves it afterwards. An add-on runs inside Nodecode with
full access to your files and keys, so every entry is pinned and every
change to one is a new pull request that gets read the same way.

## Add yours

1. Put your add-on in a public repository with its `.asd` at the top, or at
   the top of one folder of it. The
   folder contract is in Nodecode's `src/ADDONS.md`; start from
   `src/addons/template/`. Vendor anything beyond `nodecode` and the shipped
   add-ons under `vendor/`. An entry never depends on another entry here.
2. Open a pull request that adds one entry to `index.json`, keeping the list
   sorted by name:

```json
{
  "name": "nodecode-weather",
  "git": "https://github.com/someone/nodecode-weather",
  "commit": "3f9a1c7e2b8d4f60a5e19c0b7d2e4a6f8c1b3d5e",
  "description": "Current weather and a 3-day forecast, as a tool the model can call.",
  "license": "MIT",
  "depends_on": ["nodecode"],
  "platforms": ["linux", "macos", "windows"]
}
```

| Field | Rule |
| --- | --- |
| `name` | Your primary system's name, which is your `.asd` file's name. a-z, 0-9, `.`, `_`, `-`. It can't be a name Nodecode ships. |
| `git` | An `https://` URL. |
| `commit` | The full 40-character commit id. A branch or a short id is refused. |
| `path` | Optional. The folder inside the repository the add-on is, as `a/b`, when it isn't the whole repository. |
| `description` | Exactly your primary system's `:description`. |
| `license` | Your `.asd`'s `:license`. Required. |
| `depends_on` | Exactly the systems your `.asd` depends on from outside your folder. |
| `platforms` | Optional. Leave it out when the add-on runs on Linux, macOS and Windows. |

3. The `check` workflow installs every entry into a scratch home with the
   latest Nodecode release and loads it. Nodecode itself refuses an entry
   whose clone doesn't match its line: a different `HEAD`, a different
   primary system, a description or `depends_on` that isn't the `.asd`'s, or
   a symbolic link anywhere in the add-on's folder.
4. A maintainer reads the code at that commit and merges.

To update your add-on, open a pull request that changes `commit` (and
`description` or `depends_on` if they changed).

## Run the check yourself

```sh
tools/check.sh              # nodecode on PATH
tools/check.sh ./nodecode   # or a binary you name
```

It works in a scratch home and never touches your own `~/.nodecode`.
