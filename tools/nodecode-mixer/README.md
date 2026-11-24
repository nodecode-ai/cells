# nodecode-mixer

Mixer: four dials on how much the agent may change. A Nodecode rewrite of [Sahil-SS9/hermes-agent-mixer](https://github.com/Sahil-SS9/hermes-agent-mixer) by [Sahil-SS9](https://github.com/Sahil-SS9).

Nodecode has no permission modes for file edits, no write scope, no enforced
read-only mode, no per-turn change budget and no new-dependency guard. Mixer
adds those as one gate in front of the agent's one tool, `eval`.

## Get it

Tell the agent "add the mixer cell", or run `nodecode add nc://mixer`.

## Set it up

Nothing is required. Every dial has a default, and the built-in defaults
apply as soon as the cell is on. To change one, either say it (`/mixer edits
plan`) or put it in the `mixer` section of `~/.nodecode/config.jsonc`:

```jsonc
{
  "mixer": {
    "edits": "auto",              // ask | auto | plan
    "scope": ["src/**", "tests/**"], // empty means no limit
    "lines": 200,                 // 0 turns the budget off
    "deps": "ask"                 // allow | ask | block
  }
}
```

## Use it

`/mixer` draws the desk, and every value says where it came from:

```
mixer sess_3f2a9c1e
  edits  auto  (saved)
  scope  src/**, tests/**  (session)
  lines  200 per turn, 113 used this turn, 87 left  (saved)
  deps   ask  (default)
```

| Command | Effect |
| --- | --- |
| `/mixer` | The board. |
| `/mixer edits ask\|auto\|plan` | The edits dial, for this session. |
| `/mixer scope add <glob>` / `remove <glob>` / `clear` / `show` | The write-scope glob list. |
| `/mixer lines <N>\|off` | The per-turn line budget. |
| `/mixer deps allow\|ask\|block` | The dependencies dial. |
| `/mixer preset builder\|auditor\|explorer\|surgical\|show` | A named bundle of all four dials. |
| `/mixer reset` | Drop this session's overrides; the saved defaults stand. |
| `/mixer save [<dial> <value>]` | Make the current dials (or one value) the saved default, in the `mixer` section. |

The four dials:

| Dial | Values (default) | Effect |
| --- | --- | --- |
| `edits` | `ask`, `auto`, `plan` (`auto`) | `ask` puts a card up before a write: Allow once, Allow this file, Deny. `plan` is read-only: writes are refused, and the shell is limited to commands that write nothing. |
| `scope` | globs (empty = no limit) | A write outside the globs is refused, naming them. A relative glob anchors at the session's folder; a glob starting with `/` matches anywhere. |
| `lines` | `N`, `0` = off | Added plus removed lines one turn may change, counted against the file as it is now. A call that would go over is refused, naming what is left. An approval cannot override it. |
| `deps` | `allow`, `ask`, `block` (`ask`) | New dependencies: a name a manifest edit adds (`package.json`, `pyproject.toml`, `requirements*.txt`, `Cargo.toml`, `go.mod`), or a package an install command names (`npm`/`pnpm`/`yarn`/`bun add`, `pip install`, `uv add`, `poetry add`, `cargo add`, `go get`). Bumps, removals, reorders and restores pass. |

**Presets.** `builder` (auto, no scope, no budget, allow), `auditor` (plan,
no scope, no budget, ask), `explorer` (plan, no scope, no budget, allow),
`surgical` (auto, `src/**` and `tests/**`, 30 lines, ask). A preset pins all
four dials; it does not nudge what differs.

**What a call is.** Nodecode's agent works through one tool, and the call's
form carries the writes and the commands: `(write-file path text)`,
`(edit path old new)`, `(patch ...)`, `(delete-file path)`, a
`(with-open-file ... :direction :output)`, and `(sh "…")` for a command.
Mixer reads the form, not the text, so a comment or a string that mentions
`package.json` is not a dependency and a file that says `rm` is not a
command. Shell write targets (`>` and `>>`, `tee`, `sed -i`, `cp`/`mv`/`ln`
destinations, `touch`, `rm`, `mkdir`, `truncate`, `dd of=`) are read best
effort, and checked against the scope.

**Not a sandbox.** A command built from a variable, a script a command runs,
or Lisp that writes by hand can still write. Mixer catches what the call
shows.

## What was rewritten

The engine (the four dials, the stage order plan, scope, deps, lines, ask,
the glob matching, the manifest and install-command readers, the line
counting, the plan-mode command allowlist) is reimplemented in Common Lisp
against Nodecode's `:tool` hook and `ask-operator` card. See [NOTICE](NOTICE)
for what the rewrite leaves out.
