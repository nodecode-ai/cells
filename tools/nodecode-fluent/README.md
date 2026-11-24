# nodecode-fluent

Fluent: every terminal command checked against the program's own `--help` before it runs. A Nodecode rewrite of [Bruno-coder21/fluent](https://github.com/Bruno-coder21/fluent) by [Bruno-coder21](https://github.com/Bruno-coder21).

The agent knows how to write shell commands, not how *this* `docker`, `gh` or `curl` spells its options. Fluent reads a program's own help once, keeps what it said, and checks the commands the agent runs against it: `docker run --rmm` comes back with *did you mean `--rm`?* instead of an error after the container was already on its way.

## Get it

Tell the agent "add the fluent cell", or run `nodecode add nc://fluent`.

## Set it up

No key, nothing to install: it reads help only for the programs you adopt. The `fluent` section, all of it optional, is at the end of **Use it**.

Adopt a program once, and from then on every command the agent runs through `(sh ...)`, `run-program`, `launch-program` or `run-shell-command` is checked against it:

```
/fluent adopt docker      read docker's --help and its subcommands' --help (a few seconds)
/fluent status            what is read, and the mode
/fluent show docker run   the options of `docker run`, as read
/fluent check "docker run --rmm alpine"   what the checker would say, without running anything
/fluent forget docker     forget it
```

The model has the same check as a tool: `fluent_check` with a `command`, and `adopt: true` when the program has not been read yet. That is the better way to ask before writing a command you are unsure of:

```lisp
(fluent:check-command "gh pr create --drafts")
;; => (--drafts is not an option of gh pr create ..., did you mean --draft?)
```

In **block** mode (the default) a flagged call does not run, and the model reads which option the program does not have and which one it probably meant. In **warn** mode the call runs and the note goes up on the board; **off** checks nothing.

## Use it

```jsonc
{
  "fluent": {
    "mode": "block",        // block | warn | off
    "max_subcommands": 24,  // how many subcommands one adoption reads
    "adopt_at_start": []    // programs read at every start
  }
}
```

## What it judges, and what it leaves alone

Only long options (`--name`), only on a command whose help was read whole, only before `--`, and never a word the shell expands (`$VAR`, globs). Short options, positional words, subcommand names and unknown subcommands are left alone, so a new docker alias is never refused. A long option that is a unique abbreviation or a known `--no-` negation passes.

Reading a help page runs the program itself: `prog --version` and `prog --help`, once per subcommand. Nothing else is run, nothing is sent anywhere, and the cards are plain data in this home's `fluent/` folder.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-fluent
```

The tests run no program and dial nothing: a stub answers a small help page, and the store is a folder of its own.

## Not ported

The man-page reader, the destructive-subcommand approval prompts (nodecode-guard and Nodecode's own approvals cover those), the plugin's dashboard and counters, and the post-run offer to adopt a program that rejected an option. See [NOTICE](NOTICE).

MIT licensed.
