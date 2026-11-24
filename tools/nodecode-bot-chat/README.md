# nodecode-bot-chat

Bot Chat Agent: a concise bot style plus a block-only safety guard for the sessions you turn it on in. A Nodecode rewrite of [Tormen28/hermes-bot-chat-agent](https://github.com/Tormen28/hermes-bot-chat-agent) by [Tormen28](https://github.com/Tormen28).

## Get it

Tell the agent "add the bot-chat cell", or run `nodecode add nc://bot-chat`.

## Use it

Nothing changes until you say where:

```
/bot-chat on
```

From then on, that session behaves like a focused assistant. Every request
carries a short contract: act instead of narrating, one reply instead of a
tool-by-tool log, at most one question per turn, never paste a secret, and
ask before anything that loses work or leaves the machine. A tool call that
breaks a rule does not run, and the model gets the reason as the call's
failure:

```
ERROR: refused by nodecode-bot-chat: this deletes files or data. Ask the
operator for explicit approval before retrying.
```

| Rule | What it refuses |
| --- | --- |
| `secrets` | a call naming `.env`, `auth.json`, `credentials`, an ssh key, a `.pem`, `.key`, `.p12`, a `.netrc`, or printing the whole environment |
| `deletes` | `rm`, `rmdir`, `shred`, `unlink`, `find -delete`, `git rm`, `DROP TABLE`, `DELETE FROM`, `shutil.rmtree` and the like |
| `destructive_git` | `git push --force`, `reset --hard`, `clean -f`, `branch -D`, `checkout --`, `rebase`, `filter-branch`, `stash drop` |
| `protected_push` | a push or merge into `main` or `master`, and `gh pr merge` |
| `external_sends` | a tool that sends, posts, replies, publishes, tweets or emails |
| `expensive_models` | a delegating tool naming a model the section calls expensive |
| `one question` | a second question card in the same turn: it is answered unanswered, not waited on |

The guard only ever blocks. It never approves a call, rewrites one, or asks
you on the model's behalf, and it reads the call rather than its text: a file
write whose content mentions `rm -rf` is not a delete, and a comment naming a
rule is not the rule. It is a speed bump rather than a boundary: `eval` has
your machine's full authority, and a command assembled in a way no pattern
sees can get past it.

| Command | What it does |
| --- | --- |
| `/bot-chat on` | Turns the mode on for the session it is typed in |
| `/bot-chat off` | Turns it off again, and everything runs as it did |
| `/bot-chat status` | The mode for this session, the preset and the rules it refuses |
| `/bot-chat rules` | What the preset refuses, and the protected branches |

## Set it up

Nothing is required. The section in `~/.nodecode/config.jsonc` refines it:

```jsonc
{
  "bot-chat": {
    "scope": "sessions",          // sessions: the ones /bot-chat on names; all: every session
    "style": true,                // ride the style contract on every request
    "language": "en",             // en or es, for the style and the refusals
    "preset": "default",          // strict, default (all seven rules) or relaxed
    "protected_branches": ["main", "master"],
    "external_send_tools": ["send_message", "react_to_message", "send_email"],
    "expensive_models": ["\\bpro\\b", "opus", "\\bultra\\b", "-max\\b"],
    "expensive_tools": ["delegate_task", "spawn_agent", "set_model"]
  }
}
```

`preset` `relaxed` keeps only `secrets`, `destructive_git` and
`protected_push`, and drops the one-question limit; the style is `style`'s
own switch. `scope` `all` applies the mode and the style to every session,
which is what the original did for every session it recognized as a chat with
a bot.
