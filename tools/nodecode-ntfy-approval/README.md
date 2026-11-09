# nodecode-ntfy-approval

Ntfy Approval: answer a risky shell command from your phone through ntfy push buttons. A Nodecode rewrite of [AhmetArif0/hermes-ntfy-approval](https://github.com/AhmetArif0/hermes-ntfy-approval) by [AhmetArif0](https://github.com/AhmetArif0).

## Get it

Tell the agent "add the ntfy-approval cell", or run `nodecode add nc://ntfy-approval`.

## Set it up

1. Run `/ntfy setup`. It prints a fresh topic name and a link to the ntfy app.
2. Install the app on the phone and subscribe to that topic on the same server.
3. Put the topic in this home, under `ntfy-approval` in `/settings`:

```jsonc
"ntfy-approval": { "topic": "nodecode-…", "timeout_seconds": 120 }
```

4. Run `/ntfy test`: one sample approval goes to the phone, and the answer comes back to the
   terminal. Nothing runs.

An own ntfy server goes in `server`, and the variable holding its access token in `token_env`
(the token itself stays in the environment, never in the config).

## Use it

Once it is on, a shell command it considers dangerous is not refused and not run straight away:
it waits for a tap. The phone shows the rule that caught it, why, the command's text (turn
`send_command` off to withhold it) and three buttons:

- **Approve once** runs that one call; the next match asks again.
- **Approve for session** runs it, and every later call caught by the same rule in the same
  session.
- **Deny** refuses it, and the agent is told you chose Deny, so it asks you rather than working
  around it.
- **No answer** within `timeout_seconds` (default 120) refuses it, saying so. So does a server
  that cannot be reached, and a section with no topic: silence is a no.

The rules it reads are the ones a person usually wants a say in: deleting a tree, `sudo`, a force
push, a hard reset, `git clean -f`, `mkfs`, a write onto a device node, a shutdown, `chmod 777`,
and a download piped into a shell. `patterns` adds regular expressions of your own.

Only the commands a call runs are read: the cells' one tool is `eval`, and a payload, a comment
or a test string in the same form is not a command. A call that runs nothing touches no network.

## What it sends

The rule, its reason, the command's text, and one random code per button, to the configured ntfy
server and nowhere else — `ntfy.sh` unless an own server is set. The answer is posted by the phone
to `<topic>-reply` on the same server, and nothing listens on this machine.

On a public server the topic name is the secret: anyone who knows it can read the request and
answer it. So a topic shorter than 16 characters is refused when no access token is named. For
approvals that stay private, run an own server with access control; the reply topic must accept
anonymous writes, so the button needs no credentials.

## Licence

MIT. The original plugin is Copyright (c) 2026 AhmetArif0: see `LICENSE` and `NOTICE`.
