# nodecode-opencode-shepherd

Opencode Shepherd: watch the opencode sessions on your machine -- what finished and waits for you, what asks a question, what wants permission -- and answer them. A Nodecode rewrite of [okazakee/opencode-shepherd](https://github.com/okazakee/opencode-shepherd) by [okazakee](https://github.com/okazakee).

[opencode](https://opencode.ai) runs its agent as a server. A turn ends and
wants input, the agent puts a question form up, or a permission request sits
between it and the next command -- and none of it reaches you unless something
is watching. This cell watches: a worker polls the server you already run, and
while anything waits, one notice stands where the model reads it and where you
see it as a toast. Seven tools let the model look and answer:

| Tool | What it does |
| --- | --- |
| `opencode_sessions` | the inventory: id, title, folder, running, how long since it moved |
| `opencode_waiting` | what needs you now, and the exact call that answers each one |
| `opencode_tail` | one session's last messages, one line each |
| `opencode_send` | say something to a session: steer the running turn, or queue |
| `opencode_permission` | answer a permission request: once, always or reject |
| `opencode_form` | answer a question form, one value per field |
| `opencode_interrupt` | stop a turn that ran away |

It is deliberately quiet: a session waiting on a background shell is not
reported, because opencode resumes it by itself when the shell exits; a
sub-session's finished turn is not either, because its parent reports it; and
an item already reported is never reported twice. Silence means nothing waits.

## Get it

Tell the agent "add the opencode-shepherd cell", or run `nodecode add nc://opencode-shepherd`.

## Set it up

The `opencode-shepherd` section of the config asks for one thing: `url`, the address
your opencode server prints when you run `opencode serve` or `opencode web`
(usually `http://127.0.0.1:4096`). If the server asks for a password, name the
environment variable that holds it in `password_env` (default
`OPENCODE_SERVER_PASSWORD`) and start Nodecode with it exported; `password`
takes the value inline instead. `poll_seconds` is how often the watch asks
(default 60; `0` turns the watch off and leaves the tools on demand).

## Use it

Ask the agent:

- "what is my opencode doing?" -- it calls `opencode_waiting` and reads you
  the questions, the permissions and the turns that wait, with the session's
  name and id.
- "answer the question in the CORS session: use https://a.test" -- it calls
  `opencode_form` with that field, and the agent on the other side goes on.
- "let it run that command, just this once" -- `opencode_permission` with
  `once` (or `always`, or `reject`).
- "tell that session to run the tests too" -- `opencode_send`, steering the
  turn that runs, or queueing it with `queue: true`.

Or type `/opencode` yourself: it answers what waits, and `/opencode <name>`
reads one session's tail. A session is named by its id or by any fragment of
its id or title; a fragment that matches several says which.

The cell never starts, stops or deletes a session and never changes opencode's
configuration: it reads, it sends what you decided, and it stops a turn when
you ask it to.

## Credit

Rewritten from [okazakee/opencode-shepherd](https://github.com/okazakee/opencode-shepherd)
(commit `207a1443`), MIT. See [NOTICE](NOTICE) for what was rewritten and what
was left out, and [LICENSE](LICENSE) for the original licence.
