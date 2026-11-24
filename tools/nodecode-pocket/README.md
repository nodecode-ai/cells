# nodecode-pocket

Pocket: an update or a question on the operator's phone, and the decision or
the words that come back. A Nodecode rewrite of
[Belweave/hermes-notify](https://github.com/Belweave/hermes-notify) by
[Belweave](https://github.com/Belweave).

The phone side is [Notify by belweave](https://notify.belweave.ai): the update
lands in its inbox, a question waits there until the person answers or it
expires, and the answer comes back here by its interaction id.

## Get it

Tell the agent "add the pocket cell", or run `nodecode add nc://pocket`.

## Set it up

You need a Notify account and one agent key with `notifications:send`,
`interactions:create` and `interactions:read`. Put the key in an environment
variable and tell the cell that variable's name — the key itself never goes
into the config, a tool argument or a message body:

```sh
export NOTIFY_API_TOKEN=...   # the default name, so nothing else is needed
```

or, in `/settings` (the `pocket` section) or `~/.nodecode/config.jsonc`:

```jsonc
{ "pocket": { "api_token_env": "MY_NOTIFY_KEY", "project": "Nodecode" } }
```

`project` groups the updates on the phone under a name; `base_url` is Notify's
agent API and needs no change. Then `/pocket test` sends one test update, and
the answer says whether the service took it.

## Use it

The model has three tools:

| Tool | What it does |
| --- | --- |
| `notify_send` | An update for the phone: `body`, a `title`, a `summary`, a `url`, a `project`. Answers a receipt with the notification id. |
| `notify_ask` | A question on the phone: `title`, `prompt`, `kind` (`approval`, `yes_no`, `reply`), `expires_in_seconds`, `wait_seconds`. Answers the decision or the words, or that it is still pending with an interaction id. |
| `notify_poll` | Reads that interaction id again — one call, one exchange, no background polling. |

From eval, the same three, in the shape a person types:

```lisp
(pocket:notify "the build is green" :title "Release")
(pocket:ask "Deploy 2.4.1 now?" :title "Deploy" :kind "yes_no" :wait 20)
(pocket:answer "int_abc")            ; still pending? read it again
```

`/pocket` alone lists where it sends, whether the key is there, and the last
receipts; `/pocket test` sends one; `/pocket anything else` sends that as an
update titled "Nodecode".

An answer is what the person said, not permission to run anything. Only an
approved approval decides the single action its prompt named, and the caller
still runs and verifies that action itself.

## What leaves the machine

Every update and question goes to Notify's own service at
`https://notify.belweave.ai` — the origin in the section, and the only host
this cell talks to. The text you give it, the title, the summary, the url and
the project name ride that request; the key rides the `Authorization`
header. Send selected text only: no tokens (the cell refuses a body that
repeats the key), no file contents, nothing the operator did not ask to be
told. Nothing is downloaded, no shell command is run, and no cell file is
written.

## What the rewrite keeps, and what it leaves out

Kept: the same fixed agent API and bearer key, the same two writes and two
reads, the `Idempotency-Key` header for a retried send, the 25-second
one-wait ceiling, the pending/settled states of an interaction, and the rule
that a reply is task input and an approval is one decision.

Left out: the release workspace with measured stages (`notify_release`),
publishing it to an operator's Cloudflare Worker (`notify_publish_app`), the
sqlite journal and idempotency ledger (this cell keeps its last eight
receipts in memory), the native Hermes approval transport that intercepts a
command before it runs, progress Updates and Live Activities, the bundled
skill, and the web/desktop App.

## Test it

```sh
.github/test-cell.sh tools/nodecode-pocket
```

The tests are offline: the one function the cell exchanges HTTP through is
bound to a table of canned answers, and what the cell asked — method, URL,
JSON body, headers — is read back off it.
