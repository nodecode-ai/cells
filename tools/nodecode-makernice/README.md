# nodecode-makernice

Makernice: priced video and image generation in chat. A Nodecode rewrite of [makernice/makernice-plugin](https://github.com/makernice/makernice-plugin) by [makernice](https://github.com/makernice).

The original is configuration only: it points an agent at Makernice's remote MCP
server and adds skills that teach it to price a job, ask the user, run it and
fetch the result. Every tool lives on that server, so this cell is the client:
it speaks MCP over streamable HTTP to the same url and registers the server's
eight tools, with the same names, descriptions and parameters.

Seedance, Kling, Wan and MiniMax videos, Seedream and Qwen images -- each job
is priced in USD before it runs, and the job is refused if the price is above
the amount the user approved.

## Get it

Tell the agent "add the makernice cell", or run `nodecode add nc://makernice`.

## Set it up

`https://mcp.makernice.com/mcp` answers only with an OAuth bearer; without one
it answers 401 and names its resource metadata. The sign-in is a browser flow,
and Nodecode's own MCP cell already runs it: add the server there once, sign
in, and copy the access token it keeps under the server's entry in
`~/.nodecode/mcp/oauth.json`. Then put it in the `makernice` section (or
`/settings`):

```jsonc
{
  "makernice": {
    "token": "…",                              // or leave it out and export MAKERNICE_TOKEN
    "token_file": "~/.nodecode/secrets/makernice-token",  // or name a file that holds it
    "url": "https://mcp.makernice.com/mcp",
    "request_timeout": 30                       // seconds; check_job's wait_s sits inside it
  }
}
```

The token is read again at every call, so a rotated one takes effect at the
next tool call; a `token_file` is re-read the same way. Without any token the
cell refuses at start and names the variable it looked for. `/makernice` shows
the endpoint, where the token came from, the session and the last call.

## Use it

Eight tools, named as the server names them:

| Tool | What it does |
| --- | --- |
| `makernice_list_models` | The models with their modes, resolutions, durations and unit prices; `model` for one id's full parameters. |
| `makernice_estimate_cost` | Prices one job in USD, creating and charging nothing. `input` is the parameters: `{model, mode, prompt, duration, resolution, aspect_ratio, image_url, reference_images, …}` or `{template, images}`. |
| `makernice_generate` | Starts the job: the same `input`, `max_cost_usd` (the `amount_usd` the user approved) and a `request_id` of your own. Answers a `job_id`. |
| `makernice_check_job` | The status, the charged amount and the result links; `wait_s` waits up to 55 seconds for it. |
| `makernice_cancel_job` | Cancels a job still queued and releases the held amount in full. |
| `makernice_list_trends` | The trending templates with their price per resolution, for `{template, images}`. |
| `makernice_get_balance` | Available balance, what running jobs hold, today's spending and the limits. |
| `makernice_get_topup_link` | The wallet page where the user adds balance. |

The price-consent loop is the point, and two of its steps are enforced here
rather than asked for: `makernice_generate` refuses without a `max_cost_usd`
and without a printable `request_id` of at most 64 characters, so a retry
carries the same id and the same job rather than a second charge. Everything
else -- the prices, the jobs, the models -- is the server's answer, passed
through unchanged.

From `eval` the same work is one call:

```lisp
(makernice:call "estimate_cost" (list :input (list :model "…" :mode "t2v" :prompt "a paper boat")))
(makernice:call "generate" (list :input (list :model "…" :mode "t2v" :prompt "a paper boat")
                                 :max_cost_usd "0.35" :request_id "setup-test-boat-1"))
(makernice:account)                     ; the balance and the limits
```

`(help :makernice)` is the primer the model reads: price first, show the
amount, generate only after the user agrees, wait with `wait_s` 55, and tell
the user that result links expire after 24 hours. `/makernice balance` asks
the server for the balance.

## Test it

```bash
NODECODE_SRC=~/nodecode/nodecode .github/test-cell.sh tools/nodecode-makernice
```

Every request is a stubbed `nlk:http`, so the suite reaches nothing: the
handshake it sends, the session header it carries, the arguments each tool
passes through, the consent gate, an event-stream answer, a refused token, a
forgotten session and `/makernice`.

## Gaps

The cell does not run the OAuth sign-in itself: the bearer comes from the
section, a file or the environment, and the sign-in is one step done once in a
client that has one (Nodecode's MCP cell signs in and keeps the token). The
four skills are one primer rather than four documents, since a cell's manual is
one text; and the model may still charge a job if the user's approval is not
asked for, so the primer says plainly: estimate, show the price, then generate.
