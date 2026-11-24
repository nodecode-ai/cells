# nodecode-mock-review

Mock Review: review boards for UI mockups, where you mark each decision and Send brings the marks back. A Nodecode rewrite of [apelabs-dev/hermes-mock-review](https://github.com/apelabs-dev/hermes-mock-review) by [apelabs-dev](https://github.com/apelabs-dev).

## Get it

Tell the agent "add the mock-review cell", or run `nodecode add nc://mock-review`.

## Set it up

Nothing is required. The board is a page on the gateway's own port, so it
opens in any browser on this machine: `http://127.0.0.1:<port>/mock-review/<id>/`,
the link each review is answered with.

```jsonc
"mock-review": {
  "directory": "mock-reviews/",          // where reviews live, under the home
  "require_note_on_change": false,      // block Send until every Change says why
  "note_chars": 2000                    // the longest note kept from a reviewer
}
```

## Use it

Ask for a review of the options on the table and the model answers with a
link. Five tools are the model's half:

| Tool | What it does |
| --- | --- |
| `mock_review_create` | Opens a review at round 1: a title, a brief, and the decisions of the round. |
| `mock_review_add_round` | Puts a new round on the same review; the earlier rounds are kept. |
| `mock_review_feedback` | A round's marks as one markdown digest (the newest round by default). |
| `mock_review_ack` | Says a round is handled, so it does not read as still waiting. |
| `mock_review_list` | The open reviews, with their round and state. |

A round is decisions. Each decision is a question, two to six options, and
the option the model recommends; an option may carry a mock, a whole small
HTML page, which the board frames at its own sandboxed origin — a live
prototype works, and it can read nothing of the board around it.

On the page, you mark each decision **Approve**, **Change** or **Skip**, may
pick the option you want and say why, and press **Send feedback**. Send
answers the board (\"Sent. The agent has it.\"), and the marks are stored with
the round: the model reads them with `mock_review_feedback`, which answers

```
# Mock review feedback — Pricing page, round 1
Review `r-3k9-1f2` · 2026-11-08T09:31:12Z

## d1 — Which pricing page?
**APPROVE** · go with B
> B, but tighter type

## Anything else
nothing else
```

`/mock-review` lists the reviews in a session, `/mock-review r-3k9-1f2` answers
one review's marks, and `(help :mock-review)` repeats all of it to the model.
`(mockreview:feedback \"r-3k9-1f2\")` and `(mockreview:list-reviews)` are the
same two by hand.

A submission is refused when it carries nothing, when it carries the wrong
review's submit token, and — with `require_note_on_change` — when a decision
is marked Change with no note. Every refusal is `ERROR: MOCK-REVIEW-ERROR`
with the reason.

## What it is not

The original's public hosting, wake webhook, tailnet links, pins on a live
page, design profiles, passwords and headless-browser checks are left out;
[NOTICE](NOTICE) says what each needed and why.

Run `.github/test-cell.sh tools/nodecode-mock-review` from the cells repository.
