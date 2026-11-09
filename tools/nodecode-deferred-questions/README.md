# nodecode-deferred-questions

Deferred Questions: the agent asks, and moves on — the answer comes back at its next turn. A Nodecode rewrite of [web3blind/hermes-deferred-questions](https://github.com/web3blind/hermes-deferred-questions) by [web3blind](https://github.com/web3blind).

Some questions do not need the turn that asks them. Which date works,
which branch to merge, whether to delete the old export: the agent can say
what it needs and keep working, and the answer can arrive a minute or an
hour later, in the middle of something else. This cell is that gap.

A **deferred question** is a question with a life of its own: the agent
enqueues it and is free at once, it stands in a ledger until it is answered
or expires, and the answer is handed to the session that asked at its next
model request — once, in a block headed `[deferred answers: N]`. Nothing
blocks, nothing polls, and the answer is never lost to a restart.

## Get it

Tell the agent "add the deferred-questions cell", or run:

```sh
nodecode add nc://nodecode-deferred-questions
```

## Set it up

Nothing is required: the section runs on its defaults. What you may want to
change, in `~/.nodecode/config.jsonc` or on the Settings page:

```jsonc
"deferred-questions": {
  "expires_hours": 72,     // how long a question stands when the caller names no expiry
  "max_options": 24,       // the most fixed answers one question may offer
  "retention_days": 30,    // how long finished questions stay in the ledger
  "deliver_in_round": true // hand answers to the model at its next request
}
```

## Use it

From a session, through eval:

```lisp
(dq:request :question "Which date works?" :options '("Friday" "Monday"))
(dq:request :question "Which branch?" :allow-text t :expires "+7d")
(dq:board)                  ; what waits, then the rest
(dq:show 1)                 ; one question, its options, its answer
(dq:answer 1 "Friday")       ; record an answer as if the operator gave it
(dq:cancel 1)               ; drop one that still waits
```

The model has the same four verbs as one tool, `deferred_questions`:
`request`, `get`, `list`, `cancel`.

**Where the answer comes from.** `/dq` in a shell lists what waits and
offers a panel; `/dq 1 Friday` answers question 1, `/dq answer 1 TEXT` the
same in words, `/dq show 1` reads one, `/dq cancel 1` drops one, `/dq list`
lists the ledger. An answer is the number of an option — stored as that
option's own words — or free text when the question allows it. While
anything waits, a standing notice says how many.

**The binding the original is careful about, kept here.** One answer per
question and the first one wins; an answer names its question's id; it is
handed only to the session that asked; a cancelled or expired question
takes none; an idempotency key makes one question of two writes (the same
`idempotency_key`, or the same words on the same day, is one question).

## Where it lives

One JSON file, `deferred-questions/questions.json` under the home, mode
0600, its folder 0700: `id`, `key`, `question`, `options`, `allow_text`,
`session`, `state`, `answer`, `answer_from`, `answered_at`, `delivered`,
`expires_at`, `created`. The ledger is that file, so it survives a restart
and can be read by anything. Questions and answers are stored unencrypted,
as the original's are.

## Credit

Rewritten from web3blind's Hermes Agent plugin `hermes-deferred-questions`,
MIT. See [NOTICE](NOTICE): what was kept, and what was left out.

Run `.github/test-cell.sh tools/nodecode-deferred-questions` from the cells
repository.
