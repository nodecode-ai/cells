# nodecode-mask

Mask: phones, emails, ID cards, bank cards and API keys masked into numbered placeholders before the text reaches the model. A Nodecode rewrite of [PerryLink/dsh-mask](https://github.com/PerryLink/dsh-mask) by [PerryLink](https://github.com/PerryLink).

## Get it

Tell the agent "add the mask cell", or run `nodecode add nc://mask`.

## Set it up

Nothing is required: with an empty section the cell masks mainland mobile
numbers, email addresses, 18-character ID card numbers, bank card numbers and
the visible shapes of an API key in every user message before the request
leaves.

- `mask.entities` — the detections to run, any of `phone`, `email`, `id-card`,
  `bank-card`, `key`, `ip`. The default is every one but `ip`; name it and
  dotted IPv4 addresses are masked too.
- `mask.enabled` — mask at all; `/mask off` flips it for one run.
- `mask.restore-dir` — where `/mask restore` writes its plaintext file. Empty
  (the default) means a folder of its own in the temporary directory.
- `mask.max-entries` — how many placeholders one run remembers for a restore
  (500 by default).

Set them in `/settings` or in `config.jsonc`:

```jsonc
"mask": { "entities": ["phone", "email", "id-card", "bank-card", "key", "ip"],
          "restore-dir": "~/.cache/mask-restore" }
```

## Use it

Every user message of every request is masked on its way out, in a copy: the
log keeps what was written, so `/sessions`, the transcripts and the editable
context still show the original and an answer can be read against it. Only user
text is touched — the system prompt, assistant turns and tool results pass as
they are.

- `/mask` — the state: on or off, how many values replaced, by kind.
- `/mask on` / `/mask off` — mask, or stop masking, for this run.
- `/mask TEXT` — that text as it would leave, with what was found.
- `/mask restore TEXT` — the placeholders in `TEXT` put back, written to a
  file and named; the plaintext is never printed, so it stays out of the log.
- `mask_test` — the tool: the same preview for text the model holds, with the
  count and the kind of each replacement.
- `(help :mask)` — the manual, for the model.

A detected span becomes a numbered placeholder, `<PHONE_1>`, `<EMAIL_2>`,
`<KEY_1>`: counted per kind and per run, and the same words always get the
same number, so a placeholder means one thing across a whole session. Where
two detections overlap, the earlier one wins and, at the same start, the more
confident one: 18 digits are an ID card before they are a bank card.

**It fails open.** A detector that throws passes the original text through,
counts the failure and says it once in the operator's board; `/mask` reports
the count and the last class. Nothing here is a confidentiality barrier: it is
a habit that keeps the obvious personal data out of the routine request, and
the local log still has all of it.

The patterns are the original's ten, in cl-ppcre, and nothing leaves for
anywhere but the provider the request was already going to.

## What the rewrite does not do

The original could also mask tool results (its `tools` scope) and kept a
per-session restore table on disk. Here the wire fold is the one seam, so tool
results go out whole, and the placeholders of a run live in the image: a
restart starts the numbering again, which is why `/mask restore` is offered
for the text you have in hand rather than for an old transcript.

## Tests

```sh
.github/test-cell.sh tools/nodecode-mask
```

Offline, in a scratch home: lifecycle, the ten detectors and what they leave
alone, the overlap rule, the numbering, the advice on the fold (user text
only, a copy, the fold's second value untouched), a detector that throws, the
tool, the command, and the restore file.
