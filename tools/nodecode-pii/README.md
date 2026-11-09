# nodecode-pii

PII redaction: masks emails, phone numbers, card numbers, IBANs, IP addresses and the names you list in the text you send. A Nodecode rewrite of [lukefx/hermes-pii](https://github.com/lukefx/hermes-pii) by [lukefx](https://github.com/lukefx).

## Get it

Tell the agent "add the pii cell", or run `nodecode add nc://pii`.

## Set it up

Nothing is required: with an empty section the cell masks all five kinds out of the box, and the cell is on as soon as its folder is.

- `pii.entities` — the detections to run, any of `email`, `phone`, `card`, `iban`, `ip`. Narrow it, and the rest stop looking.
- `pii.terms` — names, handles and codenames to mask as `<PERSON>`, wherever they appear as whole words.

The names setting is how a name gets masked: no language model runs here, so a name nobody listed is not a name. Set it in `/settings` or in `config.jsonc`:

```jsonc
"pii": { "entities": ["email", "phone", "card", "iban", "ip"],
         "terms": ["Giulia", "Rossi"] }
```

## Use it

Every user message of every request is scrubbed on its way out, in a copy: the log keeps what was written, so `/sessions`, the transcripts and the editable context still show the original and an answer can be read against it. Only user text is touched — the system prompt, assistant turns, tool calls and tool results pass as they are.

- `/pii` — what is masked, how many messages so far, the last detector failure.
- `/pii TEXT` — that text as it would leave, with what was found.
- `redact_text` — the tool: the same preview for text the model has in hand, plus `terms` for one-off names.
- `(help :pii)` — the manual, for the model.

An email address, a phone number (nine digits or more, written with a leading `+` or separators), a card number (13 to 19 digits that pass the checksum), an IBAN (a check remainder decides) and a dotted IPv4 address become `<EMAIL_ADDRESS>`, `<PHONE_NUMBER>`, `<CREDIT_CARD>`, `<IBAN_CODE>` and `<IP_ADDRESS>`. Where two detections overlap, the union of their spans is replaced once, and the structured kind names it.

**It fails open.** A detector that throws passes the original text through, counts the failure and says it in the operator's board; `/pii` reports the count and the last class. Nothing here is a confidentiality barrier: it is a habit that keeps the obvious personal data out of the routine request, and the local log still has all of it.

No service, no model, no socket: the detectors are string rules in this folder, and nothing leaves for anywhere but the provider the request was already going to.

## Tests

```sh
.github/test-cell.sh tools/nodecode-pii
```

Offline, in a scratch home: lifecycle, the detectors and what they leave alone, the advice on the fold (user text only, a copy, the fold's second value untouched), a detector that throws, the tool and the command.
