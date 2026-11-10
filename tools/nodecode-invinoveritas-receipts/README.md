# nodecode-invinoveritas-receipts

Invinoveritas Receipts: a signed, salted receipt for each decision you want to be able to prove. A Nodecode rewrite of [babyblueviper1/hermes-invinoveritas](https://github.com/babyblueviper1/hermes-invinoveritas) by [babyblueviper1](https://github.com/babyblueviper1).

A decision made here — a dangerous command allowed, a deploy refused, a model signing off on a change — is self-graded and leaves no record anyone outside this machine can check. A receipt fixes that: [invinoveritas](https://invinoveritas.dev) signs a commitment to the question, the options, the choice and the SHA-256 of the context, and returns the salt to this machine alone. Anyone can verify the signature for free, without an account; only this machine can open the receipt to say what the decision was about.

## Get it

Tell the agent "add the invinoveritas-receipts cell", or run `nodecode add nc://invinoveritas-receipts`.

## Set it up

Register for a key at [invinoveritas.dev](https://invinoveritas.dev) (free) and put it in the environment variable `api_key_env` names, `INVINOVERITAS_API_KEY` by default. Without a key the cell still runs and still opens the receipts it kept; only asking for a new one refuses, naming the variable. Receipts are paid per call — 100 sats from that account (Lightning, USDC, or card) — and verification is free.

| Setting | Default | What it is |
| --- | --- | --- |
| `api_url` | `https://api.babyblueviper.com` | the service base; the only endpoint called |
| `api_key_env` | `INVINOVERITAS_API_KEY` | the variable holding the key |
| `timeout_seconds` | `10` | one request's ceiling |
| `listed` | `10` | how many kept receipts `/invinoveritas-receipts` shows |

## Use it

Ask for a receipt after a decision worth being able to prove, or call the verb:

```lisp
(invinoveritas-receipts:issue "approve" "rm -rf build/ before the release" :decider "guard" :reference "call-7")
```

The three tools are `issue_receipt`, `verify_receipt` and `check_receipt`; `/invinoveritas-receipts` lists what is kept, `/invinoveritas-receipts <id>` shows one, and `/invinoveritas-receipts verify <id>` checks its signature.

- **issue_receipt** asks for a receipt: paid, one per decision. The question, the options, the choice and a SHA-256 of the context are sent; the context itself is not, and a 32-byte nonce makes its hash unguessable.
- **verify_receipt** posts the kept event to `POST /verify-proof` — free, no key, no account.
- **check_receipt** opens a kept receipt here, with no network: it recomputes the context hash and both salted commitments from the salt, and reads back what the decision was about.

Receipts are kept as one JSON object a line in `invinoveritas-receipts/invinoveritas-receipts.jsonl` under the Nodecode home: the signed event, the receipt, the salt, the choice and the context. Keep that file — it is what opens the receipt later. Nothing else is written, no thread runs between calls, and no receipt ever approves, blocks or delays anything: it records what was decided.

The canonical JSON the hashes are taken over is sorted keys, no spaces, UTF-8 as it stands; the commitments are `sha256(salt + "|" + choice)` and `sha256(salt + "|" + context_sha256)`, which is how a receipt is opened with nothing but a shell and `sha256sum`.

## Test it

```sh
.github/test-cell.sh tools/nodecode-invinoveritas-receipts
```

15 tests, offline: every call is a stubbed HTTP seam and the ledger is a scratch home's.

MIT. The original plugin is babyblueviper1's; see NOTICE.
