# nodecode-alice-voice

A Yandex Alice voice skill's requests, answered as turns in one Nodecode session. A Nodecode rewrite of [bablobanov/hermes-alice-voice](https://github.com/bablobanov/hermes-alice-voice) by [bablobanov](https://github.com/bablobanov).

You speak to Alice; Alice passes the recognized words to a private skill; the skill posts them to this cell; the cell puts them in a session as a turn. The answer arrives wherever that session speaks — its shell, or the channel room it is bound to — and Alice only says it was accepted.

## Get it

Tell the agent "add the alice voice cell", or run `nodecode add nc://alice-voice`.

## Set it up

The cell is an endpoint with no key of its own: it only listens, so nothing here dials a provider.

1. **secret** — 32 random bytes as 64 hex characters, `openssl rand -hex 32`. It is the bearer in the webhook path, so the path *is* the credential: keep it out of screenshots and logs.
2. **skill_id** — your private Alice skill's id, as the skill sends it.
3. **session** — the Nodecode session the requests become turns in (default `alice`); it is created on first use.
4. **owner_user_id** — optional. Empty accepts any Alice account that holds the path; set it to accept only one.

In your private skill, point the webhook at `https://<your proxy>/alice/<secret>`, and have the proxy forward to this Nodecode's gateway port with `Host: 127.0.0.1:<port>` — the gateway refuses any other Host.

## Use it

- Alice says "Запрос принят для обработки. Ответ будет в Nodecode" once a turn is scheduled. It acknowledges scheduling, not a finished answer.
- "помощь", "выход" and "ответ готов?" are answered by the cell itself and never become turns; so is an empty utterance.
- A leading `/` is refused: a voice request cannot type a slash command.
- `/alice` shows the endpoint and the session it feeds; `/alice say <text>` sends one utterance down the same path, so you can try the whole chain without speaking.

Replies are remembered by skill, account, session and message id — the last 32, in memory — so a retried message is one turn, not two.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the route, the gates, the controller words and `/alice`. |
| `test/cell-test.lisp` | The slice: lifecycle, the secret and skill gates, deduplication, the refusals, `/alice say`. |

## License

MIT. `cell.lisp` is a rewrite of the original's `bridge.py`; see `NOTICE` and `LICENSE`.
