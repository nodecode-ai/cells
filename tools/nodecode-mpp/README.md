# nodecode-mpp

MPP Payments: HTTP that pays its own 402 challenges. A Nodecode rewrite of [tempoxyz/hermes-mpp](https://github.com/tempoxyz/hermes-mpp) by [tempoxyz](https://github.com/tempoxyz).

An API that charges per request answers `402 Payment Required` with a
`WWW-Authenticate: Payment` challenge from the Machine Payments Protocol. This
cell gives the model one tool, `mpp_fetch`, that sends the request, reads the
challenge, checks it against an origin allowlist and an amount cap, asks your
payer command to sign the charge, and retries the request once with the
credential — so a paid API answers like any other API.

## Get it

Tell the agent "add the mpp cell", or run `nodecode add nc://mpp`.

## Set it up

The `mpp` section of `~/.nodecode/config.jsonc`:

```jsonc
{
  "mpp": {
    "payer_command": "~/.nodecode/mpp-pay",   // reads the challenge JSON on stdin, prints the credential
    "allowed_origins": ["https://mpp.dev"],  // exactly which origins it may pay; empty pays any
    "max_amount": 1000,                      // the most one payment may cost, in the challenge's units; 0 = no cap
    "timeout_seconds": 60
  }
}
```

`payer_command` is what holds the wallet: it reads one challenge on stdin and
prints the `Authorization` value on stdout — `Payment <token>`, or a bare
token, or `{"authorization": "…"}`. The JSON it reads carries `url`,
`http_method`, `amount`, `currency`, `recipient` and the whole decoded
`challenge` (id, realm, method, intent, `request`, `expires`). A Tempo charge
is signed with the `pympp` library the original plugin depends on, so a payer
is a dozen lines of Python over `mpp.methods.tempo`; any MPP client will do.

`allowed_origins` is the one bound that matters: empty, any origin that
presents a readable challenge may be paid. `max_amount` is the second; a
challenge whose amount it cannot read counts as over it.

## Use it

Ask for the paid URL: "fetch https://mpp.dev/api/ping/paid". The tool answers
the response as JSON — `status`, `url`, `headers`, `body`, `truncated`,
`paid`, `payment` — and, when it did not pay, `challenges` (what was asked and
by which method) and `refusal` (why not), so the model can decide to raise the
cap or leave it.

`/mpp` alone prints the wallet and its limits; `/mpp <url>` fetches one URL
the same way and reports what it cost.

Headers a key could hide in (`authorization`, `cookie`, `token`, …) are not
repeated in the answer, and a body is cut to `max_body_chars`.
