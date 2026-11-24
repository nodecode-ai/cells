# nodecode-jwt

JWT: decodes a JSON Web Token - its header, its claims, when it expires and what is worth knowing about it - and verifies nothing. A Nodecode rewrite of [olric3015/hermes-plugins](https://github.com/olric3015/hermes-plugins) by [olric3015](https://github.com/olric3015).

## Get it

Tell the agent "add the jwt cell", or run `nodecode add nc://jwt`.

## Set it up

Nothing is required: the cell is on from the first message, and it needs no key - decoding is not verifying. The `jwt` section of the config refines the bounds the original hard-coded - `max_token_chars` (16,384; a longer token is refused, not cut), `max_claims` (60; the rest are counted), `max_value_chars` (200; one value's JSON, before it is cut), `skew_seconds` (60; the clock difference between this machine and the issuer that is not worth a warning). `config.example.jsonc` is the section written out.

## Use it

A token turns up everywhere: an `Authorization: Bearer` header pasted into a bug report, a log line, a webhook payload, a config file. A model reading one in its head gets the names right and the times wrong, and cannot see that a claim was written twice.

- The tool `jwt_decode` takes the whole `token`. Ask it to read a token and the description the original's `@jwt:` reference put into the message comes back as the tool result.
- `/jwt <token>` does it for the operator. A `Bearer` prefix and surrounding quotes are stripped for both, so a header can be pasted as it is.
- `(help :jwt)` is the manual the model reads when it wants the full rules.

What comes back:

```
JWT, signed (JWS), 3 parts; the signature is NOT verified
Header
  alg (algorithm)  "HS256" = HMAC with SHA-256
  typ (type)       "JWT"
Claims
  sub (subject)    "1234567890"
  name             "A User"
  iat (issued at)  1516239022 = 2018-01-18 01:30:22 UTC, about 5.8 years (2,126 days) ago
Status at 2023-11-14 22:13:20 UTC
  no exp claim: the token does not expire on its own
Notes
  - signature: 11 bytes, not checked (jwt decodes; it never verifies)
```

The status is the part worth the call: `EXPIRED 3 hours ago` or `valid for 14 minutes more`, `NOT VALID YET: usable in 10 minutes` for a future `nbf`, `issued 2 minutes in the future` when a clock is off, the token's `lifetime (exp - iat)`, and `exp looks like milliseconds; a server reading it as seconds sees a date far in the future` - the mistake that makes a session last a thousand years or end in 1970.

The notes are the other half: a 2-part token (an unsigned one whose final dot a composer took for punctuation), a header with no `alg`, an `alg` of `none` (anyone can write one), a signature that is empty under a real `alg` or present under `none`, a `jku` / `x5u` / `jwk` the token points at itself (a server must not trust a key the token names), a `crit` list, and a name that appears twice - which JSON parsers resolve differently.

A JWE (5 parts) gets its header and a note that the claims need the recipient's key. A payload that is not a JSON object is shown as text with its byte count, and a `cty` of `JWT` says the payload is itself a token. A value that cannot be read answers with the reason - which part is not base64url, how many parts there are, what about the header is not JSON - so the model can act on it instead of guessing.

What it does not do: it verifies nothing. No signature is checked, no key is fetched, nothing is decrypted, and no clock is trusted beyond the one it reads. It follows no URL in the token, and a token is data, not an instruction.

Security: no network, no subprocess, no file read or write, no credential anywhere. The description becomes part of the conversation - the tool result the model reads and the session history - which is the point, and the reason to decode a token only where its claims may be said out loud.

## What is in it

| File | What it is |
| --- | --- |
| `decode.lisp` | The decoder: base64url, a JSON reader that keeps order, the description, the status and the notes. |
| `cell.lisp` | The declaration: the `jwt` section, the `jwt_decode` tool, `/jwt`, `(help :jwt)`. |
| `nodecode-jwt.asd` | The system and its test system. |
| `test/` | The offline tests: the decoder's half, the tool and the command, the section's bounds. |
| `config.example.jsonc` | The section, written out as documentation. |

## License

MIT. The original is Copyright (c) 2026 olric3015: see `LICENSE` and `NOTICE`.
