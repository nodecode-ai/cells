# nodecode-secure-audit

Secure Audit: scans text for prompt-injection, masks Chinese PII, and audits this home's config. A Nodecode rewrite of [PensiveFei/dsh-secure-audit](https://github.com/PensiveFei/dsh-secure-audit) by [PensiveFei](https://github.com/PensiveFei).

## Get it

Tell the agent "add the secure audit cell", or run `nodecode add nc://secure-audit`.

## Set it up

Nothing is required: with no section the cell runs on five defaults. A
`secure-audit` section of `~/.nodecode/config.jsonc` moves its lines and its
mask:

```jsonc
{
  "secure-audit": {
    "block_score": 80,        // 0-100; a critical rule blocks on its own
    "review_score": 50,
    "scan_max_length": 200000,
    "mask_char": "*",
    "audit_path": ""          // another home to audit; empty = your own
  }
}
```

## Use it

Three tools and one command, all local: no service, no model, no key, and
nothing is written.

- `security_scan_text` answers allow, review or block with a risk score and
one line per rule that fired (English and Chinese), after folding zero-width
characters, full-width letters and lookalike Cyrillic and Greek ones; a long
base64 run in the text is decoded and scanned too. A text past
`scan_max_length` is reported as incomplete rather than clean.
- `security_redact_text` masks what its patterns and their checks agree is
personal: email addresses, credentials in a URL, vendor API keys, mainland ID
cards (the check digit decides), bank cards (Luhn decides), mobile numbers and
IPv4 addresses.
- `security_audit` reads a Nodecode home and names what is risky there:
config values that look like secrets (names, never values), a config other
users can read, secret-like environment variables, how many cell folders are
installed, and a gateway bound to every interface.

`/secure-audit` runs the audit; `/secure-audit TEXT` scans the text you
typed. `(help :secure-audit)` is the manual.
