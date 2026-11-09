# nodecode-tls-posture

TLS posture audit: certificate expiry, key strength, weak protocols and the certificate a server serves. A Nodecode rewrite of [Pacific-Cloud-Solutions/hermes-plugins](https://github.com/Pacific-Cloud-Solutions/hermes-plugins) by [Pacific-Cloud-Solutions](https://github.com/Pacific-Cloud-Solutions).

One tool, `tls_posture`: a read-only audit of the certificates and server
TLS directives under a tree. It never modifies anything and never connects to
anything — it parses files, it does not perform a handshake — so a finding
about a certificate is a finding about a file on this machine.

| Check | What it says |
| --- | --- |
| `TLS-001` | the certificate has expired |
| `TLS-002` | it expires within the window (30 days by default; 7 or fewer is HIGH) |
| `TLS-003` | its validity window has not opened yet |
| `TLS-004` | an RSA/DSA key under 2048 bits or an EC key under 224 (CRITICAL under 1024) |
| `TLS-005` | it is signed with MD5 or SHA-1 |
| `TLS-006` | it is self-signed (reported, not graded as a defect) |
| `TLS-007` | a server serves a leaf where `fullchain.pem` sits beside it, or a single certificate where nginx needs a chain |
| `TLS-008` | a server config names a certificate that is missing, or outside the audited tree |
| `TLS-009` | a server serves a certificate this audit never read (a coverage gap, INFO) |
| `TLS-010` | a deprecated protocol is ENABLED |
| `TLS-011` | a weak cipher suite is OFFERED |
| `TLS-012` | a TLS server config says nothing about protocol versions (INFO, opt-in) |

Ask "is my certificate about to expire?", "did nginx get deployed with a
1024-bit key?", "will this still negotiate TLS 1.0?" — and the model picks
the tool. A report always carries a coverage block naming what ran, what
could not run and why: a check that could not run is reported as skipped,
never as passing, and no verdict ever says the target is secure.

## Get it

Tell the agent "add the tls-posture cell", or run `nodecode add nc://tls-posture`.

## Set it up

One setting: `root`, the tree to audit. `/` for this host; the path a
mounted image is under for someone else's. In `/settings` or in the
`tls-posture` section of `config.jsonc`:

```jsonc
"tls-posture": {
  "root": "/",
  "expiry_days": 30,
  "include_info": false
}
```

| Setting | Default | What it does |
| --- | --- | --- |
| `root` | — (required) | the filesystem tree the audit reads |
| `expiry_days` | 30 | report certificates expiring within this many days |
| `include_info` | off | add INFO-severity observations |

## Use it

```
tls_posture                                  # audit the configured root
tls_posture root="/mnt/suspect" include_info=true
```

or `/tls` for the few lines an operator reads first, and `/tls /mnt/suspect`
to audit another tree. The answer is JSON: `grade` (`ACTION REQUIRED`,
`REVIEW`, `HARDEN`, `NO FINDINGS`, `INCONCLUSIVE`), `verdict`, findings
worst first with the file and line each came from, and `coverage`.

Only a FIXED list of paths is read under the root — `etc/ssl/certs`,
`etc/pki/tls/certs`, the Let's Encrypt live directories, and the nginx and
Apache configuration — so this is an audit, not a file reader, and a path
that escapes the root is refused rather than followed.

## What it deliberately gets right

**A cipher string is a list of offers and exclusions.** `HIGH:!aNULL:!MD5`
names MD5 in order to disable it; reporting that line would report the fix
as the bug. Only suites actually offered are counted.

**The two servers use opposite polarity.** nginx writes the versions it
enables (`ssl_protocols TLSv1.2 TLSv1.3;`), Apache the ones it removes
(`SSLProtocol all -SSLv3 -TLSv1`). A leading `-` or `!` is disabled.

**`TLSv1` is a substring of `TLSv1.2`.** Protocol tokens are compared whole,
so a host that requires TLS 1.2 is never reported as permitting TLS 1.0.

**A fact about a file is not a fact about a service.** The certificate a
server actually presents is joined to the file checks: the finding names the
`nginx.conf:12` block that serves it, and carries the `served` tag.

No configuration beyond `root`, no worker, no network, no dependency
beyond Nodecode itself — the certificate decoder is one small DER walk.

Run `.github/test-cell.sh tools/nodecode-tls-posture` from the cells
repository. The tests build a scratch tree of certificates and server
configs byte by byte and call the tool through the real dispatch, so no
network, clock or file of the operator's is involved.
