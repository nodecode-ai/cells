# nodecode-vt-check

VirusTotal Check: hashes a download locally and looks it up in VirusTotal, and checks links, domains and IPs read-only. A Nodecode rewrite of [Aravinth-Earth/vt-check](https://github.com/Aravinth-Earth/vt-check) by [Aravinth-Earth](https://github.com/Aravinth-Earth).

The file is never uploaded: it is read once for its SHA-256, SHA-1 and MD5, and
only the SHA-256 goes to VirusTotal. Archives are also read in memory, before
you extract anything, for the things a hash cannot tell you.

## Get it

Tell the agent "add the vt-check cell", or run `nodecode add nc://vt-check`.

## Set it up

A free VirusTotal key (4 requests/min, 500/day) comes from
https://www.virustotal.com/gui/my-apikey. Paste it into the cell's `api_key` in
`/settings`, or leave that empty and let `api_key_env` name the environment
variable that holds it (default `VT_API_KEY`).

Without a key the cell still works: it reports the local facts of a file, says
the verdict is `unverified`, and tells the model what is missing. `cache_ttl_hours`
(default 24, 0 disables the cache) decides how long a lookup is reused;
`timeout_seconds` (default 20) bounds one call.

## Use it

Ask in your own words — "is this download safe?", "check this link", "what is
this IP?" — and the model calls one of:

| Tool | What it does |
| --- | --- |
| `vt_check_file` | Hashes one file locally, looks the SHA-256 up, and reads its headers if it is an archive. Nothing is uploaded. |
| `vt_check_url` | A link, a domain or an IP, read-only; defanged indicators are accepted. `submit=true` queues an unseen URL for scanning, which publishes it. |
| `vt_check_ip` | One explicit address: detections, AS owner, country, network. |
| `vt_status` | Whether the key is set and works, the free tier's limits, the cache TTL. |

`/vt-check <path|link|domain|ip>` asks the same thing from the prompt, and
`/vt-check status` checks the key. From eval the verbs answer the same blocks:

```lisp
(vt-check:check-file "~/Downloads/tool.deb")
(vt-check:check-url "hxxps://evil[.]com/x")
(vt-check:check-ip "203.0.113.9")
(vt-check:status)
```

What an archive adds to the hash: a path traversal entry, a symlink, an
encrypted entry, a decompression bomb, a double extension (`invoice.pdf.exe`),
a document whose content is an executable, and — honestly labelled `info` — the
code and binaries a source package is supposed to contain. Nested archives are
read three deep.

A verdict is one of `malicious` (stop, do not open), `suspicious` (an engine
flags it, or an alert finding), `caution` (a warn finding), `unknown`
(VirusTotal has never seen this exact build — normal for a new build, not
evidence of malware and not proof of safety), `unverified` (no key, so only the
local findings apply) or `clean`.
