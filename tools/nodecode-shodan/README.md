# nodecode-shodan

Shodan: host intel, search, counts, DNS, CVEs and scanning from Shodan's API. A Nodecode rewrite of [Adolanium/hermes-plugin-shodan](https://github.com/Adolanium/hermes-plugin-shodan) by [Adolanium](https://github.com/Adolanium).

Twelve tools, the same names and arguments as the original:

| Tool | What it does | Cost |
| --- | --- | --- |
| `shodan_host` | Ports, products, versions, certificates, hostnames, org, ASN, geo, tags and CVEs for an IP or hostname. | free |
| `shodan_count` | Totals and facet breakdowns for a query. | free |
| `shodan_search` | Matching hosts, one line each. | 1 credit per filtered query, +1 per page |
| `shodan_dns` | Subdomain enumeration (1 credit), free forward and reverse lookups. | 1 / free |
| `shodan_cve` | CVEDB: CVSS, EPSS, CISA KEV, ransomware, CPEs. | free, no key |
| `shodan_account` | Plan, credits left, and this session's own ledger. | free |
| `shodan_meta` | Validate a query's syntax; list filters, facets, crawled ports and protocols. | free |
| `shodan_scan` | Submit, poll and list on-demand scans. | one scan credit per IP |
| `shodan_alert` | Network monitors: list, info, create, delete, triggers. | free |
| `shodan_exploits` | Exploit search for a product or a CVE. | free |
| `shodan_query` | Shodan's community directory of saved query strings. | free |
| `shodan_trends` | Match counts month by month for a query. | free |

Ask "what does 1.1.1.1 expose?", "how many RDP servers are there in Germany?",
"is CVE-2021-44228 exploited in the wild?", "what subdomains does example.com
have?" — and the model picks the tool.

## Get it

Tell the agent "add the shodan cell", or run `nodecode add nc://shodan`.

## Set it up

A key is optional. Without one the cell still answers `shodan_cve` in full and
`shodan_host` from **InternetDB** (ports, CPEs, tags and CVE ids only). With a
key from [account.shodan.io](https://account.shodan.io) you also get search,
DNS enumeration, banners, products, versions, certificates, scans and alerts.

Name the key, never paste it into the config: export it in the shell that
starts Nodecode, then, in `/settings` or in the `shodan` section of
`config.jsonc`:

```jsonc
"shodan": {
  "api_key_env": "SHODAN_API_KEY",
  "verbosity": "summary"
}
```

| Setting | Default | What it does |
| --- | --- | --- |
| `api_key_env` | `SHODAN_API_KEY` | the environment variable holding the key |
| `api_key` | — | a key written here instead; the environment variable wins |
| `verbosity` | `summary` | `summary`, `detail` (certificates, headers) or `raw` |
| `max_result_chars` | 24000 | one answer's character budget |
| `query_credits` | 50 | query credits one conversation may spend |
| `scan_credits` | 25 | scan credits one conversation may spend |
| `scan_enabled` | off | allow on-demand scanning |
| `scan_allowlist` | empty | the IPs or CIDR ranges scanning may target |
| `timeout`, `retries`, `rate_limit`, `cache_ttl` | 30, 2, 1/s, 900 | the wire |

## Use it

The model calls the tools. A person can call the same verbs from eval:

```lisp
(shodan:host (nlk:contract-decode (nlk:contract 'shodan:host-lookup)
               (nlk:make-json-object "ip" "1.1.1.1")))
(shodan:count-hosts (nlk:contract-decode (nlk:contract 'shodan:host-count)
                      (nlk:make-json-object "query" "port:22" "facets" "version:5")))
```

`/shodan` answers in chat without spending a credit for the obvious things:

```
/shodan 1.1.1.1              host intel for an IP or hostname (free)
/shodan apache country:DE    free match count, with breakdowns
/shodan info                 plan and remaining credits
/shodan budget               this session's credit ledger
```

`(help :shodan)` is the model's manual for the twelve verbs, kept while the
cell runs.

### What the budget is for

Shodan credits are real money: a Membership account gets 100 query credits a
month, and an agent paging through a broad search can spend them in a minute.
So the cell keeps its own ledger, shows the balance falling on every
credit-costing answer, and refuses rather than cross the line — saying what to
do instead. Most Shodan questions have a free answer: `shodan_count` takes the
same query and facets and costs nothing, and host lookups are free.

### What a host answer keeps

One `/shodan/host/{ip}` response for a busy host is hundreds of kilobytes of
base64 favicons, certificate chains and HTTP bodies. The summary keeps
identity, location, tags, CVEs worst-first and one tight line per service;
`detail` adds certificates and headers; `raw` is Shodan's own JSON, fitted to
`max_result_chars`. Scan results land in the same place: once a scan reports
DONE, `shodan_host` reads the targets.

Run `.github/test-cell.sh tools/nodecode-shodan` for the cell's tests: every
Shodan endpoint is a stub, so no key and no network are needed.
