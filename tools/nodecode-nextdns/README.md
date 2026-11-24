# nodecode-nextdns

NextDNS: a profile's DNS query log, its blocked queries with the blocklist that matched each, and aggregate analytics. A Nodecode rewrite of [shanelord01/hermes-nextdns](https://github.com/shanelord01/hermes-nextdns) by [shanelord01](https://github.com/shanelord01).

## Get it

Tell the agent "add the nextdns cell", or run `nodecode add nc://nextdns`.

## Set it up

Create an API key at the bottom of your account page, [my.nextdns.io/account](https://my.nextdns.io/account), and find your profile id: it is the path segment of your DNS-over-HTTPS URL, `https://dns.nextdns.io/<id>`, and it is shown at the top left of my.nextdns.io. Then:

```jsonc
{
  "nextdns": {
    "api_key_env": "NEXTDNS_API_KEY",   // the variable holding your key
    // "api_key": "...",                // or the key written here instead
    "profile_id": "abc123",              // the id in your DNS-over-HTTPS URL
    "timeout_seconds": 20,               // one request's ceiling
    "max_rows": 200                      // the most log rows one answer carries
  }
}
```

A start with no key anywhere refuses, naming `api_key_env`; an empty profile id is fine, and then `nextdns_profile` lists the profiles the key can see. Both the key and the name the cell calls itself by leave the machine only to `https://api.nextdns.io`, the key in an `X-Api-Key` header: NextDNS rejects a default library user agent with a 403, so `user_agent` is worth leaving as it is.

## Use it

The model has four read-only tools and the operator has `/nextdns`. Nothing here changes a profile: every endpoint is a `GET`.

| Tool | What it answers |
| --- | --- |
| `nextdns_logs` | The query log: timestamp, domain, status, the device that asked, the blocklists that matched and the protocol. Filter by `search`, `status`, `device`, `from`/`to` (ISO-8601 or a relative window like `-1h`), page with `cursor`. `raw` is true by default, so every query is included, not only the navigational ones. |
| `nextdns_blocked` | The same log, filtered to blocked queries, with the blocklist on each row: "was this blocked, and by what". |
| `nextdns_analytics` | Aggregate counts for one metric -- `status`, `domains`, `reasons`, `ips`, `devices`, `protocols`, `queryTypes`, `ipVersions`, `dnssec`, `encryption`, `destinations`. Far cheaper than paging the log for a volume question. |
| `nextdns_profile` | Name, whether logging is enabled, retention, log storage region and active blocklists. With no profile set, the profiles the key can see. |

`/nextdns` (optionally `/nextdns <profile-id>`) answers the profile's query totals:

```text
nextdns: profile abc123
  default: 1234 queries
  blocked: 210 queries
  total: 1444 queries
```

Things worth knowing:

- An empty log is usually logging switched off, or a retention shorter than the window asked about: read `nextdns_profile` first, which is what it is for.
- Devices that query NextDNS directly -- the NextDNS apps, or a DoH/DoT endpoint set on the device -- appear by name. Anything behind a router-level DNS setting shares the router's identity and appears as `__UNIDENTIFIED__`, because NextDNS sees only the WAN address. A `device` argument takes the name the log shows, which is resolved to the id the API filters on, and `__UNIDENTIFIED__` or an id passes through.
- `limit` is raised to 10 (the API refuses less with a 400) and capped at 1000 for the log and 500 for analytics; one answer carries at most `max_rows` rows, and an answer that had to drop some says so.
- A refusal comes back in the API's own words -- `HTTP 401 from the NextDNS API: ...` -- rather than as a bare status code, so a malformed query says what was wrong with it.

## What the plugin had that this does not

The Hermes dashboard tab: its settings form, its **Test connection** button and its recent-blocks panel. A Nodecode cell is configured in `config.jsonc` and answers in the conversation, so the same key, profile and check are the section above and the first `nextdns_profile` call. Everything the tools answer -- the same four endpoints, the same trimming, the same errors -- is carried over.

## License

MIT. See `LICENSE` and `NOTICE`.
