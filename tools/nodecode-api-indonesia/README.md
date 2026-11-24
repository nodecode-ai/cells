# nodecode-api-indonesia

API Indonesia: Indonesian public data — administrative regions down to villages, postal codes,
public holidays, prayer times, BMKG earthquakes and weather, schools, hospitals, exchange rates,
minimum wage, halal and BPOM lookups, OJK lists, KBBI, Al-Quran — through API Indonesia's hosted
MCP server. A Nodecode rewrite of [khmuhtadin/api-indonesia-examples](https://github.com/khmuhtadin/api-indonesia-examples) by [khmuhtadin](https://github.com/khmuhtadin).

API Indonesia is one server over Indonesian public data from official sources: Kemendagri for the
38 provinces, 514 kabupaten/kota, 7,285 kecamatan and 83,762 desa/kelurahan with their codes;
Kemendikdasmen for 93,500+ schools by NPSN; Kemenkes for hospitals; BMKG for earthquakes, weather
and warnings; Bank Indonesia for exchange rates; BPJPH and BPOM for halal and product
registrations; DJKI for trademarks; OJK for the legal and illegal investment lists. The original
package is a manifest and a skill pointing at it, with example clients in five languages; the cell
speaks the same two protocols itself, so there is no Node, no `npx mcp-remote` bridge and no
dependency beyond what Nodecode carries.

| Tool | What it does |
| --- | --- |
| `api_indonesia_tools` | The server's own tools, one line each. Call it first; the list is asked once and kept for the process. |
| `api_indonesia_call` | Calls one of them: `tool` is its name exactly as listed, `arguments` its own JSON object. |
| `api_indonesia_get` | Reads one documented REST path with a query object — the fallback when the served tool list lacks what a task needs. |

The server serves its tool set at run time, so the two generic verbs outlive any version of it;
`api_indonesia_get` is the map that does not move.

## Get it

Tell the agent "add the api-indonesia cell", or run:

```sh
nodecode add nc://api-indonesia
```

## Set it up

You need an API Indonesia key, `aip_live_...`, from the dashboard at
https://dashboard.apiindonesia.id — sign up, open **API Keys**, create one. A new account starts
with a one-time 3,000 credit bonus; there is no monthly free tier, a successful request costs 2
credits (Rp2) and a failed one is free.

Put the key in the environment variable the section names, `API_INDONESIA_KEY` by default:

```sh
export API_INDONESIA_KEY=aip_live_...
```

or set `api-indonesia.api_key_env` to another variable's name. The section:

```jsonc
"api-indonesia": {
  "api_key_env": "API_INDONESIA_KEY",              // the variable holding the key
  "url": "https://mcp.apiindonesia.id/mcp",        // the MCP server
  "rest_url": "https://use.apiindonesia.id/api/v1", // the REST base api_indonesia_get appends to
  "timeout_seconds": 60
}
```

Without a key the cell refuses at start, naming that variable, and `/api-indonesia` says whether
one is set. Every call, with its arguments, goes to API Indonesia — `mcp.apiindonesia.id` for the
tools and `use.apiindonesia.id` for the documented paths — and spends the key owner's credits.
Nothing else is sent anywhere.

## Use it

Ask in Indonesian public data terms and the agent lists the server's tools and calls one:

- "Which kabupaten are in Jawa Barat?" — regions down to villages, with Kemendagri codes.
- "What is the postal code of Kelurahan Sukajadi?" — the 83,762 village-to-code records.
- "Is 1 May 2026 a public holiday, and is there cuti bersama around it?"
- "Prayer times for Kota Bandung tomorrow", "the latest BMKG earthquakes", "is there a weather
  warning for this village code (adm4)?"
- "Convert 1,500,000 rupiah to US dollars at Bank Indonesia's rate", "the 2026 minimum wage for
  Papua", "spell this amount in Indonesian", "parse this QRIS code".
- "Is this NIK well formed?", "is this cosmetic halal-certified by BPJPH?", "is this app on OJK's
  illegal list?"

`/api-indonesia` shows the endpoint and whether a key is set; `/api-indonesia tools` lists the
server's tools, `/api-indonesia refresh` asks the server again. From eval:

```lisp
(help :api-indonesia)                             ; the primer the model reads
(nle::find-tool "api_indonesia_tools")            ; what the model calls
```

A NIK or NPWP check only confirms the number is well formed — never that a person or taxpayer
exists — and the answer's metadata names the official source to cite. A 401 means the key is
missing or wrong; a 402 means the account is out of credits.

## What is in it

| File | What it is |
| --- | --- |
| `wire.lisp` | MCP over Streamable HTTP (one POST per message, the event stream read when the answer comes as one, the session id echoed and remade once when it goes stale) and the REST fallback. |
| `cell.lisp` | The section, the manual topic, the three tools and `/api-indonesia`. |
| `test/` | The lifecycle, the missing key, the tool list, a call, the REST path, a refused status and the command, all offline. |

## License

MIT. The original package is Copyright (c) 2026 Khaisa Studio (Khairul Muhtadin): see `LICENSE`
and `NOTICE`.
