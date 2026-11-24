# nodecode-osint-map

OSINT Map: a world situation brief from public feeds, and passive recon, as five tools that need no key. A Nodecode rewrite of [Adolanium/hermes-osint-map](https://github.com/Adolanium/hermes-osint-map) by [Adolanium](https://github.com/Adolanium).

The model gets five tools — `osint_situation`, `osint_events`,
`osint_country`, `osint_recon` and `osint_news` — that answer from USGS
earthquakes, NASA EONET, GDACS disaster alerts, US National Weather Service
alerts, adsb.lol military aircraft, NOAA space weather, world headlines and the
World Bank. `/osint` writes the brief to the operator.

## Get it

Tell the agent "add the osint-map cell", or run `nodecode add nc://osint-map`.

## Set it up

No key is needed: every source is public. The cell runs as soon as its
`osint-map` section exists, and an absent section is a cell that starts with
its defaults — the section names what to change:

```jsonc
{
  "osint-map": {
    "markets": "crypto",             // crypto, or twelvedata to add equities and commodities
    "twelvedata_api_key": "TWELVEDATA_API_KEY",  // the variable holding an optional Twelve Data key
    "user_agent": "nodecode-osint-map/0.1 (+https://nodecode.ai)",
    "timeout_seconds": 15,           // one source's ceiling
    "max_items": 20,                 // how many items a verb lists
    "cache_seconds": 60              // how long one source's answer is reused
  }
}
```

`markets: "twelvedata"` without a key refuses the start, and says which
variable to set; a free key is at twelvedata.com.

## Use it

Ask in the conversation ("what is happening in the Pacific right now", "brief me
on Brazil", "what does CISA know about CVE-2024-3094"), or from eval:

```lisp
(osint:situation)                          ; counts, worst items, Kp, headlines
(osint:events :layer "quakes" :limit 5)    ; most severe first
(osint:events :lat 35.6 :lon 140.1 :radius 500)
(osint:country "BR")                       ; World Bank facts, events, headlines
(osint:news :query "shipping")             ; five world services plus GDELT
(osint:recon "dns" "example.com")          ; dns, rdap, subdomains, cve, ip, github, mac
```

`/osint` with no argument writes the situation brief; with an argument it
searches the headlines for it.

Two things worth telling the model: news incidents and volunteer-fed aircraft
show where reporting or coverage is, not verified events, and a layer that is
down says so in its own line rather than emptying the brief.

## Credits

A Nodecode rewrite of [Adolanium/hermes-osint-map](https://github.com/Adolanium/hermes-osint-map),
commit `8254b9e872c2718e5ea228168db3f75fb77955de`, MIT, Copyright (c) 2026 Adolanium.
The original repository's LICENSE is kept unchanged; what the rewrite carries
over and what it leaves out is in [NOTICE](NOTICE).
