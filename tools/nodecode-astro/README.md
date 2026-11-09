# nodecode-astro

Astro Agents: exact Western and Vedic astrology charts and verifiable planetary timing, from a hosted server that computes them from the NASA/JPL DE440 ephemeris. A Nodecode rewrite of [aidatatools-dev/astro-agents-mcp](https://github.com/aidatatools-dev/astro-agents-mcp) by [aidatatools-dev](https://github.com/aidatatools-dev).

## Get it

Tell the agent "add the astro cell", or run `nodecode add nc://astro`.

## Set it up

The original was a declaration of the Astro Agents MCP server and a skill; there is no account and no API key. One setting is enough:

1. In `/settings`, put the server's address in the `astro` section's `endpoint`: `https://astro-agent.dev/mcp`.
2. Optionally, a prepaid credits token in `credits_token` (or in a file named by `credits_token_file`) pays for the paid tools as you go.

The app's cheap tools — `resolve_birth_time`, `planet_positions`, `sky_today`, `aspects`, `nakshatras`, `panchang` — give one free call per client. Every other tool is paid, from $0.01 to $0.50 a call, and answers `payment_required` with its price; the cell says so and does not retry. Nothing else is needed, and no key is ever stored in the folder.

## Use it

| Tool | Use |
| --- | --- |
| `astro_chart` | a Western natal chart, planet positions, or aspects, from a local date-time and a place |
| `astro_sky` | the sky now, and the ingresses, stations, lunations and aspects of the next days; no birth data |
| `astro_kundli` | a Vedic kundli (D1 and D9), or all sixteen divisional charts with `full` |
| `astro_call` | any of the server's 24 tools by name: `transits`, `synastry`, `panchang`, `vimshottari_dasha`, `doshas`, `yogas`, `kp_chart`, `gun_milan`, `vedic_report`, `nakshatras`, `resolve_birth_time`, `planetary_lines`, `astro_event_calendar`, `sky_analogues`, `token_chart`, `token_synastry`, `astro_catalog`, `verify_result_hash` |

From the keyboard, `/astro` reads today's sky, `/astro tools` lists the 24 tools and which of them have a free call, and `/astro 1990-05-15T14:30 48.8566 2.3522` casts a birth chart from a time and a place. `/astro credits` says where the credits token in use came from, in words, never the token itself.

The skill's rules are the manual topic, so they ride every request as one line and arrive whole when the model asks: pass the local clock time and decimal latitude and longitude (north and east positive) and never convert to UTC; a chart without a birth time is cast at noon, and the ascendant is not reliable then; quote positions and scores exactly as returned and mention `meta.result_sha256` when the person wants something verifiable; one free call per cheap tool, then a stated price; Western charts are tropical and Vedic ones sidereal, and the two are never mixed in one answer. The results are data for the person's own analysis, never a trading signal.

### The tools are the server's own

The original added the server to the harness and taught the model the skill by hand. This cell speaks the same server's MCP wire itself through `nlk:http` — one JSON-RPC POST per message, `initialize` and `notifications/initialized`, `tools/call`, and an answer that is either a JSON object or a one-event stream — so it needs no Node runtime, no MCP client and no separate server process. Every result carries the server's `endpoint`, `result` and `meta`, and the cell puts `meta.result_sha256` on the first line the model reads.

## Not ported

Nothing the original reached is missing: all 24 server tools are callable, and the skill's rules are `(help :astro)`. What changed shape is the packaging — a Nodecode section instead of `mcp.json`, and four tools instead of a deferred 24-tool catalog — and the x402 wallet flow, which stays on the server: a paid call says what it costs, and a credits token in the section pays for it.
