# nodecode-letsfg

LetsFG: search and book real flights and hotels from chat through the hosted LetsFG service. A Nodecode rewrite of [LetsFG/LetsFG](https://github.com/LetsFG/LetsFG) by [LetsFG](https://github.com/LetsFG).

LetsFG searches every airline and the major travel sites in one go, prices the bags, checks
every airport in a city and tests split tickets, then books the flight or the room on your own
LetsFG account. This cell is the plugin rewritten natively: the same hosted server, called over
HTTP by the cell itself, the same tools as tools the model can call, and the plugin's
travel-booking skill as this cell's manual topic.

## Get it

Tell the agent "add the letsfg cell", or run `nodecode add nc://letsfg`.

## Set it up

LetsFG signs in with OAuth, so the cell needs a bearer token from a LetsFG sign-in: either an
environment variable that holds one, or a file that holds one.

```jsonc
"letsfg": {
  "enabled": true,
  "server": "https://letsfg.co/mcp",   // the default; the hosted endpoint
  "token_file": "~/.secrets/letsfg",   // or "token_env": "LETSFG_TOKEN"
  "timeout_seconds": 60                // a hotel search can take half a minute
}
```

Then `(restart-cells "nodecode-letsfg")`. No card is needed to sign in; when a booking needs one,
its result gives a link to add it on letsfg.co.

## Use it

Ask for a trip:

- "The cheapest flight from Warsaw to London next Friday, back Sunday."
- "A hotel in Paris for 12-14 November, two adults, breakfast, under 400 EUR."
- "Where in Southern Europe can I fly from Berlin for four nights in October?"
- "Book option A for me."

The model gets five tools: `letsfg_search_flights`, `letsfg_search_hotels`, `letsfg_results`,
`letsfg_book` (the write path: book, cancel, stop, answer a booking question) and `letsfg_call`
(every other LetsFG tool by name). From eval: `(letsfg:tools)` lists what the account can do,
`(letsfg:call-tool "find_airports" :query "Warsaw")` calls one, and `/letsfg status` says
whether the token was read.

**Booking is real.** A search changes nothing; a booking holds money on your saved card, and it
starts only when you press Approve in the LetsFG e-mail — nothing in the chat can start or
approve one. Card details never enter the chat, and the cell books only what you picked.

## Left out

The plugin's Hermes-specific pieces: the manifest, the MCP connector declaration (this cell is the
client) and the three tools that exist only for LetsFG's in-app cards in other clients.

## License

MIT. The original plugin is Copyright (c) 2026 LetsFG (MENTAL BALANCE sp. z o.o.): see `LICENSE`
and `NOTICE`.
