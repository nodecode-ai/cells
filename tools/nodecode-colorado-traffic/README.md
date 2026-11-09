# nodecode-colorado-traffic

Colorado Traffic: live Colorado highway events from CDOT's COtrip feed, split into what is happening now, what is scheduled, and what is only a Waze report. A Nodecode rewrite of [drkpxl/colorado-traffic](https://github.com/drkpxl/colorado-traffic) by [drkpxl](https://github.com/drkpxl).

CDOT publishes Colorado's road events as public GeoJSON with no key and no
account. The cell asks it one question: what is on this stretch of highway,
right now? The default corridor is I-70 mile points 259-279, C-470 at Golden
through Quebec Street in east Denver.

Each event is tagged:

| Tag | Meaning |
| --- | --- |
| `[now]` | Its schedule covers this minute |
| `[upcoming]` | Scheduled, not started |
| `[ended]` | Over, and left out unless it was asked for |

The tag comes from the event's own schedule, never from CDOT's `active` flag,
which is true for future night work. Waze reports are headed "Waze via COtrip"
because that is what they are: CDOT's republication of unverified crowd
reports, not CDOT-confirmed incidents. Direction comes from the route's
positive bearing, so positive is eastbound on I-70 and northbound on I-25.

## Get it

Tell the agent "add the colorado-traffic cell", or run:

```sh
nodecode add nc://colorado-traffic
```

## Set it up

Nothing is required: no key, no account, one host
(`api-511x-co.carsprogram.org`). The section only changes what a bare query
asks for.

```jsonc
{
  "colorado-traffic": {
    "corridor": "i70-golden-denver",  // or i70-golden-i25
    "layers": [],                     // empty: incidents, closures, work, waze
    "max_items": 12,                  // events listed per layer
    "timeout_seconds": 40
  }
}
```

## Use it

The model has one tool, `cotrip_events`, and the operator has one command.
Both take a corridor, or a route and a mile-point box.

```
/traffic                              the default corridor
/traffic i70-golden-i25               another shipped corridor
/traffic I-25 210 216                 any route, any mile-point box
```

The same query in eval, where `(help :colorado-traffic)` is the manual:

```lisp
(cotrip:events)                                        ; the default corridor
(cotrip:events :corridor "i70-golden-i25")
(cotrip:events :route "I-25" :min-mp 210 :max-mp 216)
(cotrip:events :layers "work,closures" :include-ended t)
```

Layers: `incidents`, `closures`, `work`, `waze`, `winter`, `chains`. The
answer is text: a heading, a section a layer, and one line an event with its
tag, direction, mile points, the schedule in US Mountain time and its COtrip
link. An empty layer says so rather than going missing.

Two things it does not do. It publishes no speeds — COtrip draws Google
traffic tiles on its own site, and the plugin did not scrape them — so a
travel time is the agent's own lookup, not this cell's number. And it sets up
no watcher: a poll every five minutes that notifies on a new closure is the
original's documented cron, and here it is `nodecode-cron`'s job, with this
cell as the step it runs.

## What changed from the plugin

The plugin was a skill and one Python script the agent ran through its
terminal. A cell runs in the image the model already talks to, so the same
program is one Lisp verb, and the tool and the command are that verb. The
filter, the buckets, the direction rule, the Waze label and the COtrip
caveats are the original's; the details are in [NOTICE](NOTICE).

## Tests

```sh
.github/test-cell.sh tools/nodecode-colorado-traffic
```

Offline: the one HTTP call is stubbed with COtrip answers written as the wire
writes them, so the corridor filter, the schedule outranking the `active`
flag, the Waze label, the direction rule, the contract, the command and the
US Mountain stamp are all asserted without a road.
