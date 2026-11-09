# nodecode-tab-watchdog

Tab Watchdog: closes stale blank, idle and duplicate Chrome tabs. A Nodecode rewrite of [web3blind/hermes-chrome-tab-watchdog](https://github.com/web3blind/hermes-chrome-tab-watchdog) by [web3blind](https://github.com/web3blind).

A browser driven by an agent keeps a tab for every page it was sent to, and the blanks, the
`about:blank` windows and the three copies of one url are still there tomorrow. This cell watches
one Chromium browser over its DevTools endpoint and says which of its tabs are stale; with your
yes it closes them, and never more than it can account for.

A tab is stale when

- it is a blank page (`about:blank`, a new-tab page) that has been there for `blank_minutes`;
- it is a second copy of a url another tab already shows, and has sat for `duplicate_hours`;
- nothing about it -- title or url -- has changed for `idle_hours`.

A sweep closes at most `max_close` of them, never the last page a browser has open, and never
every blank tab; each one is looked for twice before it is closed and once after, so the answer
says what really went. Ages come from the cell's own looks: Chrome does not tell it which tab was
clicked, so a page being read whose title never changes does look idle. Run the report first.

## Get it

Tell the agent "add the tab-watchdog cell", or run `nodecode add nc://tab-watchdog`.

## Set it up

Start the browser you want watched with a DevTools port, and leave it running:

```sh
chrome --remote-debugging-port=9222
```

Then set the section in `/settings`, or in `~/.nodecode/config.jsonc`:

```jsonc
{
  "tab-watchdog": {
    "endpoint": "http://127.0.0.1:9222", // the browser's DevTools address, loopback only
    "blank_minutes": 30,                 // a blank tab this old is stale
    "duplicate_hours": 2,                // a second copy this old is stale
    "idle_hours": 6,                     // a tab nothing changed in this long is stale
    "max_close": 5,                      // the most one sweep closes
    "auto": false,                       // sweep every sweep_minutes on its own
    "sweep_minutes": 15
  }
}
```

Only `endpoint` matters to reach a browser; the rest default as above. A section left out is a
cell that starts and does nothing until it is set up.

## Use it

The model has two tools, `tabs_inspect` (the report: every open tab, and which ones a sweep
would close) and `tabs_sweep` (the same, then the close; pass `apply` false for a dry run).
The operator has `/tabs` -- the report and a dialog whose first row runs the sweep -- and
`/tabs sweep`, and the same verbs from eval:

```lisp
(tab-watchdog:report)                 ; the page tabs, the stale ones marked
(tab-watchdog:sweep)                  ; what a sweep would close, and why
(tab-watchdog:sweep :apply t)        ; close them; second value: the ids it closed
(tab-watchdog:tabs)                  ; the browser's own /json/list answer, as plists
```

With `auto` on, a worker looks every `sweep_minutes` and says in a notice what it closed. The
ages it judges by are kept in a ledger under the home (`tab-watchdog/ledger.json`), so they
survive a restart; it holds a target id, a url, a title and two times, and nothing else.

### What the rewrite does not do

- **No CDP proxy.** The original sat between the harness's browser tools and the browser and read
each tool's commands off that proxy to know which tab was in use. Nodecode's browser cells do not
dial this cell, so there is nothing to sit between: a tab is judged by its own age here.
- **No observer session, no focus script.** The original injected a script that reported clicks
and focus. This rewrite asks the two DevTools HTTP routes (`/json/list`, `/json/close`) and
nothing else, so it needs no WebSocket and sees no page content.
- **No per-tool leases.** There was nothing to lease once the proxy went; the one rule it served
-- never close a tab a call is using -- is kept the safe way, by closing as little as possible.
- **No cron entry point.** `auto` is the same every-fifteen-minutes sweep, on the cell's own
worker, without a crontab to install.

MIT licensed. See `NOTICE` for what came from the original and what did not.
